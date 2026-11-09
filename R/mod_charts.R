# mod_charts.R --------------------------------------------------------------
# The chart studio. The user selects variables; the app recommends a chart type
# (Section 4 rules) and exposes the full aesthetic pipeline. Live preview, then
# add to the Word report.

mod_charts_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Chart studio"),
      shiny::uiOutput(ns("vars_ui")),
      shiny::uiOutput(ns("panel_ui")),
      shiny::div(class = "alert alert-info", shiny::textOutput(ns("rationale"))),
      shiny::selectInput(ns("type"), "Chart type", choices = NULL),
      shiny::tags$details(
        shiny::tags$summary("Advanced: show all chart types"),
        shiny::helpText(
          "By default only the chart types suited to your selected variables are ",
          "offered. Tick this to override and choose from every chart type."),
        shiny::checkboxInput(ns("show_all"), "Show all chart types", value = FALSE)
      ),
      shiny::uiOutput(ns("colour_ui")),
      shiny::uiOutput(ns("facet_ui")),
      shiny::tags$hr(),
      shiny::strong("Aesthetics"),
      shiny::selectInput(ns("palette"), "Palette", choices = names(CHART_PALETTES)),
      shiny::selectInput(ns("theme_base"), "Theme", choices = CHART_THEMES),
      shiny::sliderInput(ns("base_size"), "Text size", 8, 22, 18, step = 1),
      shiny::selectInput(ns("legend_pos"), "Legend",
                         choices = c("right", "bottom", "top", "left", "none")),
      shiny::sliderInput(ns("alpha"), "Opacity", 0.1, 1, 0.85, step = 0.05),
      shiny::sliderInput(ns("point_size"), "Point size", 0.5, 5, 2, step = 0.5),
      shiny::sliderInput(ns("bins"), "Histogram bins", 5, 80, 30, step = 1),
      shiny::checkboxInput(ns("add_trend"), "Trend line (scatter)", TRUE),
      shiny::checkboxInput(ns("stacked_fill"), "Show as proportions (bars)", FALSE),
      shiny::checkboxInput(ns("flip"), "Flip coordinates", FALSE),
      shiny::checkboxInput(ns("show_grid"), "Grid lines", TRUE),
      shiny::checkboxInput(ns("data_labels"), "Data labels (values on bars/points)", TRUE),
      shiny::tags$hr(),
      shiny::textInput(ns("title"), "Title", ""),
      shiny::textInput(ns("subtitle"), "Subtitle", ""),
      shiny::textInput(ns("caption"), "Caption", ""),
      shiny::textInput(ns("xlab"), "X-axis label", ""),
      shiny::textInput(ns("ylab"), "Y-axis label", ""),
      shiny::tags$hr(),
      shiny::actionButton(ns("add"), "Add to report",
                          icon = shiny::icon("plus"), class = "btn-primary"),
      shiny::downloadButton(ns("dl_png"), "Download PNG")
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput(ns("kind_note")),
      shiny::plotOutput(ns("plot"), height = "560px"),
      mod_interpret_ui(ns("interpret"), title = "AI interpretation of this chart")
    )
  )
}

mod_charts_server <- function(id, data, types, outcomes = NULL, panel = NULL,
                              add_report) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    panel_spec <- shiny::reactive({
      if (is.null(panel)) return(list(id = NULL, time = NULL, event = NULL))
      panel()
    })
    outcome_candidates <- shiny::reactive({
      if (is.null(outcomes)) return(default_outcome_candidates(data(), types()))
      outcomes()
    })
    chart_types <- shiny::reactive({
      tp <- types(); df <- data()
      selected <- intersect(outcome_candidates(), names(df))
      for (v in selected) {
        opts <- outcome_type_options(df[[v]])
        if (length(opts)) tp[[v]] <- preferred_outcome_type(df[[v]], tp[[v]])
      }
      tp
    })
    chart_data <- shiny::reactive({
      df <- data(); tp <- chart_types()
      selected <- intersect(outcome_candidates(), names(df))
      for (v in selected) {
        if (tp[[v]] %in% c("continuous", "count", "binary", "nominal", "ordinal"))
          df[[v]] <- coerce_outcome_vector(df[[v]], tp[[v]], v)
      }
      df
    })
    panel_mode <- shiny::reactive(panel_active(panel_spec()))

    reserved <- shiny::reactive(reserved_analysis_vars(panel_spec()))
    output$kind_note <- shiny::renderUI({
      note <- analysis_kind_note(panel_spec())
      if (is.null(note)) return(NULL)
      shiny::div(class = "alert alert-warning", shiny::HTML(note))
    })
    analysable <- shiny::reactive({
      tp <- chart_types(); df <- data(); shiny::req(tp)
      base <- names(tp)[vapply(tp, is_analysable_type, logical(1))]
      selected <- intersect(outcome_candidates(), names(df))
      safe <- selected[vapply(selected, function(v) length(outcome_type_options(df[[v]])) > 0,
                              logical(1))]
      setdiff(unique(c(base, safe)), reserved())
    })
    cat_vars <- shiny::reactive({
      tp <- types(); setdiff(names(tp)[vapply(tp, is_categorical_type, logical(1))], reserved())
    })
    panel_group_vars <- shiny::reactive({
      ps <- panel_spec()
      vars <- cat_vars()
      if (!panel_has_subject_time(ps)) return(vars)
      keep <- vapply(vars, function(v) {
        tryCatch(panel_variable_status(data(), ps$id, ps$time, v)$status == "time-invariant",
                 error = function(e) FALSE)
      }, logical(1))
      vars[keep]
    })

    output$vars_ui <- shiny::renderUI({
      shiny::selectizeInput(ns("vars"), "Variables (pick 1-3)",
                            choices = analysable(), multiple = TRUE,
                            options = list(maxItems = 3))
    })
    output$colour_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(ns("colour"), "Colour / group by (optional)",
                           choices = c("(none)", panel_group_vars())),
        shiny::helpText(
          "Splits the chart by a category, drawing each group in its own colour ",
          "on the same axes — e.g. one histogram per sex, overlaid. Use it to ",
          "compare groups within a single plot.")
      )
    })
    output$panel_ui <- shiny::renderUI({
      ps <- panel_spec()
      if (!panel_active(ps)) return(NULL)
      pure_ts <- !panel_has_subject_time(ps) && !is.null(ps$time) && nzchar(ps$time)
      shiny::tagList(
        shiny::h5("Longitudinal / time-series chart"),
        shiny::helpText("For line charts, the Data-tab time variable is used automatically."),
        shiny::checkboxInput(ns("panel_trajectories"), "Show individual trajectories",
                             value = panel_has_subject_time(ps)),
        shiny::checkboxInput(ns("panel_aggregate"), "Overlay mean trend with error bars",
                             value = TRUE),
        if (pure_ts) shiny::numericInput(ns("ts_frequency"), "Seasonal period (for TS diagnostics)",
          value = tryCatch(ts_infer_frequency(data()[[ps$time]]), error = function(e) 1L),
          min = 1, max = 366, step = 1)
      )
    })
    output$facet_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(ns("facet"), "Facet by (optional)",
                           choices = c("(none)", panel_group_vars())),
        shiny::helpText(
          "Splits the chart into a small grid of separate mini-plots, one per ",
          "category (e.g. one panel per county). Use it when overlaying groups ",
          "on one plot looks too busy, or to compare many groups side by side. ",
          "Summary charts (a plain bar or pie) can only be split if you also ",
          "pick a Colour / group by variable.")
      )
    })

    sel_types <- shiny::reactive({
      shiny::req(input$vars); unname(types()[input$vars])
    })

    # skewness flag for the single-numeric case, to tune the recommendation
    skewed <- shiny::reactive({
      v <- input$vars
      if (length(v) == 1 && is_numeric_type(chart_types()[[v]])) {
        sk <- skewness(chart_data()[[v]])
        !is.na(sk) && abs(sk) > 1
      } else FALSE
    })

    reco <- shiny::reactive({
      shiny::req(sel_types())
      r <- recommend_charts(sel_types(), skewed = skewed())
      has_numeric <- length(input$vars) &&
        any(vapply(input$vars, function(v) is_numeric_type(chart_types()[[v]]), logical(1)))
      if (panel_mode() && has_numeric) {
        r$options <- unique(c("line", "run_chart", r$options))
        r$default <- if ("line" %in% r$options) "line" else r$default
        r$rationale <- paste(r$rationale, "The selected time variable also enables longitudinal trend charts.")
      }
      # Pure time series (a time/date column, no subject clustering): offer the
      # time-series diagnostic charts alongside the trend chart.
      ps <- panel_spec()
      if (panel_mode() && has_numeric && !panel_has_subject_time(ps) &&
          !is.null(ps$time) && nzchar(ps$time)) {
        r$options <- unique(c("line", "ts_decomposition", "ts_acf", "ts_pacf",
                              "ts_seasonal", "ts_spectrum", r$options))
        r$rationale <- paste(r$rationale, "Time-series diagnostics (decomposition, ACF/PACF, seasonal and spectral plots) are available for this series.")
      }
      r
    })

    output$rationale <- shiny::renderText({
      if (length(input$vars %||% character(0)) == 0)
        return("Pick one to three variables to get a chart suggestion.")
      reco()$rationale
    })

    # Populate the chart-type dropdown. By default offer ONLY the type-appropriate
    # charts (recommend_charts() is the single source of truth). Power users can
    # tick "Show all chart types" (Advanced) to override and access every type.
    shiny::observe({
      opts <- reco()$options
      show_all <- isTRUE(input$show_all)
      ordered <- if (show_all)
        c(opts, setdiff(names(CHART_LABELS), opts)) else opts
      shiny::updateSelectInput(session, "type",
        choices = stats::setNames(ordered, CHART_LABELS[ordered]),
        selected = reco()$default)
    })

    spec <- shiny::reactive({
      shiny::req(input$vars, input$type)
      s <- default_chart_spec()
      roles <- assign_chart_roles(input$vars, chart_types(), input$type)
      s$type   <- input$type
      s$x <- roles$x; s$y <- roles$y
      s$group  <- if (!is.null(input$colour) && input$colour != "(none)")
        input$colour else roles$group
      s$facet  <- if (!is.null(input$facet) && input$facet != "(none)")
        input$facet else NULL
      s$palette <- input$palette; s$theme_base <- input$theme_base
      s$base_size <- input$base_size; s$legend_pos <- input$legend_pos
      s$alpha <- input$alpha; s$point_size <- input$point_size
      s$bins <- input$bins; s$add_trend <- input$add_trend
      s$stacked_fill <- input$stacked_fill; s$flip <- input$flip
      s$show_grid <- input$show_grid; s$data_labels <- isTRUE(input$data_labels)
      s$title <- input$title; s$subtitle <- input$subtitle
      s$caption <- input$caption
      s$xlab <- if (nzchar(input$xlab)) input$xlab else NULL
      s$ylab <- if (nzchar(input$ylab)) input$ylab else NULL
      # Time-series diagnostic charts need the declared time column and the
      # seasonal period; the value series is the (numeric) selected variable.
      if (grepl("^ts_", s$type)) {
        ps <- panel_spec()
        s$ts_time <- ps$time
        s$ts_frequency <- input$ts_frequency
      }
      s
    })

    plot_obj <- shiny::reactive({
      shiny::req(spec())
      tryCatch({
        s <- spec(); ps <- panel_spec()
        if (panel_mode() && s$type %in% c("line", "run_chart") && !is.null(ps$time)) {
          if (panel_has_subject_time(ps))
            build_panel_chart(chart_data(), s, ps,
                              trajectories = isTRUE(input$panel_trajectories),
                              aggregate = isTRUE(input$panel_aggregate))
          else
            build_time_series_chart(chart_data(), s, ps,
                                    aggregate = isTRUE(input$panel_aggregate))
        } else build_chart(chart_data(), s)
      },
               error = function(e) {
                 ggplot2::ggplot() +
                   ggplot2::annotate("text", x = 1, y = 1,
                     label = paste("Could not draw chart:", conditionMessage(e)))
               })
    })

    output$plot <- shiny::renderPlot({ plot_obj() })

    # A compact summary of what the chart depicts (counts, group summaries, or an
    # association), used to interpret the chart with AI and to draft its report
    # write-up. Only aggregate summaries are sent - never raw patient rows.
    chart_caption <- shiny::reactive({
      if (nzchar(input$title %||% "")) input$title
      else paste(CHART_LABELS[[input$type]], "-", paste(input$vars, collapse = ", "))
    })
    interp_payload <- shiny::reactive({
      s <- tryCatch(spec(), error = function(e) NULL)
      if (is.null(s)) return(list())
      tbls <- tryCatch(chart_summary_tables(chart_data(), s), error = function(e) list())
      if (!length(tbls)) return(list())
      list(tables = tbls,
           context = list(title = chart_caption(),
                          analysis = sprintf("Chart (%s)", CHART_LABELS[[s$type]] %||% s$type),
                          method = s$type))
    })
    interp <- mod_interpret_server("interpret", interp_payload)

    output$dl_png <- shiny::downloadHandler(
      filename = function() paste0("chart_", Sys.Date(), ".png"),
      content = function(f) ggplot2::ggsave(f, plot_obj(), width = 8,
                                            height = 5, dpi = 150, bg = "white")
    )

    shiny::observeEvent(input$add, {
      cap <- chart_caption()
      p <- interp_payload()
      ai <- if (length(p$tables %||% list()))
        list(tables = p$tables, context = p$context,
             interpretation = interp_reuse_text(interp$interpretation(), p$context))
      else NULL
      add_report(list(kind = "plot", caption = cap, plot = plot_obj(),
                      spec = spec(), ai = ai))
      shiny::showNotification("Chart added to report.", type = "message", duration = 3)
    })
  })
}

#' Map selected variables to chart roles (x / y / group) given the chart type.
assign_chart_roles <- function(vars, types, chart_type) {
  tp <- types[vars]
  num <- vars[vapply(tp, is_numeric_type, logical(1))]
  cat <- vars[vapply(tp, is_categorical_type, logical(1))]
  dt  <- vars[tp == "datetime"]
  first <- function(z) if (length(z)) z[[1]] else NULL

  role <- list(x = first(vars), y = NULL, group = NULL)

  if (chart_type %in% c("scatter", "density2d")) {
    role$x <- first(num); role$y <- if (length(num) >= 2) num[[2]] else first(setdiff(vars, num[1]))
  } else if (chart_type %in% c("box_by_group", "violin_by_group",
                               "raincloud_by_group", "mean_ci_bar", "jitter")) {
    role$x <- first(num); role$group <- first(cat)
  } else if (chart_type %in% c("grouped_bar", "stacked_bar", "mosaic", "heatmap")) {
    role$x <- first(cat); role$group <- if (length(cat) >= 2) cat[[2]] else first(setdiff(vars, cat[1]))
  } else if (chart_type %in% c("line", "run_chart")) {
    role$x <- if (length(dt)) first(dt) else first(vars); role$y <- first(num)
  } else if (chart_type %in% c("bar", "pareto", "pie")) {
    role$x <- if (length(cat)) first(cat) else first(vars)
  } else {
    # univariate numeric charts
    role$x <- if (length(num)) first(num) else first(vars)
  }
  role
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
