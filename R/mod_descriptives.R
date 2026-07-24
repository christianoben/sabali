# mod_descriptives.R --------------------------------------------------------
# Guided descriptives whose CONTENT matches the dataset kind declared on the
# Data tab:
#   * cross-sectional -> the type-appropriate table (+ optional grouping);
#   * longitudinal    -> per-subject summary for a time-invariant variable, or
#                        the panel decomposition (structure, by-time, within/
#                        between + ICC, change, missingness) for a time-varying
#                        one - never a pooled across-rows table;
#   * time series     -> a time-series summary (level, trend, range) + by-time
#                        missingness - never a pooled iid-style table.
# All of this comes from descriptive_tables() so the display, the report, and
# the AI interpreter always agree.

mod_descriptives_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Guided descriptives"),
      shiny::uiOutput(ns("var_ui")),
      shiny::uiOutput(ns("group_ui")),
      shiny::actionButton(ns("add"), "Add to report",
                          icon = shiny::icon("plus"), class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::helpText("The summaries shown are the ones valid for how your data is laid out (set on the Data tab) and for the variable's measurement type.")
    ),
    shiny::mainPanel(
      width = 9,
      shiny::div(class = "alert alert-info", shiny::htmlOutput(ns("note"))),
      shiny::uiOutput(ns("body")),
      mod_interpret_ui(ns("interpret"))
    )
  )
}

mod_descriptives_server <- function(id, data, types, outcomes = NULL, panel = NULL,
                                    add_report, audit = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    panel_spec <- shiny::reactive({
      if (is.null(panel)) return(list(id = NULL, time = NULL, event = NULL))
      panel()
    })
    kind <- shiny::reactive({
      ps <- panel_spec()
      k <- ps$kind %||% (if (panel_has_subject_time(ps)) "repeated"
                         else if (panel_active(ps)) "time_series" else "cross_sectional")
      # collapse to what we can actually compute (see descriptive_tables()).
      if (identical(k, "repeated") && !panel_has_subject_time(ps)) k <- "cross_sectional"
      if (identical(k, "time_series") &&
          (is.null(ps$time) || !nzchar(ps$time))) k <- "cross_sectional"
      k
    })

    outcome_candidates <- shiny::reactive({
      if (is.null(outcomes)) return(default_outcome_candidates(data(), types()))
      outcomes()
    })
    selected_type <- function(v) {
      original <- types()[[v]]
      if (v %in% outcome_candidates())
        return(preferred_outcome_type(data()[[v]], original))
      original
    }
    analysis_data <- shiny::reactive({
      df <- data(); v <- input$var
      shiny::req(v, v %in% names(df))
      tp <- selected_type(v)
      if (v %in% outcome_candidates() &&
          tp %in% c("continuous", "count", "binary", "nominal", "ordinal"))
        df[[v]] <- coerce_outcome_vector(df[[v]], tp, v)
      list(data = df, type = tp, var = v)
    })

    analysable <- shiny::reactive({
      tp <- types(); df <- data()
      base <- names(tp)[vapply(tp, is_analysable_type, logical(1))]
      selected <- intersect(outcome_candidates(), names(df))
      safe <- selected[vapply(selected, function(v)
        length(outcome_type_options(df[[v]])) > 0, logical(1))]
      unique(c(base, safe))
    })
    grouping_vars <- shiny::reactive({
      tp <- types(); names(tp)[vapply(tp, is_categorical_type, logical(1))]
    })

    output$var_ui <- shiny::renderUI({
      shiny::selectInput(ns("var"), "Variable", choices = analysable())
    })
    # Grouping is a cross-sectional idea; longitudinal/time-series data are
    # summarised BY TIME automatically, so the group control is offered only
    # for cross-sectional data.
    output$group_ui <- shiny::renderUI({
      if (!identical(kind(), "cross_sectional")) return(NULL)
      shiny::selectInput(ns("group"), "Group by (optional)",
                         choices = c("(none)", grouping_vars()))
    })

    descr <- shiny::reactive({
      cur <- analysis_data()
      grp <- if (identical(kind(), "cross_sectional")) input$group else NULL
      info <- descriptive_tables(cur$data, cur$var, cur$type, panel_spec(), group = grp)
      info$var <- cur$var; info$type <- cur$type
      info
    })

    output$note <- shiny::renderUI({
      info <- descr()
      shiny::HTML(sprintf("<b>%s</b> (type: %s). %s",
                          info$var, TYPE_LABELS[[info$type]], info$headline))
    })

    # Render the (variable number of) tables for this kind.
    output$body <- shiny::renderUI({
      info <- descr()
      tags <- lapply(seq_along(info$tables), function(i) {
        shiny::tagList(shiny::h6(names(info$tables)[i]),
                       shiny::tableOutput(ns(paste0("dtab_", i))))
      })
      if (!length(tags))
        return(shiny::helpText("Select a variable to summarise."))
      shiny::tagList(tags)
    })
    shiny::observe({
      info <- descr()
      lapply(seq_along(info$tables), function(i) {
        local({
          ii <- i
          output[[paste0("dtab_", ii)]] <- shiny::renderTable(
            descr()$tables[[ii]], digits = 4, na = "")
        })
      })
    })

    mod_interpret_server("interpret", shiny::reactive({
      info <- tryCatch(descr(), error = function(e) NULL)
      if (is.null(info) || !length(info$tables)) return(list())
      list(tables = info$tables,
           context = list(title = sprintf("Descriptive summary of %s", info$var),
                          analysis = sprintf("Descriptive statistics (%s)", info$kind),
                          method = info$type))
    }), audit = audit)

    shiny::observeEvent(input$add, {
      info <- descr()
      for (nm in names(info$tables))
        add_report(list(kind = "table",
                        caption = sprintf("%s [%s]", nm, info$var),
                        data = info$tables[[nm]]))
      shiny::showNotification("Added to report.", type = "message", duration = 3)
    })
  })
}
