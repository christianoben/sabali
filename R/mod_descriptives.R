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
      shiny::uiOutput(ns("mode_ui")),
      shiny::uiOutput(ns("var_ui")),
      shiny::uiOutput(ns("group_ui")),
      shiny::uiOutput(ns("t1_ui")),
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

    # Two modes on the Descriptives tab. "Summary table (Table 1)" is offered
    # ONLY for cross-sectional data (the pilot scope); every other layout keeps
    # the single-variable behaviour unchanged.
    mode <- shiny::reactive({
      if (!identical(kind(), "cross_sectional")) return("single")
      input$desc_mode %||% "single"
    })
    output$mode_ui <- shiny::renderUI({
      if (!identical(kind(), "cross_sectional")) return(NULL)
      shiny::radioButtons(ns("desc_mode"), "Summary",
        choices = c("Single variable" = "single",
                    "Summary table" = "table1"),
        selected = mode())
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
      if (identical(mode(), "table1")) return(NULL)
      shiny::selectInput(ns("var"), "Variable", choices = analysable())
    })
    # Grouping is a cross-sectional idea; longitudinal/time-series data are
    # summarised BY TIME automatically, so the group control is offered only
    # for cross-sectional data.
    output$group_ui <- shiny::renderUI({
      if (!identical(kind(), "cross_sectional") || identical(mode(), "table1"))
        return(NULL)
      shiny::selectInput(ns("group"), "Group by (optional)",
                         choices = c("(none)", grouping_vars()))
    })

    # --- Summary table (Table 1), cross-sectional only ----------------------
    # Effective types + outcome-coerced data, so binary/categorical outcome
    # columns are summarised on their intended scale (mirrors the charts tab).
    t1_types <- shiny::reactive({
      tp <- types(); df <- data()
      for (v in intersect(outcome_candidates(), names(df))) {
        opts <- outcome_type_options(df[[v]])
        if (length(opts)) tp[[v]] <- preferred_outcome_type(df[[v]], tp[[v]])
      }
      tp
    })
    t1_data <- shiny::reactive({
      df <- data(); tp <- t1_types()
      for (v in intersect(outcome_candidates(), names(df)))
        if (tp[[v]] %in% c("continuous", "count", "binary", "nominal", "ordinal"))
          df[[v]] <- coerce_outcome_vector(df[[v]], tp[[v]], v)
      df
    })
    output$t1_ui <- shiny::renderUI({
      if (!identical(mode(), "table1")) return(NULL)
      rows <- analysable()
      shiny::tagList(
        shiny::selectizeInput(ns("t1_vars"), "Variables (rows)",
          choices = rows, multiple = TRUE,
          options = list(placeholder = "Pick the characteristics to summarise")),
        shiny::selectInput(ns("t1_strata"), "Split by (outcome, optional)",
          choices = c("(none)", grouping_vars())),
        shiny::selectInput(ns("t1_cont"), "Continuous statistic",
          choices = c("Auto (match the test)" = "auto",
                      "Mean (SD)" = "mean_sd", "Median [IQR]" = "median_iqr")),
        shiny::checkboxInput(ns("t1_overall"), "Include an Overall column", TRUE),
        shiny::checkboxInput(ns("t1_missing"), "Show a Missing row", FALSE),
        shiny::textInput(ns("t1_title"), "Table title",
                         value = "Participant characteristics"),
        shiny::helpText("A p-value column and a test footnote are added only when you split by an outcome.")
      )
    })
    t1_obj <- shiny::reactive({
      shiny::req(identical(mode(), "table1"), length(input$t1_vars) > 0)
      strata <- if (is.null(input$t1_strata) || input$t1_strata == "(none)") NULL
                else input$t1_strata
      vars <- setdiff(input$t1_vars, strata %||% character(0))
      shiny::req(length(vars) > 0)
      build_table_one(t1_data(), vars, strata = strata, types = t1_types(),
                      opts = list(cont_style = input$t1_cont %||% "auto",
                                  show_overall = isTRUE(input$t1_overall),
                                  show_missing = isTRUE(input$t1_missing),
                                  title = input$t1_title %||% "Participant characteristics"))
    })

    descr <- shiny::reactive({
      cur <- analysis_data()
      grp <- if (identical(kind(), "cross_sectional")) input$group else NULL
      info <- descriptive_tables(cur$data, cur$var, cur$type, panel_spec(), group = grp)
      info$var <- cur$var; info$type <- cur$type
      info
    })

    output$note <- shiny::renderUI({
      if (identical(mode(), "table1")) {
        t1 <- tryCatch(t1_obj(), error = function(e) NULL)
        if (is.null(t1)) return(shiny::HTML("Pick one or more variables to build a summary table. Split by an outcome to add p-values."))
        return(shiny::HTML(sprintf("<b>%s.</b> %s", t1$title, t1$note)))
      }
      info <- descr()
      shiny::HTML(sprintf("<b>%s</b> (type: %s). %s",
                          info$var, TYPE_LABELS[[info$type]], info$headline))
    })

    # Render the (variable number of) tables for this kind.
    output$body <- shiny::renderUI({
      if (identical(mode(), "table1")) {
        if (is.null(tryCatch(t1_obj(), error = function(e) NULL)))
          return(shiny::helpText("Select variables to summarise."))
        return(shiny::tagList(
          shiny::h6("Preview (this is how the table will appear in the downloaded report)"),
          shiny::uiOutput(ns("t1_preview"))))
      }
      info <- descr()
      tags <- lapply(seq_along(info$tables), function(i) {
        shiny::tagList(shiny::h6(names(info$tables)[i]),
                       shiny::tableOutput(ns(paste0("dtab_", i))))
      })
      if (!length(tags))
        return(shiny::helpText("Select a variable to summarise."))
      shiny::tagList(tags)
    })
    # WYSIWYG preview: render the SAME styled flextable used in the report.
    output$t1_preview <- shiny::renderUI({
      shiny::req(identical(mode(), "table1"))
      t1 <- t1_obj()
      if (requireNamespace("flextable", quietly = TRUE))
        flextable::htmltools_value(tableone_flextable(t1))
      else
        shiny::tableOutput(ns("t1_preview_fallback"))
    })
    output$t1_preview_fallback <- shiny::renderTable({
      shiny::req(identical(mode(), "table1"))
      t1_obj()$table
    }, na = "")
    shiny::observe({
      if (identical(mode(), "table1")) return()
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
      if (identical(mode(), "table1")) {
        t1 <- tryCatch(t1_obj(), error = function(e) NULL)
        if (is.null(t1)) return(list())
        return(list(tables = list("Summary table" = t1$table),
                    context = list(title = t1$title,
                                   analysis = "Descriptive summary table (Table 1)",
                                   method = "table1")))
      }
      info <- tryCatch(descr(), error = function(e) NULL)
      if (is.null(info) || !length(info$tables)) return(list())
      list(tables = info$tables,
           context = list(title = sprintf("Descriptive summary of %s", info$var),
                          analysis = sprintf("Descriptive statistics (%s)", info$kind),
                          method = info$type))
    }), audit = audit)

    shiny::observeEvent(input$add, {
      if (identical(mode(), "table1")) {
        t1 <- t1_obj()
        add_report(list(kind = "table1", title = t1$title, t1 = t1))
        shiny::showNotification("Summary table added to report.",
                                type = "message", duration = 3)
        return()
      }
      info <- descr()
      for (nm in names(info$tables))
        add_report(list(kind = "table",
                        caption = sprintf("%s [%s]", nm, info$var),
                        data = info$tables[[nm]]))
      shiny::showNotification("Added to report.", type = "message", duration = 3)
    })
  })
}
