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

    # The Summary-table mode is offered for cross-sectional and repeated-measures
    # data; time-series and survival layouts keep the single-variable behaviour.
    mode <- shiny::reactive({
      if (!kind() %in% c("cross_sectional", "repeated")) return("single")
      input$desc_mode %||% "single"
    })
    output$mode_ui <- shiny::renderUI({
      if (!kind() %in% c("cross_sectional", "repeated")) return(NULL)
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
    # Row-variable and split-by choices exclude the subject-ID and time columns.
    t1_reserved <- shiny::reactive({
      ps <- panel_spec(); v <- c(ps$id, ps$time); v[nzchar(v)]
    })
    output$t1_ui <- shiny::renderUI({
      if (!identical(mode(), "table1")) return(NULL)
      reserved <- t1_reserved()
      rows <- setdiff(analysable(), reserved)
      vars_input <- shiny::selectizeInput(ns("t1_vars"), "Variables (rows)",
        choices = rows, multiple = TRUE,
        options = list(placeholder = "Pick the characteristics to summarise"))
      cont_input <- shiny::selectInput(ns("t1_cont"), "Continuous statistic",
        choices = c("Auto" = "auto", "Mean (SD)" = "mean_sd", "Median [IQR]" = "median_iqr"))
      title_input <- shiny::textInput(ns("t1_title"), "Table title",
                                      value = "Participant characteristics")

      if (identical(kind(), "repeated")) {
        shiny::tagList(
          shiny::radioButtons(ns("t1_rm_mode"), "Table type",
            choices = c("Baseline (first time point)" = "baseline",
                        "Across time points" = "evolution"),
            selected = input$t1_rm_mode %||% "baseline"),
          vars_input,
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'baseline'", ns("t1_rm_mode")),
            shiny::selectInput(ns("t1_strata"), "Split by group (optional)",
              choices = c("(none)", setdiff(grouping_vars(), reserved))),
            shiny::checkboxInput(ns("t1_overall"), "Include an Overall column", TRUE),
            shiny::checkboxInput(ns("t1_missing"), "Show a Missing row", FALSE)),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'evolution'", ns("t1_rm_mode")),
            shiny::checkboxInput(ns("t1_overall_evo"),
              "Include a pooled Overall column (unusual for longitudinal data)", FALSE)),
          cont_input,
          title_input,
          shiny::helpText("Repeated-measures summaries are descriptive (no p-values). 'Across time points' shows each visit as a column with participant retention and per-visit missingness.")
        )
      } else {
        shiny::tagList(
          vars_input,
          shiny::selectInput(ns("t1_strata"), "Split by (outcome, optional)",
            choices = c("(none)", grouping_vars())),
          cont_input,
          shiny::checkboxInput(ns("t1_overall"), "Include an Overall column", TRUE),
          shiny::checkboxInput(ns("t1_missing"), "Show a Missing row", FALSE),
          title_input,
          shiny::helpText("A p-value column and a test footnote are added only when you split by an outcome.")
        )
      }
    })
    t1_obj <- shiny::reactive({
      shiny::req(identical(mode(), "table1"), length(input$t1_vars) > 0)
      reserved <- t1_reserved()
      cont <- input$t1_cont %||% "auto"
      title <- input$t1_title %||% "Participant characteristics"
      strata_in <- if (is.null(input$t1_strata) || input$t1_strata == "(none)") NULL
                   else input$t1_strata

      if (identical(kind(), "repeated")) {
        ps <- panel_spec()
        rm_mode <- input$t1_rm_mode %||% "baseline"
        vars <- setdiff(input$t1_vars, reserved)
        if (identical(rm_mode, "baseline")) {
          shiny::req(ps$id, ps$time)
          vars <- setdiff(vars, strata_in %||% character(0))
          shiny::req(length(vars) > 0)
          bdf <- tableone_baseline(t1_data(), ps$id, ps$time)
          build_table_one(bdf, vars, strata = strata_in, types = t1_types(),
            opts = list(cont_style = cont, show_p = FALSE,
                        show_overall = isTRUE(input$t1_overall),
                        show_missing = isTRUE(input$t1_missing), title = title))
        } else {
          shiny::req(ps$time, length(vars) > 0)
          # An across-time table needs at least two distinct time points to have
          # more than one column. There is no upper bound: the table renders a
          # column per distinct visit however many there are (wide tables scroll
          # horizontally in the preview and the report).
          n_tp <- nlevels(droplevels(as.factor(t1_data()[[ps$time]])))
          shiny::validate(shiny::need(n_tp >= 2, sprintf(
            "The visit/time variable '%s' has only %d distinct value; an across-time table needs at least 2 time points. Pick a variable that varies over visits.",
            ps$time, n_tp)))
          build_table_one(t1_data(), vars, strata = ps$time, types = t1_types(),
            opts = list(cont_style = cont, show_p = FALSE,
                        show_overall = isTRUE(input$t1_overall_evo),
                        show_missing = TRUE, retention = TRUE, title = title,
                        strata_order = .t1_time_order(t1_data()[[ps$time]])))
        }
      } else {
        vars <- setdiff(input$t1_vars, strata_in %||% character(0))
        shiny::req(length(vars) > 0)
        build_table_one(t1_data(), vars, strata = strata_in, types = t1_types(),
          opts = list(cont_style = cont, show_overall = isTRUE(input$t1_overall),
                      show_missing = isTRUE(input$t1_missing), title = title))
      }
    })

    descr <- shiny::reactive({
      cur <- analysis_data()
      grp <- if (identical(kind(), "cross_sectional")) input$group else NULL
      info <- descriptive_tables(cur$data, cur$var, cur$type, panel_spec(), group = grp)
      info$var <- cur$var; info$type <- cur$type
      info
    })

    # Evaluate the summary-table object, distinguishing three states so the UI
    # can react appropriately:
    #   "ok"      -> the built table object (value = the object)
    #   "message" -> a validation message to show the user (e.g. too many
    #                time-point columns); value = the message text
    #   "waiting" -> a silent req() short-circuit (nothing selected yet)
    # Previously both note and body swallowed every error into NULL, so a
    # genuine "too many time points" validation message was hidden behind a
    # generic "pick variables" placeholder even after variables were chosen.
    t1_status <- shiny::reactive({
      tryCatch(list(state = "ok", value = t1_obj()),
        error = function(e) {
          msg <- conditionMessage(e)
          if (nzchar(msg)) list(state = "message", value = msg)
          else list(state = "waiting", value = NULL)
        })
    })

    output$note <- shiny::renderUI({
      if (identical(mode(), "table1")) {
        st <- t1_status()
        if (identical(st$state, "ok"))
          return(shiny::HTML(sprintf("<b>%s.</b> %s", st$value$title, st$value$note)))
        if (identical(st$state, "message"))
          return(shiny::HTML(st$value))
        return(shiny::HTML("Pick one or more variables to build a summary table. Split by an outcome to add p-values."))
      }
      info <- descr()
      shiny::HTML(sprintf("<b>%s</b> (type: %s). %s",
                          info$var, TYPE_LABELS[[info$type]], info$headline))
    })

    # Render the (variable number of) tables for this kind.
    output$body <- shiny::renderUI({
      if (identical(mode(), "table1")) {
        st <- t1_status()
        if (identical(st$state, "ok"))
          return(shiny::tagList(
            shiny::h6("Preview (this is how the table will appear in the downloaded report)"),
            shiny::uiOutput(ns("t1_preview"))))
        # A validation message is shown in the note alert above; keep the body
        # empty rather than contradicting it with "select variables".
        if (identical(st$state, "message")) return(NULL)
        return(shiny::helpText("Select variables to summarise."))
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
    # Wrapped in a horizontally-scrolling box so wide across-time tables (one
    # column per visit - there can be many) stay readable instead of overflowing.
    output$t1_preview <- shiny::renderUI({
      shiny::req(identical(mode(), "table1"))
      t1 <- t1_obj()
      inner <- if (requireNamespace("flextable", quietly = TRUE))
        flextable::htmltools_value(tableone_flextable(t1))
      else
        shiny::tableOutput(ns("t1_preview_fallback"))
      shiny::div(style = "overflow-x: auto; max-width: 100%;", inner)
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

    interp_payload <- shiny::reactive({
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
    })
    interp <- mod_interpret_server("interpret", interp_payload, audit = audit)

    # Payload the report write-up worker needs, reusing the tab interpretation
    # when the analyst already produced one for this same analysis.
    ai_for_report <- function() {
      p <- interp_payload()
      if (!length(p$tables %||% list())) return(NULL)
      list(tables = p$tables, context = p$context,
           interpretation = interp_reuse_text(interp$interpretation(), p$context))
    }

    shiny::observeEvent(input$add, {
      if (identical(mode(), "table1")) {
        st <- t1_status()
        if (!identical(st$state, "ok")) {
          shiny::showNotification(
            if (identical(st$state, "message")) st$value
            else "Pick at least one variable before adding the summary table.",
            type = "warning", duration = 6)
          return()
        }
        t1 <- st$value
        add_report(list(kind = "table1", title = t1$title, t1 = t1,
                        ai = ai_for_report()))
        shiny::showNotification("Summary table added to report.",
                                type = "message", duration = 3)
        return()
      }
      info <- descr()
      nms <- names(info$tables)
      ai <- ai_for_report()   # one write-up, placed under the first table
      for (k in seq_along(nms))
        add_report(list(kind = "table",
                        caption = sprintf("%s [%s]", nms[k], info$var),
                        data = info$tables[[nms[k]]],
                        ai = if (k == 1L) ai else NULL))
      shiny::showNotification("Added to report.", type = "message", duration = 3)
    })
  })
}
