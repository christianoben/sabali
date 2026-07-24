# mod_phase4.R ---------------------------------------------------------------
# Phase 4 Advanced Models tab: clustered/repeated models and survival analysis.

mod_phase4_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Advanced models"),
      shiny::uiOutput(ns("ai_gate_ui")),
      shiny::uiOutput(ns("analysis_ui")),
      shiny::uiOutput(ns("cluster_outcome_ui")),
      shiny::uiOutput(ns("cluster_outcome_type_ui")),
      shiny::uiOutput(ns("cluster_method_ui")),
      shiny::uiOutput(ns("cluster_predictors_ui")),
      shiny::uiOutput(ns("cluster_id_ui")),
      shiny::uiOutput(ns("correlation_ui")),
      shiny::uiOutput(ns("cluster_time_ui")),
      shiny::uiOutput(ns("random_slope_ui")),
      shiny::uiOutput(ns("survival_method_ui")),
      shiny::uiOutput(ns("survival_info_ui")),
      shiny::uiOutput(ns("survival_strata_ui")),
      shiny::uiOutput(ns("survival_predictors_ui")),
      shiny::uiOutput(ns("ts_value_ui")),
      shiny::uiOutput(ns("ts_method_ui")),
      shiny::uiOutput(ns("ts_info_ui")),
      shiny::uiOutput(ns("ts_options_ui")),
      shiny::uiOutput(ns("ts_predictors_ui")),
      shiny::tags$hr(),
      shiny::actionButton(ns("run"), "Fit analysis", icon = shiny::icon("play"),
                          class = "btn-primary"),
      shiny::actionButton(ns("add"), "Add to report", icon = shiny::icon("plus"))
    ),
    shiny::mainPanel(
      width = 9,
      shiny::div(class = "alert alert-info", shiny::htmlOutput(ns("preview"))),
      shiny::uiOutput(ns("recommend_badge")),
      shiny::h5("Results"),
      shiny::tableOutput(ns("coefficients")),
      shiny::h5("Model fit"),
      shiny::tableOutput(ns("fit_stats")),
      shiny::h5("Diagnostics and assumptions"),
      shiny::uiOutput(ns("assumptions")),
      shiny::plotOutput(ns("diagnostic_plot"), height = "420px"),
      shiny::h5("Interpretation"),
      shiny::textOutput(ns("interpretation")),
      mod_interpret_ui(ns("interpret"))
    )
  )
}

mod_phase4_server <- function(id, data, types, outcomes = NULL, panel = NULL,
                              plan_rec = NULL, add_report, audit = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # AI-plan gate (see mod_tests). Narrows the analysis family and model method
    # to what the AI recommended, on top of the structure gate; the override
    # checkbox (shown only when a plan exists) restores the full feasible menu.
    plan_now <- shiny::reactive({
      if (is.null(plan_rec)) return(NULL)
      tryCatch(plan_rec(), error = function(e) NULL)
    })
    ai_gate_on <- shiny::reactive(
      !is.null(plan_now()) && !isTRUE(input$include_unrecommended))
    output$ai_gate_ui <- shiny::renderUI({
      if (is.null(plan_now())) return(NULL)
      shiny::div(class = "alert alert-info", style = "padding:8px 10px;",
        shiny::icon("magic"), " Guided by your AI analysis plan.",
        shiny::checkboxInput(ns("include_unrecommended"),
          "Include un-recommended analyses (override the AI plan)",
          value = FALSE))
    })
    panel_spec <- shiny::reactive({
      if (is.null(panel)) return(list(id = NULL, time = NULL, event = NULL))
      panel()
    })
    # What the user's Data-tab answers say this dataset supports. Only the
    # applicable analysis families are offered - a cross-sectional survival
    # dataset shows only "survival", a repeated-measures dataset shows
    # "clustered" (and "survival" too if a time-to-event was also declared).
    structure_info <- shiny::reactive({
      advanced_data_structure(data(), types(), panel_spec())
    })
    output$analysis_ui <- shiny::renderUI({
      s <- structure_info()
      fams <- c(if (isTRUE(s$clustered)) c("Clustered / repeated observations" = "clustered"),
                if (isTRUE(s$survival))  c("Time-to-event (survival)" = "survival"),
                if (isTRUE(s$time_series)) c("Time series (trend / seasonality / forecast)" = "time_series"))
      if (!length(fams))
        return(shiny::helpText("Set how your data is organised on the Data tab to enable these analyses."))
      # AI-plan gate: keep only the recommended family (preserving labels), but
      # never empty the menu.
      allow <- plan_advanced_family(plan_now())
      if (ai_gate_on() && length(allow)) {
        keep <- fams[unname(fams) %in% allow]
        if (length(keep)) fams <- keep
      }
      shiny::radioButtons(ns("analysis"), "Analysis family",
                          choices = fams, selected = unname(fams)[[1]])
    })
    outcome_vars <- shiny::reactive({
      df <- data(); tp <- types()
      selected <- if (is.null(outcomes)) default_outcome_candidates(df, tp) else outcomes()
      analysis_outcome_vars(df, tp, selected, MODEL_OUTCOME_TYPES)
    })
    predictor_vars <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% MODEL_PREDICTOR_TYPES]
      setdiff(nm, input$cluster_outcome)
    })
    resolved_cluster_outcome_type <- shiny::reactive({
      shiny::req(input$cluster_outcome, data())
      opts <- intersect(outcome_type_options(data()[[input$cluster_outcome]]),
                        MODEL_OUTCOME_TYPES)
      shiny::req(length(opts) > 0)
      if (!is.null(input$cluster_outcome_type) && input$cluster_outcome_type %in% opts)
        input$cluster_outcome_type else opts[[1]]
    })
    cluster_vars <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[!tp %in% c("text", "datetime")]
      setdiff(nm, c(input$cluster_outcome, input$cluster_predictors))
    })
    time_vars <- shiny::reactive({
      df <- data(); tp <- types()
      nm <- names(df)[vapply(df, function(x) {
        inherits(x, c("Date", "POSIXct", "POSIXt")) ||
          any(outcome_type_options(x) %in% c("continuous", "count"))
      }, logical(1))]
      ps <- panel_spec()
      unique(c(intersect(ps$time %||% character(0), names(df)), nm))
    })
    event_vars <- shiny::reactive({
      df <- data()
      nm <- names(df)[vapply(df, function(x) "binary" %in% outcome_type_options(x), logical(1))]
      ps <- panel_spec()
      unique(c(intersect(ps$event %||% character(0), names(df)), nm))
    })
    # Survival time / event / ID come from the Data-tab declaration, not from
    # pickers here. `counting` = many rows per subject with interval endpoints.
    surv_decl <- shiny::reactive({
      ps <- panel_spec()
      counting <- identical(ps$kind, "repeated") &&
        !is.null(ps$surv_start) && !is.null(ps$surv_end) && !is.null(ps$id)
      list(time = ps$surv_time, start = ps$surv_start, end = ps$surv_end,
           event = ps$surv_event, id = if (counting) ps$id else NULL,
           counting = counting)
    })
    surv_used_cols <- shiny::reactive({
      d <- surv_decl()
      c(d$time, d$start, d$end, d$event, d$id)
    })
    survival_predictors <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% MODEL_PREDICTOR_TYPES]
      setdiff(nm, surv_used_cols())
    })
    survival_groups <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% c("binary", "nominal", "ordinal")]
      setdiff(nm, surv_used_cols())
    })

    output$cluster_outcome_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered")
      shiny::selectInput(ns("cluster_outcome"), "Outcome variable",
                         choices = outcome_vars())
    })

    output$cluster_outcome_type_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_outcome, data())
      opts <- intersect(outcome_type_options(data()[[input$cluster_outcome]]),
                        MODEL_OUTCOME_TYPES)
      shiny::req(length(opts) > 0)
      shiny::selectInput(ns("cluster_outcome_type"), "Use outcome as",
                         choices = stats::setNames(opts, OUTCOME_TARGET_LABELS[opts]),
                         selected = if (!is.null(input$cluster_outcome_type) &&
                                        input$cluster_outcome_type %in% opts)
                           input$cluster_outcome_type else opts[[1]])
    })

    output$cluster_method_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_outcome)
      mp <- suggest_model_plan(resolved_cluster_outcome_type(), "clustered")
      methods <- gate_choices(mp$methods, plan_advanced_id(plan_now()),
                              ai_gate_on())
      selected <- if (mp$default %in% methods) mp$default else methods[[1]]
      shiny::radioButtons(ns("cluster_method"), "Clustered model",
                          choices = stats::setNames(methods,
                                                    MODEL_METHOD_LABELS[methods]),
                          selected = selected)
    })

    output$cluster_predictors_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_outcome)
      shiny::selectizeInput(ns("cluster_predictors"),
                            "Predictors / fixed effects",
                            choices = predictor_vars(), multiple = TRUE,
                            options = list(placeholder = "Choose one or more predictors"))
    })

    output$cluster_id_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_outcome)
      shiny::selectInput(ns("cluster"), "Subject / cluster ID",
                         choices = cluster_vars(),
                         selected = panel_spec()$id %||% NULL)
    })

    output$correlation_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_method)
      if (!grepl("^gee", input$cluster_method)) return(NULL)
      shiny::selectInput(ns("corstr"), "GEE working correlation",
                         choices = c("Independence" = "independence",
                                     "Exchangeable" = "exchangeable",
                                     "AR-1" = "ar1"),
                         selected = "exchangeable")
    })

    output$cluster_time_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_method,
                 grepl("^gee", input$cluster_method), input$corstr == "ar1")
      choices <- setdiff(names(types()), c(input$cluster_outcome, input$cluster,
                                           input$cluster_predictors))
      choices <- unique(c(intersect(panel_spec()$time %||% character(0), choices), choices))
      shiny::selectInput(ns("cluster_time"), "Time / ordering variable",
                         choices = choices, selected = panel_spec()$time %||% NULL)
    })

    output$random_slope_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_method)
      if (!input$cluster_method %in% c("lmm", "glmm_binomial", "glmm_poisson"))
        return(NULL)
      shiny::selectInput(ns("random_slope"), "Random slope (optional)",
                         choices = c("(none)", predictor_vars()),
                         selected = "(none)")
    })

    output$survival_method_ui <- shiny::renderUI({
      shiny::req(input$analysis == "survival")
      # Start-stop (time-varying) data has many rows per subject; Kaplan-Meier
      # assumes one row per subject, so only Cox is offered in that case.
      choices <- if (isTRUE(surv_decl()$counting))
        stats::setNames("cox", SURVIVAL_METHOD_LABELS[["cox"]])
      else
        stats::setNames(names(SURVIVAL_METHOD_LABELS), unname(SURVIVAL_METHOD_LABELS))
      # AI-plan gate: keep only the recommended survival method (preserving
      # labels), but never empty the menu.
      allow <- plan_survival_method(plan_now())
      if (ai_gate_on() && length(allow)) {
        keep <- choices[unname(choices) %in% allow]
        if (length(keep)) choices <- keep
      }
      shiny::radioButtons(ns("survival_method"), "Survival method",
                          choices = choices, selected = unname(choices)[[1]])
    })

    # The time / event / ID roles were set on the Data tab; show them here
    # (read-only) rather than asking again, so the two never disagree.
    output$survival_info_ui <- shiny::renderUI({
      shiny::req(input$analysis == "survival")
      d <- surv_decl()
      time_desc <- if (!is.null(d$time)) sprintf("follow-up time <b>%s</b>", d$time)
        else if (!is.null(d$start) && !is.null(d$end))
          sprintf("the interval from <b>%s</b> to <b>%s</b>", d$start, d$end)
        else "<i>(not set - go back to the Data tab)</i>"
      extra <- if (isTRUE(d$counting))
        sprintf("<br>Repeated intervals per subject <b>%s</b> (start-stop / time-varying).", d$id) else ""
      shiny::div(class = "alert alert-info", style = "padding:8px 12px;",
        shiny::HTML(sprintf("Using %s and event column <b>%s</b> (from the Data tab).%s",
                            time_desc, d$event %||% "?", extra)))
    })

    output$survival_strata_ui <- shiny::renderUI({
      shiny::req(input$analysis == "survival", input$survival_method)
      if (input$survival_method != "kaplan_meier") return(NULL)
      shiny::selectInput(ns("survival_strata"),
                         "Compare groups (optional)",
                         choices = c("(none)" = "", survival_groups()),
                         selected = "")
    })

    output$survival_predictors_ui <- shiny::renderUI({
      shiny::req(input$analysis == "survival", input$survival_method)
      if (input$survival_method != "cox") return(NULL)
      shiny::selectizeInput(ns("survival_predictors"),
                            "Cox predictors",
                            choices = survival_predictors(), multiple = TRUE,
                            options = list(placeholder = "Choose one or more predictors"))
    })

    # --- time-series family -------------------------------------------------
    # The time/date column is declared on the Data tab (panel$time); the user
    # picks the numeric series to analyse and the method here.
    ts_time_col <- shiny::reactive(panel_spec()$time)
    ts_value_vars <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% c("continuous", "count")]
      setdiff(nm, c(ts_time_col(), reserved_analysis_vars(panel_spec())))
    })
    ts_methods <- shiny::reactive({
      m <- names(TS_METHOD_LABELS)
      # The GARCH method needs the optional rugarch package; hide it when absent
      # (the id stays in the vocabulary, so the AI plan can never gate to an
      # empty menu - gate_choices falls back to the feasible list).
      if (!ts_rugarch_available()) m <- setdiff(m, "ts_garch")
      gate_choices(m, plan_ts_method(plan_now()), ai_gate_on())
    })

    output$ts_value_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series")
      vars <- ts_value_vars()
      if (!length(vars))
        return(shiny::helpText("No numeric series is available to analyse. Confirm a continuous or count variable on the Data tab."))
      shiny::selectInput(ns("ts_value"), "Series to analyse", choices = vars)
    })
    output$ts_method_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series")
      methods <- ts_methods()
      shiny::radioButtons(ns("ts_method"), "Time-series analysis",
                          choices = stats::setNames(methods, TS_METHOD_LABELS[methods]),
                          selected = methods[[1]])
    })
    output$ts_info_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series", ts_time_col())
      shiny::div(class = "alert alert-info", style = "padding:8px 12px;",
        shiny::HTML(sprintf("Time ordered by <b>%s</b> (from the Data tab). The seasonal period is the number of observations in one cycle (e.g. 12 for monthly data with a yearly season, 4 for quarterly, 7 for daily-with-weekly).",
                            ts_time_col())))
    })
    output$ts_options_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series", input$ts_method)
      inferred <- tryCatch(ts_infer_frequency(data()[[ts_time_col()]]),
                           error = function(e) 1L)
      controls <- list(shiny::numericInput(ns("ts_frequency"), "Seasonal period",
                                           value = inferred, min = 1, max = 366, step = 1))
      if (input$ts_method %in% c("ts_holt_winters", "ts_arima", "ts_garch"))
        controls <- c(controls, list(shiny::numericInput(ns("ts_horizon"),
          "Forecast horizon (steps)", value = min(12L, inferred), min = 1, max = 240, step = 1)))
      if (identical(input$ts_method, "ts_regression"))
        controls <- c(controls, list(shiny::selectInput(ns("ts_trend_degree"),
          "Trend shape", choices = c("Linear" = "1", "Quadratic" = "2"), selected = "1")))
      do.call(shiny::tagList, controls)
    })
    output$ts_predictors_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series",
                 identical(input$ts_method, "ts_regression"))
      choices <- setdiff(ts_value_vars(), input$ts_value)
      shiny::selectizeInput(ns("ts_predictors"), "External predictors (optional)",
                            choices = choices, multiple = TRUE,
                            options = list(placeholder = "Numeric covariates measured over the same times"))
    })

    spec <- shiny::reactive({
      shiny::req(input$analysis)
      if (input$analysis == "time_series") {
        shiny::req(input$ts_value, input$ts_method, ts_time_col())
        return(list(
          analysis = "time_series",
          method = input$ts_method,
          value = input$ts_value,
          time = ts_time_col(),
          frequency = input$ts_frequency,
          horizon = input$ts_horizon,
          trend_degree = suppressWarnings(as.integer(input$ts_trend_degree %||% "1")),
          predictors = input$ts_predictors))
      }
      if (input$analysis == "clustered") {
        shiny::req(input$cluster_outcome, input$cluster_method,
                   input$cluster_predictors, input$cluster)
        list(
          analysis = "clustered",
          outcome = input$cluster_outcome,
          outcome_type = resolved_cluster_outcome_type(),
          predictors = input$cluster_predictors,
          method = input$cluster_method,
          cluster = input$cluster,
          corstr = input$corstr,
          time = input$cluster_time,
          random_slope = input$random_slope
        )
      } else {
        d <- surv_decl()
        shiny::req(input$survival_method, d$event,
                   !is.null(d$time) || (!is.null(d$start) && !is.null(d$end)))
        list(
          analysis = "survival",
          method = input$survival_method,
          time = d$time,
          start = d$start,
          end = d$end,
          id = d$id,
          event = d$event,
          strata = input$survival_strata,
          predictors = input$survival_predictors
        )
      }
    })

    output$preview <- shiny::renderUI({
      shiny::req(input$analysis)
      if (input$analysis == "time_series") {
        shiny::req(input$ts_method)
        return(shiny::HTML(sprintf(
          "<b>Plan:</b> %s. Time-series methods assume the rows are in time order; confirm stationarity before forecasting.",
          TS_METHOD_LABELS[[input$ts_method]])))
      }
      if (input$analysis == "clustered") {
        shiny::req(input$cluster_outcome)
        plan <- suggest_model_plan(resolved_cluster_outcome_type(), "clustered")
        return(shiny::HTML(sprintf(
          "<b>Plan:</b> %s<br>GEE gives population-average effects; LMM/GLMM give subject-specific effects.",
          plan$why)))
      }
      shiny::HTML("<b>Plan:</b> Survival analysis accounts for right-censored follow-up. Kaplan-Meier describes or compares curves; Cox regression estimates adjusted hazard ratios.")
    })

    result <- shiny::eventReactive(input$run, {
      shiny::req(spec())
      tryCatch({
        if (spec()$analysis == "time_series")
          shiny::withProgress(message = "Fitting the time-series analysis...", value = 0.35, {
            run_guided_timeseries(data(), spec())
          })
        else if (spec()$analysis == "clustered")
          {
            prepared <- prepare_outcome_data(data(), spec()$outcome,
                                             spec()$outcome_type,
                                             c(spec()$predictors, spec()$cluster,
                                               spec()$time))
            shiny::withProgress(message = "Fitting the clustered model...", value = 0.35, {
              run_guided_clustered_model(prepared$data, spec())
            })
          }
        else
          shiny::withProgress(message = "Fitting the survival model...", value = 0.35, {
            run_guided_survival(data(), spec())
          })
      }, error = function(e) list(error = conditionMessage(e)))
    })
    is_ok <- shiny::reactive(!is.null(result()) && is.null(result()$error))

    output$recommend_badge <- shiny::renderUI({
      shiny::req(result())
      if (!is_ok()) return(shiny::div(class = "alert alert-danger",
                                      paste("Could not fit analysis:", result()$error)))
      shiny::div(class = "alert alert-success",
                 shiny::HTML(sprintf("<b>Analysis:</b> %s<br>%s",
                                     result()$label, result()$rationale)))
    })
    output$coefficients <- shiny::renderTable({
      shiny::req(is_ok()); result()$coefficients
    }, digits = 4)
    output$fit_stats <- shiny::renderTable({
      shiny::req(is_ok()); result()$fit_stats
    }, digits = 4)
    output$assumptions <- shiny::renderUI({
      shiny::req(is_ok())
      a <- result()$assumptions
      if (length(a) == 0) return(shiny::tags$em("No formal diagnostics were available."))
      shiny::tags$ul(lapply(a, function(x) shiny::tags$li(x$note)))
    })
    output$diagnostic_plot <- shiny::renderPlot({
      shiny::req(is_ok())
      p <- result()$diagnostic_plot
      shiny::req(!is.null(p))
      p
    })
    output$interpretation <- shiny::renderText({
      shiny::req(is_ok()); result()$interpretation
    })

    mod_interpret_server("interpret", shiny::reactive({
      if (!is_ok()) return(list())
      res <- result()
      list(
        tables = list("Results" = res$coefficients,
                      "Model fit" = res$fit_stats),
        context = list(title = res$label,
                       analysis = switch(spec()$analysis,
                         survival = "Time-to-event survival analysis",
                         time_series = "Time-series analysis",
                         "Clustered / repeated-measures model"),
                       method = res$method, effect_name = res$effect_name,
                       app_interpretation = res$interpretation)
      )
    }), audit = audit)

    shiny::observeEvent(input$add, {
      shiny::req(is_ok())
      res <- result()
      add_report(list(kind = "table",
                      caption = paste("Advanced analysis results:", res$label),
                      data = res$coefficients))
      add_report(list(kind = "table",
                      caption = paste("Analysis fit:", res$label),
                      data = res$fit_stats))
      if (length(res$assumptions))
        add_report(list(kind = "table",
                        caption = paste("Analysis diagnostics:", res$label),
                        data = model_assumptions_df(res$assumptions)))
      if (!is.null(res$diagnostic_plot))
        add_report(list(kind = "plot",
                        caption = paste("Analysis plot:", res$label),
                        plot = res$diagnostic_plot))
      shiny::showNotification("Advanced analysis added to report.",
                              type = "message", duration = 3)
    })
  })
}
