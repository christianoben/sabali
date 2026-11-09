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
      shiny::uiOutput(ns("cluster_interactions_ui")),
      shiny::uiOutput(ns("cluster_id_ui")),
      shiny::uiOutput(ns("correlation_ui")),
      shiny::uiOutput(ns("cluster_time_ui")),
      shiny::uiOutput(ns("random_slope_ui")),
      shiny::uiOutput(ns("survival_method_ui")),
      shiny::uiOutput(ns("survival_info_ui")),
      shiny::uiOutput(ns("survival_strata_ui")),
      shiny::uiOutput(ns("survival_predictors_ui")),
      shiny::uiOutput(ns("ts_value_ui")),
      shiny::uiOutput(ns("ts_group_ui")),
      shiny::uiOutput(ns("ts_series_ui")),
      shiny::uiOutput(ns("ts_method_ui")),
      shiny::uiOutput(ns("ts_info_ui")),
      shiny::uiOutput(ns("ts_options_ui")),
      shiny::uiOutput(ns("ts_predictors_ui")),
      shiny::uiOutput(ns("path_ui")),
      shiny::uiOutput(ns("rates_ui")),
      shiny::tags$hr(),
      shiny::actionButton(ns("run"), "Fit analysis", icon = shiny::icon("play"),
                          class = "btn-primary"),
      shiny::actionButton(ns("add"), "Add to report", icon = shiny::icon("plus")),
      shiny::checkboxInput(ns("include_diag"),
                           "Include diagnostics in the report", value = FALSE)
    ),
    shiny::mainPanel(
      width = 9,
      shiny::div(class = "alert alert-info", shiny::htmlOutput(ns("preview"))),
      shiny::uiOutput(ns("recommend_badge")),
      shiny::h5("Results"),
      shiny::tableOutput(ns("coefficients")),
      shiny::h5("Model fit"),
      shiny::tableOutput(ns("fit_stats")),
      shiny::conditionalPanel(
        condition = sprintf("output['%s']", ns("has_random_effects")),
        shiny::h5("Random effects (variance components)"),
        shiny::tableOutput(ns("random_effects"))),
      shiny::conditionalPanel(
        condition = sprintf("output['%s']", ns("has_rates")),
        shiny::h5("Rates"), shiny::tableOutput(ns("rates_table")),
        shiny::h5("Rate ratios"), shiny::tableOutput(ns("rate_ratios"))),
      shiny::conditionalPanel(
        condition = sprintf("output['%s']", ns("has_effects")),
        shiny::h5("Indirect and total effects"),
        shiny::tableOutput(ns("effects"))),
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
                              survey = NULL, plan_rec = NULL, add_report,
                              audit = NULL) {
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
                if (isTRUE(s$time_series)) c("Time series (trend / seasonality / forecast)" = "time_series"),
                # A path model needs no particular data layout - only an
                # outcome, a mediator and an exposure - so unlike the others it
                # is not gated on the declared structure.
                c("Path analysis / mediation" = "path"),
                # Rates need only an event count and person-time, so like path
                # analysis they are not gated on the declared data structure.
                c("Incidence rates & standardisation" = "rates"))
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

    output$rates_ui <- shiny::renderUI({
      shiny::req(identical(input$analysis, "rates"))
      num <- names(types())[types() %in% c("continuous", "count")]
      cat_ <- names(types())[types() %in% c("binary", "nominal", "ordinal")]
      pick <- function(id, label, choices, help, none = TRUE) shiny::tagList(
        shiny::selectInput(ns(id), label,
                           choices = if (none) c("(none)" = "", choices) else choices,
                           selected = shiny::isolate(input[[id]])),
        shiny::helpText(help))
      shiny::tagList(
        shiny::helpText(shiny::strong("A rate divides events by person-TIME"),
          ", so it stays interpretable when people are followed for different ",
          "lengths - which a proportion does not. One row per group or stratum."),
        pick("rate_events", "Event count", num,
             "Number of events in each row.", none = FALSE),
        pick("rate_time", "Person-time at risk", num,
             "Total follow-up in each row, in one consistent unit.", none = FALSE),
        shiny::numericInput(ns("rate_per"), "Report rates per", value = 1000,
                            min = 1, step = 100),
        shiny::textInput(ns("rate_unit"), "Time unit", value = "person-years"),
        shiny::tags$hr(),
        shiny::helpText(shiny::strong("Optional:"), " compare groups, OR ",
          "standardise. Comparing crude rates between populations with ",
          "different age structures compares the structures as much as the risk."),
        pick("rate_group", "Compare rates across", cat_,
             "Gives a rate per group and a rate ratio against the reference."),
        pick("rate_std", "Standard population (direct standardisation)", num,
             "Reference population size per stratum. Use when this population's own stratum rates are stable."),
        pick("rate_ref", "Reference rates (indirect standardisation / SMR)", num,
             "A reference population's rate per stratum, on the same time unit. Use when the stratum rates here are sparse."))
    })

    # --- path analysis controls ---------------------------------------------
    output$path_ui <- shiny::renderUI({
      shiny::req(identical(input$analysis, "path"))
      pool <- setdiff(names(types()), reserved())
      sel <- function(id, label, help, multiple = FALSE) shiny::tagList(
        shiny::selectizeInput(ns(id), label, choices = pool, multiple = multiple,
                              selected = shiny::isolate(input[[id]]),
                              options = list(placeholder = "Choose...")),
        shiny::helpText(help))
      shiny::tagList(
        shiny::helpText(shiny::strong("A path model splits an exposure's effect"),
          " into the part that acts directly on the outcome and the parts that ",
          "act through the mediators. A regression adjusted for a mediator does ",
          "not decompose that - it removes it."),
        sel("path_outcome", "Final outcome",
            "What the exposures are thought to affect."),
        sel("path_mediators", "Mediators, in causal order (earliest first)",
            "The variables the exposure acts THROUGH. The order you list them in is your causal claim - it is not estimated from the data.",
            multiple = TRUE),
        sel("path_exposures", "Exposures",
            "The variables whose effect you want to decompose.", multiple = TRUE),
        sel("path_covariates", "Adjust for (optional)",
            "Held constant in every equation; no indirect effects are computed for these.",
            multiple = TRUE),
        shiny::checkboxInput(ns("path_linear"),
          "Treat binary variables as continuous (linear probability)",
          value = FALSE),
        shiny::helpText(
          "Leave this unticked to model binary outcomes as probit, which is ",
          "usually what you want. Tick it only to apply a declared sampling ",
          "weight or cluster-robust standard errors, which lavaan cannot ",
          "combine with a probit model - the estimates then become ",
          "risk-difference effects rather than probit coefficients.")
      )
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

    # Same contract as the Regression tab: pairs of already-chosen predictors
    # only, so both main effects are always in the model.
    output$cluster_interactions_ui <- shiny::renderUI({
      shiny::req(input$analysis == "clustered", input$cluster_outcome)
      chosen <- input$cluster_predictors
      if (length(chosen) < 2) return(NULL)
      combos <- utils::combn(chosen, 2, FUN = function(p) paste(p, collapse = ":"))
      prev <- shiny::isolate(input$cluster_interactions) %||% character(0)
      if (!length(prev)) prev <- plan_interactions(plan_now())
      keep <- combos[.interaction_key(combos) %in% .interaction_key(prev)]
      shiny::tagList(
        shiny::selectizeInput(ns("cluster_interactions"), "Interaction terms (optional)",
                              choices = combos, selected = keep, multiple = TRUE,
                              options = list(placeholder = "e.g. treatment:sex - does one effect depend on the other?")),
        shiny::helpText("An interaction tests whether the effect of one predictor changes with the level of another. Both main effects stay in the model.")
      )
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
    # Candidate "series / group" columns for stacked (multi-series) data: the
    # categorical / identifier columns, minus the time and value columns.
    ts_group_vars <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% c("binary", "nominal", "ordinal", "identifier")]
      setdiff(nm, c(ts_time_col(), input$ts_value,
                    reserved_analysis_vars(panel_spec())))
    })
    ts_group_col <- shiny::reactive({
      g <- input$ts_group
      if (is.null(g) || !nzchar(g) || !g %in% names(data())) NULL else g
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
    output$ts_group_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series")
      gv <- ts_group_vars()
      if (!length(gv)) return(NULL)
      shiny::tagList(
        shiny::selectInput(ns("ts_group"),
          "Series / group column (for stacked data, optional)",
          choices = c("Single series (none)" = "", gv),
          selected = input$ts_group %||% ""),
        shiny::helpText("If your file stacks several series together (e.g. one row per site per month), pick the column that identifies each series."))
    })
    output$ts_series_ui <- shiny::renderUI({
      shiny::req(input$analysis == "time_series", ts_group_col())
      levs <- tryCatch(.ts_group_levels(data()[[ts_group_col()]]),
                       error = function(e) character(0))
      shiny::req(length(levs) >= 1)
      shiny::selectInput(ns("ts_series"), "Which series?",
        choices = c(stats::setNames("__all__",
                      sprintf("All %d series (combined table)", length(levs))),
                    stats::setNames(levs, levs)),
        selected = input$ts_series %||% "__all__")
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
          group = ts_group_col(),
          group_value = if (is.null(ts_group_col())) NULL else (input$ts_series %||% "__all__"),
          frequency = input$ts_frequency,
          horizon = input$ts_horizon,
          trend_degree = suppressWarnings(as.integer(input$ts_trend_degree %||% "1")),
          predictors = input$ts_predictors))
      }
      if (input$analysis == "rates") {
        shiny::req(input$rate_events, input$rate_time)
        return(list(analysis = "rates", events = input$rate_events,
                    person_time = input$rate_time,
                    per = input$rate_per %||% 1000,
                    time_unit = input$rate_unit %||% "person-time",
                    group = input$rate_group %||% "",
                    std_population = input$rate_std %||% "",
                    reference_rates = input$rate_ref %||% ""))
      }
      if (input$analysis == "path") {
        shiny::req(input$path_outcome, input$path_mediators, input$path_exposures)
        svy <- if (is.null(survey)) list() else
          tryCatch(survey() %||% list(), error = function(e) list())
        return(list(
          analysis = "path", outcome = input$path_outcome,
          mediators = input$path_mediators, exposures = input$path_exposures,
          covariates = input$path_covariates %||% character(0),
          linear = isTRUE(input$path_linear), types = types(),
          weights = svy$weights, psu = svy$psu))
      }
      if (input$analysis == "clustered") {
        shiny::req(input$cluster_outcome, input$cluster_method,
                   input$cluster_predictors, input$cluster)
        list(
          analysis = "clustered",
          outcome = input$cluster_outcome,
          outcome_type = resolved_cluster_outcome_type(),
          predictors = input$cluster_predictors,
          interactions = input$cluster_interactions %||% character(0),
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
      if (input$analysis == "rates") {
        return(shiny::HTML("<b>Plan:</b> Incidence rates over person-time with exact Poisson intervals, plus a rate ratio when a grouping variable is chosen, or a standardised rate / SMR when a standard population or reference rates are supplied."))
      }
      if (input$analysis == "path") {
        return(shiny::HTML("<b>Plan:</b> An observed-variable path model. Each exposure gets a direct path to the outcome and an indirect route through every mediator; the indirect effects are products of path coefficients, reported with confidence intervals. Latent factors are not fitted."))
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

    # The fit is held in a reactiveVal so it can be INVALIDATED when the
    # underlying data changes (e.g. a level removed via the tab-0 Data contract).
    # Otherwise a stale fit would keep showing coefficients for values the data
    # no longer contains. The analyst re-runs to refit on the current data.
    fit_store <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, {
      shiny::req(spec())
      out <- tryCatch({
        if (spec()$analysis == "rates")
          shiny::withProgress(message = "Computing rates...", value = 0.35, {
            run_rate_analysis(data(), spec())
          })
        else if (spec()$analysis == "path")
          shiny::withProgress(message = "Fitting the path model...", value = 0.35, {
            run_path_model(data(), spec())
          })
        else if (spec()$analysis == "time_series")
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
      fit_store(out)
    })
    # Drop a stale fit the moment the data changes, so removed levels/ranges can
    # never appear in the displayed results.
    shiny::observeEvent(data(), fit_store(NULL), ignoreInit = TRUE)
    result <- shiny::reactive(fit_store())
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
    output$rates_table <- shiny::renderTable({
      shiny::req(is_ok()); result()$rates
    }, rownames = FALSE)
    output$has_rates <- shiny::reactive(is_ok() && !is.null(result()$rates))
    shiny::outputOptions(output, "has_rates", suspendWhenHidden = FALSE)
    output$rate_ratios <- shiny::renderTable({
      shiny::req(is_ok()); result()$rate_ratios
    }, rownames = FALSE)

    output$effects <- shiny::renderTable({
      shiny::req(is_ok()); result()$effects
    }, rownames = FALSE)
    output$has_effects <- shiny::reactive(is_ok() && !is.null(result()$effects))
    shiny::outputOptions(output, "has_effects", suspendWhenHidden = FALSE)

    output$random_effects <- shiny::renderTable({
      shiny::req(is_ok()); result()$random_effects
    }, digits = 4)
    output$has_random_effects <- shiny::reactive({
      is_ok() && !is.null(result()$random_effects)
    })
    shiny::outputOptions(output, "has_random_effects", suspendWhenHidden = FALSE)
    output$assumptions <- shiny::renderUI({
      shiny::req(is_ok())
      assumption_ui(result()$assumptions,
                    empty = "No formal diagnostics were available.")
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

    interp_payload <- shiny::reactive({
      if (!is_ok()) return(list())
      res <- result()
      list(
        tables = c(list("Results" = res$coefficients,
                        "Model fit" = res$fit_stats),
                   if (!is.null(res$random_effects))
                     list("Random effects" = res$random_effects),
                   if (!is.null(res$effects))
                     list("Indirect and total effects" = res$effects),
                   if (!is.null(res$rates)) list("Rates" = res$rates),
                   if (!is.null(res$rate_ratios))
                     list("Rate ratios" = res$rate_ratios),
                   if (!is.null(res$missing_summary))
                     list("Missing data" = res$missing_summary)),
        context = list(title = res$label,
                       analysis = switch(spec()$analysis,
                         survival = "Time-to-event survival analysis",
                         time_series = "Time-series analysis",
                         "Clustered / repeated-measures model"),
                       method = res$method, effect_name = res$effect_name,
                       app_interpretation = res$interpretation)
      )
    })
    interp <- mod_interpret_server("interpret", interp_payload, audit = audit)

    shiny::observeEvent(input$add, {
      shiny::req(is_ok())
      res <- result()
      p <- interp_payload()
      ai <- if (length(p$tables %||% list()))
        list(tables = p$tables, context = p$context,
             interpretation = interp_reuse_text(interp$interpretation(), p$context))
      else NULL
      add_report(list(kind = "table",
                      caption = paste("Advanced analysis results:", res$label),
                      data = res$coefficients, ai = ai))
      add_report(list(kind = "table",
                      caption = paste("Analysis fit:", res$label),
                      data = res$fit_stats))
      if (!is.null(res$random_effects))
        add_report(list(kind = "table",
                        caption = paste("Random effects (variance components):", res$label),
                        data = res$random_effects))
      # Diagnostics are working material; include only on request (paper-style).
      if (isTRUE(input$include_diag)) {
        if (length(res$assumptions))
          add_report(list(kind = "table",
                          caption = paste("Analysis diagnostics:", res$label),
                          data = model_assumptions_df(res$assumptions)))
        if (!is.null(res$diagnostic_plot))
          add_report(list(kind = "plot",
                          caption = paste("Analysis plot:", res$label),
                          plot = res$diagnostic_plot))
      }
      shiny::showNotification("Advanced analysis added to report.",
                              type = "message", duration = 3)
    })
  })
}
