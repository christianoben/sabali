# mod_models.R ---------------------------------------------------------------
# Phase 3 Models tab: independent regression only.

mod_models_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Independent regression"),
      shiny::helpText(
        "GLM methods are shown for binary and count outcomes: logistic, Poisson, and negative-binomial GLMs. Select candidate outcomes in 1 · Data first."
      ),
      shiny::uiOutput(ns("ai_gate_ui")),
      shiny::uiOutput(ns("unrecommended_ui")),
      shiny::uiOutput(ns("outcome_ui")),
      shiny::uiOutput(ns("outcome_notice")),
      shiny::uiOutput(ns("outcome_type_ui")),
      shiny::uiOutput(ns("method_ui")),
      shiny::uiOutput(ns("predictors_ui")),
      shiny::uiOutput(ns("interactions_ui")),
      shiny::uiOutput(ns("tau_ui")),
      shiny::uiOutput(ns("lambda_ui")),
      shiny::uiOutput(ns("impute_ui")),
      shiny::tags$hr(),
      shiny::actionButton(ns("run"), "Fit model", icon = shiny::icon("play"),
                          class = "btn-primary"),
      shiny::actionButton(ns("add"), "Add to report", icon = shiny::icon("plus")),
      shiny::checkboxInput(ns("include_diag"),
                           "Include diagnostics in the report", value = FALSE)
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput(ns("kind_note")),
      shiny::div(class = "alert alert-info", shiny::htmlOutput(ns("preview"))),
      shiny::uiOutput(ns("recommend_badge")),
      shiny::h5("Coefficients"),
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

mod_models_server <- function(id, data, types, outcomes = NULL, panel = NULL,
                             survey = NULL, plan_rec = NULL, add_report,
                             audit = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # AI-plan gate (see mod_tests for the pattern). Narrows the offered model
    # methods to the AI-recommended regression, on top of the existing layout
    # gate; the override checkbox (shown only when a plan exists) restores the
    # full feasible menu.
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
    # Survival-role columns and the subject ID are reserved: modelling censored
    # follow-up time with OLS/GLM is invalid (use Cox on the Advanced tab), and
    # an ID is not an analysis variable.
    reserved <- shiny::reactive(reserved_analysis_vars(panel_spec()))
    model_kind <- shiny::reactive(analysis_layout_kind(panel_spec()))
    show_all <- shiny::reactive(isTRUE(input$show_all))
    # Independent regression assumes independent rows: recommended only for
    # cross-sectional data. For repeated / time-series data it is hidden by
    # default and the user is pointed to the Advanced models tab, unless they
    # opt in to the un-recommended analyses.
    regression_ok <- shiny::reactive(regression_recommended_for(model_kind()) || show_all())
    output$kind_note <- shiny::renderUI({
      note <- analysis_kind_note(panel_spec())
      if (is.null(note)) return(NULL)
      shiny::div(class = "alert alert-warning", shiny::HTML(note))
    })
    output$unrecommended_ui <- shiny::renderUI({
      if (regression_recommended_for(model_kind())) return(NULL)
      shiny::checkboxInput(ns("show_all"),
        "Include un-recommended analyses (fit independent regression anyway)",
        value = FALSE)
    })
    outcome_vars <- shiny::reactive({
      if (!regression_ok()) return(character(0))
      df <- data(); tp <- types()
      selected <- if (is.null(outcomes)) default_outcome_candidates(df, tp) else outcomes()
      setdiff(analysis_outcome_vars(df, tp, selected, MODEL_OUTCOME_TYPES), reserved())
    })
    predictor_vars <- shiny::reactive({
      tp <- types()
      nm <- names(tp)[tp %in% MODEL_PREDICTOR_TYPES]
      setdiff(nm, c(input$outcome, reserved()))
    })
    resolved_outcome_type <- shiny::reactive({
      shiny::req(input$outcome, data())
      opts <- intersect(outcome_type_options(data()[[input$outcome]]),
                        MODEL_OUTCOME_TYPES)
      shiny::req(length(opts) > 0)
      source_type <- types()[[input$outcome]]
      default_type <- if (!is.null(source_type) && source_type %in% opts) {
        source_type
      } else {
        opts[[1]]
      }
      if (!is.null(input$outcome_type) && input$outcome_type %in% opts)
        input$outcome_type else default_type
    })

    output$outcome_ui <- shiny::renderUI({
      shiny::selectInput(ns("outcome"), "Outcome variable",
                         choices = outcome_vars())
    })

    output$outcome_notice <- shiny::renderUI({
      shiny::req(data())
      if (!regression_ok())
        return(shiny::div(class = "alert alert-info",
          shiny::HTML("Independent regression is not appropriate for this data layout. Use the <b>Advanced models</b> tab (GEE / LMM / GLMM) for clustered or repeated data, or tick the box above to fit it anyway.")))
      if (length(outcome_vars())) return(NULL)
      shiny::div(
        class = "alert alert-warning",
        "No candidate outcome variables are selected. Go to 1 · Data and select one or more candidate outcomes."
      )
    })

    output$outcome_type_ui <- shiny::renderUI({
      shiny::req(input$outcome, data())
      opts <- outcome_type_options(data()[[input$outcome]])
      opts <- intersect(opts, MODEL_OUTCOME_TYPES)
      shiny::req(length(opts) > 0)
      source_type <- types()[[input$outcome]]
      default_type <- if (!is.null(source_type) && source_type %in% opts) {
        source_type
      } else {
        opts[[1]]
      }
      shiny::selectInput(ns("outcome_type"), "Use outcome as",
                         choices = stats::setNames(opts, OUTCOME_TARGET_LABELS[opts]),
                         selected = if (!is.null(input$outcome_type) && input$outcome_type %in% opts)
                           input$outcome_type else default_type)
    })

    # A complex-sample design declared on the Data tab, or an empty list.
    survey_spec <- shiny::reactive({
      if (is.null(survey)) return(list())
      tryCatch(survey() %||% list(), error = function(e) list())
    })
    survey_on <- shiny::reactive(survey_design_declared(survey_spec()))

    output$method_ui <- shiny::renderUI({
      shiny::req(input$outcome)
      mp <- suggest_model_plan(resolved_outcome_type(), "independent")
      methods <- gate_choices(mp$methods, plan_regression_id(plan_now()),
                              ai_gate_on())
      # With a design declared, the design-based method is the defensible one:
      # every method in the ordinary menu assumes the rows are a simple random
      # sample, which they are not. It is offered first and selected by default,
      # and the others stay available for a deliberate unweighted comparison.
      svy <- if (survey_on())
        survey_available_methods(resolved_outcome_type()) else character(0)
      methods <- unique(c(svy, methods))
      selected <- if (length(svy)) svy[[1]]
                  else if (mp$default %in% methods) mp$default else methods[[1]]
      shiny::tagList(
        if (length(svy)) shiny::div(class = "alert alert-info",
          style = "padding:8px 10px;", shiny::icon("layer-group"),
          " A complex sample design is declared, so design-based estimation is offered. The other methods treat the rows as a simple random sample."),
        shiny::radioButtons(ns("method"), "Model method",
                            choices = stats::setNames(methods,
                                                      MODEL_METHOD_LABELS[methods]),
                            selected = selected))
    })

    output$predictors_ui <- shiny::renderUI({
      shiny::req(input$outcome)
      shiny::selectizeInput(ns("predictors"),
                            "Predictors / fixed effects",
                            choices = predictor_vars(), multiple = TRUE,
                            options = list(placeholder = "Choose one or more predictors"))
    })

    # Interactions are offered only as pairs of already-chosen predictors, which
    # keeps both main effects in the model by construction. Rebuilding the list
    # when the predictors change would otherwise silently drop a selection, so
    # still-valid picks are carried across; the match is order-insensitive
    # because `a:b` and `b:a` are the same term to R.
    output$interactions_ui <- shiny::renderUI({
      chosen <- input$predictors
      if (length(chosen) < 2) return(NULL)
      combos <- utils::combn(chosen, 2, FUN = function(p) paste(p, collapse = ":"))
      # A previous selection wins; failing that, an interaction the AI plan asked
      # for is offered pre-ticked, so a planned effect-modification question does
      # not have to be re-entered by hand.
      prev <- shiny::isolate(input$interactions) %||% character(0)
      if (!length(prev)) prev <- plan_interactions(plan_now())
      keep <- combos[.interaction_key(combos) %in% .interaction_key(prev)]
      shiny::tagList(
        shiny::selectizeInput(ns("interactions"), "Interaction terms (optional)",
                              choices = combos, selected = keep, multiple = TRUE,
                              options = list(placeholder = "e.g. treatment:sex - does one effect depend on the other?")),
        shiny::helpText("An interaction tests whether the effect of one predictor changes with the level of another. Both main effects stay in the model.")
      )
    })

    output$tau_ui <- shiny::renderUI({
      shiny::req(input$method == "quantile")
      shiny::numericInput(ns("tau"), "Quantile (tau)",
                          value = 0.5, min = 0.05, max = 0.95, step = 0.05)
    })

    output$lambda_ui <- shiny::renderUI({
      shiny::req(input$method %in% c("lasso", "ridge"))
      shiny::selectInput(ns("lambda_choice"),
                         "Penalty (lambda) chosen by",
                         choices = c("Minimum CV error" = "min",
                                     "1-SE rule (stronger shrinkage)" = "1se"),
                         selected = "min")
    })

    # Multiple imputation, offered only when it would actually change anything:
    # the decision turns on how many COMPLETE rows the model would be left with,
    # not on per-variable missingness.
    output$impute_ui <- shiny::renderUI({
      shiny::req(input$outcome, input$predictors)
      if (!requireNamespace("mice", quietly = TRUE)) return(NULL)
      if (!input$method %in% c("ols", "logistic", "poisson")) return(NULL)
      ms <- tryCatch(missing_summary(data(), c(input$outcome, input$predictors)),
                     error = function(e) NULL)
      if (is.null(ms) || !ms$any_missing || ms$pct_lost < 1) return(NULL)
      shiny::tagList(
        shiny::tags$hr(),
        shiny::div(class = "alert alert-warning", style = "padding:8px 10px;",
          sprintf("Complete-case analysis would drop %.1f%% of rows (%s of %s).",
                  ms$pct_lost,
                  format(ms$n_rows - ms$n_complete, big.mark = ","),
                  format(ms$n_rows, big.mark = ","))),
        shiny::checkboxInput(ns("use_impute"),
          "Handle the missing data by multiple imputation", value = FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("use_impute")),
          shiny::numericInput(ns("impute_m"), "Imputations (m)", value = 20,
                              min = 2, max = 200, step = 5),
          shiny::helpText(
            "Each missing value is imputed several times and the fits are pooled ",
            "by Rubin's rules, so the uncertainty about the missing values ends ",
            "up in the standard errors. This assumes the data are missing at ",
            "random given the model's variables - an assumption about the ",
            "missingness that the data cannot confirm.")))
    })

    spec <- shiny::reactive({
      shiny::req(input$outcome, input$method, input$predictors)
      list(
        outcome = input$outcome,
        outcome_type = resolved_outcome_type(),
        predictors = input$predictors,
        interactions = input$interactions %||% character(0),
        method = input$method,
        # The declared design travels with the spec so the fit, the report and
        # a re-run all describe the same sample.
        weights = survey_spec()$weights, strata = survey_spec()$strata,
        psu = survey_spec()$psu, fpc = survey_spec()$fpc,
        psu2 = survey_spec()$psu2, fpc2 = survey_spec()$fpc2,
        repweights = survey_spec()$repweights,
        reptype = survey_spec()$reptype, scale = survey_spec()$scale,
        rho = survey_spec()$rho,
        tau = input$tau,
        lambda_choice = input$lambda_choice %||% "min",
        impute = isTRUE(input$use_impute),
        m = input$impute_m %||% 20L
      )
    })

    output$preview <- shiny::renderUI({
      shiny::req(input$outcome)
      plan <- suggest_model_plan(resolved_outcome_type(), "independent")
      shiny::HTML(sprintf(
        "<b>Plan:</b> %s<br>Complete rows are used for the selected outcome and predictors; the number used is shown after fitting.",
        plan$why))
    })

    # Held in a reactiveVal so it can be invalidated when the data changes,
    # so a stale fit never shows coefficients for levels/ranges the data no
    # longer contains (e.g. after a tab-0 Data-contract edit).
    fit_store <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, {
      shiny::req(spec())
      out <- tryCatch({
        design_cols <- unlist(spec()[c("weights", "strata", "psu", "fpc",
                                       "psu2", "fpc2")])
        design_cols <- design_cols[!is.na(design_cols) & nzchar(design_cols)]
        if (survey_is_replicate(spec()))
          design_cols <- c(design_cols,
                           survey_resolve_repweights(data(), spec()$repweights))
        prepared <- prepare_outcome_data(data(), spec()$outcome,
                                         spec()$outcome_type,
                                         c(spec()$predictors, design_cols))
        shiny::withProgress(message = "Fitting the model...", value = 0.35, {
          if (spec()$method %in% names(SURVEY_METHOD_LABELS))
            run_survey_model(prepared$data, spec())
          else if (isTRUE(spec()$impute))
            run_imputed_model(prepared$data, spec())
          else run_guided_model(prepared$data, spec())
        })
      },
               error = function(e) list(error = conditionMessage(e)))
      fit_store(out)
    })
    shiny::observeEvent(data(), fit_store(NULL), ignoreInit = TRUE)
    result <- shiny::reactive(fit_store())
    is_ok <- shiny::reactive(!is.null(result()) && is.null(result()$error))

    output$recommend_badge <- shiny::renderUI({
      shiny::req(result())
      if (!is_ok()) return(shiny::div(class = "alert alert-danger",
                                      paste("Could not fit model:", result()$error)))
      shiny::div(class = "alert alert-success",
                 shiny::HTML(sprintf("<b>Model:</b> %s<br>%s",
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
        tables = list("Coefficients" = res$coefficients,
                      "Model fit" = res$fit_stats),
        context = list(title = res$label, analysis = "Independent regression",
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
                      caption = paste("Model coefficients:", res$label),
                      data = res$coefficients, ai = ai))
      add_report(list(kind = "table",
                      caption = paste("Model fit:", res$label),
                      data = res$fit_stats))
      # Diagnostics are working material, not usually part of a paper's tables;
      # include them only when the analyst explicitly asks.
      if (isTRUE(input$include_diag)) {
        if (length(res$assumptions))
          add_report(list(kind = "table",
                          caption = paste("Model diagnostics:", res$label),
                          data = model_assumptions_df(res$assumptions)))
        if (!is.null(res$diagnostic_plot))
          add_report(list(kind = "plot",
                          caption = paste("Diagnostic plot:", res$label),
                          plot = res$diagnostic_plot))
      }
      shiny::showNotification("Model results added to report.",
                              type = "message", duration = 3)
    })
  })
}
