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
      shiny::uiOutput(ns("tau_ui")),
      shiny::uiOutput(ns("lambda_ui")),
      shiny::tags$hr(),
      shiny::actionButton(ns("run"), "Fit model", icon = shiny::icon("play"),
                          class = "btn-primary"),
      shiny::actionButton(ns("add"), "Add to report", icon = shiny::icon("plus"))
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
                             plan_rec = NULL, add_report, audit = NULL) {
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

    output$method_ui <- shiny::renderUI({
      shiny::req(input$outcome)
      mp <- suggest_model_plan(resolved_outcome_type(), "independent")
      methods <- gate_choices(mp$methods, plan_regression_id(plan_now()),
                              ai_gate_on())
      selected <- if (mp$default %in% methods) mp$default else methods[[1]]
      shiny::radioButtons(ns("method"), "Model method",
                          choices = stats::setNames(methods,
                                                    MODEL_METHOD_LABELS[methods]),
                          selected = selected)
    })

    output$predictors_ui <- shiny::renderUI({
      shiny::req(input$outcome)
      shiny::selectizeInput(ns("predictors"),
                            "Predictors / fixed effects",
                            choices = predictor_vars(), multiple = TRUE,
                            options = list(placeholder = "Choose one or more predictors"))
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

    spec <- shiny::reactive({
      shiny::req(input$outcome, input$method, input$predictors)
      list(
        outcome = input$outcome,
        outcome_type = resolved_outcome_type(),
        predictors = input$predictors,
        method = input$method,
        tau = input$tau,
        lambda_choice = input$lambda_choice %||% "min"
      )
    })

    output$preview <- shiny::renderUI({
      shiny::req(input$outcome)
      plan <- suggest_model_plan(resolved_outcome_type(), "independent")
      shiny::HTML(sprintf(
        "<b>Plan:</b> %s<br>Complete rows are used for the selected outcome and predictors; the number used is shown after fitting.",
        plan$why))
    })

    result <- shiny::eventReactive(input$run, {
      shiny::req(spec())
      tryCatch({
        prepared <- prepare_outcome_data(data(), spec()$outcome,
                                         spec()$outcome_type, spec()$predictors)
        shiny::withProgress(message = "Fitting the model...", value = 0.35, {
          run_guided_model(prepared$data, spec())
        })
      },
               error = function(e) list(error = conditionMessage(e)))
    })
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
        tables = list("Coefficients" = res$coefficients,
                      "Model fit" = res$fit_stats),
        context = list(title = res$label, analysis = "Independent regression",
                       method = res$method, effect_name = res$effect_name,
                       app_interpretation = res$interpretation)
      )
    }), audit = audit)

    shiny::observeEvent(input$add, {
      shiny::req(is_ok())
      res <- result()
      add_report(list(kind = "table",
                      caption = paste("Model coefficients:", res$label),
                      data = res$coefficients))
      add_report(list(kind = "table",
                      caption = paste("Model fit:", res$label),
                      data = res$fit_stats))
      if (length(res$assumptions))
        add_report(list(kind = "table",
                        caption = paste("Model diagnostics:", res$label),
                        data = model_assumptions_df(res$assumptions)))
      if (!is.null(res$diagnostic_plot))
        add_report(list(kind = "plot",
                        caption = paste("Diagnostic plot:", res$label),
                        plot = res$diagnostic_plot))
      shiny::showNotification("Model results added to report.",
                              type = "message", duration = 3)
    })
  })
}
