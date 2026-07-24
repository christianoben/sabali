# mod_data.R ----------------------------------------------------------------
# Step 1-2 of the workflow: import a file (any format) and confirm/override the
# auto-detected measurement type of every column (mandatory before analysis).
#
# Returns reactives:
#   data()  - the type-coerced data frame
#   types() - named character vector: variable -> measurement type

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::wellPanel(
          shiny::h4("1. Load your data"),
          shiny::fileInput(ns("file"),
                           "Choose a file (CSV, RDA, RDS, Excel, SPSS, Stata, ...)",
                           accept = NULL),
          shiny::helpText("Any common data format is accepted. CSV and RDA are typical."),
          shiny::verbatimTextOutput(ns("summary"))
        ),
        # AI analysis planner. Deliberately placed AFTER "Load your data": it is
        # only usable once the data is loaded and the setup steps (2-3, right)
        # have been completed, so its controls are gated below.
        shiny::wellPanel(
          shiny::h4(shiny::icon("magic"), " Ask the AI planner"),
          shiny::helpText(
            "Optional. Paste your research question or objectives and the app ",
            "will read your variables (names, types, factor levels) and suggest ",
            "an outcome, allowable statistical tests and a regression method. ",
            "It is a suggestion you confirm - nothing is run automatically, and ",
            "only variable metadata (never row-level data) leaves this computer."),
          shiny::textAreaInput(
            ns("research_question"), label = NULL, rows = 4, width = "100%",
            placeholder = "e.g. What patient factors are associated with viral suppression?"),
          shiny::uiOutput(ns("planner_controls")),
          shiny::uiOutput(ns("planner_result"))
        )
      ),
      shiny::column(
        7,
        shiny::wellPanel(
          shiny::h4("2. Confirm variable types"),
          shiny::helpText("We auto-detect each column's type. Correct any that are wrong ",
                          "before analysing - this is the single most common source of mistakes. ",
                          "For each binary or categorical variable you can also pick the ",
                          "base (reference) level: every regression compares the other ",
                          "categories against this one, so you are no longer left with whichever ",
                          "level R happens to sort first."),
          shiny::div(
            class = "type-editor-scroll",
            style = paste(
              "max-height: 430px;",
              "overflow-y: auto;",
              "overflow-x: hidden;",
              "padding: 8px 10px 8px 0;",
              "border: 1px solid #dfeceb;",
              "border-radius: 4px;"
            ),
            shiny::uiOutput(ns("type_editor"))
          ),
          shiny::uiOutput(ns("structure_editor")),
          shiny::uiOutput(ns("role_editor")),
          shiny::uiOutput(ns("survival_editor")),
          shiny::uiOutput(ns("outcome_editor"))
        )
      )
    ),
    shiny::h4("Data preview"),
    shiny::uiOutput(ns("structure_status")),
    DT::dataTableOutput(ns("preview"))
  )
}

mod_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shiny normally removes upload files itself, but an explicit per-session
    # cleanup is part of the data-governance contract.  Only paths supplied by
    # this module's fileInput are retained here; no directory roots are used.
    session_uploads <- character(0)
    session$onSessionEnded(function() session_cleanup(session_uploads))
    audit <- shiny::reactiveVal(audit_record("session_started"))
    record_audit <- function(event, df = NULL, fields = character(),
                              n_rows = NA_integer_, n_columns = NA_integer_) {
      rec <- audit_record(event,
                          fields = if (is.data.frame(df)) names(df) else fields,
                          n_rows = if (is.data.frame(df)) nrow(df) else n_rows,
                          n_columns = if (is.data.frame(df)) ncol(df) else n_columns)
      # The importer itself must not depend on the audit reactive; otherwise
      # appending an audit row would invalidate and re-run the import.
      shiny::isolate(audit(rbind(audit(), rec)))
    }

    # Handles for the per-column base-level observers, kept so they can be torn
    # down and rebuilt when a new file (with different columns) is loaded.
    base_watchers <- list()

    raw <- shiny::reactive({
      shiny::req(input$file)
      session_uploads <<- unique(c(session_uploads, input$file$datapath))
      tryCatch(
        {
          out <- read_any_format(input$file$datapath, input$file$name)
          record_audit("file_imported", out)
          out
        },
        error = function(e) {
          shiny::showNotification(paste("Could not read file:", conditionMessage(e)),
                                  type = "error", duration = 8)
          NULL
        }
      )
    })

    detected <- shiny::reactive({
      shiny::req(raw())
      detect_all_types(raw())
    })

    output$summary <- shiny::renderText({
      shiny::req(raw()); import_summary(raw())
    })

    output$outcome_editor <- shiny::renderUI({
      shiny::req(raw(), types())
      df <- data_coerced(); tp <- types()
      choices <- names(df)
      # Outcome roles are deliberately user-selected. Do not prefill a
      # plausible-looking default: an outcome is a substantive study decision,
      # not something the app should infer from column types.
      selected <- intersect(input$outcomes %||% character(0), choices)
      shiny::tagList(
        shiny::h5("3. Which variables are your outcomes (what you want to explain or compare)?"),
        shiny::selectizeInput(
          ns("outcomes"), label = NULL,
          choices = choices, selected = selected, multiple = TRUE,
          options = list(placeholder = "Select one or more outcome variables")
        ),
        shiny::helpText("Optional. Choose one or more outcome variables - the things your study is really about. You can pick a column whose imported type needs temporary conversion for a chosen analysis. Leave blank to keep all variables available.")
      )
    })

    # An EXPLICIT, plain-language question about how the data is laid out. The
    # answer - not a heuristic guess - decides the dataset "kind" and which
    # role pickers appear. Depends only on raw() (column names), so editing a
    # variable type does not reset the choice.
    output$structure_editor <- shiny::renderUI({
      shiny::req(raw())
      shiny::tagList(
        shiny::h5("1. How is your data laid out?"),
        shiny::radioButtons(
          ns("data_kind"), label = NULL,
          choices = c(
            "One row per subject - each person (or record) appears exactly once" = "cross_sectional",
            "Several rows per subject - the same people were measured on more than one visit or occasion" = "repeated",
            "A single run of readings over time - periodic totals, not individual people (a time series)" = "time_series"),
          selected = shiny::isolate(input$data_kind) %||% character(0)),
        shiny::helpText("Pick what matches how the data was collected. This is not guessed from the file, and it decides which analyses you will be offered.")
      )
    })

    # Role pickers for the chosen layout. Deliberately blank (no prefilled
    # guess): the user makes each choice explicitly.
    output$role_editor <- shiny::renderUI({
      shiny::req(raw())
      k <- input$data_kind
      if (is.null(k) || !nzchar(k)) return(NULL)
      cols <- names(raw())
      pick <- c("(choose a column)" = "", cols)
      isolate_sel <- function(v) shiny::isolate(input[[v]]) %||% ""
      if (identical(k, "repeated")) {
        shiny::tagList(
          shiny::selectInput(ns("id_var"),
            "Which column is the subject's ID? (the same code repeats on all of that person's rows)",
            choices = pick, selected = isolate_sel("id_var")),
          shiny::selectInput(ns("time_var"),
            "Which column shows the visit or time point of each measurement?",
            choices = pick, selected = isolate_sel("time_var")),
          shiny::helpText("These unlock repeated-measures and mixed-model analyses.")
        )
      } else if (identical(k, "time_series")) {
        shiny::selectInput(ns("time_var"),
          "Which column holds the date or time of each reading?",
          choices = pick, selected = isolate_sel("time_var"))
      } else {
        shiny::helpText("Each row is treated as an independent observation - no subject-ID or time column is needed.")
      }
    })

    # A SEPARATE, plain-language question for time-to-event (survival) studies.
    # Survival data is often one row per subject with either a follow-up
    # duration OR a start date and an end/event date - so it is asked
    # independently of the layout question above.
    output$survival_editor <- shiny::renderUI({
      shiny::req(raw())
      cols <- names(raw())
      pick <- c("(choose a column)" = "", cols)
      isolate_sel <- function(v) shiny::isolate(input[[v]]) %||% ""
      body <- if (isTRUE(input$is_survival)) {
        mode_ui <- shiny::radioButtons(ns("surv_mode"),
          "How is the length of time recorded?",
          choices = c(
            "As a single number - how long each person was followed (e.g. days, months or years)" = "duration",
            "As two dates - a start date and an end/event date (the app works out the length)" = "dates"),
          selected = shiny::isolate(input$surv_mode) %||% "duration")
        time_ui <- if (identical(input$surv_mode, "dates")) {
          shiny::tagList(
            shiny::selectInput(ns("surv_start"), "Which column is the start date?",
                               choices = pick, selected = isolate_sel("surv_start")),
            shiny::selectInput(ns("surv_end"), "Which column is the end / event date?",
                               choices = pick, selected = isolate_sel("surv_end")))
        } else {
          shiny::selectInput(ns("surv_time"),
            "Which column holds the length of follow-up time?",
            choices = pick, selected = isolate_sel("surv_time"))
        }
        shiny::tagList(
          mode_ui, time_ui,
          shiny::selectInput(ns("surv_event"),
            "Which column tells you whether the event happened?",
            choices = pick, selected = isolate_sel("surv_event")),
          shiny::uiOutput(ns("surv_event_level_ui")),
          shiny::helpText("'Event happened' means the outcome occurred (e.g. died, relapsed). Anyone who did not is treated as event-free at their last contact.")
        )
      } else NULL
      shiny::tagList(
        shiny::h5("2. Is this a time-to-event (survival) study?"),
        shiny::checkboxInput(ns("is_survival"),
          "Yes - I am measuring the time until an event happens (e.g. time to death, recovery, or treatment failure)",
          value = shiny::isolate(input$is_survival) %||% FALSE),
        body
      )
    })

    output$surv_event_level_ui <- shiny::renderUI({
      shiny::req(isTRUE(input$is_survival), input$surv_event, nzchar(input$surv_event))
      col <- data_coerced()[[input$surv_event]]
      lev <- levels(droplevels(as.factor(col[!is.na(col)])))
      shiny::req(length(lev) >= 1)
      shiny::selectInput(ns("surv_event_level"),
        "Which value in that column means the event happened?",
        choices = lev, selected = shiny::isolate(input$surv_event_level) %||% lev[[length(lev)]])
    })

    output$structure_status <- shiny::renderUI({
      shiny::req(raw())
      structure <- advanced_data_structure(data_coerced(), types(), panel())
      if (isTRUE(structure$available)) {
        shiny::div(class = "alert alert-success",
                   shiny::strong("Advanced models available. "), structure$reason,
                   shiny::tags$br(), sprintf("%d candidate outcome%s selected.",
                                             length(outcomes()), if (length(outcomes()) == 1) "" else "s"))
      } else {
        shiny::div(class = "alert alert-warning",
                   shiny::strong("Advanced models hidden. "), structure$reason,
                   shiny::tags$br(), sprintf("%d candidate outcome%s selected.",
                                             length(outcomes()), if (length(outcomes()) == 1) "" else "s"))
      }
    })

    # One row per column: variable name, a type dropdown pre-set to the
    # detected type, and a base-level dropdown for factor variables. The base
    # dropdown is populated for binary/categorical columns and shows "(n/a)" for
    # everything else; an observer (below) keeps it in step when the type is
    # changed by hand.
    output$type_editor <- shiny::renderUI({
      d <- detected(); df <- raw()
      header <- shiny::fluidRow(
        style = "font-weight:600; color:#4a6360; padding:0 0 4px 0;",
        shiny::column(4, "Variable"),
        shiny::column(4, "Type"),
        shiny::column(4, "Base (reference) level")
      )
      rows <- lapply(seq_len(nrow(d)), function(i) {
        nm <- d$variable[i]
        lv <- factor_levels_of(df[[nm]])
        base_choices <- if (is_relevellable_type(d$type[i]) && length(lv))
          lv else c("(n/a)" = "")
        shiny::fluidRow(
          shiny::column(4, shiny::tags$div(
            style = "padding-top:7px;",
            shiny::strong(nm),
            shiny::tags$small(sprintf(" (%d distinct, %.0f%% missing)",
                                      d$distinct[i], d$missing_pct[i]))
          )),
          shiny::column(4, shiny::selectInput(
            ns(paste0("type_", i)), label = NULL,
            choices = stats::setNames(MEASUREMENT_TYPES, TYPE_LABELS[MEASUREMENT_TYPES]),
            selected = d$type[i], width = "100%")),
          shiny::column(4, shiny::selectInput(
            ns(paste0("base_", i)), label = NULL,
            choices = base_choices,
            selected = base_choices[[1]], width = "100%"))
        )
      })
      shiny::tagList(header, rows)
    })

    # Keep each base-level dropdown in step with its type dropdown. When a
    # column is set to binary/categorical we offer its observed levels (keeping
    # the current pick if still valid); otherwise the base is not applicable.
    shiny::observeEvent(detected(), {
      for (o in base_watchers) o$destroy()
      base_watchers <<- list()
      d <- detected(); df <- raw()
      for (i in seq_len(nrow(d))) {
        local({
          ii <- i
          nm <- d$variable[ii]
          lv <- factor_levels_of(df[[nm]])
          base_watchers[[length(base_watchers) + 1L]] <<- shiny::observeEvent(
            input[[paste0("type_", ii)]],
            {
              t <- input[[paste0("type_", ii)]]
              if (isTRUE(is_relevellable_type(t)) && length(lv)) {
                cur <- input[[paste0("base_", ii)]]
                sel <- if (!is.null(cur) && cur %in% lv) cur else lv[[1]]
                shiny::updateSelectInput(session, paste0("base_", ii),
                                         choices = lv, selected = sel)
              } else {
                shiny::updateSelectInput(session, paste0("base_", ii),
                                         choices = c("(n/a)" = ""), selected = "")
              }
            },
            ignoreInit = TRUE
          )
        })
      }
    })

    # Collect the (possibly corrected) types, keyed by variable name.
    types <- shiny::reactive({
      d <- detected()
      vals <- vapply(seq_len(nrow(d)), function(i) {
        v <- input[[paste0("type_", i)]]
        if (is.null(v)) d$type[i] else v
      }, character(1))
      stats::setNames(vals, d$variable)
    })

    # The user-chosen base (reference) level for each variable, keyed by name.
    # "" / NULL means "leave R's default", which also covers non-factor types.
    base_levels <- shiny::reactive({
      d <- detected()
      vals <- vapply(seq_len(nrow(d)), function(i) {
        v <- input[[paste0("base_", i)]]
        if (is.null(v)) "" else v
      }, character(1))
      stats::setNames(vals, d$variable)
    })

    # Coerce each column to its confirmed type, then set the chosen reference
    # level on unordered factors. Doing it here means every downstream analysis
    # - regressions, tests, tables - uses the same base, instead of whichever
    # level R happens to sort first.
    data_coerced <- shiny::reactive({
      df <- raw(); tp <- types(); base <- base_levels()
      shiny::req(df, tp)
      for (nm in names(df)) {
        if (!is.null(tp[[nm]])) df[[nm]] <- coerce_to_type(df[[nm]], tp[[nm]])
        ref <- base[[nm]]
        if (is_relevellable_type(tp[[nm]] %||% "") && is.factor(df[[nm]]) &&
            !is.null(ref) && nzchar(ref) && ref %in% levels(df[[nm]])) {
          df[[nm]] <- stats::relevel(df[[nm]], ref = ref)
        }
      }
      df
    })

    outcomes <- shiny::reactive({
      shiny::req(data_coerced())
      selected <- input$outcomes %||% character(0)
      intersect(selected, names(data_coerced()))
    })

    # Panel roles come from the two explicit questions. Layout: a subject ID is
    # meaningful only for repeated measures; a lone time variable means a time
    # series. Survival is a separate declaration that can sit on any layout
    # (usually one row per subject) and carries either a duration column or a
    # start/end date pair, plus the event column and which value means "event".
    panel <- shiny::reactive({
      k <- input$data_kind
      if (is.null(k) || !nzchar(k)) k <- "cross_sectional"
      nz <- function(v) if (is.null(v) || !nzchar(v)) NULL else v

      is_surv <- isTRUE(input$is_survival)
      mode <- if (is_surv) (input$surv_mode %||% "duration") else NULL
      surv <- list(
        survival = is_surv,
        surv_mode = mode,
        surv_time = if (is_surv && identical(mode, "duration")) nz(input$surv_time) else NULL,
        surv_start = if (is_surv && identical(mode, "dates")) nz(input$surv_start) else NULL,
        surv_end = if (is_surv && identical(mode, "dates")) nz(input$surv_end) else NULL,
        surv_event = if (is_surv) nz(input$surv_event) else NULL,
        surv_event_level = if (is_surv) nz(input$surv_event_level) else NULL)

      base <- if (identical(k, "cross_sectional"))
        list(id = NULL, time = NULL, kind = k)
      else
        list(id = if (identical(k, "repeated")) nz(input$id_var) else NULL,
             time = nz(input$time_var), kind = k)
      # `event` kept for backward-compatibility with existing consumers.
      c(base, surv, list(event = surv$surv_event))
    })

    output$preview <- DT::renderDataTable({
      shiny::req(raw())
      DT::datatable(utils::head(raw(), 100), options = list(scrollX = TRUE,
                                                            pageLength = 5),
                    rownames = FALSE)
    })

    # --- AI analysis planner (gated) ---------------------------------------
    # The planner is only usable once the earlier click-steps in this tab are
    # done: a file is loaded and the "how is your data laid out?" question (and
    # its role/survival follow-ups) have been answered. planner_missing() lists
    # whatever is still outstanding; the action button appears only when empty.
    planner_missing <- shiny::reactive({
      miss <- character(0)
      if (is.null(raw())) miss <- c(miss, "load a data file")
      k <- input$data_kind
      if (is.null(k) || !nzchar(k)) {
        miss <- c(miss, "answer 'How is your data laid out?'")
      } else if (identical(k, "repeated")) {
        if (is.null(input$id_var) || !nzchar(input$id_var))
          miss <- c(miss, "choose the subject ID column")
        if (is.null(input$time_var) || !nzchar(input$time_var))
          miss <- c(miss, "choose the time / visit column")
      } else if (identical(k, "time_series")) {
        if (is.null(input$time_var) || !nzchar(input$time_var))
          miss <- c(miss, "choose the date / time column")
      }
      if (isTRUE(input$is_survival)) {
        mode <- input$surv_mode %||% "duration"
        if (identical(mode, "duration")) {
          if (is.null(input$surv_time) || !nzchar(input$surv_time))
            miss <- c(miss, "choose the follow-up time column")
        } else {
          if (is.null(input$surv_start) || !nzchar(input$surv_start))
            miss <- c(miss, "choose the start date column")
          if (is.null(input$surv_end) || !nzchar(input$surv_end))
            miss <- c(miss, "choose the end / event date column")
        }
        if (is.null(input$surv_event) || !nzchar(input$surv_event))
          miss <- c(miss, "choose the event column")
      }
      miss
    })

    output$planner_controls <- shiny::renderUI({
      # AI features are inactive whenever no AI endpoint is configured.
      if (!ai_available()) {
        return(shiny::div(
          style = paste("margin-bottom:0; padding:10px 12px; color:#8a8a8a;",
                        "background:#f4f4f4; border:1px solid #e2e2e2; border-radius:4px;"),
          shiny::tags$button(class = "btn btn-default", disabled = "disabled",
                             shiny::icon("magic"), " Suggest an analysis plan"),
          shiny::helpText(
            "AI features are turned off because no AI endpoint is configured. ",
            "Set an AI endpoint (AI_INTERPRET_ENDPOINT / OPENCODE_API_KEY) to enable ",
            "the planner.")))
      }
      miss <- planner_missing()
      if (length(miss)) {
        shiny::div(class = "alert alert-warning", style = "margin-bottom:0;",
                   shiny::strong("Finish the setup steps first: "),
                   shiny::tags$ul(lapply(miss, shiny::tags$li)))
      } else {
                            shiny::actionButton(ns("run_plan"),
                            "Suggest an analysis plan", class = "btn-primary",
                            icon = shiny::icon("magic"))
      }
    })

    plan_result <- shiny::eventReactive(input$run_plan, {
      q <- trimws(input$research_question %||% "")
      if (!nzchar(q)) {
        shiny::showNotification("Type a research question first.", type = "warning")
        return(NULL)
      }
      shiny::withProgress(message = "Asking the AI planner...", value = 0.5, {
        tryCatch(
          {
            record_audit("planner_metadata_sent", fields = names(data_coerced()),
                         n_rows = nrow(data_coerced()),
                         n_columns = ncol(data_coerced()))
            ai_plan(q, data_coerced(), types(), base_levels(), outcomes(),
                    panel(), raw = raw())
          },
          error = function(e) {
            shiny::showNotification(paste("Planner failed:", conditionMessage(e)),
                                    type = "error", duration = 8)
            NULL
          })
      })
    })

    output$planner_result <- shiny::renderUI({
      res <- plan_result()
      if (is.null(res)) return(NULL)
      if (!isTRUE(res$valid))
        return(shiny::div(class = "alert alert-danger",
          shiny::strong("The AI returned choices outside the supported catalogue: "),
          paste(res$errors, collapse = "; "), shiny::tags$br(),
          "Please rephrase the question and try again."))
      planner_plan_ui(res)
    })

    # The validated plan (or NULL) exposed to the analysis tabs so they can gate
    # their offered analyses to what the AI recommended.
    plan_out <- shiny::reactive({
      res <- tryCatch(plan_result(), error = function(e) NULL)
      if (is.null(res) || !isTRUE(res$valid)) return(NULL)
      res$plan
    })

    list(data = data_coerced, types = types, outcomes = outcomes,
         panel = panel, base_levels = base_levels, plan = plan_out,
         audit = audit, audit_record = record_audit)
  })
}
