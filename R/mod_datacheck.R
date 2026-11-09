# mod_datacheck.R -----------------------------------------------------------
# Tab 0: data-readiness dashboard, data contract, analysis context, review
# workflow, privacy-safe audit export, and subset builder.

mod_datacheck_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("tabs"),
    shiny::tabPanel(
      "Data quality report", shiny::br(),
      shiny::uiOutput(ns("readiness_banner")),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::wellPanel(
            shiny::h4("Automated data checks"),
            shiny::helpText("The report checks the uploaded data before analysis. Findings are prioritised and include an impact and recommended next action."),
            shiny::uiOutput(ns("qc_summary")),
            shiny::downloadButton(ns("dl_report"), "Download action report (.txt)", class = "btn-primary"),
            shiny::br(), shiny::br(),
            shiny::downloadButton(ns("dl_findings"), "Download findings (.csv)"),
            shiny::downloadButton(ns("dl_scorecard"), "Download scorecard (.csv)")
          )
        ),
        shiny::column(
          8,
          shiny::h4("Action findings"),
          shiny::helpText("Open findings are shown first. Accepted findings remain visible with their review decision. Passed rules are available in the scorecard."),
          DT::dataTableOutput(ns("qc_table"))
        )
      ),
      shiny::h4("22-point coverage scorecard"),
      DT::dataTableOutput(ns("scorecard_table"))
    ),
    shiny::tabPanel(
      "Data contract", shiny::br(),
      shiny::helpText(
        "The starting contract is populated from the uploaded data: every field is marked required, numeric/date bounds use the observed non-missing range, and categorical fields list their observed levels. Changing the expected type refreshes the minimum, maximum, and allowed-level defaults for that variable. Edit or clear any value when the study protocol allows something different.",
        shiny::tags$b(" These rules are enforced:"),
        " a value outside a variable's allowed levels or its numeric min/max is set to missing (NA) in the data every analysis tab uses (descriptives, charts, tests, models, report). Leaving a field at its observed default changes nothing. The audit trail and the data-quality findings on the first tab always describe the original file."),
      shiny::uiOutput(ns("contract_editor")),
      shiny::uiOutput(ns("contract_subgroup_editor"))
    ),
    shiny::tabPanel(
      "Analysis readiness", shiny::br(),
      shiny::helpText("Select an outcome and predictors when you want model-specific checks such as class imbalance, correlation, separation, and modeling suitability. No analysis is run from this panel."),
      shiny::uiOutput(ns("readiness_editor")),
      shiny::uiOutput(ns("analysis_summary")),
      shiny::tags$hr(),
      shiny::h4("Assumption screen"),
      shiny::helpText(
        "The assumptions your chosen analysis rests on, checked on the data as it stands.",
        shiny::tags$b(" Nothing is fitted and nothing is changed."),
        " Where a check fails, the fix is offered directly - a transformation you can apply here, or an alternative method to choose on the analysis tab."),
      shiny::uiOutput(ns("assump_suggested")),
      shiny::uiOutput(ns("assump_results")),
      shiny::uiOutput(ns("assump_remedies")),
      shiny::uiOutput(ns("assump_postfit")),
      DT::dataTableOutput(ns("assump_table")),
      shiny::br(),
      shiny::downloadButton(ns("dl_assumptions"), "Download the assumption screen (.csv)")
    ),
    shiny::tabPanel(
      "Review & audit", shiny::br(),
      shiny::helpText("A data owner or analyst can record a decision for each finding. An accepted finding requires a written reason."),
      shiny::uiOutput(ns("review_editor")),
      DT::dataTableOutput(ns("review_table")),
      shiny::downloadButton(ns("dl_reviews"), "Download review decisions (.csv)"),
      shiny::downloadButton(ns("dl_audit"), "Download metadata audit (.csv)")
    ),
    shiny::tabPanel(
      "Transform & derive", shiny::br(),
      shiny::helpText(
        "Derive new variables from the ones you uploaded - log a skewed measure, group a number into quartiles, work out change from baseline.",
        shiny::tags$b(" Nothing is overwritten:"),
        " each transformation ADDS a column and leaves the original exactly as collected, so Table 1 and the data contract still describe the data as it arrived. Every derived column is available on the analysis tabs and every step is recorded for the methods section."),
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::uiOutput(ns("tf_suggested")),
          shiny::wellPanel(
            shiny::h4("Derive a variable"),
            shiny::uiOutput(ns("tf_var_picker")),
            shiny::uiOutput(ns("tf_method_picker")),
            shiny::uiOutput(ns("tf_params")),
            shiny::uiOutput(ns("tf_guard")),
            shiny::actionButton(ns("tf_apply"), "Add this column",
                                class = "btn-primary", icon = shiny::icon("plus"))
          )
        ),
        shiny::column(
          7,
          shiny::h4("Preview"),
          shiny::helpText("How the new column would look before and after, on the current data."),
          shiny::uiOutput(ns("tf_preview")),
          shiny::tags$hr(),
          shiny::h4("Derived columns"),
          shiny::uiOutput(ns("tf_applied")),
          DT::dataTableOutput(ns("tf_table")),
          shiny::br(),
          shiny::downloadButton(ns("dl_transforms"), "Download the transformation log (.csv)")
        )
      )
    ),
    shiny::tabPanel(
      "Build a subset", shiny::br(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::wellPanel(
            shiny::h4("Select columns to keep"),
            shiny::div(
              class = "subset-column-picker-scroll",
              style = paste("max-height: 430px;", "overflow-y: auto;",
                            "overflow-x: hidden;", "padding: 8px 10px 8px 0;",
                            "border: 1px solid #dfeceb;", "border-radius: 4px;"),
              shiny::uiOutput(ns("col_picker"))
            ),
            shiny::actionLink(ns("select_all"), "Select all"), " | ",
            shiny::actionLink(ns("select_none"), "Clear"),
            shiny::tags$hr(), shiny::textOutput(ns("subset_dim")), shiny::br(),
            shiny::downloadButton(ns("dl_subset"), "Download subset (.csv)", class = "btn-primary")
          )
        ),
        shiny::column(
          8,
          shiny::h4("Subset preview"),
          shiny::helpText("First 100 rows of the columns you selected."),
          DT::dataTableOutput(ns("subset_preview"))
        )
      )
    )
  )
}

mod_datacheck_server <- function(id, data, types,
                                 panel = shiny::reactive(NULL),
                                 data_name = shiny::reactive("active dataset"),
                                 raw_data = shiny::reactive(NULL),
                                 outcomes = shiny::reactive(character(0)),
                                 plan = shiny::reactive(NULL),
                                 audit = NULL, audit_record = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    empty_reviews <- data.frame(
      finding_id = character(0), status = character(0), reviewer = character(0),
      review_note = character(0), reviewed_at = character(0),
      stringsAsFactors = FALSE)
    review_state <- shiny::reactiveVal(empty_reviews)

    current_df <- shiny::reactive({
      d <- tryCatch(data(), error = function(e) NULL)
      if (!is.data.frame(d)) return(NULL)
      d
    })

    current_types <- shiny::reactive({
      tp <- tryCatch(types(), error = function(e) character(0))
      if (is.null(tp)) character(0) else tp
    })

    type_choices <- function() {
      if (exists("MEASUREMENT_TYPES", inherits = TRUE)) {
        vals <- get("MEASUREMENT_TYPES", inherits = TRUE)
        labs <- if (exists("TYPE_LABELS", inherits = TRUE)) get("TYPE_LABELS", inherits = TRUE) else vals
        return(stats::setNames(vals, labs[vals]))
      }
      stats::setNames(c("continuous", "count", "binary", "nominal", "ordinal",
                        "datetime", "identifier", "text"),
                     c("Continuous", "Count", "Binary", "Nominal", "Ordinal",
                       "Date/time", "Identifier", "Text"))
    }

    contract_type_state <- shiny::reactiveVal(character(0))

    # Type changes are handled independently of the UI renderer. This keeps
    # the user's selectInput interaction from rebuilding the whole editor and
    # lets us replace only the three dependent contract fields.
    shiny::observe({
      df <- current_df()
      if (is.null(df)) {
        contract_type_state(character(0))
        return()
      }

      nm <- names(df)
      tp <- current_types()
      selected <- stats::setNames(vapply(seq_along(nm), function(i) {
        value <- input[[paste0("contract_type_", i)]]
        if (is.null(value) || length(value) == 0L) {
          value <- tp[[nm[[i]]]] %||% "text"
        }
        as.character(value)[[1]]
      }, character(1)), nm)

      previous <- contract_type_state()
      same_columns <- identical(names(previous), nm)
      changed <- if (same_columns) {
        nm[!vapply(nm, function(col) identical(previous[[col]], selected[[col]]), logical(1))]
      } else {
        character(0)
      }
      contract_type_state(selected)

      if (!length(changed)) return()
      for (col in changed) {
        i <- match(col, nm)
        fields <- dq_contract_column_defaults(df, col, selected[[col]])
        # This is already inside a module server. Passing the local input ID
        # lets Shiny's session proxy apply the module namespace exactly once.
        shiny::updateTextInput(session, paste0("contract_min_", i),
                               value = fields$min)
        shiny::updateTextInput(session, paste0("contract_max_", i),
                               value = fields$max)
        shiny::updateTextInput(session, paste0("contract_levels_", i),
                               value = fields$levels)
      }
    })

    output$contract_editor <- shiny::renderUI({
      df <- current_df()
      if (is.null(df)) return(shiny::div(class = "alert alert-info", "Load a dataset on the Data tab to define expectations."))
      tp <- current_types(); nm <- names(df); choices <- type_choices()
      defaults <- dq_contract_defaults(df, tp)
      saved_or_default <- function(id, default) {
        value <- shiny::isolate(input[[id]])
        if (is.null(value) || length(value) == 0) default else value
      }
      default_range <- function(col, bound) {
        value <- defaults$ranges[[col]] %||% list()
        value[[bound]] %||% ""
      }
      default_levels <- function(col) {
        paste(defaults$allowed_levels[[col]] %||% character(0), collapse = ", ")
      }
      header <- shiny::fluidRow(
        style = "font-weight:600; color:#4a6360; padding:0 0 4px 0;",
        shiny::column(2, "Variable"), shiny::column(2, "Expected type"),
        shiny::column(1, "Required"), shiny::column(1, "Unique"),
        shiny::column(2, "Expected minimum"), shiny::column(2, "Expected maximum"),
        shiny::column(2, "Allowed levels (comma-separated)"))
      rows <- lapply(seq_along(nm), function(i) {
        col <- nm[[i]]; key <- as.character(i)
        selected_type <- shiny::isolate(input[[paste0("contract_type_", key)]]) %||%
          (tp[[col]] %||% "text")
        shiny::fluidRow(
          shiny::column(2, shiny::tags$div(style = "padding-top:7px;", shiny::strong(col))),
          shiny::column(2, shiny::selectInput(ns(paste0("contract_type_", key)), NULL,
                                                choices = choices, selected = selected_type)),
          shiny::column(1, shiny::checkboxInput(ns(paste0("contract_required_", key)), NULL,
                                                 value = isTRUE(saved_or_default(
                                                   paste0("contract_required_", key),
                                                   col %in% defaults$required)))),
          shiny::column(1, shiny::checkboxInput(ns(paste0("contract_unique_", key)), NULL,
                                                 value = isTRUE(shiny::isolate(input[[paste0("contract_unique_", key)]])))),
          shiny::column(2, shiny::textInput(ns(paste0("contract_min_", key)), NULL,
                                             value = saved_or_default(
                                               paste0("contract_min_", key),
                                               default_range(col, "min")),
                                             placeholder = "number or YYYY-MM-DD")),
          shiny::column(2, shiny::textInput(ns(paste0("contract_max_", key)), NULL,
                                             value = saved_or_default(
                                               paste0("contract_max_", key),
                                               default_range(col, "max")),
                                             placeholder = "number or YYYY-MM-DD")),
          shiny::column(2, shiny::textInput(ns(paste0("contract_levels_", key)), NULL,
                                             value = saved_or_default(
                                               paste0("contract_levels_", key),
                                               default_levels(col)),
                                             placeholder = "optional"))
        )
      })
      shiny::tagList(header, rows)
    })

    output$contract_subgroup_editor <- shiny::renderUI({
      df <- current_df(); if (is.null(df)) return(NULL)
      nm <- names(df)
      shiny::wellPanel(
        shiny::selectizeInput(ns("contract_subgroups"),
          "Check missingness by these subgroups", choices = nm,
          selected = shiny::isolate(input$contract_subgroups) %||% character(0),
          multiple = TRUE, options = list(placeholder = "Optional: site, sex, treatment, time...")),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("contract_date_start"),
            "Optional date interval: start", choices = c("(none)" = "", nm),
            selected = shiny::isolate(input$contract_date_start) %||% "")),
          shiny::column(6, shiny::selectInput(ns("contract_date_end"),
            "Optional date interval: end", choices = c("(none)" = "", nm),
            selected = shiny::isolate(input$contract_date_end) %||% ""))
        ),
        shiny::helpText("Only groups with at least the minimum configured size are compared; group labels are not printed in the default report.")
      )
    })

    contract <- shiny::reactive({
      df <- current_df(); if (is.null(df)) return(dq_contract())
      nm <- names(df); tp <- current_types()
      defaults <- dq_contract_defaults(df, tp)
      get_input <- function(prefix, i, default = NULL) {
        v <- input[[paste0(prefix, i)]]
        if (is.null(v) || length(v) == 0) default else v
      }
      expected <- vapply(seq_along(nm), function(i) {
        get_input("contract_type_", i, tp[[nm[[i]]]] %||% "text")
      }, character(1)); names(expected) <- nm
      required <- nm[vapply(seq_along(nm), function(i)
        isTRUE(get_input("contract_required_", i, nm[[i]] %in% defaults$required)), logical(1))]
      identifiers <- nm[vapply(seq_along(nm), function(i) isTRUE(get_input("contract_unique_", i, FALSE)), logical(1))]
      ranges <- list(); levels <- list()
      for (i in seq_along(nm)) {
        range_default <- defaults$ranges[[nm[[i]]]] %||% list()
        lo <- trimws(as.character(get_input("contract_min_", i, range_default$min %||% "")))
        hi <- trimws(as.character(get_input("contract_max_", i, range_default$max %||% "")))
        if (nzchar(lo) || nzchar(hi)) ranges[[nm[[i]]]] <- list(min = lo, max = hi)
        levels_default <- paste(defaults$allowed_levels[[nm[[i]]]] %||% character(0), collapse = ", ")
        lv <- trimws(as.character(get_input("contract_levels_", i, levels_default)))
        if (nzchar(lv)) levels[[nm[[i]]]] <- trimws(strsplit(lv, ",", fixed = TRUE)[[1]])
      }
      p <- tryCatch(panel(), error = function(e) NULL)
      id <- if (is.list(p)) p$id %||% NULL else NULL
      identifiers <- unique(c(identifiers, if (is.character(id)) id else character(0)))
      start <- input$contract_date_start %||% ""
      end <- input$contract_date_end %||% ""
      date_order <- if (nzchar(start) && nzchar(end) && start != end) list(c(start, end)) else list()
      dq_contract(expected_types = expected, required = required, ranges = ranges,
                  allowed_levels = levels, identifiers = identifiers,
                  subgroup_vars = input$contract_subgroups %||% character(0),
                  date_order = date_order)
    })

    output$readiness_editor <- shiny::renderUI({
      df <- current_df(); if (is.null(df)) return(shiny::div(class = "alert alert-info", "Load a dataset before selecting an analysis context."))
      nm <- names(df); candidate <- tryCatch(outcomes(), error = function(e) character(0))
      candidate <- intersect(as.character(candidate %||% character(0)), nm)
      outcome_choices <- c("(choose an outcome)" = "", stats::setNames(nm, nm))
      selected_outcome <- shiny::isolate(input$readiness_outcome) %||% ""
      selected_predictors <- intersect(shiny::isolate(input$readiness_predictors) %||% character(0), nm)
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("readiness_outcome"), "Outcome", choices = outcome_choices,
                                               selected = selected_outcome)),
          shiny::column(4, shiny::selectizeInput(ns("readiness_predictors"), "Predictors",
                                                  choices = setdiff(nm, selected_outcome), selected = selected_predictors,
                                                  multiple = TRUE)),
          shiny::column(4, shiny::selectInput(ns("readiness_family"), "Analysis context",
                                               choices = c("General predictor review" = "general",
                                                            "Linear/general regression" = "regression",
                                                            "Logistic regression" = "logistic"),
                                               selected = shiny::isolate(input$readiness_family) %||% "general"))
        ),
        shiny::helpText(if (length(candidate))
          sprintf("The Data tab currently lists %d candidate outcome(s), but this panel requires an explicit selection before model-specific checks run.", length(candidate))
          else "Choose an outcome explicitly. The application does not infer a study outcome from the column names.")
      )
    })

    analysis_context <- shiny::reactive({
      df <- current_df(); if (is.null(df)) return(list())
      outcome <- input$readiness_outcome %||% ""
      if (!nzchar(outcome) || !outcome %in% names(df)) outcome <- NULL
      predictors <- intersect(input$readiness_predictors %||% character(0), names(df))
      if (!is.null(outcome)) predictors <- setdiff(predictors, outcome)
      list(outcome = outcome, predictors = predictors,
           family = input$readiness_family %||% "general")
    })

    # --- Assumption screen ---------------------------------------------------
    # Read-only, so unlike the transforms nothing flows back into mod_data. The
    # screen runs on the DERIVED data (tf_df()), which is what closes the loop:
    # a failed check offers a transformation, applying it adds a column, and
    # re-screening on the new column shows whether it actually helped.
    assump_results <- shiny::reactive({
      ctx <- analysis_context()
      df <- tf_df()
      if (is.null(df) || is.null(ctx$outcome)) return(list())
      # Each check inside readiness_assumptions() guards itself, so a failure
      # there returns an "untested" row rather than nothing. This outer guard is
      # the last resort only, and it reports what went wrong instead of
      # returning an empty list that would read as "nothing to check".
      # Resolve the inputs FIRST. Evaluated as arguments inside the tryCatch,
      # a failure in either would have been caught and turned into an empty
      # screen indistinguishable from "nothing to check".
      ty <- tryCatch(tf_types(), error = function(e) current_types())
      pn <- tryCatch(panel(), error = function(e) NULL)
      tryCatch(
        readiness_assumptions(df, ty, ctx$outcome, ctx$predictors, pn),
        error = function(e) list(assumption_result(
          "normality", "untested",
          sprintf("The assumption screen could not run: %s", conditionMessage(e)),
          scope = ctx$outcome)))
    })

    # Types including anything derived, so a screen run on viral_load_log10
    # knows it is continuous.
    tf_types <- shiny::reactive({
      ty <- current_types()
      r <- tf_result()
      if (is.null(r) || !length(r$records)) return(ty)
      for (rec in r$records) for (nm in rec$output) ty[[nm]] <- rec$produces
      ty
    })

    output$assump_suggested <- shiny::renderUI({
      p <- tryCatch(plan(), error = function(e) NULL)
      if (is.null(p)) return(NULL)
      ids <- tryCatch(plan_assumption_ids(p), error = function(e) NULL)
      if (is.null(ids) || !length(c(ids$prefit, ids$postfit))) return(NULL)
      nm <- function(v) vapply(v, function(i) {
        a <- assumption_check(i); if (is.null(a)) i else a$name }, character(1))
      shiny::div(
        class = "well", style = "background:#f2fbfa;border-color:#cfe9e7;padding:8px 12px;",
        shiny::tags$small(
          shiny::icon("magic"), shiny::strong(" The analysis plan asks for: "),
          paste(nm(c(ids$prefit, ids$postfit)), collapse = "; "), ". ",
          if (length(ids$prefit))
            sprintf("%d can be screened here; ", length(ids$prefit)),
          if (length(ids$postfit))
            sprintf("%d need a fitted model and appear on the analysis tab.",
                    length(ids$postfit)),
          if (length(ids$unmatched))
            sprintf(" %d further note(s) are advice only.", length(ids$unmatched))))
    })

    output$assump_results <- shiny::renderUI({
      ctx <- analysis_context()
      if (is.null(ctx$outcome))
        return(shiny::div(class = "alert alert-info",
                          "Choose an outcome above to screen the assumptions."))
      res <- assump_results()
      if (!length(res))
        return(shiny::div(class = "alert alert-warning",
          shiny::strong("No checks ran. "),
          sprintf("An outcome ('%s') and %d predictor(s) are selected, so this is unexpected - it usually means the outcome's confirmed type is not one the screen can work with (identifier, text or date). Check its type on the Data tab.",
                  ctx$outcome, length(ctx$predictors))))
      assumption_ui(res)
    })

    # Remedies that this app can actually carry out become buttons. This is the
    # loop closing: check fails -> the catalogue names the transformation ->
    # one click adds the derived column -> the screen re-runs on it.
    assump_fixes <- shiny::reactive({
      res <- assump_results()
      ctx <- analysis_context()
      df <- tf_df()
      if (!length(res) || is.null(df)) return(list())
      out <- list(); seen <- character(0)
      for (r in res) {
        if (!(r$severity %in% c("review", "violated"))) next
        # Apply the fix to the variable the check was about.
        target <- if (nzchar(r$scope %||% "")) strsplit(r$scope, " ")[[1]][1] else ctx$outcome
        for (rem in (r$remedy %||% list())) {
          tid <- rem$transform %||% ""
          if (!nzchar(tid) || !(target %in% names(df))) next
          key <- paste0(tid, "|", target)
          if (key %in% seen) next
          sp <- transform_spec(tid, variable = target)
          g <- transform_applicable(tid, df[[target]],
                                    tf_types()[[target]] %||% NULL, sp, df,
                                    tryCatch(panel(), error = function(e) NULL))
          if (!isTRUE(g$ok)) next
          seen <- c(seen, key)
          out[[length(out) + 1L]] <- list(
            key = key, spec = sp, target = target,
            what = rem$what %||% "", check = r$label %||% r$check)
        }
      }
      utils::head(out, 8)
    })

    output$assump_remedies <- shiny::renderUI({
      fx <- assump_fixes()
      if (!length(fx)) return(NULL)
      done <- vapply(tf_specs(), function(s) paste0(s$method, "|", s$variable),
                     character(1))
      shiny::div(
        class = "well", style = "background:#fdf7ef;border-color:#f0e2cc;",
        shiny::h5(shiny::icon("wrench"), " Fixes you can apply now"),
        shiny::helpText("These add a derived column on the Transform & derive tab; the screen above then re-runs on it, so you can see whether it worked."),
        shiny::tags$ul(style = "padding-left:18px;", lapply(seq_along(fx), function(i) {
          f <- fx[[i]]
          shiny::tags$li(
            style = "margin-bottom:6px;",
            f$what, " on ", shiny::tags$code(f$target),
            shiny::tags$small(style = "color:#7b8f8d;", sprintf(" (for: %s)", f$check)),
            " ",
            if (f$key %in% done)
              shiny::tags$small(style = "color:#018786;font-weight:600;", "Applied")
            else shiny::actionButton(ns(paste0("assump_fix_", i)), "Apply",
                                     class = "btn-xs btn-warning"))
        })))
    })

    lapply(seq_len(8), function(i) {
      shiny::observeEvent(input[[paste0("assump_fix_", i)]], {
        fx <- assump_fixes()
        if (length(fx) < i) return()
        tf_add(fx[[i]]$spec)
      }, ignoreInit = TRUE)
    })

    # Say what this screen cannot answer, so it never reads as exhaustive.
    output$assump_postfit <- shiny::renderUI({
      if (is.null(analysis_context()$outcome)) return(NULL)
      nm <- vapply(ASSUMPTION_POSTFIT, function(i) {
        a <- assumption_check(i); if (is.null(a)) i else a$name }, character(1))
      shiny::tags$details(
        style = "margin:8px 0;",
        shiny::tags$summary(style = "cursor:pointer;color:#7b8f8d;",
          sprintf("%d further assumption(s) can only be checked once a model is fitted",
                  length(nm))),
        shiny::tags$ul(style = "padding-left:18px;",
          lapply(unname(nm), function(x) shiny::tags$li(
            style = "color:#7b8f8d;font-size:90%;", x))),
        shiny::tags$small(style = "color:#7b8f8d;",
          "These appear under Diagnostics on the Regression and Advanced tabs after you run the analysis."))
    })

    output$assump_table <- DT::renderDataTable({
      DT::datatable(assumption_df(assump_results()), rownames = FALSE,
                    options = list(dom = "t", paging = FALSE))
    })

    output$dl_assumptions <- shiny::downloadHandler(
      filename = function() sprintf("assumption-screen-%s.csv", Sys.Date()),
      content = function(file)
        utils::write.csv(assumption_df(assump_results()), file, row.names = FALSE))

    review_state_for_result <- shiny::reactive({
      current <- review_state()
      if (!is.data.frame(current)) empty_reviews else current
    })

    qc <- shiny::reactive({
      df <- current_df(); shiny::req(df)
      raw <- tryCatch(raw_data(), error = function(e) NULL)
      result <- dq_run_checks(df, current_types(), panel(), contract = contract(),
                              raw_df = raw, analysis = analysis_context())
      dq_apply_reviews(result, review_state_for_result())
    })

    # The audit trail records only aggregate dimensions and field names.  It is
    # intentionally independent of qc() so appending a record cannot trigger a
    # recursive re-run of the data-quality engine.
    shiny::observeEvent(qc(), {
      df <- current_df()
      if (is.data.frame(df) && is.function(audit_record))
        audit_record("data_quality_checked", fields = names(df),
                     n_rows = nrow(df), n_columns = ncol(df))
    }, ignoreInit = FALSE)

    output$readiness_banner <- shiny::renderUI({
      if (is.null(current_df())) return(shiny::div(class = "alert alert-info", shiny::strong("No dataset loaded. "), "Load data on the Data tab to start the 22-point review."))
      s <- qc()$summary
      cls <- switch(s$readiness, blocked = "alert alert-danger",
                    `review required` = "alert alert-warning",
                    `ready with notes` = "alert alert-warning", "alert alert-success")
      shiny::div(class = cls,
        shiny::strong(sprintf("Data readiness: %s. ", toupper(s$readiness))),
        sprintf("%d open error(s), %d warning(s), %d note(s); %d scorecard item(s) not evaluated because they require an explicit contract or analysis context.",
                s$open_errors, s$open_warnings, s$open_notes, s$scorecard_not_evaluated))
    })

    output$qc_summary <- shiny::renderUI({
      if (is.null(current_df())) return(shiny::div(class = "alert alert-info", "Load a dataset on the Data tab to run the checks."))
      s <- qc()$summary
      shiny::tagList(
        shiny::HTML(sprintf("<b>%d rows x %d columns.</b><br>%d error(s), %d warning(s), %d note(s), %d passed rule(s).<br>%d of 22 scorecard items have issues.",
                           s$n_rows, s$n_cols, s$errors, s$warnings, s$notes, s$passed, s$scorecard_issues)),
        shiny::tags$hr(), shiny::helpText("Configure expectations under Data contract and select an outcome under Analysis readiness to complete the contextual checks.")
      )
    })

    output$qc_table <- DT::renderDataTable({
      if (is.null(current_df())) return(DT::datatable(data.frame(Note = "Load a dataset to view findings."), rownames = FALSE, options = list(dom = "t")))
      f <- qc()$findings; f <- f[f$severity != "pass", , drop = FALSE]
      if (!nrow(f)) return(DT::datatable(data.frame(Note = "No active findings. Review the scorecard for contextual checks."), rownames = FALSE, options = list(dom = "t")))
      show <- f[, c("finding_id", "priority", "domain", "scope", "issue", "impact",
                    "recommendation", "affected_n", "affected_pct", "status"), drop = FALSE]
      names(show) <- c("ID", "Priority", "Domain", "Scope", "Issue", "Impact",
                       "Recommended action", "Affected", "Affected %", "Status")
      dt <- DT::datatable(show, rownames = FALSE, filter = "top",
                          options = list(pageLength = 10, scrollX = TRUE, order = list()))
      DT::formatStyle(dt, "Priority", backgroundColor = DT::styleEqual(
        c("blocker", "high", "medium", "low"), c("#f8d7da", "#fff3cd", "#e2e3e5", "#d9edf7")))
    })

    output$scorecard_table <- DT::renderDataTable({
      if (is.null(current_df())) return(DT::datatable(data.frame(Note = "Load a dataset to view the scorecard."), rownames = FALSE, options = list(dom = "t")))
      sc <- qc()$scorecard[, c("point", "domain", "label", "context", "status", "findings")]
      names(sc) <- c("Point", "Domain", "Check", "Context", "Status", "Findings")
      DT::datatable(sc, rownames = FALSE, filter = "top", options = list(pageLength = 22, order = list()))
    })

    output$analysis_summary <- shiny::renderUI({
      if (is.null(current_df())) return(NULL)
      a <- analysis_context(); q <- qc();
      if (is.null(a$outcome)) return(shiny::div(class = "alert alert-info", "No outcome selected. General data checks are running; model-specific checks remain not evaluated."))
      shiny::div(class = "alert alert-secondary",
        shiny::strong("Context: "), sprintf("%s outcome with %d predictor(s), family '%s'. ", a$outcome, length(a$predictors), a$family),
        sprintf("Current readiness is %s.", q$summary$readiness))
    })

    # Keep the review selector synchronised with the current finding set.
    shiny::observeEvent(qc(), {
      f <- qc()$findings; f <- f[f$severity != "pass", , drop = FALSE]
      choices <- if (nrow(f)) stats::setNames(f$finding_id, paste(f$priority, f$scope, f$issue)) else character(0)
      shiny::updateSelectInput(session, "review_finding", choices = choices,
                               selected = if (length(choices)) choices[[1]] else character(0))
    }, ignoreInit = FALSE)

    output$review_editor <- shiny::renderUI({
      if (is.null(current_df())) return(shiny::div(class = "alert alert-info", "Load a dataset before recording review decisions."))
      f <- qc()$findings; f <- f[f$severity != "pass", , drop = FALSE]
      choices <- if (nrow(f)) stats::setNames(f$finding_id, paste(f$priority, f$scope, f$issue)) else character(0)
      shiny::wellPanel(
        shiny::selectInput(ns("review_finding"), "Finding", choices = choices),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("reviewer"), "Reviewer", value = "")),
          shiny::column(4, shiny::selectInput(ns("review_status"), "Decision",
            choices = c("Open" = "open", "Accepted with justification" = "accepted",
                        "Not applicable" = "not_applicable", "Fixed - rerun required" = "fixed_pending_recheck"),
            selected = "open")),
          shiny::column(4, shiny::textAreaInput(ns("review_note"), "Reason / action taken", rows = 2))
        ),
        shiny::actionButton(ns("save_review"), "Save review decision", class = "btn-primary")
      )
    })

    observeEvent(input$save_review, {
      id <- input$review_finding %||% ""; status <- input$review_status %||% "open"
      note <- trimws(input$review_note %||% "")
      if (!nzchar(id)) return(shiny::showNotification("Choose a finding first.", type = "warning"))
      if (identical(status, "accepted") && !nzchar(note))
        return(shiny::showNotification("An accepted finding requires a written reason.", type = "warning"))
      row <- data.frame(finding_id = id, status = status,
                        reviewer = trimws(input$reviewer %||% ""), review_note = note,
                        reviewed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                        stringsAsFactors = FALSE)
      old <- review_state(); old <- old[old$finding_id != id, , drop = FALSE]
      review_state(rbind(old, row))
      if (is.function(audit_record)) audit_record("data_quality_review_saved", fields = c("finding_id", "status"))
      shiny::showNotification("Review decision saved for this session.", type = "message")
    })

    output$review_table <- DT::renderDataTable({
      r <- review_state()
      if (!nrow(r)) r <- data.frame(Note = "No review decisions recorded in this session.")
      DT::datatable(r, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    audit_data <- shiny::reactive({
      if (is.function(audit)) {
        x <- tryCatch(audit(), error = function(e) NULL)
        if (is.data.frame(x)) return(x)
      }
      data.frame(timestamp = character(0), event = character(0), fields = character(0),
                 n_rows = integer(0), n_columns = integer(0), stringsAsFactors = FALSE)
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() sprintf("data_readiness_report_%s.txt", Sys.Date()),
      content = function(file) writeLines(dq_report_text(qc(), dataset_name = data_name()), con = file))
    output$dl_findings <- shiny::downloadHandler(
      filename = function() sprintf("data_readiness_findings_%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(qc()$findings, file, row.names = FALSE, na = ""))
    output$dl_scorecard <- shiny::downloadHandler(
      filename = function() sprintf("data_readiness_scorecard_%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(qc()$scorecard, file, row.names = FALSE, na = ""))
    output$dl_reviews <- shiny::downloadHandler(
      filename = function() sprintf("data_quality_reviews_%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(review_state(), file, row.names = FALSE, na = ""))
    output$dl_audit <- shiny::downloadHandler(
      filename = function() sprintf("sabali_metadata_audit_%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(audit_data(), file, row.names = FALSE, na = ""))

    # --- subset builder ----------------------------------------------------
    output$col_picker <- shiny::renderUI({
      df <- current_df(); if (is.null(df)) return(shiny::helpText("Load data first."))
      shiny::checkboxGroupInput(ns("cols"), label = NULL, choices = names(df), selected = names(df))
    })
    observeEvent(input$select_all, {
      df <- current_df(); if (!is.null(df)) shiny::updateCheckboxGroupInput(session, "cols", choices = names(df), selected = names(df))
    })
    observeEvent(input$select_none, {
      df <- current_df(); if (!is.null(df)) shiny::updateCheckboxGroupInput(session, "cols", choices = names(df), selected = character(0))
    })
    subset_df <- shiny::reactive({
      df <- current_df(); shiny::req(df)
      cols <- intersect(input$cols %||% names(df), names(df)); df[, cols, drop = FALSE]
    })
    output$subset_dim <- shiny::renderText({
      d <- subset_df(); sprintf("Subset: %d rows x %d columns selected.", nrow(d), ncol(d))
    })
    output$subset_preview <- DT::renderDataTable({
      d <- subset_df()
      if (ncol(d) == 0) return(DT::datatable(data.frame(Note = "Select at least one column."), rownames = FALSE, options = list(dom = "t")))
      DT::datatable(utils::head(d, 100), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$dl_subset <- shiny::downloadHandler(
      filename = function() sprintf("data_subset_%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(subset_df(), file, row.names = FALSE, na = ""))

    # Enforcement rules derived from the contract, for the app to feed back into
    # the data pipeline. Built from the PRE-enforcement data (current_df is the
    # base dataset) so this never depends on the enforced result - no cycle.
    enforcement <- shiny::reactive({
      df <- current_df()
      if (is.null(df)) return(list())
      tryCatch(dq_contract_to_constraints(contract(), df, current_types()),
               error = function(e) list())
    })

    # --- Transform & derive -------------------------------------------------
    # The tab owns an ordered list of transform specs and nothing else. It never
    # writes the derived data itself: the specs are handed back to the app,
    # which passes them to mod_data so a single apply_transforms() call produces
    # the columns every analysis tab sees. That mirrors how the data contract
    # already works, and keeps this tab reading pre-derivation data so there is
    # no reactive cycle.
    tf_specs <- shiny::reactiveVal(list())

    # The tab's own view of the derived data, computed from the same specs. It
    # is what the preview and the applied-columns table read, and it lets a
    # transformation build on an earlier one without reading anything back.
    tf_result <- shiny::reactive({
      df <- current_df()
      if (is.null(df)) return(NULL)
      apply_transforms(df, tf_specs(), current_types(), tryCatch(panel(), error = function(e) NULL))
    })

    tf_df <- shiny::reactive({ r <- tf_result(); if (is.null(r)) current_df() else r$df })

    # --- the AI's suggestions, as buttons ------------------------------------
    tf_suggestions <- shiny::reactive({
      p <- tryCatch(plan(), error = function(e) NULL)
      if (is.null(p)) return(list())
      tryCatch(plan_transform_specs(p, tf_df(), current_types(),
                                    tryCatch(panel(), error = function(e) NULL)),
               error = function(e) list())
    })

    output$tf_suggested <- shiny::renderUI({
      sg <- tf_suggestions()
      if (!length(sg)) return(NULL)
      done <- vapply(tf_specs(), function(s)
        paste0(s$method, "|", s$variable), character(1))
      items <- lapply(seq_along(sg), function(i) {
        s <- sg[[i]]
        already <- nzchar(s$method) &&
          paste0(s$method, "|", s$variable) %in% done
        shiny::tags$li(
          style = "margin-bottom:8px;",
          shiny::strong(s$label), " on ", shiny::tags$code(s$variable),
          shiny::tags$br(),
          shiny::tags$small(style = "color:#4a6360;", s$reason),
          shiny::tags$br(),
          if (already)
            shiny::tags$small(style = "color:#018786;font-weight:600;", "Already added")
          else if (isTRUE(s$ok))
            shiny::actionButton(ns(paste0("tf_sugg_", i)), "Apply this",
                                class = "btn-xs btn-success")
          else shiny::tags$small(style = "color:#8a6d3b;", s$why_not))
      })
      shiny::div(
        class = "well", style = "background:#f2fbfa;border-color:#cfe9e7;",
        shiny::h4(shiny::icon("magic"), " Suggested by the analysis plan"),
        shiny::helpText("From the research question you asked on the Data tab. Each one is a suggestion - apply it only if it suits your protocol."),
        shiny::tags$ul(style = "padding-left:18px;", items))
    })

    # One observer per suggestion slot, created once. The cap is generous; a
    # plan never returns anywhere near this many transformations.
    lapply(seq_len(12), function(i) {
      shiny::observeEvent(input[[paste0("tf_sugg_", i)]], {
        sg <- tf_suggestions()
        if (length(sg) < i) return()
        s <- sg[[i]]
        if (!isTRUE(s$ok) || is.null(s$spec)) return()
        tf_add(s$spec)
      }, ignoreInit = TRUE)
    })

    # --- manual builder ------------------------------------------------------
    output$tf_var_picker <- shiny::renderUI({
      df <- tf_df(); shiny::req(df)
      shiny::selectInput(ns("tf_var"), "Variable", choices = names(df),
                         selected = input$tf_var %||% names(df)[1])
    })

    tf_options <- shiny::reactive({
      df <- tf_df(); v <- input$tf_var
      shiny::req(df, v, v %in% names(df))
      ty <- current_types()
      transform_options_for(df[[v]],
                            if (v %in% names(ty)) unname(ty[[v]]) else NULL,
                            df, tryCatch(panel(), error = function(e) NULL), v)
    })

    output$tf_method_picker <- shiny::renderUI({
      opts <- tf_options()
      usable <- Filter(function(o) isTRUE(o$ok), opts)
      if (!length(usable))
        return(shiny::div(class = "alert alert-warning",
                          "No transformation applies to this column."))
      # Grouped by family, so the menu reads as a set of intentions rather than
      # a flat list of 66 names.
      fams <- unique(vapply(usable, function(o) o$family, character(1)))
      choices <- lapply(fams, function(f) {
        ids <- vapply(Filter(function(o) identical(o$family, f), usable),
                      function(o) o$id, character(1))
        nms <- vapply(Filter(function(o) identical(o$family, f), usable),
                      function(o) o$name, character(1))
        stats::setNames(as.list(ids), nms)
      })
      names(choices) <- unname(TRANSFORM_FAMILY_LABELS[fams])
      shiny::tagList(
        shiny::selectInput(ns("tf_method"), "Transformation", choices = choices,
                           selected = input$tf_method),
        if (length(usable) < length(opts)) shiny::tags$details(
          shiny::tags$summary(style = "cursor:pointer;color:#7b8f8d;font-size:90%;",
            sprintf("%d transformation(s) do not apply here - why",
                    length(opts) - length(usable))),
          shiny::tags$ul(style = "padding-left:18px;",
            lapply(Filter(function(o) !isTRUE(o$ok), opts), function(o)
              shiny::tags$li(style = "color:#7b8f8d;font-size:90%;",
                             shiny::strong(o$name), " - ", o$reason)))))
    })

    # Parameter controls are generated from the catalogue, so a new method in
    # transform_catalogue() gets its controls here with no UI change.
    output$tf_params <- shiny::renderUI({
      m <- transform_method(input$tf_method %||% "")
      if (is.null(m) || !length(m$params)) return(NULL)
      df <- tf_df()
      ctrl <- lapply(names(m$params), function(p) {
        def <- m$params[[p]]
        iid <- ns(paste0("tfp_", p))
        lab <- gsub("_", " ", p)
        substring(lab, 1, 1) <- toupper(substring(lab, 1, 1))
        if (p %in% c("with")) {
          shiny::selectInput(iid, "Second column",
                             choices = setdiff(names(df), input$tf_var))
        } else if (p == "columns") {
          shiny::selectizeInput(iid, "Columns to combine", choices = names(df),
                                multiple = TRUE)
        } else if (p == "reference") {
          lv <- factor_levels_of(df[[input$tf_var]])
          if (length(lv)) shiny::selectInput(iid, "Reference category", choices = lv)
          else shiny::textInput(iid, lab, value = "")
        } else if (p == "order") {
          shiny::selectizeInput(iid, "Order (lowest first)",
                                choices = factor_levels_of(df[[input$tf_var]]),
                                multiple = TRUE)
        } else if (is.logical(def)) {
          shiny::checkboxInput(iid, lab, value = isTRUE(def))
        } else if (is.character(def) && length(def) <= 1) {
          shiny::textInput(iid, lab, value = if (length(def)) def else "")
        } else if (is.numeric(def) && length(def) == 1) {
          shiny::numericInput(iid, lab, value = if (is.na(def)) NA else def)
        } else {
          shiny::textInput(iid, paste0(lab, " (comma-separated)"), value = "")
        }
      })
      shiny::tagList(ctrl)
    })

    # Assemble the spec the user has configured.
    tf_current_spec <- shiny::reactive({
      m <- transform_method(input$tf_method %||% "")
      v <- input$tf_var
      if (is.null(m) || is.null(v) || !nzchar(v)) return(NULL)
      extra <- list()
      for (p in names(m$params)) {
        val <- input[[paste0("tfp_", p)]]
        if (is.null(val)) next
        def <- m$params[[p]]
        if (is.character(val) && length(val) == 1 && is.numeric(def) && length(def) != 1) {
          # A comma-separated list typed into a text box (breaks, labels, scores).
          parts <- trimws(strsplit(val, ",")[[1]])
          parts <- parts[nzchar(parts)]
          val <- suppressWarnings(as.numeric(parts))
          if (any(is.na(val))) next
        }
        if (length(val) == 1 && is.character(val) && !nzchar(val)) next
        extra[[p]] <- val
      }
      do.call(transform_spec, c(list(method = m$id, variable = v), extra))
    })

    output$tf_guard <- shiny::renderUI({
      sp <- tf_current_spec(); df <- tf_df()
      if (is.null(sp) || is.null(df)) return(NULL)
      r <- apply_transform(df, sp, current_types(),
                           tryCatch(panel(), error = function(e) NULL))
      if (isTRUE(r$ok)) return(NULL)
      shiny::div(class = "alert alert-warning", style = "padding:6px 10px;",
                 shiny::tags$small(r$reason))
    })

    output$tf_preview <- shiny::renderUI({
      sp <- tf_current_spec(); df <- tf_df()
      if (is.null(sp) || is.null(df))
        return(shiny::tags$em("Choose a variable and a transformation."))
      r <- apply_transform(df, sp, current_types(),
                           tryCatch(panel(), error = function(e) NULL))
      if (!isTRUE(r$ok))
        return(shiny::div(class = "alert alert-warning", r$reason))
      new <- r$df[[r$added[1]]]
      old <- df[[sp$variable]]
      fmt <- function(x) {
        if (is.numeric(x)) {
          q <- stats::quantile(x[is.finite(x)], c(0, .5, 1), names = FALSE)
          sprintf("min %s, median %s, max %s",
                  formatC(q[1], format = "g", digits = 4),
                  formatC(q[2], format = "g", digits = 4),
                  formatC(q[3], format = "g", digits = 4))
        } else {
          lv <- levels(as.factor(x))
          sprintf("%d categories: %s", length(lv),
                  paste(utils::head(lv, 5), collapse = ", "))
        }
      }
      shp <- function(x) {
        if (!is.numeric(x)) return(NULL)
        s <- .as_skewness(x[is.finite(x)])
        if (is.finite(s)) sprintf(" (skewness %.2f)", s) else NULL
      }
      shiny::tagList(
        shiny::tags$p(shiny::strong("New column: "),
                      shiny::tags$code(paste(r$added, collapse = ", ")),
                      shiny::tags$small(sprintf("  -  measured as %s",
                                                r$record$produces))),
        shiny::tags$table(
          class = "table table-condensed", style = "margin-bottom:6px;",
          shiny::tags$tr(shiny::tags$th("Before"), shiny::tags$td(fmt(old), shp(old))),
          shiny::tags$tr(shiny::tags$th("After"), shiny::tags$td(fmt(new), shp(new)))),
        if (nzchar(r$record$note))
          shiny::tags$p(shiny::tags$small(style = "color:#4a6360;", r$record$note)),
        if (r$record$n_lost > 0)
          shiny::div(class = "alert alert-warning", style = "padding:6px 10px;",
                     shiny::tags$small(sprintf("%d value(s) become missing.",
                                               r$record$n_lost))))
    })

    # --- adding, listing and removing ----------------------------------------
    tf_add <- function(spec) {
      df <- tf_df()
      r <- apply_transform(df, spec, current_types(),
                           tryCatch(panel(), error = function(e) NULL))
      if (!isTRUE(r$ok)) {
        shiny::showNotification(r$reason, type = "warning", duration = 8)
        return(invisible(FALSE))
      }
      tf_specs(c(tf_specs(), list(spec)))
      if (!is.null(audit_record))
        try(audit_record("transform_applied", fields = c(spec$variable, r$added)),
            silent = TRUE)
      shiny::showNotification(
        sprintf("Added %s.", paste(r$added, collapse = ", ")),
        type = "message", duration = 5)
      invisible(TRUE)
    }

    shiny::observeEvent(input$tf_apply, {
      sp <- tf_current_spec()
      if (is.null(sp)) return()
      tf_add(sp)
    })

    output$tf_applied <- shiny::renderUI({
      r <- tf_result()
      if (is.null(r) || !length(r$records))
        return(shiny::tags$em("No derived columns yet. The dataset is exactly as uploaded."))
      shiny::tagList(
        shiny::div(class = "alert alert-success", style = "padding:6px 10px;",
          shiny::strong(sprintf("%d derived column%s. ", length(r$added),
                                if (length(r$added) == 1) "" else "s")),
          "These are available on every analysis tab and are described in the report."),
        shiny::actionButton(ns("tf_undo"), "Remove the last one",
                            class = "btn-xs btn-default"), " ",
        shiny::actionButton(ns("tf_clear"), "Remove all",
                            class = "btn-xs btn-default"))
    })

    shiny::observeEvent(input$tf_undo, {
      s <- tf_specs()
      if (length(s)) tf_specs(s[-length(s)])
    })
    shiny::observeEvent(input$tf_clear, tf_specs(list()))

    output$tf_table <- DT::renderDataTable({
      r <- tf_result()
      tab <- if (is.null(r)) transform_record_df(list()) else transform_record_df(r$records)
      DT::datatable(tab, rownames = FALSE, options = list(dom = "t", paging = FALSE))
    })

    output$dl_transforms <- shiny::downloadHandler(
      filename = function() sprintf("transformations-%s.csv", Sys.Date()),
      content = function(file) {
        r <- tf_result()
        utils::write.csv(if (is.null(r)) transform_record_df(list())
                         else transform_record_df(r$records), file, row.names = FALSE)
      })

    list(enforcement = enforcement, transforms = tf_specs)
  })
}
