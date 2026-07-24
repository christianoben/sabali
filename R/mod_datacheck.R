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
        "The starting contract is populated from the uploaded data: every field is marked required, numeric/date bounds use the observed non-missing range, and categorical fields list their observed levels. Changing the expected type refreshes the minimum, maximum, and allowed-level defaults for that variable. Edit or clear any value when the study protocol allows something different. These settings do not change the uploaded data; they only make the checks explicit and auditable."),
      shiny::uiOutput(ns("contract_editor")),
      shiny::uiOutput(ns("contract_subgroup_editor"))
    ),
    shiny::tabPanel(
      "Analysis readiness", shiny::br(),
      shiny::helpText("Select an outcome and predictors when you want model-specific checks such as class imbalance, correlation, separation, and modeling suitability. No analysis is run from this panel."),
      shiny::uiOutput(ns("readiness_editor")),
      shiny::uiOutput(ns("analysis_summary"))
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
  })
}
