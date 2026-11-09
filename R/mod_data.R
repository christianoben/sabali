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
          shiny::verbatimTextOutput(ns("summary")),
          # A REDCap export codes its categorical fields as numbers, which no
          # shape-based rule can tell from a count. The dictionary that ships
          # with the export settles it, and carries the value labels too.
          shiny::fileInput(ns("dict_file"),
                           "Form metadata (optional): REDCap dictionary or ODK XLSForm",
                           accept = c(".csv", "text/csv", ".xlsx", ".xls", ".xlsm")),
          shiny::helpText(
            "Optional. Upload the definition your data was collected with - a ",
            "REDCap \"Data Dictionary (codebook)\" CSV, or an ODK / Kobo ",
            "XLSForm workbook - and the app takes each field's type and value ",
            "labels from it instead of guessing, so coded categories are read ",
            "as categories and show their real names. The format is detected ",
            "from the file."),
          shiny::uiOutput(ns("dict_status"))
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
          shiny::uiOutput(ns("type_editor_controls")),
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
            shiny::uiOutput(ns("type_editor")),
            shiny::uiOutput(ns("derived_types_editor"))
          ),
          shiny::uiOutput(ns("type_editor_pager")),
          shiny::uiOutput(ns("date_format_editor")),
          shiny::uiOutput(ns("structure_editor")),
          shiny::uiOutput(ns("role_editor")),
          shiny::uiOutput(ns("survival_editor")),
          shiny::uiOutput(ns("survey_editor")),
          shiny::uiOutput(ns("outcome_editor"))
        )
      )
    ),
    # Optional subset filters. Rendered server-side (needs the loaded columns and
    # their confirmed types) so it only appears once a file is in. Restricts every
    # downstream analysis to the rows that pass all active filters.
    shiny::uiOutput(ns("subset_panel")),
    shiny::h4("Data preview"),
    shiny::uiOutput(ns("structure_status")),
    DT::dataTableOutput(ns("preview"))
  )
}

mod_data_server <- function(id, ai_runner = ai_async_run,
                            enforce = shiny::reactive(list()),
                            derive = shiny::reactive(list())) {
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

    raw_file <- shiny::reactive({
      shiny::req(input$file)
      session_uploads <<- unique(c(session_uploads, input$file$datapath))
      tryCatch(
        {
          out <- read_any_format(input$file$datapath, input$file$name)
          record_audit("file_imported", out)
          # Data-quality notes (constant / all-missing / single-column) are
          # advisory: the data loads, and we tell the analyst what can't be used
          # rather than refusing the whole file.
          warns <- attr(out, "import_warnings", exact = TRUE)
          if (length(warns))
            shiny::showNotification(
              shiny::tagList(
                shiny::strong("Data loaded, with notes:"),
                shiny::tags$ul(lapply(warns, shiny::tags$li))),
              type = "warning", duration = 12)
          out
        },
        error = function(e) {
          shiny::showNotification(paste("Could not read file:", conditionMessage(e)),
                                  type = "error", duration = 8)
          NULL
        }
      )
    })

    # Optional REDCap data dictionary. Kept separate from the data import so a
    # dictionary can be added (or replaced) after the data is already loaded
    # without re-reading the upload for its own sake.
    dict <- shiny::reactive({
      if (is.null(input$dict_file)) return(NULL)
      session_uploads <<- unique(c(session_uploads, input$dict_file$datapath))
      tryCatch(
        {
          out <- read_form_metadata(input$dict_file$datapath,
                                    input$dict_file$name)
          record_audit("form_metadata_imported", fields = out$field,
                       n_rows = nrow(out), n_columns = ncol(out))
          out
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not read the form metadata:", conditionMessage(e)),
            type = "error", duration = 10)
          NULL
        })
    })

    # The dictionary applied to the imported data: coded fields relabelled to
    # factors, validated dates parsed with the stated day/month order, numeric
    # fields made numeric. NULL when no dictionary is loaded.
    redcap_applied <- shiny::reactive({
      df <- raw_file(); shiny::req(df)
      d <- dict()
      if (is.null(d)) return(NULL)
      tryCatch(apply_form_metadata(df, d),
               error = function(e) {
                 shiny::showNotification(
                   paste("Could not apply the form metadata:",
                         conditionMessage(e)), type = "error", duration = 10)
                 NULL
               })
    })

    # Everything downstream reads raw(), so applying the dictionary here means
    # the confirm-types screen, descriptives, charts, Table 1, the planner
    # metadata and the report all see labelled categories with no further work.
    raw <- shiny::reactive({
      df <- raw_file(); shiny::req(df)
      a <- redcap_applied()
      if (is.null(a)) df else a$data
    })

    output$dict_status <- shiny::renderUI({
      a <- redcap_applied()
      if (is.null(a)) return(NULL)
      shiny::div(class = "alert alert-success", style = "padding:8px 10px;",
                 shiny::icon("book"), " ",
                 form_apply_summary(a, raw_file()))
    })

    data_name <- shiny::reactive({
      if (is.null(input$file) || is.null(input$file$name) || !nzchar(input$file$name))
        "active dataset"
      else input$file$name
    })

    detected <- shiny::reactive({
      shiny::req(raw())
      d <- detect_all_types(raw())
      a <- redcap_applied()
      d$from_dictionary <- FALSE
      if (!is.null(a) && length(a$types)) {
        hit <- match(names(a$types), d$variable)
        ok <- !is.na(hit)
        # A stated type beats an inferred one. Columns the dictionary does not
        # describe - REDCap's own redcap_event_name, <form>_complete and the
        # like - keep the detected type.
        d$type[hit[ok]] <- unname(a$types)[ok]
        d$from_dictionary[hit[ok]] <- TRUE
      }
      d
    })

    output$summary <- shiny::renderText({
      shiny::req(raw()); import_summary(raw())
    })

    output$outcome_editor <- shiny::renderUI({
      shiny::req(raw(), types())
      # Include derived columns. A variable created on the Data-check
      # "Transform & derive" tab is only useful if it can take a role, and the
      # most likely reason to derive one (log10 of a skewed outcome) makes it
      # the OUTCOME. Listing only the uploaded columns here made a derived
      # column look as though it had not been created at all.
      df <- tryCatch(data_derived(), error = function(e) NULL)
      if (is.null(df)) df <- data_coerced()
      original <- names(data_coerced())
      choices <- names(df)
      # Mark the derived ones so it is obvious which came from the file.
      labels <- ifelse(choices %in% original, choices,
                       paste0(choices, "  (derived)"))
      choice_list <- stats::setNames(as.list(choices), labels)
      # Outcome roles are deliberately user-selected. Do not prefill a
      # plausible-looking default: an outcome is a substantive study decision,
      # not something the app should infer from column types.
      selected <- intersect(input$outcomes %||% character(0), choices)
      shiny::tagList(
        shiny::h5("3. Which variables are your outcomes (what you want to explain or compare)?"),
        shiny::selectizeInput(
          ns("outcomes"), label = NULL,
          choices = choice_list, selected = selected, multiple = TRUE,
          options = list(placeholder = "Select one or more outcome variables")
        ),
        shiny::helpText("Optional. Choose one or more outcome variables - the things your study is really about. You can pick a column whose imported type needs temporary conversion for a chosen analysis. Leave blank to keep all variables available."),
        if (length(choices) > length(original)) shiny::helpText(
          shiny::icon("plus-circle"),
          sprintf(" %d derived column%s from the Data-check tab %s also listed.",
                  length(choices) - length(original),
                  if (length(choices) - length(original) == 1) "" else "s",
                  if (length(choices) - length(original) == 1) "is" else "are"))
      )
    })

    # Derived columns, shown with the uploaded ones under "confirm variable
    # types" so the analyst can see they exist and go on to give them a role.
    #
    # They are listed READ-ONLY, and deliberately so: the type editor's inputs
    # feed types(), which the transformations themselves consume, so an editable
    # row here would close a reactive loop (type -> transform -> new row ->
    # type). It is not a loss - a derived column's type is not a judgement call.
    # It follows from the transformation that made it, which is what the
    # catalogue's `produces` records: a log is continuous, quartiles are ordinal,
    # a split at a threshold is binary.
    output$derived_types_editor <- shiny::renderUI({
      recs <- tryCatch(derived()$records, error = function(e) list())
      if (!length(recs)) return(NULL)
      tp <- tryCatch(types_with_derived(), error = function(e) types())
      rows <- do.call(c, lapply(recs, function(r) lapply(r$output, function(nm)
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(nm)),
          shiny::tags$td(shiny::tags$span(
            class = "label", style = "background:#018786;",
            unname(tp[[nm]] %||% r$produces))),
          shiny::tags$td(shiny::tags$small(
            sprintf("%s of %s", r$label, r$variable)))))))
      shiny::div(
        style = "margin-top:10px;",
        shiny::h5(shiny::icon("plus-circle"), " Derived variables"),
        shiny::helpText(
          "Created on the Data check tab (Transform & derive). These are ready to use: pick them as outcomes below, or as predictors on any analysis tab. Their type follows from the transformation that produced them, so there is nothing to confirm here. Remove one on the Data check tab and it disappears from here too."),
        shiny::tags$table(
          class = "table table-condensed", style = "margin-bottom:4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Column"), shiny::tags$th("Type"),
            shiny::tags$th("Derived from"))),
          shiny::tags$tbody(rows)))
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

    # --- confirm-types editor (paged) ---------------------------------------
    # Wide clinical exports cannot be rendered in one pass. A household survey
    # extract measured here had 1,743 columns, which as one list is 3,486
    # dropdowns, ~3.6 MB of HTML and one observer per column before the analyst
    # can click anything. So only the current page is rendered, and only the
    # rendered rows get observers.
    #
    # Row inputs stay keyed by the column's position in detected(), never by its
    # position on the page: ids therefore survive paging and searching, and
    # types() / base_levels() / date_formats() keep reading them unchanged. An
    # unvisited row has no input, and those readers already fall back to the
    # detected type, so an untouched column behaves exactly as before.
    type_page <- shiny::reactiveVal(1L)

    # Row inputs are keyed by a DETECTION GENERATION as well as the row index.
    # Within one generation the ids are stable, so paging away and back retains a
    # correction. When the detected types themselves change - a new file, or a
    # REDCap dictionary applied to the one already loaded - the generation
    # advances and the rows get fresh inputs. Without that, a value retained from
    # the previous detection wins over the new one: uploading a dictionary left
    # `ward_group` reading "count" from the pre-dictionary render even though the
    # dictionary says it is categorical, and types() read that stale input, so
    # the column would have been ANALYSED as a count.
    type_gen <- shiny::reactiveVal(0L)
    shiny::observeEvent(detected(),
                        type_gen(shiny::isolate(type_gen()) + 1L))
    # Input id for a row control. Kept in one place so the editor and the three
    # readers below can never disagree about it.
    tid <- function(kind, i) sprintf("%s_%d_%d", kind, type_gen(), i)

    # Rows passing the search box and the filter, as indices into detected().
    # The search is a literal substring match, not a pattern: an analyst typing
    # "hh(" should get a search, not a regex error.
    type_rows <- shiny::reactive(
      type_editor_rows(detected(), input$type_search %||% "",
                       input$type_filter %||% "all"))
    type_n_pages <- shiny::reactive(type_editor_n_pages(type_rows()))
    type_page_rows <- shiny::reactive(
      type_editor_page_rows(type_rows(), type_page()))
    # A new search, filter or file starts again at the first page, so the view is
    # never left showing "page 40 of 3".
    shiny::observeEvent(list(input$type_search, input$type_filter, detected()),
                        type_page(1L), ignoreInit = TRUE)
    shiny::observeEvent(input$type_prev, type_page(max(1L, type_page() - 1L)))
    shiny::observeEvent(input$type_next,
                        type_page(min(type_n_pages(), type_page() + 1L)))

    output$type_editor_controls <- shiny::renderUI({
      shiny::req(raw())
      d <- detected()
      n_flag <- sum(types_needing_attention(d))
      n_dead <- sum(types_unusable(d))
      wide <- nrow(d) > 4L * TYPE_EDITOR_PAGE_SIZE
      # On a wide file the unfiltered list is dozens of pages of columns that
      # need no decision, so the flagged subset is the more useful first view.
      # The dropdown says so and switching back is one click.
      default_filter <- if (wide && n_flag > 0L) "attention" else "all"
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(
            ns("type_search"), label = NULL,
            value = shiny::isolate(input$type_search) %||% "",
            placeholder = sprintf("Search %s columns by name",
                                  format(nrow(d), big.mark = ",")),
            width = "100%")),
          shiny::column(6, shiny::selectInput(
            ns("type_filter"), label = NULL,
            choices = c(
              "All columns" = "all",
              stats::setNames("attention", sprintf("Needs a look (%d)", n_flag)),
              stats::setNames("unusable",
                              sprintf("Empty or constant (%d)", n_dead))),
            selected = shiny::isolate(input$type_filter) %||% default_filter,
            width = "100%"))),
        if (wide) shiny::helpText(shiny::icon("info-circle"), sprintf(
          " This file has %s columns, so the list starts with the %d that usually need a decision - small numeric code sets that may really be categories, and columns detected as IDs. %s cannot be analysed at all (empty or constant) and have their own filter. Switch to \"All columns\" to see everything.",
          format(nrow(d), big.mark = ","), n_flag, format(n_dead, big.mark = ",")))
      )
    })

    output$type_editor_pager <- shiny::renderUI({
      shiny::req(raw())
      total <- length(type_rows())
      if (!total) return(shiny::helpText(
        "No columns match this search or filter."))
      p <- min(max(1L, type_page()), type_n_pages())
      from <- (p - 1L) * TYPE_EDITOR_PAGE_SIZE + 1L
      to <- min(from + TYPE_EDITOR_PAGE_SIZE - 1L, total)
      shiny::div(
        style = "padding-top:8px; display:flex; align-items:center; gap:8px;",
        shiny::actionButton(ns("type_prev"), "Previous", class = "btn-sm",
                            disabled = if (p <= 1L) "disabled"),
        shiny::actionButton(ns("type_next"), "Next", class = "btn-sm",
                            disabled = if (p >= type_n_pages()) "disabled"),
        shiny::tags$small(style = "color:#4a6360;", sprintf(
          "Showing %d-%d of %s columns (page %d of %d)",
          from, to, format(total, big.mark = ","), p, type_n_pages()))
      )
    })

    # One row per column on the current page: variable name, a type dropdown,
    # and a base-level dropdown for factor variables (populated for
    # binary/categorical columns, "(n/a)" otherwise; an observer below keeps it
    # in step when the type is changed by hand).
    output$type_editor <- shiny::renderUI({
      d <- detected(); df <- raw()
      idx <- type_page_rows()
      if (!length(idx)) return(NULL)
      flag <- types_needing_attention(d)
      header <- shiny::fluidRow(
        style = "font-weight:600; color:#4a6360; padding:0 0 4px 0;",
        shiny::column(4, "Variable"),
        shiny::column(4, "Type"),
        shiny::column(4, "Base (reference) level")
      )
      rows <- lapply(idx, function(i) {
        nm <- d$variable[i]
        lv <- factor_levels_of(df[[nm]])
        # Render from the RETAINED input value, falling back to the detected
        # type. Shiny keeps an input's value when its UI is swapped out, but
        # re-rendering the widget with the detected type as `selected` would
        # push that default back and silently discard a correction the analyst
        # made before paging away.
        cur_type <- shiny::isolate(input[[tid("type", i)]]) %||% d$type[i]
        base_choices <- if (is_relevellable_type(cur_type) && length(lv))
          lv else c("(n/a)" = "")
        cur_base <- shiny::isolate(input[[tid("base", i)]])
        sel_base <- if (!is.null(cur_base) && cur_base %in% base_choices)
          cur_base else base_choices[[1]]
        from_dict <- isTRUE(d$from_dictionary[i])
        reason <- if (isTRUE(flag[i]))
          type_attention_reason(d$type[i], d$distinct[i], d$missing_pct[i]) else ""
        shiny::fluidRow(
          shiny::column(4, shiny::tags$div(
            style = "padding-top:7px;",
            shiny::strong(nm),
            shiny::tags$small(sprintf(" (%d distinct, %.0f%% missing)",
                                      d$distinct[i], d$missing_pct[i])),
            if (nzchar(reason)) shiny::tags$div(
              style = "font-size:11px; color:#b26a00;", reason),
            if (from_dict) shiny::tags$div(
              style = "font-size:11px; color:#2e7d32;",
              shiny::icon("book"), " from the form metadata")
          )),
          shiny::column(4, shiny::selectInput(
            ns(tid("type", i)), label = NULL,
            choices = stats::setNames(MEASUREMENT_TYPES, TYPE_LABELS[MEASUREMENT_TYPES]),
            selected = cur_type, width = "100%")),
          shiny::column(4, shiny::selectInput(
            ns(tid("base", i)), label = NULL,
            choices = base_choices,
            selected = sel_base, width = "100%"))
        )
      })
      shiny::tagList(header, rows)
    })

    # Keep each base-level dropdown in step with its type dropdown. When a
    # column is set to binary/categorical we offer its observed levels (keeping
    # the current pick if still valid); otherwise the base is not applicable.
    # Only the rendered page gets watchers - a hidden row has no widget to
    # update, and it is re-rendered from its retained values when paged back to.
    shiny::observeEvent(type_page_rows(), {
      for (o in base_watchers) o$destroy()
      base_watchers <<- list()
      d <- detected(); df <- raw()
      for (i in type_page_rows()) {
        local({
          ii <- i
          nm <- d$variable[ii]
          lv <- factor_levels_of(df[[nm]])
          type_id <- tid("type", ii); base_id <- tid("base", ii)
          base_watchers[[length(base_watchers) + 1L]] <<- shiny::observeEvent(
            input[[type_id]],
            {
              t <- input[[type_id]]
              if (isTRUE(is_relevellable_type(t)) && length(lv)) {
                cur <- input[[base_id]]
                sel <- if (!is.null(cur) && cur %in% lv) cur else lv[[1]]
                shiny::updateSelectInput(session, base_id,
                                         choices = lv, selected = sel)
              } else {
                shiny::updateSelectInput(session, base_id,
                                         choices = c("(n/a)" = ""), selected = "")
              }
            },
            ignoreInit = TRUE
          )
        })
      }
    }, ignoreNULL = FALSE)

    # Date-format editor: appears for every column typed as datetime that still
    # holds text (already-parsed Date/POSIXct columns need nothing). Each row
    # offers a format dropdown ("Automatic" + explicit shapes) and a live
    # preview so the analyst can confirm the parse or fix an ambiguous order.
    output$date_format_editor <- shiny::renderUI({
      shiny::req(raw(), types())
      d <- detected()
      idx <- datetime_rows()
      if (!length(idx)) return(NULL)
      rows <- lapply(idx, function(i) {
        nm <- d$variable[i]
        shiny::div(
          style = "padding:6px 0; border-top:1px solid #eef3f2;",
          shiny::fluidRow(
            shiny::column(5, shiny::div(style = "padding-top:26px;",
                                        shiny::strong(nm))),
            shiny::column(7, shiny::selectInput(
              ns(tid("datefmt", i)), "Date format",
              choices = DATE_FORMAT_CHOICES,
              selected = shiny::isolate(input[[tid("datefmt", i)]]) %||% "auto",
              width = "100%"))),
          shiny::uiOutput(ns(tid("dateprev", i)))
        )
      })
      shiny::tagList(
        shiny::hr(),
        shiny::h5(shiny::icon("calendar-alt"), " Confirm date formats"),
        shiny::helpText(
          "These columns look like dates. Check the preview; if a value did not ",
          "parse, or the day/month order is wrong, pick the exact format. ",
          "Month-only values (e.g. Apr-2024) are placed on the last day of that ",
          "month."),
        rows)
    })

    # Columns currently typed as datetime that still hold text: the only ones
    # that get a format dropdown and a parse preview. An already-parsed
    # Date/POSIXct column needs neither.
    datetime_rows <- shiny::reactive({
      d <- detected(); tp <- types(); df <- raw()
      which(vapply(seq_len(nrow(d)), function(i) {
        nm <- d$variable[i]
        identical(tp[[nm]], "datetime") &&
          !inherits(df[[nm]], c("Date", "POSIXct", "POSIXt"))
      }, logical(1)))
    })

    # Live per-column preview of raw -> parsed, rebuilt whenever the set of
    # datetime columns changes. Reacts to that column's format dropdown so edits
    # update instantly. Registered only for those columns: doing it for every
    # column meant 1,743 render outputs on that same wide extract, all but 60 of
    # which could never appear.
    shiny::observeEvent(datetime_rows(), {
      d <- detected()
      for (i in datetime_rows()) local({
        ii <- i
        nm <- d$variable[ii]
        prev_id <- tid("dateprev", ii); fmt_id <- tid("datefmt", ii)
        output[[prev_id]] <- shiny::renderUI({
          col <- raw()[[nm]]
          fmt <- input[[fmt_id]] %||% "auto"
          r <- sabali_parse_dates(col, fmt)
          keep <- which(!is.na(col))
          keep <- keep[seq_len(min(4L, length(keep)))]
          sample <- lapply(keep, function(j) shiny::tags$div(
            shiny::tags$code(as.character(col[j])), " → ",
            shiny::tags$strong(as.character(r$value[j]) %||% "NA")))
          status_col <- if (r$ok_pct >= 1) "#2e7d32" else "#b26a00"
          status <- shiny::tags$div(
            style = sprintf("margin-top:4px; color:%s;", status_col),
            sprintf("%d of %d values parsed.", r$n_ok, r$n_total))
          warn <- if (r$ok_pct < 1) shiny::tags$div(
            style = "color:#c0392b;",
            "Some values could not be read with this format - choose the one that matches.")
          amb <- if (identical(fmt, "auto") && isTRUE(r$ambiguous)) shiny::tags$div(
            style = "color:#b26a00;",
            "Day/month order is ambiguous - values are read DAY-first (03/04 = 3 April). ",
            "If your data is month-first, choose 'Month/Day/Year'.")
          shiny::div(style = paste("font-size:12px; background:#f7fbfa;",
                                   "border-radius:4px; padding:6px 8px; margin-top:4px;"),
                     sample, status, warn, amb)
        })
      })
    })

    # Collect the (possibly corrected) types, keyed by variable name.
    types <- shiny::reactive({
      d <- detected()
      vals <- vapply(seq_len(nrow(d)), function(i) {
        v <- input[[tid("type", i)]]
        if (is.null(v)) d$type[i] else v
      }, character(1))
      stats::setNames(vals, d$variable)
    })

    # The user-chosen base (reference) level for each variable, keyed by name.
    # "" / NULL means "leave R's default", which also covers non-factor types.
    base_levels <- shiny::reactive({
      d <- detected()
      vals <- vapply(seq_len(nrow(d)), function(i) {
        v <- input[[tid("base", i)]]
        if (is.null(v)) "" else v
      }, character(1))
      stats::setNames(vals, d$variable)
    })

    # The user-chosen date format for each variable, keyed by name ("auto" =
    # detect by shape). Only meaningful for columns typed as datetime.
    date_formats <- shiny::reactive({
      d <- detected()
      vals <- vapply(seq_len(nrow(d)), function(i) {
        v <- input[[tid("datefmt", i)]]
        if (is.null(v) || !nzchar(v)) "auto" else v
      }, character(1))
      stats::setNames(vals, d$variable)
    })

    # Coerce each column to its confirmed type, then set the chosen reference
    # level on unordered factors. Doing it here means every downstream analysis
    # - regressions, tests, tables - uses the same base, instead of whichever
    # level R happens to sort first. Value-level cleaning (contract enforcement)
    # is applied AFTER this, in data_enforced().
    data_coerced <- shiny::reactive({
      df <- raw(); tp <- types(); base <- base_levels(); fmt <- date_formats()
      shiny::req(df, tp)
      for (nm in names(df)) {
        if (!is.null(tp[[nm]]))
          df[[nm]] <- coerce_to_type(df[[nm]], tp[[nm]], format = fmt[[nm]] %||% "auto")
        # Drop factor levels left empty by import-time blank->NA cleaning.
        if (is.factor(df[[nm]])) df[[nm]] <- droplevels(df[[nm]])
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
      # Validate against the derived column set, so a derived variable can hold
      # the outcome role; removing the transformation drops it again.
      available <- tryCatch(names(data_derived()), error = function(e) NULL)
      if (is.null(available)) available <- names(data_coerced())
      intersect(selected, available)
    })

    # --- Optional subset filters -------------------------------------------
    # Up to three filters, each: pick a variable, then keep a numeric/date RANGE
    # (continuous, count, datetime) or a set of LEVELS (binary, nominal, ordinal,
    # and anything else). A row is kept only if it passes every active filter.
    # An "inactive" filter (no variable chosen, full range kept, or all levels
    # kept) is a no-op and does not even drop rows missing that variable.
    N_SUBSET_FILTERS <- 3L
    # A categorical column with more distinct values than this is not usefully
    # filtered by ticking levels (and a huge picker is slow to render), so we
    # show a note instead.
    MAX_FILTER_LEVELS <- 500L

    output$subset_panel <- shiny::renderUI({
      shiny::req(raw(), types())
      tp <- types(); cols <- names(data_coerced())
      labels <- sprintf("%s  -  %s", cols,
                        TYPE_LABELS[tp[cols]] %||% tp[cols])
      choices <- c("(no filter)" = "", stats::setNames(cols, labels))
      one <- function(i) shiny::column(
        4,
        shiny::selectInput(ns(paste0("filter_var_", i)),
                           sprintf("Filter %d - variable", i),
                           choices = choices,
                           selected = shiny::isolate(input[[paste0("filter_var_", i)]]) %||% ""),
        shiny::uiOutput(ns(paste0("filter_ctrl_", i)))
      )
      shiny::wellPanel(
        shiny::h4("Optional: analyse a subset"),
        shiny::helpText(
          "Restrict the analysis to part of your data using up to three filters. ",
          "Pick a variable; for numbers or dates you choose a range, for categories ",
          "you tick the levels to keep (leave empty to keep all). A row is kept only ",
          "if it passes every filter. Every downstream tab (descriptives, tests, ",
          "regression, ...) then uses only those rows. Leave a filter blank to ",
          "analyse everything."),
        shiny::fluidRow(lapply(seq_len(N_SUBSET_FILTERS), one)),
        shiny::uiOutput(ns("subset_status"))
      )
    })

    # Per-filter control: range slider for numeric/datetime, level picker
    # otherwise. Defined once at top level (renderUI reacts to the chosen
    # variable), never inside another reactive.
    for (i in seq_len(N_SUBSET_FILTERS)) local({
      ii <- i
      output[[paste0("filter_ctrl_", ii)]] <- shiny::renderUI({
        v <- input[[paste0("filter_var_", ii)]]
        if (is.null(v) || !nzchar(v)) return(NULL)
        df <- data_coerced()
        shiny::req(v %in% names(df))
        col <- df[[v]]
        id <- ns(paste0("filter_val_", ii))
        # Range control only when the column is genuinely orderable on a scale
        # (numeric, or a parsed date/time); everything else uses a level picker.
        if (is.numeric(col) || inherits(col, c("Date", "POSIXct", "POSIXt"))) {
          vals <- col[!is.na(col)]
          if (!length(vals))
            return(shiny::helpText("No non-missing values to filter on."))
          lo <- min(vals); hi <- max(vals)
          step <- if (identical(types()[[v]] %||% "", "count")) 1 else NULL
          shiny::sliderInput(id, "Keep values in this range",
                             min = lo, max = hi, value = c(lo, hi),
                             step = step, width = "100%")
        } else {
          lev <- if (is.factor(col)) levels(droplevels(col))
                 else sort(unique(as.character(col[!is.na(col)])))
          if (length(lev) > MAX_FILTER_LEVELS)
            return(shiny::helpText(sprintf(
              paste0("'%s' has %d distinct values - too many to filter by ticking ",
                     "levels. Pick a variable with fewer categories, or filter on a ",
                     "numeric/date column with the range slider."),
              v, length(lev))))
          # Start with nothing selected: an empty pick means "keep all", so we
          # never render (and the browser never has to draw) one tag per level.
          shiny::selectizeInput(
            id, "Keep these levels (empty = keep all)", choices = lev,
            selected = character(0), multiple = TRUE, width = "100%",
            options = list(
              placeholder = "Leave empty to keep all; or pick levels to keep",
              plugins = list("remove_button")))
        }
      })
    })

    # A debounced snapshot of the three filter controls. Debouncing is what
    # keeps large-data filtering responsive: dragging a slider or toggling
    # levels fires continuously, but the actual (whole-dataset) filter and the
    # downstream recompute only run once the user pauses (~0.4 s), instead of
    # on every pixel of movement.
    filter_inputs_now <- shiny::reactive({
      lapply(seq_len(N_SUBSET_FILTERS), function(i) list(
        var = input[[paste0("filter_var_", i)]],
        val = input[[paste0("filter_val_", i)]]))
    })
    filter_inputs <- shiny::debounce(filter_inputs_now, 400)

    # Apply the active filters to the coerced data. This is what the analysis
    # tabs receive as `data`. When no filter is active it returns the coerced
    # data unchanged - no row mask, no copy - so the common "analyse everything"
    # path costs nothing even on a large dataset.
    data_filtered <- shiny::reactive({
      df <- data_coerced()
      shiny::req(df)
      specs <- filter_inputs()
      keep <- NULL
      for (f in specs) {
        v <- f$var; val <- f$val
        if (is.null(v) || !nzchar(v) || !(v %in% names(df))) next
        if (is.null(val)) next
        col <- df[[v]]
        if (is.numeric(col) || inherits(col, c("Date", "POSIXct", "POSIXt"))) {
          if (length(val) != 2) next
          rng <- suppressWarnings(range(col, na.rm = TRUE))
          if (!all(is.finite(as.numeric(rng)))) next
          # Full range kept -> treat as no filter (also keeps NA rows).
          if (val[1] <= rng[1] && val[2] >= rng[2]) next
          mask <- !is.na(col) & col >= val[1] & col <= val[2]
        } else {
          # Empty pick = keep all (no filter, and NA rows are retained).
          if (!length(val)) next
          mask <- !is.na(col) & as.character(col) %in% val
        }
        keep <- if (is.null(keep)) mask else keep & mask
      }
      if (is.null(keep)) df else df[keep, , drop = FALSE]
    })

    # Contract enforcement: coerce values outside the tab-0 Data contract's
    # declared levels / numeric range to NA. `enforce` is supplied by the app
    # (derived from the Data-contract editor on tab 0) and is empty by default,
    # in which case this is a no-op. This is the dataset the analysis tabs use;
    # tab 0 itself reads the PRE-enforcement `data_base` so there is no cycle.
    data_enforced <- shiny::reactive({
      df <- data_filtered(); shiny::req(df)
      rules <- tryCatch(enforce(), error = function(e) list())
      if (!length(rules)) return(df)
      df <- apply_value_constraints(df, rules)
      for (nm in names(df))
        if (is.factor(df[[nm]])) df[[nm]] <- droplevels(df[[nm]])
      df
    })

    # Derived variables from the Data-check "Transform & derive" tab, applied
    # AFTER contract enforcement so a transformation works on cleaned values.
    # The specs are owned by tab 0; this is the single place they are applied,
    # so every analysis tab sees the same derived columns. Additive by
    # construction: apply_transforms() never overwrites an existing column.
    derived <- shiny::reactive({
      df <- data_enforced(); shiny::req(df)
      specs <- tryCatch(derive(), error = function(e) list())
      if (!length(specs)) return(list(df = df, records = list(), added = character(0)))
      tryCatch(apply_transforms(df, specs, types(),
                                tryCatch(panel(), error = function(e) NULL)),
               error = function(e) list(df = df, records = list(),
                                        added = character(0)))
    })
    data_derived <- shiny::reactive(derived()$df)

    # Measurement types for the derived columns come from the catalogue's
    # `produces`, so a log10 column is continuous and a quartile column is
    # ordinal without the analyst re-confirming anything. Without this the new
    # columns exist in the data but no analysis tab would offer them.
    types_with_derived <- shiny::reactive({
      ty <- types()
      recs <- derived()$records
      if (!length(recs)) return(ty)
      for (r in recs)
        for (nm in r$output) ty[[nm]] <- r$produces
      ty
    })

    output$subset_status <- shiny::renderUI({
      full <- nrow(data_coerced()); sub <- nrow(data_filtered())
      if (identical(full, sub)) {
        shiny::div(class = "alert alert-info", style = "margin:8px 0 0 0;",
                   sprintf("No subset filters active - analysing all %d rows.", full))
      } else {
        shiny::div(class = "alert alert-success", style = "margin:8px 0 0 0;",
                   shiny::strong("Subset active. "),
                   sprintf("Analysing %d of %d rows (%.0f%%). All downstream tabs use this subset.",
                           sub, full, if (full > 0) 100 * sub / full else 0))
      }
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

    # The planner call runs in a background worker (see fct_async.R) so the app
    # stays responsive while the AI is thinking - the user can keep working in
    # other tabs and the suggestion appears here when it is ready.
    plan_task <- shiny::ExtendedTask$new(
      function(payload) ai_runner("plan", payload))

    shiny::observeEvent(input$run_plan, {
      q <- trimws(input$research_question %||% "")
      if (!nzchar(q)) {
        shiny::showNotification("Type a research question first.", type = "warning")
        return()
      }
      record_audit("planner_metadata_sent", fields = names(data_coerced()),
                   n_rows = nrow(data_coerced()), n_columns = ncol(data_coerced()))
      plan_task$invoke(list(
        question = q, df = data_coerced(), types = types(),
        base_levels = base_levels(), outcomes = outcomes(),
        panel = panel(), raw = raw()))
    })

    # Normalised outcome of the async planner: NULL while idle/running, else the
    # worker's list(ok, value|error).
    plan_outcome <- shiny::reactive({
      st <- plan_task$status()
      if (identical(st, "success")) plan_task$result()
      else if (identical(st, "error"))
        list(ok = FALSE, error = tryCatch({ plan_task$result(); "unknown error" },
                                          error = function(e) conditionMessage(e)))
      else NULL
    })

    output$planner_result <- shiny::renderUI({
      if (identical(plan_task$status(), "running"))
        return(shiny::div(class = "alert alert-info",
          shiny::icon("spinner", class = "fa-spin"),
          " Asking the AI planner... you can keep working in other tabs; the suggestion will appear here when it is ready."))
      o <- plan_outcome()
      if (is.null(o)) return(NULL)
      if (!isTRUE(o$ok))
        return(shiny::div(class = "alert alert-danger",
          shiny::strong("Planner failed: "), o$error))
      res <- o$value
      # ai_plan() already relocates misfiled ids and drops unrunnable ones, so a
      # plan that still fails here is unusable rather than merely untidy.
      if (!isTRUE(res$valid))
        return(shiny::div(class = "alert alert-danger",
          shiny::strong("The AI returned choices this app cannot run: "),
          paste(res$errors, collapse = "; "), shiny::tags$br(),
          "Please rephrase the question and try again."))
      planner_plan_ui(res)
    })

    # The validated plan (or NULL) exposed to the analysis tabs so they can gate
    # their offered analyses to what the AI recommended.
    plan_out <- shiny::reactive({
      o <- plan_outcome()
      if (is.null(o) || !isTRUE(o$ok) || !isTRUE(o$value$valid)) return(NULL)
      o$value$plan
    })

    # `data` is the enforced dataset the analysis tabs consume; `data_base` is
    # the pre-enforcement (subset-filtered, coerced) data the Data-check tab
    # reads to build/check the contract without creating a reactive cycle.
    # `data` is what the analysis tabs consume: enforced, then widened with any
    # derived columns. `data_base` stays pre-enforcement and pre-derivation, so
    # tab 0 can build the contract and the transform list without a cycle.
    # --- survey (complex sample) design ------------------------------------
    # A complex sample analysed as a simple random sample gets both the estimate
    # and its uncertainty wrong. Most population-based public-health data is
    # sampled this way - DHS/MICS and national surveys, cluster-randomised
    # trials, point-prevalence surveys sampling patients within hospitals - so
    # the declaration has to be first-class. It is optional: leave it blank and
    # nothing changes.
    output$survey_editor <- shiny::renderUI({
      shiny::req(raw(), types())
      cols <- c("(none)" = "", stats::setNames(names(raw()), names(raw())))
      pick <- function(key, help) {
        shiny::tagList(
          shiny::selectInput(ns(paste0("svy_", key)),
                             SURVEY_ROLE_LABELS[[key]], choices = cols,
                             selected = shiny::isolate(input[[paste0("svy_", key)]]) %||% ""),
          shiny::helpText(help))
      }
      shiny::tagList(
        shiny::hr(),
        shiny::h5(shiny::icon("layer-group"), " Complex sample design (optional)"),
        shiny::helpText(
          "Declare this only if your data comes from a weighted, stratified or ",
          "clustered sample. When you do, the Regression tab offers design-based ",
          "estimation, whose standard errors account for the design; leaving it ",
          "blank treats the rows as a simple random sample, exactly as before."),
        shiny::fluidRow(
          shiny::column(6, pick("weights",
            "The number of population units each row represents (Stata's pweight).")),
          shiny::column(6, pick("strata",
            "The variable the sample was stratified by."))),
        shiny::fluidRow(
          shiny::column(6, pick("psu",
            "The cluster sampled first - village, ward, hospital, household.")),
          shiny::column(6, pick("fpc",
            "Sampling fraction or population size, if sampling was without replacement."))),
        shiny::tags$details(
          shiny::tags$summary("Replicate weights (BRR / jackknife / bootstrap)"),
          shiny::helpText(
            "Some producers - NHANES, several DHS recodes, national statistics ",
            "offices - publish a block of replicate weight columns INSTEAD of ",
            "the PSU and stratum, so the design can be honoured without ",
            "disclosing which cluster a household is in. Declare them here and ",
            "leave the PSU/stratum blank."),
          shiny::textInput(ns("svy_repweights"), "Replicate weight columns",
                           value = shiny::isolate(input$svy_repweights) %||% "",
                           placeholder = "a name pattern, e.g. ^BRR[0-9]+$ or ^wgt"),
          shiny::helpText("A regular expression is easier than ticking eighty boxes; every matching column is used."),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(ns("svy_reptype"), "Replicate scheme",
              choices = SURVEY_REP_TYPES,
              selected = shiny::isolate(input$svy_reptype) %||% "BRR")),
            shiny::column(6, shiny::textInput(ns("svy_repscale"),
              "Scale factor (optional)",
              value = shiny::isolate(input$svy_repscale) %||% "",
              placeholder = "from the data dictionary"))),
          shiny::helpText("Fay's BRR also needs its rho constant - put it in the scale box.")),
        shiny::fluidRow(
          shiny::column(6, pick("psu2",
            "Only for a two-stage design: the unit sampled WITHIN the PSU (Stata's `|| second stage`).")),
          shiny::column(6, pick("fpc2",
            "Finite population correction for that second stage."))),
        shiny::uiOutput(ns("survey_status"))
      )
    })

    survey_spec <- shiny::reactive({
      spec <- list(weights = input$svy_weights %||% "",
                   strata  = input$svy_strata  %||% "",
                   psu     = input$svy_psu     %||% "",
                   fpc     = input$svy_fpc     %||% "",
                   psu2    = input$svy_psu2    %||% "",
                   fpc2    = input$svy_fpc2    %||% "",
                   repweights = input$svy_repweights %||% "",
                   reptype = input$svy_reptype %||% "BRR",
                   scale   = input$svy_repscale %||% "",
                   rho     = input$svy_repscale %||% "")
      spec[!vapply(spec, function(v) is.null(v) || !nzchar(v), logical(1))]
    })

    output$survey_status <- shiny::renderUI({
      spec <- survey_spec()
      if (!survey_design_declared(spec))
        return(shiny::helpText("No design declared - rows are treated as a simple random sample."))
      built <- tryCatch(build_survey_design(data_derived(), spec),
                        error = function(e) e)
      if (inherits(built, "error"))
        return(shiny::div(class = "alert alert-warning", style = "padding:8px 10px;",
                          shiny::icon("exclamation-triangle"), " ",
                          conditionMessage(built)))
      shiny::div(class = "alert alert-success", style = "padding:8px 10px;",
        shiny::icon("check"), " Design declared. ",
        shiny::tags$small(.survey_note(built)))
    })

    list(raw = raw, data = data_derived, data_base = data_filtered,
         data_full = data_coerced,
         data_name = data_name,
         types = types_with_derived,
         # Pre-derivation types, for tab 0: it authors the transform list, so
         # feeding it the derived types would close a reactive loop.
         types_base = types,
         outcomes = outcomes, panel = panel, survey = survey_spec,
         base_levels = base_levels, plan = plan_out,
         transform_records = shiny::reactive(derived()$records),
         audit = audit, audit_record = record_audit)
  })
}
