# mod_datacheck.R -----------------------------------------------------------
# Tab 0 "Data check". Two tools that operate on the currently active
# (type-confirmed) dataset:
#   1. Data quality report - runs every code-checkable data-error test and
#      offers the result as a downloadable .txt notebook.
#   2. Subset builder - pick the columns you need and download the subset (CSV).

mod_datacheck_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("tabs"),
    shiny::tabPanel(
      "Data quality report",
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::wellPanel(
            shiny::h4("Automated data checks"),
            shiny::helpText("Runs the full battery of code-checkable data-quality tests against the dataset currently loaded on the Data tab."),
            shiny::uiOutput(ns("qc_summary")),
            shiny::downloadButton(ns("dl_report"), "Download report (.txt)",
                                  class = "btn-primary")
          )
        ),
        shiny::column(
          8,
          shiny::h4("Findings"),
          shiny::helpText("Errors should be fixed before analysis; warnings need a reason; notes are informational."),
          DT::dataTableOutput(ns("qc_table"))
        )
      )
    ),
    shiny::tabPanel(
      "Build a subset",
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::wellPanel(
            shiny::h4("Select columns to keep"),
            shiny::div(
              class = "subset-column-picker-scroll",
              style = paste(
                "max-height: 430px;",
                "overflow-y: auto;",
                "overflow-x: hidden;",
                "padding: 8px 10px 8px 0;",
                "border: 1px solid #dfeceb;",
                "border-radius: 4px;"
              ),
              shiny::uiOutput(ns("col_picker"))
            ),
            shiny::actionLink(ns("select_all"), "Select all"), " | ",
            shiny::actionLink(ns("select_none"), "Clear"),
            shiny::tags$hr(),
            shiny::textOutput(ns("subset_dim")),
            shiny::br(),
            shiny::downloadButton(ns("dl_subset"), "Download subset (.csv)",
                                  class = "btn-primary")
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

mod_datacheck_server <- function(id, data, types, panel = shiny::reactive(NULL),
                                 data_name = shiny::reactive("active dataset")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Data quality --------------------------------------------------
    qc <- shiny::reactive({
      df <- data(); shiny::req(df)
      dq_run_checks(df, types(), panel())
    })

    output$qc_summary <- shiny::renderUI({
      df <- data()
      if (is.null(df)) return(shiny::div(class = "alert alert-info",
        "Load a dataset on the Data tab to run the checks."))
      s <- qc()$summary
      cls <- if (s$errors > 0) "alert alert-danger"
             else if (s$warnings > 0) "alert alert-warning"
             else "alert alert-success"
      shiny::div(class = cls,
        shiny::HTML(sprintf(
          "<b>%d rows x %d columns.</b><br>%d error(s), %d warning(s), %d note(s) across %d checks.",
          s$n_rows, s$n_cols, s$errors, s$warnings, s$notes, qc()$n_checks)))
    })

    output$qc_table <- DT::renderDataTable({
      shiny::req(data())
      f <- qc()$findings
      show <- f[, c("severity", "check", "scope", "message")]
      names(show) <- c("Severity", "Check", "Where", "Detail")
      dt <- DT::datatable(show, rownames = FALSE, filter = "top",
                          options = list(pageLength = 15, order = list()))
      DT::formatStyle(dt, "Severity",
        backgroundColor = DT::styleEqual(
          c("error", "warning", "note", "pass"),
          c("#f8d7da", "#fff3cd", "#e2e3e5", "#d4edda")))
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() sprintf("data_quality_report_%s.txt", Sys.Date()),
      content = function(file) {
        writeLines(dq_report_text(qc(), dataset_name = data_name()), con = file)
      }
    )

    # --- 2. Subset builder ------------------------------------------------
    output$col_picker <- shiny::renderUI({
      df <- data(); shiny::req(df)
      shiny::checkboxGroupInput(ns("cols"), label = NULL,
                                choices = names(df), selected = names(df))
    })
    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "cols",
        choices = names(data()), selected = names(data()))
    })
    shiny::observeEvent(input$select_none, {
      shiny::updateCheckboxGroupInput(session, "cols",
        choices = names(data()), selected = character(0))
    })

    subset_df <- shiny::reactive({
      df <- data(); shiny::req(df)
      cols <- intersect(input$cols %||% names(df), names(df))
      df[, cols, drop = FALSE]
    })

    output$subset_dim <- shiny::renderText({
      d <- subset_df()
      sprintf("Subset: %d rows x %d columns selected.", nrow(d), ncol(d))
    })

    output$subset_preview <- DT::renderDataTable({
      d <- subset_df()
      if (ncol(d) == 0)
        return(DT::datatable(data.frame(Note = "Select at least one column."),
                             rownames = FALSE, options = list(dom = "t")))
      DT::datatable(utils::head(d, 100), rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_subset <- shiny::downloadHandler(
      filename = function() sprintf("data_subset_%s.csv", Sys.Date()),
      content = function(file) {
        utils::write.csv(subset_df(), file, row.names = FALSE, na = "")
      }
    )
  })
}
