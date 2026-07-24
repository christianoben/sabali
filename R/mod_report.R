# mod_report.R --------------------------------------------------------------
# Collects the tables and charts the user has added, and exports them to a
# single Word (.docx) document with one click.

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h4("Report builder"),
    shiny::helpText("Items you add from the Descriptives, Charts, Tests, Models, and Advanced Models tabs collect here. ",
                    "Export them all to a Word document."),
    shiny::fluidRow(
      shiny::column(4,
        shiny::textInput(ns("title"), "Report title", "Statistics Report"),
        shiny::textInput(ns("subtitle"), "Subtitle / source", "Sabali"),
        shiny::downloadButton(ns("download"), "Export to Word (.docx)",
                              class = "btn-primary"),
        shiny::br(), shiny::br(),
        shiny::actionButton(ns("clear"), "Clear all items", icon = shiny::icon("trash"))
      ),
      shiny::column(8,
        shiny::h5("Items in this report"),
        shiny::tableOutput(ns("manifest"))
      )
    )
  )
}

mod_report_server <- function(id, report_store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$manifest <- shiny::renderTable({
      items <- report_store$items
      if (length(items) == 0)
        return(data.frame(`#` = integer(0), Kind = character(0),
                          Caption = character(0), check.names = FALSE))
      data.frame(
        `#` = seq_along(items),
        Kind = vapply(items, function(x) x$kind, character(1)),
        Caption = vapply(items, function(x) x$caption %||% "", character(1)),
        check.names = FALSE
      )
    })

    shiny::observeEvent(input$clear, {
      report_store$items <- list()
      shiny::showNotification("Report cleared.", type = "message", duration = 3)
    })

    output$download <- shiny::downloadHandler(
      filename = function() paste0("Sabali_report_", Sys.Date(), ".docx"),
      content = function(file) {
        export_word_report(report_store$items, file,
                           title = input$title, subtitle = input$subtitle)
      }
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
