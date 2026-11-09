# mod_report.R --------------------------------------------------------------
# Collects the tables and charts the user has added, and exports them to a
# single Word (.docx) document with one click.

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h4("Report builder"),
    shiny::helpText("Items you add from the Descriptives, Charts, Tests, Models, and Advanced Models tabs collect here. ",
                    "Export them all to a Word document. When AI is configured, each table or chart gets a short ",
                    "publication-style write-up drafted in the background and placed beneath it in the report."),
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
                          Caption = character(0), `AI write-up` = character(0),
                          check.names = FALSE))
      status <- vapply(items, function(x) {
        if (is.null(x$ai)) return("—")
        switch(x$writeup_status %||% "pending",
               pending = "writing", done = "done", error = "failed",
               "—")
      }, character(1))
      data.frame(
        `#` = seq_along(items),
        Kind = vapply(items, function(x) x$kind, character(1)),
        Caption = vapply(items, function(x) x$caption %||% "", character(1)),
        `AI write-up` = status,
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
        # Backstop: finish any write-ups still pending/failed so the downloaded
        # document is complete even if a background job had not landed yet.
        items <- report_finalise_writeups(report_store$items)
        report_store$items <- items
        export_word_report(items, file,
                           title = input$title, subtitle = input$subtitle)
      }
    )
  })
}

#' Build the per-session `add_report()` and wire the background write-up worker.
#'
#' Must be called INSIDE a Shiny server (it creates reactives, an ExtendedTask,
#' and observers). Returns the `add_report(item)` function the analysis tabs call.
#'
#' Each item carrying an `ai` payload (tables + context [+ interpretation]) is
#' queued; a single ExtendedTask processes the queue one job at a time and
#' patches the finished prose (and a "done"/"error" status) back onto the item
#' in `report_store$items`, so the report list reflects progress live.
#'
#' @param report_store reactiveValues with `items` (list) and `next_id` (int).
#' @param ai_runner    function(kind, payload) -> promise; injectable for tests.
#' @param ai_is_available function() -> logical; gates whether write-ups run.
report_enricher <- function(report_store, ai_runner = ai_async_run,
                            ai_is_available = ai_available) {
  queue   <- shiny::reactiveVal(list())   # FIFO of list(id, payload)
  current <- shiny::reactiveVal(NULL)      # id of the job in flight, or NULL
  task <- shiny::ExtendedTask$new(function(payload) ai_runner("writeup", payload))

  add_report <- function(item) {
    id <- (report_store$next_id %||% 0L) + 1L
    report_store$next_id <- id
    item$id <- id
    if (is.null(item$writeup_status) && !is.null(item$ai) && isTRUE(ai_is_available()))
      item$writeup_status <- "pending"
    report_store$items <- c(report_store$items, list(item))
    if (identical(item$writeup_status, "pending"))
      queue(c(queue(), list(list(id = id, payload = item$ai))))
    invisible(id)
  }

  # Pump: when the worker is idle and something is queued, start the next job.
  shiny::observe({
    if (identical(task$status(), "running")) return()
    if (!is.null(current())) return()
    q <- queue(); if (!length(q)) return()
    job <- q[[1]]; queue(q[-1])
    current(job$id)
    task$invoke(job$payload)
  })

  # Collect: on each completion, patch the matching item and free the pump.
  shiny::observeEvent(task$status(), {
    st <- task$status()
    if (!st %in% c("success", "error")) return()
    id <- current(); if (is.null(id)) return()
    out <- if (identical(st, "success"))
      tryCatch(task$result(),
               error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    else list(ok = FALSE, error = "The AI worker did not return a result.")
    items <- report_store$items
    idx <- which(vapply(items, function(x) identical(x$id, id), logical(1)))
    if (length(idx)) {
      if (isTRUE(out$ok)) {
        items[[idx]]$writeup <- out$value$text
        items[[idx]]$writeup_source <- out$value$source
        items[[idx]]$writeup_status <- "done"
      } else {
        items[[idx]]$writeup_status <- "error"
        items[[idx]]$writeup_error <- out$error %||% "unknown error"
      }
      report_store$items <- items
    }
    current(NULL)
  })

  add_report
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
