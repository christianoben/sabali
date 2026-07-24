# mod_interpret.R ------------------------------------------------------------
# A small, reusable "AI interpretation" panel that can be dropped onto any
# analysis tab. It reads the SUMMARY / RESULT tables that the tab already shows
# (supplied through a payload reactive) and produces a plain-language
# interpretation via ai_interpret() - offline by default, LLM if configured.
#
# Usage:
#   UI:     mod_interpret_ui(ns("interpret"))
#   server: mod_interpret_server("interpret", payload)
# where payload is a reactive returning:
#   list(tables = <named list of data frames>, context = <list>)
# Return an empty list (or NULL) from payload when nothing is ready yet.

mod_interpret_ui <- function(id, title = "AI interpretation") {
  ns <- shiny::NS(id)
  # AI features are inactive whenever no AI endpoint is configured. Render a
  # clearly-disabled panel (no action control) instead of the working one, so
  # the behaviour is consistent on every analysis tab.
  if (!ai_available()) {
    return(shiny::div(
      class = "well",
      style = "background:#f4f4f4; border-color:#e2e2e2; margin-top:14px; color:#8a8a8a;",
      shiny::h5(shiny::icon("robot"), title),
      shiny::tags$button(class = "btn btn-default btn-sm", disabled = "disabled",
                         shiny::icon("magic"), " Interpret the results above"),
      shiny::helpText(
        "AI features are turned off because no AI endpoint is configured. ",
        "Set an AI endpoint (AI_INTERPRET_ENDPOINT / OPENCODE_API_KEY) to enable ",
        "AI interpretation.")
    ))
  }
  shiny::div(
    class = "well", style = "background:#eef6f5; border-color:#cfe6e3; margin-top:14px;",
    shiny::h5(shiny::icon("robot"), title),
    shiny::helpText(
      "Reads the result tables above and explains them in plain language. ",
      "Only summary results are used - never raw patient rows."
    ),
    shiny::actionButton(ns("go"), "Interpret the results above",
                        icon = shiny::icon("magic"),
                        class = "btn-primary btn-sm"),
    shiny::uiOutput(ns("busy")),
    shiny::uiOutput(ns("out")),
    shiny::uiOutput(ns("source"))
  )
}

mod_interpret_server <- function(id, payload, audit = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render Markdown to HTML, with graceful fallbacks across Shiny versions.
    render_md <- function(text) {
      if ("markdown" %in% getNamespaceExports("shiny"))
        return(shiny::markdown(text))
      if (requireNamespace("commonmark", quietly = TRUE))
        return(shiny::HTML(commonmark::markdown_html(text)))
      shiny::tags$pre(style = "white-space:pre-wrap;", text)
    }

    result <- shiny::eventReactive(input$go, {
      p <- tryCatch(payload(), error = function(e) NULL)
      tables <- p$tables %||% list()
      # keep only non-empty data frames
      tables <- tables[vapply(tables, function(t)
        is.data.frame(t) && nrow(t) > 0, logical(1))]
      if (!length(tables))
        return(list(text = NULL,
                    source = NULL,
                    empty = TRUE))
      if (is.function(audit)) {
        audit("interpreter_metadata_sent", fields = names(tables),
              n_rows = sum(vapply(tables, nrow, integer(1))),
              n_columns = length(tables))
      }
      res <- tryCatch(
        ai_interpret(tables, p$context %||% list()),
        error = function(e) list(text = paste("Could not interpret the results:",
                                              conditionMessage(e)),
                                 source = "error"))
      res$empty <- FALSE
      res
    })

    output$out <- shiny::renderUI({
      shiny::req(result())
      r <- result()
      if (isTRUE(r$empty))
        return(shiny::div(class = "alert alert-warning",
                          "There are no result tables to interpret yet. Run the analysis first, then try again."))
      shiny::div(
        style = "margin-top:10px; padding:10px 14px; background:#fff; border:1px solid #dfeceb; border-radius:4px;",
        render_md(r$text)
      )
    })

    output$source <- shiny::renderUI({
      shiny::req(result())
      r <- result()
      if (isTRUE(r$empty) || is.null(r$source)) return(NULL)
      shiny::tags$small(style = "color:#5a726f; display:block; margin-top:6px;",
                        shiny::icon("shield-alt"), " Source: ", r$source, ".")
    })

    invisible(NULL)
  })
}
