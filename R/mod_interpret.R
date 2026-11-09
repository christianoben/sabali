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

mod_interpret_server <- function(id, payload, audit = NULL,
                                 ai_runner = ai_async_run) {
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

    # TRUE when the last click had no result tables to interpret (handled
    # synchronously, without troubling a worker).
    empty_flag <- shiny::reactiveVal(FALSE)

    # The most recent SUCCESSFUL interpretation, kept so that "Add to report"
    # can reuse it (and skip pass 1 of the write-up) instead of recomputing.
    # Stores the text alongside the payload it was produced from, so callers can
    # confirm it still matches the analysis on screen before trusting it.
    last_interp <- shiny::reactiveVal(NULL)
    last_payload <- shiny::reactiveVal(NULL)

    # The interpretation runs in a background worker (see fct_async.R) so the
    # session stays responsive while the AI is thinking.
    interp_task <- shiny::ExtendedTask$new(
      function(payload) ai_runner("interpret", payload))

    shiny::observeEvent(input$go, {
      p <- tryCatch(payload(), error = function(e) NULL)
      tables <- p$tables %||% list()
      tables <- tables[vapply(tables, function(t)
        is.data.frame(t) && nrow(t) > 0, logical(1))]
      if (!length(tables)) { empty_flag(TRUE); return() }
      empty_flag(FALSE)
      last_payload(list(tables = tables, context = p$context %||% list()))
      if (is.function(audit)) {
        audit("interpreter_metadata_sent", fields = names(tables),
              n_rows = sum(vapply(tables, nrow, integer(1))),
              n_columns = length(tables))
      }
      interp_task$invoke(list(tables = tables, context = p$context %||% list()))
    })

    # Normalised outcome: NULL while idle/running, else list(ok, value|error).
    interp_outcome <- shiny::reactive({
      st <- interp_task$status()
      if (identical(st, "success")) interp_task$result()
      else if (identical(st, "error"))
        list(ok = FALSE, error = tryCatch({ interp_task$result(); "unknown error" },
                                          error = function(e) conditionMessage(e)))
      else NULL
    })

    output$busy <- shiny::renderUI({
      if (identical(interp_task$status(), "running"))
        shiny::div(class = "alert alert-info", style = "margin-top:8px;",
          shiny::icon("spinner", class = "fa-spin"),
          " Interpreting... you can keep working in other tabs; the result will appear here when it is ready.")
    })

    output$out <- shiny::renderUI({
      if (isTRUE(empty_flag()))
        return(shiny::div(class = "alert alert-warning",
          "There are no result tables to interpret yet. Run the analysis first, then try again."))
      o <- interp_outcome()
      if (is.null(o)) return(NULL)
      # An AI error is shown inline (as before) rather than crashing the panel.
      text <- if (isTRUE(o$ok)) o$value$text
              else paste("Could not interpret the results:", o$error)
      if (is.null(text) || !nzchar(text)) return(NULL)
      shiny::div(
        style = "margin-top:10px; padding:10px 14px; background:#fff; border:1px solid #dfeceb; border-radius:4px;",
        render_md(text))
    })

    output$source <- shiny::renderUI({
      if (isTRUE(empty_flag())) return(NULL)
      o <- interp_outcome()
      if (is.null(o) || !isTRUE(o$ok) || is.null(o$value$source)) return(NULL)
      shiny::tags$small(style = "color:#5a726f; display:block; margin-top:6px;",
                        shiny::icon("shield-alt"), " Source: ", o$value$source, ".")
    })

    # Remember the last good interpretation (with the payload behind it).
    shiny::observe({
      o <- interp_outcome()
      if (!is.null(o) && isTRUE(o$ok) && !is.null(o$value$text) && nzchar(o$value$text))
        last_interp(list(text = o$value$text, payload = last_payload()))
    })

    # Accessor for the parent: reactive giving list(text, payload) of the most
    # recent successful interpretation, or NULL if the tab was never interpreted.
    list(interpretation = shiny::reactive(last_interp()))
  })
}
