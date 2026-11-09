# tests/test_report_enrich.R ------------------------------------------------
# The background report write-up worker (report_enricher in R/mod_report.R):
# items with an `ai` payload go pending -> done (or -> failed), and the finished
# prose is stitched back onto the item so the report list reflects progress.
# Uses an injected stub runner so no network/future is involved.
#
# Run:  Rscript tests/test_report_enrich.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({ library(promises); library(later) })

# Turn the crank until pending promises resolve, then flush reactives.
pump <- function(session, n = 80L)
  for (i in seq_len(n)) { later::run_now(); session$flushReact() }

stub_runner <- function(result) function(kind, payload) promises::promise_resolve(result)

cat("\n== report_enricher: pending -> done ==\n")

ok("an AI item becomes 'done' and receives its write-up; a no-AI item is left alone", {
  fake <- list(ok = TRUE, value = list(text = "Finished prose.", source = "stub-model"))
  srv <- function(input, output, session) {
    rs  <- shiny::reactiveValues(items = list(), next_id = 0L)
    add <- report_enricher(rs, ai_runner = stub_runner(fake),
                           ai_is_available = function() TRUE)
  }
  shiny::testServer(srv, {
    add(list(kind = "table", caption = "with-ai", data = data.frame(a = 1),
             ai = list(tables = list(T = data.frame(a = 1)), context = list(title = "t"))))
    add(list(kind = "table", caption = "no-ai", data = data.frame(a = 1)))
    pump(session)
    items <- rs$items
    stopifnot(length(items) == 2L,
              identical(items[[1]]$writeup_status, "done"),
              identical(items[[1]]$writeup, "Finished prose."),
              identical(items[[1]]$writeup_source, "stub-model"),
              is.null(items[[2]]$writeup_status))     # no ai -> never queued
  })
})

cat("\n== report_enricher: several AI items all complete (queue drains) ==\n")

ok("three queued AI items all reach 'done' (pump processes them one by one)", {
  fake <- list(ok = TRUE, value = list(text = "P", source = "s"))
  srv <- function(input, output, session) {
    rs  <- shiny::reactiveValues(items = list(), next_id = 0L)
    add <- report_enricher(rs, ai_runner = stub_runner(fake),
                           ai_is_available = function() TRUE)
  }
  shiny::testServer(srv, {
    for (k in 1:3)
      add(list(kind = "table", caption = paste0("t", k), data = data.frame(a = 1),
               ai = list(tables = list(T = data.frame(a = 1)), context = list(title = k))))
    pump(session)
    st <- vapply(rs$items, function(x) x$writeup_status %||% "NA", character(1))
    stopifnot(length(st) == 3L, all(st == "done"))
  })
})

cat("\n== report_enricher: a worker failure marks the item 'failed' ==\n")

ok("an AI failure sets status 'failed', not stuck on 'writing'", {
  fail <- list(ok = FALSE, error = "endpoint unreachable")
  srv <- function(input, output, session) {
    rs  <- shiny::reactiveValues(items = list(), next_id = 0L)
    add <- report_enricher(rs, ai_runner = stub_runner(fail),
                           ai_is_available = function() TRUE)
  }
  shiny::testServer(srv, {
    add(list(kind = "table", caption = "x", data = data.frame(a = 1),
             ai = list(tables = list(T = data.frame(a = 1)), context = list(title = "t"))))
    pump(session)
    stopifnot(identical(rs$items[[1]]$writeup_status, "error"),
              grepl("unreachable", rs$items[[1]]$writeup_error))
  })
})

cat("\n== report_enricher: no write-up when AI is unavailable ==\n")

ok("with AI unavailable, an ai item is added but never queued (status stays unset)", {
  srv <- function(input, output, session) {
    rs  <- shiny::reactiveValues(items = list(), next_id = 0L)
    add <- report_enricher(rs, ai_runner = stub_runner(list(ok = TRUE, value = list(text = "x"))),
                           ai_is_available = function() FALSE)
  }
  shiny::testServer(srv, {
    add(list(kind = "table", caption = "x", data = data.frame(a = 1),
             ai = list(tables = list(T = data.frame(a = 1)), context = list(title = "t"))))
    pump(session)
    stopifnot(length(rs$items) == 1L, is.null(rs$items[[1]]$writeup_status))
  })
})

report_and_exit()
