# tests/test_async.R --------------------------------------------------------
# Phase 1 async: the two AI calls (analysis planner and results interpreter) run
# through an ExtendedTask so the session stays responsive. These tests cover
#   * the pure worker dispatcher (env + normalised list(ok, value|error)),
#   * a real end-to-end future_promise round-trip in AI-OFF mode (no network),
#   * the module wiring in mod_data (planner) and mod_interpret with an injected
#     stub runner - success, failure, and "nothing to interpret" paths.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(promises); library(later)
  has_future <- requireNamespace("future", quietly = TRUE)
})

# Turn the crank until pending promises resolve, then flush reactives.
pump <- function(session = NULL, n = 60L) {
  for (i in seq_len(n)) {
    later::run_now()
    if (!is.null(session)) session$flushReact()
  }
}

# Force AI OFF for the duration of an expression (so ai_plan/ai_interpret take
# the "no endpoint configured" error path without any network).
with_ai_off <- function(expr) {
  keys <- c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY", "AI_INTERPRET_MODEL", "AI_INTERPRET_THINKING",
            "OPENCODE_API_KEY")
  old <- Sys.getenv(keys, names = TRUE)
  on.exit({ Sys.unsetenv(keys); do.call(Sys.setenv, as.list(old[nzchar(old)])) })
  Sys.unsetenv(keys)
  force(expr)
}

cat("\n== Worker dispatcher ==\n")

ok("ai_worker_dispatch wraps an AI failure as list(ok = FALSE, error = ...)", {
  with_ai_off({
    r <- ai_worker_dispatch("interpret",
      list(tables = list(T = data.frame(a = 1)), context = list()))
    stopifnot(is.list(r), isFALSE(r$ok), nzchar(r$error))
    r2 <- ai_worker_dispatch("plan",
      list(question = "q", df = data.frame(x = 1), types = c(x = "continuous")))
    stopifnot(isFALSE(r2$ok), nzchar(r2$error))
  })
})

ok("ai_worker_dispatch reports an unknown job kind", {
  r <- ai_worker_dispatch("nonsense", list())
  stopifnot(isFALSE(r$ok), grepl("Unknown AI job", r$error))
})

ok("ai_worker_dispatch returns list(ok = TRUE, value) on success (stubbed)", {
  # Stub the AI call in this process; dispatch resolves it via lexical scope.
  # Restore BEFORE asserting so a failure can never leak the stub into later
  # tests (on.exit is unreliable here - the expr runs in the global frame).
  old <- ai_interpret
  ai_interpret <<- function(tables, context = list())
    list(text = "ok", source = "stub")
  r <- ai_worker_dispatch("interpret", list(tables = list(T = data.frame(a = 1))))
  ai_interpret <<- old
  stopifnot(isTRUE(r$ok), identical(r$value$text, "ok"))
})

cat("\n== End-to-end promise round-trip (AI off, no network) ==\n")

if (has_future) {
  ok("ai_async_run resolves to a normalised failure under the sequential plan", {
    # Keep the promise in-process: mark async "ready" so ai_async_setup() is a
    # no-op and does not switch us to a multisession worker.
    old_opt <- getOption("sabali_async_ready")
    options(sabali_async_ready = TRUE)
    future::plan(future::sequential)
    on.exit({ future::plan(future::sequential)
              options(sabali_async_ready = old_opt) })
    with_ai_off({
      r <- NULL
      p <- ai_async_run("interpret",
        list(tables = list(T = data.frame(a = 1)), context = list()))
      p %...>% (function(v) r <<- v)
      pump()
      stopifnot(is.list(r), isFALSE(r$ok), nzchar(r$error))
    })
  })
} else {
  cat("  SKIP  future not installed\n")
}

cat("\n== Module wiring: planner (mod_data) ==\n")

make_fixture <- function() {
  df <- data.frame(id = 1:8, age = c(20:26, NA),
                   arm = factor(rep(c("a", "b"), 4)), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv"); utils::write.csv(df, path, row.names = FALSE)
  list(path = path, name = basename(path))
}

# A stub runner: no future, no network - resolves immediately to what the real
# worker would return.
stub_runner <- function(result) function(kind, payload) promises::promise_resolve(result)

ok("planner success surfaces the plan and exposes it to other tabs", {
  fx <- make_fixture()
  fake <- list(ok = TRUE, value = list(
    valid = TRUE,
    plan = list(outcome_variable = "age", outcome_type = "continuous",
                allowable_tests = list(), assumptions_to_check = list())))
  shiny::testServer(mod_data_server,
    args = list(ai_runner = stub_runner(fake)), {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(research_question = "What predicts age?")
    session$setInputs(run_plan = 1)
    pump(session)
    stopifnot(identical(plan_task$status(), "success"))
    out <- session$returned
    stopifnot(!is.null(out$plan()), identical(out$plan()$outcome_variable, "age"))
    html <- html_text(output$planner_result)
    stopifnot(!grepl("Planner failed", html))
  })
})

ok("planner failure shows an error and leaves the exposed plan NULL", {
  fx <- make_fixture()
  fail <- list(ok = FALSE, error = "endpoint unreachable")
  shiny::testServer(mod_data_server,
    args = list(ai_runner = stub_runner(fail)), {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(research_question = "q")
    session$setInputs(run_plan = 1)
    pump(session)
    stopifnot(is.null(session$returned$plan()))
    stopifnot(grepl("Planner failed", html_text(output$planner_result)))
  })
})

ok("planner does not fire the worker when the question is blank", {
  fx <- make_fixture()
  fired <- new.env(); fired$n <- 0L
  runner <- function(kind, payload) { fired$n <- fired$n + 1L
    promises::promise_resolve(list(ok = TRUE, value = list(valid = TRUE, plan = list()))) }
  shiny::testServer(mod_data_server, args = list(ai_runner = runner), {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(research_question = "   ")   # blank
    session$setInputs(run_plan = 1)
    pump(session)
    stopifnot(identical(fired$n, 0L))
    stopifnot(identical(plan_task$status(), "initial"))
  })
})

cat("\n== Module wiring: interpreter (mod_interpret) ==\n")

payload_ok <- shiny::reactive(list(
  tables = list(Results = data.frame(term = "x", est = 1.2)),
  context = list(title = "t", method = "m")))

ok("interpreter success renders the returned text", {
  fake <- list(ok = TRUE, value = list(text = "Plain-language summary.",
                                       source = "stub-model"))
  shiny::testServer(mod_interpret_server,
    args = list(payload = payload_ok, ai_runner = stub_runner(fake)), {
    session$setInputs(go = 1)
    pump(session)
    stopifnot(identical(interp_task$status(), "success"))
    stopifnot(grepl("Plain-language summary", html_text(output$out)))
    stopifnot(grepl("stub-model", html_text(output$source)))
  })
})

ok("interpreter error is shown inline, not crashed", {
  fail <- list(ok = FALSE, error = "model timed out")
  shiny::testServer(mod_interpret_server,
    args = list(payload = payload_ok, ai_runner = stub_runner(fail)), {
    session$setInputs(go = 1)
    pump(session)
    stopifnot(grepl("Could not interpret", html_text(output$out)))
    stopifnot(grepl("model timed out", html_text(output$out)))
  })
})

ok("interpreter with no tables shows the empty-state note and skips the worker", {
  empty_payload <- shiny::reactive(list(tables = list(), context = list()))
  fired <- new.env(); fired$n <- 0L
  runner <- function(kind, payload) { fired$n <- fired$n + 1L
    promises::promise_resolve(list(ok = TRUE, value = list(text = "x"))) }
  shiny::testServer(mod_interpret_server,
    args = list(payload = empty_payload, ai_runner = runner), {
    session$setInputs(go = 1)
    pump(session)
    stopifnot(identical(fired$n, 0L))
    stopifnot(grepl("no result tables", html_text(output$out)))
  })
})

report_and_exit()
