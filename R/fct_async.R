# fct_async.R ---------------------------------------------------------------
# Run the slow, network-bound AI calls (the analysis planner and the results
# interpreter) OFF the main Shiny thread, so the rest of the app stays usable
# while a suggestion or interpretation is being fetched. The heavy call runs in
# a background R process (future multisession); the session's R thread is freed
# to service other tabs, and the result arrives asynchronously through an
# ExtendedTask.
#
# Design notes:
#   * Only DATA crosses to the worker (the payload + a small context), never a
#     reactive or the session - that is a hard requirement of async Shiny.
#   * A fresh worker has none of the app's functions, so it sources R/ once
#     (guarded); in-process (sequential plan, e.g. tests) they already exist and
#     are reused, which also lets tests stub ai_plan()/ai_interpret().
#   * ai_worker_dispatch() normalises the outcome to list(ok, value|error) so an
#     ordinary "planner failed" is a value, not a promise rejection to special
#     case.

ai_async_available <- function()
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("promises", quietly = TRUE)

# Start a small persistent pool of background R processes, once per process.
# Returns FALSE (and leaves the active plan untouched) when future is missing or
# a plan cannot be created - callers then run under whatever plan is active
# (sequential = in-process), so the feature still works, just without concurrency.
ai_async_setup <- function(workers = 2L) {
  if (!ai_async_available()) return(invisible(FALSE))
  if (isTRUE(getOption("sabali_async_ready"))) return(invisible(TRUE))
  ok <- tryCatch({
    future::plan(future::multisession, workers = workers)
    TRUE
  }, error = function(e) FALSE)
  options(sabali_async_ready = ok)
  invisible(ok)
}

# The bits of the current process a worker must reproduce: where to source the
# app's functions from, and the AI endpoint/key environment variables (workers
# start clean, so these are carried across explicitly rather than inherited).
.ai_worker_ctx <- function() {
  list(
    r_dir = normalizePath("R", mustWork = FALSE),
    env = as.list(Sys.getenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
                               "AI_INTERPRET_MODEL", "AI_INTERPRET_THINKING",
                               "OPENCODE_API_KEY"))))
}

# Runs in the worker: set the AI env, dispatch the requested call, and normalise
# the outcome. Pure R (no Shiny), so it is unit-testable directly.
ai_worker_dispatch <- function(kind, payload, ctx = list()) {
  if (length(ctx$env)) do.call(Sys.setenv, ctx$env)
  tryCatch({
    value <- switch(kind,
      plan = ai_plan(payload$question, payload$df, payload$types,
                     payload$base_levels, payload$outcomes, payload$panel,
                     raw = payload$raw),
      interpret = ai_interpret(payload$tables, payload$context %||% list()),
      writeup = ai_report_writeup(payload$tables, payload$context %||% list(),
                                  payload$interpretation),
      stop("Unknown AI job kind: ", kind))
    list(ok = TRUE, value = value)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

# Build the promise an ExtendedTask runs. In a fresh background worker the app's
# functions do not exist yet, so R/ is sourced once (guarded); in-process they
# already exist and are reused.
ai_async_run <- function(kind, payload, ctx = .ai_worker_ctx()) {
  # No future backend: run in-process and hand back an already-resolved promise.
  # This blocks (as before async), but keeps the feature working everywhere.
  if (!ai_async_available())
    return(promises::promise_resolve(ai_worker_dispatch(kind, payload, ctx)))
  ai_async_setup()
  promises::future_promise(
    {
      if (!exists("ai_worker_dispatch", mode = "function"))
        for (f in list.files(ctx$r_dir, pattern = "^[^_].*[.][Rr]$",
                             full.names = TRUE))
          source(f, local = FALSE)
      ai_worker_dispatch(kind, payload, ctx)
    },
    globals = list(kind = kind, payload = payload, ctx = ctx),
    seed = TRUE)
}
