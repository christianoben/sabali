# tests/test_interpret.R -----------------------------------------------------
# The embedded interpreter (fct_interpret.R) is AI-ONLY: there is no offline
# rule-based backend. These tests confirm that:
#   * ai_available() reflects whether an endpoint is configured;
#   * ai_interpret() refuses (errors) when no endpoint is configured, rather
#     than falling back to any offline text;
#   * df_to_markdown still renders a table (used to build the LLM prompt).
#
# Run:  Rscript tests/test_interpret.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# Ensure a clean, unconfigured environment (the project .Renviron may set
# OPENCODE_API_KEY, which would otherwise switch the engine into live mode).
.clear_ai_env <- function()
  Sys.unsetenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
                 "AI_INTERPRET_MODEL", "OPENCODE_API_KEY"))
.clear_ai_env()

fit_tab <- data.frame(
  Metric = c("n used", "R-squared", "Adjusted R-squared", "AIC"),
  Value = c(320, 0.42, 0.40, 512.3),
  stringsAsFactors = FALSE
)

# --- AI availability + no offline fallback ----------------------------------
ok("ai_backend_config is NULL when no endpoint set", {
  stopifnot(is.null(ai_backend_config()))
})
ok("ai_available() is FALSE when no endpoint set", {
  stopifnot(isFALSE(ai_available()))
})
ok("the offline interpreter has been removed", {
  stopifnot(!exists("interpret_offline"))
})
ok("ai_interpret() ERRORS (no offline fallback) when unconfigured", {
  err <- tryCatch({ ai_interpret(list("Model fit" = fit_tab),
                                 context = list(title = "L")); NA },
                  error = function(e) conditionMessage(e))
  stopifnot(is.character(err), grepl("no AI endpoint", err, ignore.case = TRUE))
})

# --- df_to_markdown ---------------------------------------------------------
md <- df_to_markdown(fit_tab)
ok("df_to_markdown produces a pipe table", {
  stopifnot(grepl("\\| Metric \\| Value \\|", md), grepl("\\| --- \\| --- \\|", md))
})

report_and_exit()
