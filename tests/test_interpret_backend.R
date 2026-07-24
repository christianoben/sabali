# tests/test_interpret_backend.R --------------------------------------------
# Backend configuration and the AI-ONLY contract (no offline fallback):
#   * ai_backend_config(): OpenCode "Big Pickle" turnkey path and the generic
#     AI_INTERPRET_* precedence;
#   * ai_available() tracks configuration;
#   * a configured-but-unreachable endpoint ERRORS (it does NOT fall back to any
#     offline text). Tested on loopback only - no data ever leaves the machine.
#
# Run:  Rscript tests/test_interpret_backend.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

.clear_ai_env <- function()
  Sys.unsetenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
                 "AI_INTERPRET_MODEL", "OPENCODE_API_KEY"))
.clear_ai_env()

coef_tab <- data.frame(
  Term = c("(Intercept)", "age", "resistant_infectionYes"),
  Estimate = c(-1.2, 0.01, -0.85),
  `p-value` = c(0.001, 0.6, 0.0001),
  check.names = FALSE, stringsAsFactors = FALSE)

cat("\n== backend configuration ==\n")
ok("no environment set -> NULL (AI off)", {
  .clear_ai_env(); stopifnot(is.null(ai_backend_config()), isFALSE(ai_available()))
})
ok("OPENCODE_API_KEY alone -> turnkey Big Pickle config + ai_available()", {
  .clear_ai_env(); Sys.setenv(OPENCODE_API_KEY = "sk-fake-not-real")
  cfg <- ai_backend_config(); avail <- ai_available(); .clear_ai_env()
  stopifnot(is.list(cfg), isTRUE(avail),
            identical(cfg$endpoint, "https://opencode.ai/zen/v1/chat/completions"),
            identical(cfg$model, "big-pickle"),
            identical(cfg$key, "sk-fake-not-real"))
})
ok("generic AI_INTERPRET_* overrides the OpenCode default", {
  .clear_ai_env()
  Sys.setenv(OPENCODE_API_KEY = "sk-fake-not-real",
             AI_INTERPRET_ENDPOINT = "https://on-prem.example/v1/chat/completions",
             AI_INTERPRET_MODEL = "local-llm")
  cfg <- ai_backend_config(); .clear_ai_env()
  stopifnot(identical(cfg$endpoint, "https://on-prem.example/v1/chat/completions"),
            identical(cfg$model, "local-llm"))
})

cat("\n== no offline fallback (loopback only - no egress) ==\n")
ok("configured-but-unreachable endpoint -> ERROR, not offline text", {
  .clear_ai_env()
  Sys.setenv(AI_INTERPRET_ENDPOINT = "http://127.0.0.1:1/v1/chat/completions",
             AI_INTERPRET_KEY = "x", AI_INTERPRET_MODEL = "big-pickle")
  err <- tryCatch({ ai_interpret(list("Coefficients" = coef_tab),
                                 context = list(title = "L")); NA },
                  error = function(e) conditionMessage(e))
  .clear_ai_env()
  stopifnot(is.character(err), !is.na(err), nchar(err) > 0)
})
ok("environment is clean again after the backend tests", {
  .clear_ai_env(); stopifnot(is.null(ai_backend_config()))
})

report_and_exit()
