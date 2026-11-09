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
                 "AI_INTERPRET_MODEL", "AI_INTERPRET_THINKING", "OPENCODE_API_KEY"))
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

ok("AI_INTERPRET_THINKING is carried into the config, empty when unset", {
  .clear_ai_env()
  Sys.setenv(AI_INTERPRET_ENDPOINT = "https://on-prem.example/v1/chat/completions")
  off <- ai_backend_config()$thinking
  Sys.setenv(AI_INTERPRET_THINKING = "disabled")
  on <- ai_backend_config()$thinking
  .clear_ai_env()
  stopifnot(identical(off, ""), identical(on, "disabled"))
})

cat("\n== pinned-temperature providers ==\n")
ok(".ai_rejects_temperature() fires only on a temperature complaint", {
  fake <- function(body) list(resp = httr2::response(
    status_code = 400L, headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)))
  stopifnot(
    isTRUE(.ai_rejects_temperature(fake(
      '{"error":{"message":"invalid temperature: only 1 is allowed for this model"}}'))),
    isFALSE(.ai_rejects_temperature(fake(
      '{"error":{"message":"model not found"}}'))))
})
ok("the retry body drops temperature; thinking rides only when configured", {
  cfg <- list(model = "m", key = "", thinking = "disabled")
  first <- .llm_body(cfg, "sys", "user", temperature = 0.2)
  retry <- .llm_body(cfg, "sys", "user", temperature = NULL)
  plain <- .llm_body(list(model = "m", key = "", thinking = ""), "sys", "user", 0.2)
  stopifnot(
    identical(first$temperature, 0.2), is.null(retry$temperature),
    identical(first$thinking, list(type = "disabled")),
    identical(retry$thinking, list(type = "disabled")),
    is.null(plain$thinking), identical(plain$temperature, 0.2),
    identical(retry$model, "m"), length(retry$messages) == 2L)
})

ok("the planner gets a longer timeout than a results-table interpretation", {
  # A 51-variable dataset (data/messy_cross_sectional_dataset.csv) plus the full
  # test catalogue takes the model past a one-minute budget, so the planner must
  # not share the interpreter's default.
  stopifnot(AI_CHAT_TIMEOUT_S == 60, AI_PLAN_TIMEOUT_S > AI_CHAT_TIMEOUT_S,
            identical(formals(.llm_chat)$timeout, as.name("AI_CHAT_TIMEOUT_S")),
            grepl("AI_PLAN_TIMEOUT_S", paste(deparse(body(plan_via_llm)),
                                             collapse = " ")))
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
