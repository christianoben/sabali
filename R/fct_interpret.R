# fct_interpret.R -----------------------------------------------------------
# Embedded "interpret the results" engine used by mod_interpret on every
# analysis tab. It reads the SUMMARY / RESULT tables already shown on a page
# (coefficients, fit statistics, test output, descriptives) and turns them into
# a plain-language interpretation.
#
# AI-ONLY: this engine has NO offline/rule-based fallback. It works only when an
# AI endpoint is configured (see ai_backend_config). When none is configured,
# ai_available() is FALSE and every AI control in the app is rendered inactive,
# so ai_interpret() is never reached; if it is called anyway it errors clearly.
#
# The app is provider-agnostic: any OpenAI-compatible chat/completions endpoint
# works (OpenCode Zen / "Big Pickle", Anthropic-via-gateway, an on-prem model).
#
#        AI_INTERPRET_ENDPOINT  full chat/completions URL (enables the LLM)
#        AI_INTERPRET_KEY       bearer token, if the endpoint needs one
#        AI_INTERPRET_MODEL     model name (default "big-pickle")
#        AI_INTERPRET_THINKING  optional; "disabled" or "enabled" for reasoning
#                               models that bill thinking tokens as output. Sent
#                               only when set, so endpoints that do not know the
#                               field are unaffected.
#
# IMPORTANT (governance): raw, row-level patient data must never be sent to an
# external endpoint. mod_interpret only ever passes aggregate/result tables to
# this engine.

# --- small helpers ----------------------------------------------------------

# Render a data frame as a GitHub-flavoured Markdown table (for the LLM prompt
# and, if desired, for display). Values are rounded lightly for readability.
df_to_markdown <- function(df, max_rows = 40) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    return("_(empty table)_")
  d <- utils::head(df, max_rows)
  fmt <- function(v) {
    if (is.numeric(v)) {
      out <- ifelse(is.finite(v),
                    formatC(v, format = "g", digits = 4), "")
      return(out)
    }
    as.character(v)
  }
  cells <- lapply(d, fmt)
  cells <- as.data.frame(cells, stringsAsFactors = FALSE,
                         check.names = FALSE, optional = TRUE)
  names(cells) <- names(d)
  header <- paste0("| ", paste(names(cells), collapse = " | "), " |")
  rule   <- paste0("| ", paste(rep("---", ncol(cells)), collapse = " | "), " |")
  body <- apply(cells, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  extra <- if (nrow(df) > max_rows)
    sprintf("\n_(showing first %d of %d rows)_", max_rows, nrow(df)) else ""
  paste0(c(header, rule, body), collapse = "\n") |> paste0(extra)
}

# --- backend configuration --------------------------------------------------

#' Read the LLM backend configuration from the environment.
#' Returns NULL when no endpoint is configured (i.e. AI features are OFF).
#'
#' Convenience: setting just OPENCODE_API_KEY is enough to enable OpenCode Zen's
#' free "Big Pickle" model - the endpoint and model default automatically. The
#' generic AI_INTERPRET_* variables still work for any other OpenAI-compatible
#' endpoint and take precedence when set.
ai_backend_config <- function() {
  endpoint <- Sys.getenv("AI_INTERPRET_ENDPOINT", "")
  key <- Sys.getenv("AI_INTERPRET_KEY", "")
  model <- Sys.getenv("AI_INTERPRET_MODEL", "")

  opencode_key <- Sys.getenv("OPENCODE_API_KEY", "")
  if (!nzchar(endpoint) && nzchar(opencode_key)) {
    endpoint <- "https://opencode.ai/zen/v1/chat/completions"
    if (!nzchar(key)) key <- opencode_key
    if (!nzchar(model)) model <- "big-pickle"
  }

  if (!nzchar(endpoint)) return(NULL)
  if (!nzchar(model)) model <- "big-pickle"
  list(endpoint = endpoint, key = key, model = model,
       thinking = Sys.getenv("AI_INTERPRET_THINKING", ""))
}

#' TRUE when an AI endpoint is configured. This is the single switch every AI
#' feature in the app checks: when FALSE, all AI controls render inactive.
ai_available <- function() !is.null(ai_backend_config())

# --- LLM backend ------------------------------------------------------------

# A short, endpoint-agnostic provenance label ("<model> via <host>").
.ai_source <- function(cfg) {
  host <- tryCatch(sub("^https?://([^/]+).*$", "\\1", cfg$endpoint),
                   error = function(e) "AI endpoint")
  sprintf("%s via %s", cfg$model, host)
}

# TRUE when an HTTP 400 is the provider objecting to the temperature we sent
# rather than to anything else about the request.
.ai_rejects_temperature <- function(cnd) {
  txt <- tryCatch(httr2::resp_body_string(cnd$resp), error = function(e) "")
  grepl("temperature", txt, ignore.case = TRUE)
}

# The chat/completions request body. temperature = NULL omits the field, leaving
# the model on its own default; the thinking field is sent only when the operator
# has set AI_INTERPRET_THINKING, so endpoints that do not know it are unaffected.
.llm_body <- function(cfg, system_prompt, user_prompt, temperature = 0.2) {
  body <- list(
    model = cfg$model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ))
  if (!is.null(temperature)) body$temperature <- temperature
  if (nzchar(cfg$thinking %||% ""))
    body$thinking <- list(type = cfg$thinking)
  body
}

#' Send one chat/completions request to an OpenAI-compatible endpoint and return
#' the assistant text (or throw). Retries transient failures - crucially HTTP
#' 429 (rate limit), which free tiers such as Gemini return on bursts - with
#' exponential backoff that honours any Retry-After header, so a temporary limit
#' does not fail an interpretation/write-up outright.
#'
#' Some providers pin the sampling temperature (Moonshot/Kimi allows only its own
#' default) and reject any explicit value with HTTP 400. Such a rejection is
#' retried once with the field omitted, so the low temperatures the callers ask
#' for remain in force wherever they are honoured.
#'
#' @param timeout seconds to wait for the reply. Interpreting a results table is
#'   a short generation, but the planner sends the whole test catalogue plus a
#'   wide dataset's metadata and asks for a long structured plan back, which on a
#'   50-variable file exceeds the default comfortably - hence AI_PLAN_TIMEOUT_S.

#' Seconds allowed for a single chat/completions reply.
AI_CHAT_TIMEOUT_S <- 60     # interpreting a results table: a short generation
AI_PLAN_TIMEOUT_S <- 240    # a full analysis plan over a wide dataset

.llm_chat <- function(cfg, system_prompt, user_prompt, temperature = 0.2,
                      timeout = AI_CHAT_TIMEOUT_S) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("The httr2 package is needed for the AI backend.", call. = FALSE)

  build <- function(temp) {
    req <- httr2::request(cfg$endpoint)
    req <- httr2::req_headers(req, `Content-Type` = "application/json")
    if (nzchar(cfg$key))
      req <- httr2::req_auth_bearer_token(req, cfg$key)
    req <- httr2::req_body_json(
      req, .llm_body(cfg, system_prompt, user_prompt, temp))
    req <- httr2::req_retry(
      req, max_tries = 5,
      is_transient = function(resp)
        httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L))
    httr2::req_timeout(req, timeout)
  }

  resp <- tryCatch(
    httr2::req_perform(build(temperature)),
    httr2_http_400 = function(cnd) {
      if (!.ai_rejects_temperature(cnd)) stop(cnd)
      httr2::req_perform(build(NULL))
    })
  body <- httr2::resp_body_json(resp)
  txt <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt))
    stop("The AI endpoint returned no usable text.", call. = FALSE)
  trimws(txt)
}

#' Interpret summary tables in plain language. Returns the assistant text, or
#' throws (there is no offline fallback - the caller surfaces the error).
interpret_via_llm <- function(tables, context, cfg) {
  tbl_md <- paste(vapply(names(tables), function(nm)
    sprintf("### %s\n%s", nm, df_to_markdown(tables[[nm]])),
    character(1)), collapse = "\n\n")

  system_prompt <- paste(
    "You are a careful biostatistician helping a public-health analyst.",
    "You are given SUMMARY result tables from a statistical analysis (never raw",
    "patient records). Explain what they mean in plain language for a clinician:",
    "state which associations are significant, give effect sizes with confidence",
    "intervals, comment on model fit, and flag caveats and assumptions. Be concise.",
    "Always note that association is not causation. Use UK English and metric units."
  )
  user_prompt <- sprintf(
    "Analysis: %s\nMethod: %s\n\nResult tables:\n\n%s",
    context$title %||% context$analysis %||% "analysis",
    context$method %||% "(unspecified)", tbl_md)

  .llm_chat(cfg, system_prompt, user_prompt, temperature = 0.2)
}

#' Entry point used by mod_interpret. Requires a configured AI endpoint; there
#' is NO offline fallback. Errors are allowed to propagate so the caller can
#' show them. Returns list(text, source).
ai_interpret <- function(tables, context = list()) {
  cfg <- ai_backend_config()
  if (is.null(cfg))
    stop("AI interpretation is unavailable: no AI endpoint is configured.",
         call. = FALSE)
  txt <- interpret_via_llm(tables, context, cfg)
  list(text = txt, source = .ai_source(cfg))
}

# --- publication-style prose write-up ---------------------------------------
# The report builder turns each added item into a short Results-section
# paragraph. This is a SINGLE AI call per item: it writes the prose directly
# from the summary tables, using the tab's interpretation as extra context when
# the analyst already produced one (reused - never a second network call). One
# call per item keeps well under free-tier rate limits (e.g. Gemini's HTTP 429).

#' Turn summary tables (and, optionally, an interpretation) into a short prose
#' paragraph written in the style of a paper's Results section. Returns the
#' assistant text, or throws (no offline fallback - the caller surfaces it).
writeup_via_llm <- function(tables, context, interpretation, cfg) {
  tbl_md <- paste(vapply(names(tables), function(nm)
    sprintf("### %s\n%s", nm, df_to_markdown(tables[[nm]])),
    character(1)), collapse = "\n\n")

  system_prompt <- paste(
    "You are drafting the Results section of a scientific paper for a",
    "public-health journal. You are given SUMMARY result tables (never raw",
    "patient records), and sometimes a plain-language interpretation to draw on.",
    "Write ONE tight paragraph (two only if genuinely needed) of finished prose",
    "that a reader could drop straight under the table or figure. Report the key",
    "figures exactly as they appear - effect sizes with 95% confidence intervals",
    "and p-values - in past tense. Do NOT restate every row; foreground what",
    "matters. No headings, no bullet lists, no table, no preamble such as 'This",
    "table shows'. For observational data, note that association does not",
    "establish causation. Use UK English, metric units, and a measured register."
  )
  user_prompt <- sprintf(
    "Analysis: %s\nMethod: %s\n\nInterpretation (may be absent):\n%s\n\nResult tables:\n\n%s",
    context$title %||% context$analysis %||% "analysis",
    context$method %||% "(unspecified)",
    interpretation %||% "(none supplied)", tbl_md)

  .llm_chat(cfg, system_prompt, user_prompt, temperature = 0.3)
}

#' Decide whether a tab's already-computed interpretation can be reused for the
#' report write-up. Returns the text when the interpretation was produced for
#' the SAME analysis (matched on context title), else NULL so a fresh pass runs.
#' `pre` is the accessor value returned by mod_interpret_server (list(text, payload)).
interp_reuse_text <- function(pre, context = list()) {
  if (is.null(pre) || is.null(pre$text) || !nzchar(pre$text)) return(NULL)
  pt <- pre$payload$context$title %||% NA_character_
  ct <- context$title %||% NA_character_
  if (identical(pt, ct)) pre$text else NULL
}

#' Entry point used by the report builder. Given the summary tables and context,
#' and OPTIONALLY an interpretation the tab already produced (reused as extra
#' context, never re-fetched), return finished prose for the report in a SINGLE
#' AI call. Returns list(text, source).
ai_report_writeup <- function(tables, context = list(), interpretation = NULL) {
  cfg <- ai_backend_config()
  if (is.null(cfg))
    stop("AI write-up is unavailable: no AI endpoint is configured.",
         call. = FALSE)
  txt <- writeup_via_llm(tables, context, interpretation, cfg)
  list(text = txt, source = .ai_source(cfg))
}
