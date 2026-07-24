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
  list(endpoint = endpoint, key = key, model = model)
}

#' TRUE when an AI endpoint is configured. This is the single switch every AI
#' feature in the app checks: when FALSE, all AI controls render inactive.
ai_available <- function() !is.null(ai_backend_config())

# --- LLM backend ------------------------------------------------------------

#' Call an OpenAI-compatible chat/completions endpoint. Returns the assistant
#' text, or throws (there is no offline fallback - the caller surfaces the error).
interpret_via_llm <- function(tables, context, cfg) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("The httr2 package is needed for the AI backend.", call. = FALSE)

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

  req <- httr2::request(cfg$endpoint)
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  if (nzchar(cfg$key))
    req <- httr2::req_auth_bearer_token(req, cfg$key)
  req <- httr2::req_body_json(req, list(
    model = cfg$model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    temperature = 0.2
  ))
  req <- httr2::req_timeout(req, 60)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  txt <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt))
    stop("The AI endpoint returned no usable text.", call. = FALSE)
  txt
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
  list(text = txt, source = sprintf("%s via OpenCode Zen", cfg$model))
}
