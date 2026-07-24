# fct_structure.R -----------------------------------------------------------
# Decide whether the dataset supports repeated/longitudinal or time-series
# analyses (which control access to the Advanced models tab). This is driven
# ENTIRELY by the user's explicit answer to the "How is your data organised?"
# question on the Data tab (captured in `panel`), never by heuristic guessing -
# the intended non-statistician users were tripped up by auto-detected guesses.
#
# `panel` is a list with fields:
#   kind  : "cross_sectional" | "repeated" | "time_series" (the user's answer)
#   id    : subject-ID column (repeated measures only)
#   time  : time / occasion column (repeated or time series)
#   event : optional event/status column (survival)

# Returns a list with: available (any advanced analysis?), and the flags
# `clustered` (repeated-measures / mixed models), `time_series`, and `survival`
# (time-to-event) - each driven purely by the user's explicit answers so the
# Advanced tab can offer only the applicable families.
# The layout the user declared (ignoring survival, which only reserves columns):
# "cross_sectional" | "repeated" | "time_series".
analysis_layout_kind <- function(panel) {
  if (is.null(panel) || !is.list(panel)) return("cross_sectional")
  k <- panel$kind
  if (!is.null(k) && nzchar(k)) return(k)
  if (panel_has_subject_time(panel)) "repeated"
  else if (panel_active(panel)) "time_series" else "cross_sectional"
}

# --- Which methods are RECOMMENDED (valid) for a layout kind ----------------
# These encode the STUDY-DESIGN gate driven by the Data-tab layout choice; the
# AI-plan gate (fct_planner.R) stacks on top of whatever these return. Each
# returns the allowed subset, or NULL meaning "no restriction". Designs that
# assume a particular layout are dropped BOTH ways: independence-assuming
# designs are dropped for repeated / time-series data, and within-subject
# designs (paired/repeated, mixed-model) are dropped for cross-sectional data.
# The "include un-recommended analyses" toggle reveals the full menu.
recommended_test_families <- function(kind) {
  switch(kind %||% "cross_sectional",
    repeated    = c("location", "categorical", "correlation", "cochran_q"),
    time_series = character(0),
    NULL)
}
recommended_location_designs <- function(kind) {
  switch(kind %||% "cross_sectional",
    # Within-subject designs only (repeated measures over conditions/time).
    repeated    = c("paired_repeated", "mixed"),
    time_series = character(0),
    # Cross-sectional: independent-observation designs only. Paired/repeated and
    # mixed-model designs need repeated within-subject data, so they are not
    # offered for one-row-per-subject data (revealable via the override).
    c("one_sample", "two_independent", "multi_independent", "factorial", "ancova"))
}
recommended_cat_designs <- function(kind) {
  if (identical(kind, "repeated")) "repeated_binary" else NULL
}
recommended_corr_designs <- function(kind) {
  if (identical(kind, "repeated")) "repeated" else NULL
}
# Independent regression (OLS/GLM/penalised/PLS) assumes independent rows, so it
# is recommended only for cross-sectional data; repeated/time-series users are
# steered to the Advanced models tab (GEE / LMM / GLMM).
regression_recommended_for <- function(kind) identical(kind %||% "cross_sectional", "cross_sectional")

# Columns that must NOT appear in the ordinary analysis pools (charts, tests,
# regression) because using them there is a statistical error or meaningless:
#   * the subject ID (an identifier, never an analysis variable);
#   * for a time-to-event study, the follow-up-time / start / end / event
#     columns - censored time and censoring status belong to the survival
#     methods (Kaplan-Meier / Cox on the Advanced models tab), not to t-tests,
#     correlations, OLS, histograms, etc.
reserved_analysis_vars <- function(panel) {
  if (is.null(panel) || !is.list(panel)) return(character(0))
  res <- panel$id
  if (isTRUE(panel$survival))
    res <- c(res, panel$surv_time, panel$surv_start, panel$surv_end,
             panel$surv_event, panel$event)
  res <- res[!is.na(res) & nzchar(res)]
  unique(res)
}

# Plain-language guidance shown on the Charts / Tests / Regression tabs when the
# declared data kind means the ordinary (independence-assuming) methods there
# need care or are only partly applicable. Returns an HTML string, or NULL when
# the data is plain cross-sectional (nothing to warn about).
analysis_kind_note <- function(panel) {
  if (is.null(panel) || !is.list(panel)) return(NULL)
  msgs <- character(0)
  if (isTRUE(panel$survival))
    msgs <- c(msgs, "This is a <b>time-to-event (survival)</b> study. The follow-up-time and event columns are analysed on the <b>Advanced models</b> tab (Kaplan-Meier / Cox) and are hidden here - analysing censored time with ordinary methods is invalid.")
  k <- panel$kind %||% "cross_sectional"
  if (identical(k, "repeated"))
    msgs <- c(msgs, "Your data has <b>repeated measures</b> (several rows per subject). Methods on this tab assume <b>independent observations</b>, so pooling the repeated rows can mislead (pseudoreplication). Use the repeated-measures designs here, or the <b>Advanced models</b> tab (GEE / LMM / GLMM), for valid within-subject inference.")
  if (identical(k, "time_series"))
    msgs <- c(msgs, "Your data is a <b>time series</b>. Ordinary methods here assume independent observations, but time-ordered values are usually autocorrelated - interpret with care and prefer the time-series chart and models.")
  if (!length(msgs)) return(NULL)
  paste(msgs, collapse = "<br>")
}

advanced_data_structure <- function(df, types, panel = NULL) {
  none <- function(reason) list(available = FALSE, kind = "none", reason = reason,
                                clustered = FALSE, time_series = FALSE, survival = FALSE)
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || is.null(types) || !length(types))
    return(none("Upload data and confirm its variable types first."))
  if (is.null(panel) || !is.list(panel))
    return(none("Tell the app how your data is organised on the Data tab to enable these analyses."))

  present <- function(v) !is.null(v) && nzchar(v) && v %in% names(df)
  has_id   <- present(panel$id)
  has_time <- present(panel$time)
  k <- panel$kind %||% "cross_sectional"

  clustered   <- identical(k, "repeated") && has_id && has_time
  time_series <- identical(k, "time_series") && has_time
  # Survival needs an event column plus a time specification: a duration column,
  # a start/end pair, or (many rows per subject) interval start/stop + ID.
  surv_event <- present(panel$surv_event)
  surv_time  <- present(panel$surv_time)
  surv_dates <- present(panel$surv_start) && present(panel$surv_end)
  survival <- isTRUE(panel$survival) && surv_event && (surv_time || surv_dates)

  reasons <- character(0)
  if (clustered)   reasons <- c(reasons, sprintf("repeated measures (subject '%s' over '%s')", panel$id, panel$time))
  if (time_series) reasons <- c(reasons, sprintf("a time series ('%s')", panel$time))
  if (survival) {
    how <- if (surv_time) sprintf("follow-up '%s'", panel$surv_time)
           else sprintf("interval '%s' to '%s'", panel$surv_start, panel$surv_end)
    counting <- has_id && surv_dates
    reasons <- c(reasons, sprintf("a %stime-to-event study (%s, event '%s')",
                                  if (counting) "start-stop " else "", how, panel$surv_event))
  }

  available <- clustered || time_series || survival
  if (!available) {
    # Explain what is still needed, based on what the user asked for.
    if (identical(k, "repeated"))
      return(none("You chose repeated measures - also select the subject-ID and time columns."))
    if (identical(k, "time_series"))
      return(none("You chose a time series - also select the time/date column."))
    if (isTRUE(panel$survival))
      return(none("You chose a time-to-event study - also select the event column and the follow-up time (or start/end dates)."))
    return(none("This dataset is cross-sectional with no time-to-event outcome, so repeated-measures, mixed-model and survival analyses do not apply."))
  }
  kind <- if (clustered) "longitudinal" else if (survival) "survival" else "time_series"
  list(available = TRUE, kind = kind,
       reason = paste0("You indicated ", paste(reasons, collapse = "; "), "."),
       clustered = clustered, time_series = time_series, survival = survival)
}
