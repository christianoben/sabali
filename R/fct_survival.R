# fct_survival.R -------------------------------------------------------------
# Phase 4 time-to-event engine: Kaplan-Meier analysis and Cox regression.
# The engine uses {survival}'s reference implementations and returns the same
# result contract used by the model tabs and Word report.

SURVIVAL_METHOD_LABELS <- c(
  kaplan_meier = "Kaplan-Meier survival analysis",
  cox = "Cox proportional-hazards regression"
)

survival_event_status <- function(x) {
  observed <- x[!is.na(x)]
  if (!length(observed))
    stop("The event indicator has no observed values.", call. = FALSE)

  if (is.logical(x)) {
    status <- as.integer(x)
    event_label <- "TRUE"
  } else if (is.factor(x) || is.character(x)) {
    # A factor can legitimately retain its declared event level even when the
    # current sample contains no events.  Preserve the two declared levels so
    # survival_frame() can issue the specific "at least one observed event"
    # diagnostic instead of the less helpful "exactly two observed levels".
    lev <- if (is.factor(x) && length(levels(x)) == 2L) levels(x)
           else levels(factor(observed))
    if (length(lev) != 2)
      stop("The event indicator must have exactly two observed levels.", call. = FALSE)
    status <- as.integer(factor(x, levels = lev) == lev[2])
    event_label <- lev[2]
  } else {
    vals <- sort(unique(as.numeric(observed)))
    if (length(vals) != 2)
      stop("The event indicator must have exactly two observed values.", call. = FALSE)
    if (all(vals %in% c(0, 1))) {
      status <- as.integer(x)
      event_label <- "1"
    } else {
      status <- as.integer(as.numeric(x) == vals[2])
      event_label <- as.character(vals[2])
    }
  }

  if (anyNA(status) || any(!status %in% c(0L, 1L)))
    stop("The event indicator could not be converted to 0 = censored and 1 = event.",
         call. = FALSE)
  list(status = status, event_label = event_label)
}

survival_frame <- function(df, time, event, vars = character(0)) {
  needed <- unique(c(time, event, vars))
  if (any(!needed %in% names(df)))
    stop("The survival analysis references a variable that is not present in the data.",
         call. = FALSE)
  keep <- stats::complete.cases(df[, needed, drop = FALSE])
  for (nm in needed) {
    if (is.numeric(df[[nm]])) keep <- keep & is.finite(df[[nm]])
  }
  mf <- df[keep, , drop = FALSE]
  if (nrow(mf) < 5)
    stop(sprintf("Only %d complete rows are available; at least 5 are needed for survival analysis.",
                 nrow(mf)), call. = FALSE)

  follow_up <- suppressWarnings(as.numeric(mf[[time]]))
  if (any(!is.finite(follow_up)) || any(follow_up < 0))
    stop("Follow-up time must contain finite, non-negative numeric values.", call. = FALSE)
  event_info <- survival_event_status(mf[[event]])
  mf[[".__surv_time"]] <- follow_up
  mf[[".__surv_status"]] <- event_info$status
  attr(mf, "event_label") <- event_info$event_label
  attr(mf, "time_name") <- time
  attr(mf, "event_name") <- event
  mf
}

survival_curve_data <- function(fit) {
  s <- summary(fit, censored = TRUE)
  if (!length(s$time))
    return(data.frame(time = numeric(0), survival = numeric(0),
                     lower = numeric(0), upper = numeric(0),
                     strata = character(0), censored = logical(0)))
  strata <- if (is.null(s$strata)) {
    rep("Overall", length(s$time))
  } else if (length(s$strata) == length(s$time)) {
    as.character(s$strata)
  } else {
    rep(names(s$strata), s$strata)
  }
  data.frame(
    time = s$time,
    survival = s$surv,
    lower = s$lower,
    upper = s$upper,
    strata = strata,
    censored = s$n.censor > 0,
    stringsAsFactors = FALSE
  )
}

survival_curve_plot <- function(fit) {
  d <- survival_curve_data(fit)
  if (!nrow(d)) return(NULL)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = survival,
                                      colour = strata, group = strata)) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::geom_step(ggplot2::aes(y = lower), linetype = "dotted",
                       alpha = 0.55, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::geom_step(ggplot2::aes(y = upper), linetype = "dotted",
                       alpha = 0.55, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::labs(title = "Kaplan-Meier survival curve", x = "Follow-up time",
                  y = "Estimated survival probability", colour = "Group") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 13)
  censored <- d[d$censored, , drop = FALSE]
  if (nrow(censored))
    p <- p + ggplot2::geom_point(data = censored, shape = 3, size = 2.2)
  p
}

survival_summary_table <- function(fit) {
  tab <- summary(fit)$table
  if (is.null(dim(tab))) {
    tab <- matrix(tab, nrow = 1,
                  dimnames = list("Overall", names(tab)))
  }
  get_col <- function(nm) {
    if (nm %in% colnames(tab)) as.numeric(tab[, nm])
    else rep(NA_real_, nrow(tab))
  }
  data.frame(
    Group = rownames(tab),
    Records = get_col("records"),
    Events = get_col("events"),
    `Median survival` = get_col("median"),
    `95% CI lower` = get_col("0.95LCL"),
    `95% CI upper` = get_col("0.95UCL"),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

survival_assumptions <- function(mf, event_label, strata = NULL) {
  n <- nrow(mf)
  events <- sum(mf$.__surv_status == 1L)
  notes <- list(
    list(note = sprintf("Follow-up time was validated as finite and non-negative; %d complete record%s were analysed.",
                        n, if (n == 1) "" else "s")),
    list(note = sprintf("The event level was interpreted as '%s'; %d event%s and %d censored record%s were observed.",
                        event_label, events, if (events == 1) "" else "s",
                        n - events, if (n - events == 1) "" else "s"))
  )
  if (!is.null(strata)) {
    counts <- table(mf[[strata]])
    notes[[length(notes) + 1]] <- list(
      note = sprintf("Survival curves are stratified by %s; the smallest analysed group has %d record%s.",
                     strata, min(counts), if (min(counts) == 1) "" else "s"))
  }
  notes
}

run_kaplan_meier <- function(mf, time, event, strata, n_total) {
  has_strata <- !is.null(strata) && nzchar(strata)
  if (has_strata) {
    if (!strata %in% names(mf))
      stop("The selected survival grouping variable is not present in the data.", call. = FALSE)
    mf[[strata]] <- droplevels(as.factor(mf[[strata]]))
    if (nlevels(mf[[strata]]) < 2)
      stop("Kaplan-Meier group comparison needs at least two observed groups.", call. = FALSE)
  }
  rhs <- if (has_strata) sprintf("`%s`", strata) else "1"
  fit_formula <- stats::as.formula(
    sprintf("survival::Surv(.__surv_time, .__surv_status) ~ %s", rhs))
  fit <- tryCatch(
    survival::survfit(fit_formula, data = mf),
    error = function(e) stop("Kaplan-Meier analysis could not be fitted: ",
                             conditionMessage(e), call. = FALSE)
  )
  coef_tab <- survival_summary_table(fit)
  n_used <- nrow(mf)
  events <- sum(mf$.__surv_status == 1L)
  logrank_p <- NA_real_
  if (has_strata) {
    lr <- tryCatch(survival::survdiff(fit_formula, data = mf), error = function(e) NULL)
    lr_df <- if (!is.null(lr)) max(0, length(lr$n) - 1L) else 0L
    if (!is.null(lr) && lr_df > 0 && is.finite(lr$chisq))
      logrank_p <- stats::pchisq(lr$chisq, df = lr_df, lower.tail = FALSE)
  }
  metrics <- c(
    "n used" = n_used,
    "events" = events,
    "censored" = n_used - events
  )
  if (is.finite(logrank_p)) metrics <- c(metrics, "Log-rank p-value" = logrank_p)
  label <- if (has_strata) "Kaplan-Meier curves with log-rank comparison"
           else "Kaplan-Meier survival estimate"
  interpretation <- if (has_strata && is.finite(logrank_p)) {
    sprintf("The Kaplan-Meier curves were compared with a log-rank test (p = %s). A small p-value indicates evidence that survival differs between groups; it does not identify the cause of the difference.",
            p_fmt(logrank_p))
  } else {
    "The Kaplan-Meier estimate describes the probability of remaining event-free over follow-up time. Median survival is reported when it is estimable."
  }
  list(
    label = label,
    method = "kaplan_meier",
    formula = paste(deparse(fit_formula), collapse = " "),
    fit = fit,
    coefficients = coef_tab,
    fit_stats = named_metric_table(metrics),
    assumptions = survival_assumptions(mf, attr(mf, "event_label"),
                                       if (has_strata) strata else NULL),
    diagnostic_plot = survival_curve_plot(fit),
    interpretation = interpretation,
    rationale = if (has_strata)
      "Kaplan-Meier curves and a log-rank test are appropriate for describing and comparing time-to-event distributions between groups."
    else
      "Kaplan-Meier estimation describes event-free survival while accounting for right-censored observations.",
    note = "Censored records contribute information up to their last observed follow-up time.",
    n_total = n_total,
    n_used = n_used,
    effect_name = NULL,
    recommendation = NULL
  )
}

cox_diagnostic_plot <- function(fit) {
  zph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  if (is.null(zph) || is.null(zph$y) || !length(zph$y)) return(NULL)
  y <- as.matrix(zph$y)
  term <- colnames(y)[1]
  if (is.null(term)) term <- "first predictor"
  d <- data.frame(time = zph$x, residual = y[, 1])
  ggplot2::ggplot(d, ggplot2::aes(x = time, y = residual)) +
    ggplot2::geom_point(alpha = 0.65, colour = "#018786") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "#D55E00") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
    ggplot2::labs(title = paste("Scaled Schoenfeld residuals -", term),
                  x = "Event time", y = "Scaled residual") +
    ggplot2::theme_minimal(base_size = 13)
}

cox_assumptions <- function(fit, mf, event_label) {
  zph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  notes <- survival_assumptions(mf, event_label)
  events <- sum(mf$.__surv_status == 1L)
  p_count <- length(stats::coef(fit))
  if (p_count > 0) {
    epv <- events / p_count
    notes[[length(notes) + 1]] <- assumption_result("events_per_variable",
      if (epv >= 10) "ok" else if (epv >= 5) "review" else "violated",
      sprintf("There were %d events for %d model coefficient%s (%.1f per coefficient)%s.",
              events, p_count, if (p_count == 1) "" else "s", epv,
              if (epv >= 10) "" else
                "; below ten per coefficient, estimates and intervals become unreliable"),
      statistic = epv, detail = list(events = events, coefficients = p_count))
  }
  if (is.null(zph)) {
    notes[[length(notes) + 1]] <- assumption_result("proportional_hazards", "untested",
      "The proportional-hazards Schoenfeld residuals test could not be evaluated.")
  } else {
    global_row <- which(rownames(zph$table) == "GLOBAL")
    if (length(global_row)) {
      p <- zph$table[global_row[1], "p"]
      sev <- if (!is.finite(p)) "untested"
             else if (p >= 0.05) "ok" else if (p >= 0.01) "review" else "violated"
      notes[[length(notes) + 1]] <- assumption_result("proportional_hazards", sev,
        sprintf("Global proportional-hazards test p = %s: %s.", p_fmt(p),
                switch(sev,
                  ok = "no strong evidence against proportional hazards",
                  review = "some evidence of time-varying effects",
                  violated = "clear evidence of time-varying effects; review stratified or time-varying alternatives",
                  "not evaluated")),
        p = p, test = "cox.zph")
    }
  }
  notes
}

run_cox_model <- function(mf, predictors, time, event, n_total) {
  if (!length(predictors))
    stop("Cox regression needs at least one predictor.", call. = FALSE)
  rhs <- paste(sprintf("`%s`", predictors), collapse = " + ")
  fit_formula <- stats::as.formula(
    sprintf("survival::Surv(.__surv_time, .__surv_status) ~ %s", rhs))
  fit <- tryCatch(
    survival::coxph(fit_formula, data = mf, ties = "efron", x = TRUE, y = TRUE,
                    model = TRUE),
    error = function(e) stop("Cox regression could not be fitted: ",
                             conditionMessage(e), call. = FALSE)
  )
  sm <- summary(fit)
  co <- sm$coefficients
  ci <- suppressWarnings(stats::confint.default(fit))
  # summary(coxph)$coefficients has columns: coef, exp(coef), se(coef), z,
  # Pr(>|z|). The generic positional mapper assumes the lm/glm order and would
  # read exp(coef) as the Std. Error and se(coef) as the Statistic, so map the
  # Cox columns by name instead.
  coef_tab <- coefficient_table(
    term      = rownames(co),
    estimate  = co[, "coef"],
    std_error = co[, "se(coef)"],
    statistic = co[, "z"],
    p_value   = co[, "Pr(>|z|)"],
    ci_lower  = ci[, 1], ci_upper = ci[, 2],
    effect_name = "Hazard ratio")
  concordance <- if (!is.null(sm$concordance)) sm$concordance[1] else NA_real_
  events <- sum(mf$.__surv_status == 1L)
  stats_tab <- named_metric_table(c(
    "n used" = nrow(mf),
    "events" = events,
    "censored" = nrow(mf) - events,
    "Concordance" = concordance,
    "AIC" = safe_aic(fit)
  ))
  interpretation <- model_interpretation(
    coef_tab, "Cox proportional-hazards regression", "Hazard ratios")
  list(
    label = "Cox proportional-hazards regression",
    method = "cox",
    formula = paste(deparse(fit_formula), collapse = " "),
    fit = fit,
    coefficients = coef_tab,
    fit_stats = stats_tab,
    assumptions = cox_assumptions(fit, mf, attr(mf, "event_label")),
    diagnostic_plot = cox_diagnostic_plot(fit),
    interpretation = interpretation,
    rationale = "Cox regression estimates adjusted hazard ratios while allowing the baseline hazard to remain unspecified; proportional hazards must be checked.",
    note = "Hazard ratios compare the instantaneous event rate for a one-unit change in a predictor, adjusted for the other selected predictors.",
    n_total = n_total,
    n_used = nrow(mf),
    effect_name = "Hazard ratio",
    recommendation = NULL
  )
}

# Counting-process ("start-stop") Cox regression for survival data with MANY
# rows per subject and time-varying covariates: each row is an interval
# (t0, t1] for a subject, with a per-interval event/censoring indicator. Fits
# an Andersen-Gill model with a robust (cluster) variance for the subject ID.
run_cox_counting <- function(df, start, end, event, id, predictors, n_total) {
  if (!length(predictors))
    stop("Cox regression needs at least one predictor.", call. = FALSE)
  needed <- unique(c(start, end, event, id, predictors))
  if (any(!needed %in% names(df)))
    stop("The counting-process survival analysis references a variable that is not in the data.", call. = FALSE)
  d <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (nrow(d) < 5)
    stop("At least 5 complete interval rows are needed for a start-stop survival model.", call. = FALSE)

  s <- d[[start]]; e <- d[[end]]
  if (inherits(s, c("Date", "POSIXct", "POSIXt")) ||
      inherits(e, c("Date", "POSIXct", "POSIXt"))) {
    origin <- min(suppressWarnings(as.Date(s)), na.rm = TRUE)
    t0 <- as.numeric(suppressWarnings(as.Date(s)) - origin)
    t1 <- as.numeric(suppressWarnings(as.Date(e)) - origin)
  } else {
    t0 <- suppressWarnings(as.numeric(s)); t1 <- suppressWarnings(as.numeric(e))
  }
  if (any(!is.finite(t0)) || any(!is.finite(t1)))
    stop("The interval start/stop columns could not be read as numbers or dates.", call. = FALSE)
  if (any(t1 <= t0))
    stop("Each interval's end time must be later than its start time.", call. = FALSE)

  ev <- survival_event_status(d[[event]])
  d[[".__t0"]] <- t0; d[[".__t1"]] <- t1
  d[[".__surv_status"]] <- ev$status
  d[[".__id"]] <- factor(d[[id]])
  rhs <- paste(sprintf("`%s`", predictors), collapse = " + ")
  fit_formula <- stats::as.formula(sprintf(
    "survival::Surv(.__t0, .__t1, .__surv_status) ~ %s + cluster(.__id)", rhs))
  fit <- tryCatch(
    survival::coxph(fit_formula, data = d, ties = "efron"),
    error = function(e) stop("Start-stop Cox regression could not be fitted: ",
                             conditionMessage(e), call. = FALSE))
  sm <- summary(fit); co <- sm$coefficients
  ci <- suppressWarnings(stats::confint.default(fit))
  se_col <- if ("robust se" %in% colnames(co)) "robust se" else "se(coef)"
  z_col  <- if ("z" %in% colnames(co)) "z" else "z"
  coef_tab <- coefficient_table(
    term = rownames(co), estimate = co[, "coef"], std_error = co[, se_col],
    statistic = co[, z_col], p_value = co[, "Pr(>|z|)"],
    ci_lower = ci[, 1], ci_upper = ci[, 2], effect_name = "Hazard ratio")
  events <- sum(d$.__surv_status == 1L)
  n_subj <- nlevels(d$.__id)
  stats_tab <- named_metric_table(c(
    "interval rows used" = nrow(d), "subjects" = n_subj, "events" = events,
    "Concordance" = if (!is.null(sm$concordance)) sm$concordance[1] else NA_real_,
    "AIC" = safe_aic(fit)))
  list(
    label = "Cox regression (start-stop, time-varying covariates)",
    method = "cox", formula = paste(deparse(fit_formula), collapse = " "),
    fit = fit, coefficients = coef_tab, fit_stats = stats_tab,
    assumptions = list(list(note = sprintf(
      "Andersen-Gill counting-process model over %d intervals from %d subject(s); a robust variance clusters the repeated intervals within each subject.",
      nrow(d), n_subj))),
    diagnostic_plot = tryCatch(cox_diagnostic_plot(fit), error = function(e) NULL),
    interpretation = model_interpretation(coef_tab,
      "start-stop Cox regression", "Hazard ratios"),
    rationale = "The start-stop (Andersen-Gill) Cox model handles multiple time intervals per subject and time-varying covariates, with a robust variance for the within-subject correlation.",
    note = "Hazard ratios are adjusted for the other predictors; the robust (cluster) standard error accounts for repeated intervals per subject.",
    n_total = n_total, n_used = nrow(d), effect_name = "Hazard ratio",
    recommendation = NULL)
}

#' Fit a selected Phase 4 time-to-event analysis.
#'
#' spec fields: method, time | (start,end), event, strata, predictors, id.
#' When `id` plus interval start/stop are supplied the data is treated as a
#' counting-process (start-stop) dataset with time-varying covariates.
# Follow-up time in days from a start date and an end / event date. Used when a
# survival dataset stores dates rather than a precomputed duration.
survival_followup_from_dates <- function(start, end) {
  s <- suppressWarnings(as.Date(start)); e <- suppressWarnings(as.Date(end))
  as.numeric(difftime(e, s, units = "days"))
}

run_guided_survival <- function(df, spec) {
  method <- spec$method
  if (!method %in% names(SURVIVAL_METHOD_LABELS))
    stop("Choose Kaplan-Meier or Cox survival analysis.", call. = FALSE)

  # Counting-process (start-stop) data: many rows per subject with interval
  # endpoints and a subject ID -> Andersen-Gill Cox (Kaplan-Meier assumes one
  # row per subject, so it is not offered here).
  counting <- !is.null(spec$id) && nzchar(spec$id) && spec$id %in% names(df) &&
    !is.null(spec$start) && nzchar(spec$start) &&
    !is.null(spec$end) && nzchar(spec$end)
  if (counting) {
    if (!identical(method, "cox"))
      stop("This data has repeated intervals per subject, so use Cox regression - Kaplan-Meier assumes one row per subject.", call. = FALSE)
    return(run_cox_counting(df, spec$start, spec$end, spec$event, spec$id,
                            spec$predictors %||% character(0), nrow(df)))
  }

  # Follow-up time can be given directly (a duration column) OR derived from a
  # start date and an end/event date.
  if ((is.null(spec$time) || !nzchar(spec$time)) &&
      !is.null(spec$start) && nzchar(spec$start) &&
      !is.null(spec$end) && nzchar(spec$end)) {
    if (!all(c(spec$start, spec$end) %in% names(df)))
      stop("The start-date and end-date columns must both be present in the data.", call. = FALSE)
    fu <- survival_followup_from_dates(df[[spec$start]], df[[spec$end]])
    if (all(is.na(fu)))
      stop("The start and end columns could not be read as dates.", call. = FALSE)
    df[[".__followup"]] <- fu
    spec$time <- ".__followup"
  }
  if (is.null(spec$time) || !nzchar(spec$time) || !spec$time %in% names(df))
    stop("Choose a valid follow-up time variable (or a start and end date).", call. = FALSE)
  if (is.null(spec$event) || !nzchar(spec$event) || !spec$event %in% names(df))
    stop("Choose a valid event / censoring indicator.", call. = FALSE)
  if (spec$time == spec$event)
    stop("The follow-up time and event indicator must be different variables.", call. = FALSE)
  predictors <- spec$predictors %||% character(0)
  strata <- spec$strata %||% NULL
  if (method == "kaplan_meier") {
    vars <- if (!is.null(strata) && nzchar(strata)) strata else character(0)
  } else {
    if (!length(predictors))
      stop("Cox regression needs at least one predictor.", call. = FALSE)
    if (any(!predictors %in% names(df)))
      stop("One or more Cox predictors are not present in the data.", call. = FALSE)
    if (any(predictors %in% c(spec$time, spec$event)))
      stop("The follow-up time and event indicator cannot also be Cox predictors.", call. = FALSE)
    vars <- predictors
  }
  if (!is.null(strata) && nzchar(strata) &&
      strata %in% c(spec$time, spec$event, predictors))
    stop("The survival grouping variable must be different from time, event, and Cox predictors.", call. = FALSE)
  survival_strata <- if (method == "kaplan_meier" &&
                         !is.null(strata) && nzchar(strata)) strata else character(0)
  mf <- survival_frame(df, spec$time, spec$event,
                       unique(c(vars, survival_strata)))
  if (sum(mf$.__surv_status == 1L) < 1)
    stop("At least one observed event is required for survival analysis.", call. = FALSE)
  if (method == "kaplan_meier")
    return(run_kaplan_meier(mf, spec$time, spec$event, strata, nrow(df)))
  run_cox_model(mf, predictors, spec$time, spec$event, nrow(df))
}
