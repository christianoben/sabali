# fct_epi.R ------------------------------------------------------------------
# Epidemiological effect measures for a binary outcome across two groups, and
# the trend test for an ordered exposure.
#
# Why this exists. A chi-square test says whether two groups differ; it does not
# say by how much, and "by how much" is the entire question in public health. The
# same 2x2 table yields a risk difference (how many extra cases per 100 exposed),
# a risk ratio (how many times more likely), an odds ratio (what a logistic model
# reports), and a number needed to treat - and they answer different questions,
# are not interchangeable, and diverge sharply when the outcome is common.
#
# The odds ratio in particular is routinely read as a risk ratio. At 10% baseline
# risk an OR of 2 is a risk ratio of about 1.8; at 50% baseline it is about 1.33.
# Because the app reports odds ratios from every logistic model, it should also
# be able to report the risk measures, and say plainly when the two part company.
#
# Intervals: Newcombe's hybrid-score method for the risk difference, and the
# usual log-scale intervals for the ratios. Newcombe rather than Wald because a
# risk difference near 0 or 1 has Wald limits outside [-1, 1].

#' Effect measures for a binary outcome in two independent groups.
#'
#' @param outcome  Binary outcome.
#' @param group    Two-level grouping variable (exposure / arm).
#' @param positive Outcome value counted as an event. Defaults to the level that
#'   sorts last, matching the app's convention elsewhere, and is reported.
#' @param exposed  Group level treated as exposed / intervention. Defaults to the
#'   level that sorts last; the other is the reference.
#' @return A list with the 2x2 counts, risks, risk difference, risk ratio, odds
#'   ratio, NNT, and the chi-square and Fisher tests of no difference.
two_group_effects <- function(outcome, group, positive = NULL, exposed = NULL,
                              conf = 0.95) {
  keep <- !is.na(outcome) & !is.na(group)
  n_dropped <- sum(!keep)
  outcome <- outcome[keep]; group <- group[keep]
  if (!length(outcome))
    stop("An effect measure needs at least one case with both the outcome and the group recorded.",
         call. = FALSE)
  olv <- levels(droplevels(as.factor(outcome)))
  glv <- levels(droplevels(as.factor(group)))
  if (length(olv) != 2L)
    stop(sprintf("The outcome must have exactly two observed values; %d were found (%s).",
                 length(olv), paste(utils::head(olv, 5), collapse = ", ")),
         call. = FALSE)
  if (length(glv) != 2L)
    stop(sprintf("The group must have exactly two observed values; %d were found (%s). For three or more groups use the chi-square test, or collapse the groups first.",
                 length(glv), paste(utils::head(glv, 5), collapse = ", ")),
         call. = FALSE)
  ev <- positive %||% olv[[2]]
  ex <- exposed %||% glv[[2]]
  if (!ev %in% olv)
    stop(sprintf("'%s' is not an observed outcome value (%s).", ev,
                 paste(olv, collapse = ", ")), call. = FALSE)
  if (!ex %in% glv)
    stop(sprintf("'%s' is not an observed group value (%s).", ex,
                 paste(glv, collapse = ", ")), call. = FALSE)
  ref <- setdiff(glv, ex)

  a <- sum(group == ex & outcome == ev)     # exposed, event
  b <- sum(group == ex & outcome != ev)     # exposed, no event
  c_ <- sum(group == ref & outcome == ev)   # reference, event
  d_ <- sum(group == ref & outcome != ev)   # reference, no event
  n1 <- a + b; n0 <- c_ + d_
  if (n1 == 0 || n0 == 0)
    stop("Both groups need at least one observation.", call. = FALSE)

  r1 <- .prop_est_ci(a, n1, conf)              # risk in the exposed
  r0 <- .prop_est_ci(c_, n0, conf)             # risk in the reference
  p1 <- r1[["estimate"]]; p0 <- r0[["estimate"]]

  # Newcombe method 10: a hybrid score interval for the difference, built from
  # the two Wilson intervals. Stays inside [-1, 1], unlike Wald.
  rd <- p1 - p0
  rd_lo <- rd - sqrt((p1 - r1[["lower"]])^2 + (r0[["upper"]] - p0)^2)
  rd_hi <- rd + sqrt((r1[["upper"]] - p1)^2 + (p0 - r0[["lower"]])^2)
  z <- stats::qnorm(1 - (1 - conf) / 2)

  log_ci <- function(est, se) {
    if (!is.finite(est) || est <= 0 || !is.finite(se))
      return(c(estimate = est, lower = NA_real_, upper = NA_real_))
    c(estimate = est, lower = est * exp(-z * se), upper = est * exp(z * se))
  }
  rr <- if (p0 > 0 && p1 >= 0)
    log_ci(p1 / p0, sqrt(1 / a - 1 / n1 + 1 / c_ - 1 / n0)) else
      c(estimate = NA_real_, lower = NA_real_, upper = NA_real_)
  or <- if (all(c(a, b, c_, d_) > 0))
    log_ci((a * d_) / (b * c_), sqrt(1 / a + 1 / b + 1 / c_ + 1 / d_)) else
      c(estimate = if (b == 0 || c_ == 0) Inf else NA_real_,
        lower = NA_real_, upper = NA_real_)

  tab <- matrix(c(a, b, c_, d_), nrow = 2, byrow = TRUE,
                dimnames = list(Group = c(paste("Exposed:", ex),
                                          paste("Reference:", ref)),
                                Outcome = c(paste("Event:", ev), "No event")))
  chi <- tryCatch(suppressWarnings(stats::chisq.test(tab, correct = FALSE)),
                  error = function(e) NULL)
  fisher <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
  exp_ok <- tryCatch(check_expected_counts(tab), error = function(e) NULL)

  # NNT is 1/|RD|, but its interval is only a simple reciprocal when the risk
  # difference is entirely on one side of zero. When the RD interval spans zero
  # the NNT interval is two disjoint pieces (NNT to benefit ... infinity ... NNT
  # to harm), which cannot be written as a single range - so it is not.
  nnt <- if (is.finite(rd) && rd != 0) 1 / abs(rd) else Inf
  nnt_spans_null <- is.finite(rd_lo) && is.finite(rd_hi) && rd_lo <= 0 && rd_hi >= 0
  nnt_ci <- if (nnt_spans_null || !is.finite(rd) || rd == 0)
    c(NA_real_, NA_real_) else sort(c(1 / abs(rd_lo), 1 / abs(rd_hi)))

  list(table = tab, n = as.integer(a + b + c_ + d_),
       n_dropped = as.integer(n_dropped), conf = conf,
       event = ev, exposed = ex, reference = ref,
       a = a, b = b, c = c_, d = d_,
       risk_exposed = r1, risk_reference = r0,
       risk_difference = c(estimate = rd, lower = max(-1, rd_lo),
                           upper = min(1, rd_hi)),
       risk_ratio = rr, odds_ratio = or,
       nnt = nnt, nnt_ci = nnt_ci, nnt_spans_null = nnt_spans_null,
       nnt_direction = if (!is.finite(rd) || rd == 0) "none"
                       else if (rd > 0) "harm" else "benefit",
       chisq = chi, fisher = fisher, expected_ok = exp_ok)
}

#' How far apart the odds ratio and the risk ratio are, in words.
#'
#' The odds ratio approximates the risk ratio only when the outcome is rare. This
#' says when that approximation has broken down, because a logistic model's odds
#' ratio read as a risk ratio is one of the most common errors in applied work.
or_rr_divergence_note <- function(e) {
  or <- e$odds_ratio[["estimate"]]; rr <- e$risk_ratio[["estimate"]]
  p0 <- e$risk_reference[["estimate"]]
  if (!all(is.finite(c(or, rr, p0)))) return("")
  if (p0 < 0.10)
    return(sprintf("The outcome is uncommon in the reference group (%.1f%%), so the odds ratio (%.2f) and the risk ratio (%.2f) are close.",
                   100 * p0, or, rr))
  sprintf("The outcome is common in the reference group (%.1f%%), so the odds ratio (%.2f) OVERSTATES the risk ratio (%.2f). A logistic model reports the odds ratio; do not read it as \"%.1f times more likely\".",
          100 * p0, or, rr, or)
}

#' Two-group effect measures as a guided-test result.
run_two_group_effects <- function(outcome, group, positive = NULL,
                                  exposed = NULL, alpha = 0.05) {
  e <- two_group_effects(outcome, group, positive = positive,
                         exposed = exposed, conf = 1 - alpha)
  ci <- function(v) if (!is.finite(v[["estimate"]])) "not estimable" else
    sprintf("%.4g (%.4g to %.4g)", v[["estimate"]], v[["lower"]], v[["upper"]])
  p_chi <- if (!is.null(e$chisq)) e$chisq$p.value else NA_real_
  p_fish <- if (!is.null(e$fisher)) e$fisher$p.value else NA_real_
  use_fisher <- !is.null(e$expected_ok) && !isTRUE(e$expected_ok$ok)

  stats_named <- c(
    "n" = e$n,
    "events / n (exposed)" = sprintf("%d / %d", e$a, e$a + e$b),
    "events / n (reference)" = sprintf("%d / %d", e$c, e$c + e$d),
    "risk (exposed)" = round(e$risk_exposed[["estimate"]], 4),
    "risk (reference)" = round(e$risk_reference[["estimate"]], 4),
    "risk difference" = round(e$risk_difference[["estimate"]], 4),
    "risk ratio" = round(e$risk_ratio[["estimate"]], 4),
    "odds ratio" = round(e$odds_ratio[["estimate"]], 4),
    "number needed to treat/harm" =
      if (e$nnt_spans_null) "not estimable (interval spans no effect)"
      else round(e$nnt, 1))

  interp <- sprintf(
    "Risk was %s in the %s group and %s in the %s group. The risk difference was %s, the risk ratio %s and the odds ratio %s.",
    sprintf("%.1f%%", 100 * e$risk_exposed[["estimate"]]), e$exposed,
    sprintf("%.1f%%", 100 * e$risk_reference[["estimate"]]), e$reference,
    ci(e$risk_difference), ci(e$risk_ratio), ci(e$odds_ratio))
  div <- or_rr_divergence_note(e)
  if (nzchar(div)) interp <- paste(interp, div)
  if (e$nnt_spans_null)
    interp <- paste(interp,
      "The number needed to treat is not reported: the risk-difference interval includes zero, so the NNT interval is two disjoint ranges (benefit and harm) rather than one, and quoting a single range would be wrong.")
  else if (is.finite(e$nnt))
    interp <- paste(interp, sprintf(
      "About %.0f people would need to be exposed for one additional %s (%s).",
      e$nnt, if (identical(e$nnt_direction, "harm")) "case" else "case avoided",
      if (all(is.finite(e$nnt_ci)))
        sprintf("%.0f to %.0f", e$nnt_ci[1], e$nnt_ci[2]) else "interval not estimable"))
  if (e$n_dropped > 0)
    interp <- paste(interp, sprintf("%d row%s missing the outcome or the group and were excluded.",
                                    e$n_dropped, if (e$n_dropped == 1L) " was" else "s were"))

  primary <- mk_result("Effect measures for a binary outcome", stats_named,
    p = if (use_fisher) p_fish else p_chi,
    effect = list("risk difference" = round(e$risk_difference[["estimate"]], 4)),
    interpretation = interp,
    note = sprintf("Event = '%s'; exposed = '%s', reference = '%s'. The risk-difference interval uses Newcombe's hybrid score method; the ratios use log-scale intervals. %s",
                   e$event, e$exposed, e$reference,
                   if (use_fisher) "Expected counts are small, so Fisher's exact p-value is reported."
                   else "The p-value is from the chi-square test of no difference."))

  alt <- if (!is.null(e$fisher)) mk_result("Fisher's exact test",
    c("n" = e$n), p = p_fish,
    effect = if (!is.null(e$fisher$estimate))
      list("conditional odds ratio" = round(unname(e$fisher$estimate), 4)) else NULL,
    interpretation = sig_phrase(p_fish, alpha, "difference between the groups"),
    note = "Exact, and preferred when any expected count is below 5.") else NULL

  list(primary = primary, alternative = alt,
       recommended = if (use_fisher) "alternative" else "primary",
       assumptions = if (!is.null(e$expected_ok))
         list(expected_counts = e$expected_ok) else list(),
       rationale = "A test says whether the groups differ; these measures say by how much. The risk difference drives how many people are affected, the ratios describe relative change, and they diverge when the outcome is common.")
}

#' Cochran-Armitage test for trend in a binary outcome across an ordered exposure.
#'
#' A chi-square test across ordered categories throws the ordering away and asks
#' only whether the proportions differ somehow. When the exposure is ordered -
#' a dose, a wealth quintile, an age band, a duration - the question is almost
#' always whether risk moves monotonically with it, which is what this asks. It
#' has more power against that alternative, and it is the standard dose-response
#' test.
#'
#' @param outcome  Binary outcome.
#' @param exposure Ordered exposure: an ordered factor, or a numeric score.
#' @param scores   Optional numeric score per level. Defaults to the ordered
#'   factor's position (1, 2, 3, ...), or the numeric values themselves.
run_trend_test <- function(outcome, exposure, positive = NULL, scores = NULL,
                           alpha = 0.05) {
  keep <- !is.na(outcome) & !is.na(exposure)
  n_dropped <- sum(!keep)
  outcome <- outcome[keep]; exposure <- exposure[keep]
  olv <- levels(droplevels(as.factor(outcome)))
  if (length(olv) != 2L)
    stop(sprintf("A trend test needs a binary outcome; %d observed values were found (%s).",
                 length(olv), paste(utils::head(olv, 5), collapse = ", ")),
         call. = FALSE)
  ev <- positive %||% olv[[2]]
  if (!ev %in% olv)
    stop(sprintf("'%s' is not an observed outcome value (%s).", ev,
                 paste(olv, collapse = ", ")), call. = FALSE)

  # The exposure's ORDER is the whole point, so it is taken from the data's own
  # ordering (an ordered factor, or the numeric values) rather than invented.
  if (is.numeric(exposure)) {
    lv <- sort(unique(exposure))
    sc <- scores %||% lv
    grp <- factor(exposure, levels = lv)
  } else {
    f <- droplevels(as.factor(exposure))
    lv <- levels(f)
    sc <- scores %||% seq_along(lv)
    grp <- f
  }
  if (length(lv) < 3L)
    stop(sprintf("A trend test needs at least three ordered exposure levels; %d were found. With two groups use the effect measures instead.",
                 length(lv)), call. = FALSE)
  if (length(sc) != length(lv))
    stop(sprintf("%d scores were supplied for %d exposure levels.",
                 length(sc), length(lv)), call. = FALSE)

  events <- as.integer(tapply(outcome == ev, grp, sum))
  n_i <- as.integer(table(grp))
  tt <- stats::prop.trend.test(events, n_i, score = sc)

  props <- events / n_i
  direction <- if (stats::cor(sc, props) >= 0) "increases" else "decreases"
  by_level <- data.frame(
    Level = lv, Score = sc, Events = events, N = n_i,
    Proportion = round(props, 4),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)

  interp <- sprintf(
    "Across the %d ordered levels the proportion with the outcome %s from %.1f%% to %.1f%%. %s",
    length(lv), direction, 100 * props[1], 100 * props[length(props)],
    sig_phrase(tt$p.value, alpha, "monotonic trend across the ordered levels"))
  interp <- paste(interp,
    "A chi-square test would ask only whether the proportions differ somehow; this asks whether they move in a consistent direction, which is the dose-response question.")
  if (n_dropped > 0)
    interp <- paste(interp, sprintf("%d row%s excluded for a missing value.",
                                    n_dropped, if (n_dropped == 1L) " was" else "s were"))

  primary <- mk_result("Cochran-Armitage test for trend",
    c("chi-squared (trend)" = round(unname(tt$statistic), 4),
      "df" = unname(tt$parameter), "n" = sum(n_i),
      "levels" = length(lv)),
    p = tt$p.value, interpretation = interp,
    note = sprintf("Event = '%s'. Scores used: %s. The scores set what 'linear' means - equally spaced by default, but supply the midpoints if the categories are unequal (e.g. age bands).",
                   ev, paste(sc, collapse = ", ")))

  # The unordered comparison, so the analyst can see what the ordering bought.
  tab <- table(grp, outcome == ev)
  chi <- tryCatch(suppressWarnings(stats::chisq.test(tab)), error = function(e) NULL)
  alt <- if (!is.null(chi)) mk_result("Chi-square test (ordering ignored)",
    c("chi-squared" = round(unname(chi$statistic), 4),
      "df" = unname(chi$parameter)),
    p = chi$p.value,
    interpretation = paste(
      sig_phrase(chi$p.value, alpha, "difference between the levels"),
      "This ignores the ordering; compare it with the trend test above."),
    note = "Reported for contrast, not as the recommended analysis for an ordered exposure.") else NULL

  list(primary = primary, alternative = alt, recommended = "primary",
       assumptions = list(), by_level = by_level,
       rationale = "For an ordered exposure the dose-response question is whether risk moves monotonically with the level, which the trend test asks directly and with more power than a chi-square test that discards the ordering.")
}
