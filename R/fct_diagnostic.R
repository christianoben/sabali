# fct_diagnostic.R -----------------------------------------------------------
# Diagnostic and screening test accuracy.
#
# Why this exists. "Does this test agree with the reference?" and "how well does
# this test perform?" are different questions, and only the second one is
# directional. Kappa (fct_agreement.R) treats the two measurements
# symmetrically; a diagnostic evaluation does not, because one of them is the
# reference standard and the other is being judged against it. Sensitivity and
# specificity are properties of the test; predictive values are properties of the
# test AND the prevalence where it is used, which is why a test that looks
# excellent in a clinic performs differently in a screening programme.
#
# This is everyday public-health work: rapid diagnostic tests against culture or
# PCR, screening questionnaires against a clinical diagnosis, case definitions
# against laboratory confirmation, a triage rule against an outcome.
#
# Proportions get Wilson intervals rather than the normal approximation. At the
# sensitivities a screening test is usually asked about - 0.95 and above, often
# with few false negatives - the normal approximation produces intervals that
# cross 1, which is not a possible value for a proportion.

# Point estimate plus its Wilson interval.
#
# Deliberately NOT a second definition of wilson_ci(): that already exists in
# fct_descriptives.R, and R/ is sourced alphabetically, so redefining it here
# silently replaced it everywhere - the two returned different shapes and the
# descriptives tests broke. One statistic, one definition; this only adds the
# point estimate alongside it.
#
# Wilson rather than Wald throughout the diagnostic module because diagnostic
# work lives at the boundaries: at 20/20 a Wald interval is [1, 1], zero width,
# claiming certainty from twenty observations.
.prop_est_ci <- function(x, n, conf = 0.95) {
  if (!is.finite(x) || !is.finite(n) || n <= 0)
    return(c(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  ci <- wilson_ci(x, n, conf)
  c(estimate = x / n, lower = ci[[1]], upper = ci[[2]])
}

#' Accuracy of a binary test against a binary reference standard.
#'
#' @param test       The test being evaluated.
#' @param reference  The reference ("gold") standard.
#' @param positive   Value counted as positive. When NULL, the level that sorts
#'   LAST is used - matching how the app treats a binary outcome elsewhere - and
#'   the choice is reported so it is never silently wrong.
#' @param conf       Confidence level.
#' @return A list with the 2x2 counts, and sensitivity, specificity, predictive
#'   values, likelihood ratios, accuracy, Youden's J and observed prevalence.
diagnostic_accuracy <- function(test, reference, positive = NULL, conf = 0.95) {
  keep <- !is.na(test) & !is.na(reference)
  n_dropped <- sum(!keep)
  test <- test[keep]; reference <- reference[keep]
  if (!length(test))
    stop("Diagnostic accuracy needs at least one case with both the test and the reference recorded.",
         call. = FALSE)
  # OBSERVED values, not declared levels: a factor may carry levels that never
  # occur in this extract (a subset filter, or an imported codebook listing every
  # possible answer), and refusing the analysis over a level with no rows behind
  # it would be wrong.
  lv <- union(levels(droplevels(as.factor(test))),
              levels(droplevels(as.factor(reference))))
  if (length(lv) != 2L)
    stop(sprintf("A diagnostic evaluation needs exactly two values across the test and the reference; %d were found (%s). Recode to positive/negative first.",
                 length(lv), paste(utils::head(lv, 5), collapse = ", ")),
         call. = FALSE)
  pos <- positive %||% lv[[2]]
  if (!pos %in% lv)
    stop(sprintf("'%s' is not one of the observed values (%s).", pos,
                 paste(lv, collapse = ", ")), call. = FALSE)
  neg <- setdiff(lv, pos)

  tp <- sum(test == pos & reference == pos)
  fp <- sum(test == pos & reference == neg)
  fn <- sum(test == neg & reference == pos)
  tn <- sum(test == neg & reference == neg)
  n <- tp + fp + fn + tn

  sens <- .prop_est_ci(tp, tp + fn, conf)
  spec <- .prop_est_ci(tn, tn + fp, conf)
  ppv  <- .prop_est_ci(tp, tp + fp, conf)
  npv  <- .prop_est_ci(tn, tn + fn, conf)
  acc  <- .prop_est_ci(tp + tn, n, conf)
  prev <- .prop_est_ci(tp + fn, n, conf)

  # Likelihood ratios with the standard log-scale interval. Infinite when a cell
  # is empty, which is reported rather than hidden: LR+ of Inf means no false
  # positives were OBSERVED, not that none can occur.
  lr <- function(num, den, a, b, c_, d_) {
    if (!is.finite(num) || !is.finite(den) || den == 0)
      return(c(estimate = if (is.finite(num) && num > 0) Inf else NA_real_,
               lower = NA_real_, upper = NA_real_))
    est <- num / den
    se <- sqrt(1 / a - 1 / b + 1 / c_ - 1 / d_)
    z <- stats::qnorm(1 - (1 - conf) / 2)
    if (!is.finite(se)) return(c(estimate = est, lower = NA_real_, upper = NA_real_))
    c(estimate = est, lower = est * exp(-z * se), upper = est * exp(z * se))
  }
  lr_pos <- lr(sens[["estimate"]], 1 - spec[["estimate"]], tp, tp + fn, fp, fp + tn)
  lr_neg <- lr(1 - sens[["estimate"]], spec[["estimate"]], fn, tp + fn, tn, fp + tn)

  list(
    table = matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE,
                   dimnames = list(`Test` = c(paste("Positive:", pos),
                                              paste("Negative:", neg)),
                                   `Reference` = c(paste("Positive:", pos),
                                                   paste("Negative:", neg)))),
    n = as.integer(n), n_dropped = as.integer(n_dropped),
    positive = pos, negative = neg, conf = conf,
    tp = tp, fp = fp, fn = fn, tn = tn,
    sensitivity = sens, specificity = spec,
    ppv = ppv, npv = npv, accuracy = acc, prevalence = prev,
    lr_positive = lr_pos, lr_negative = lr_neg,
    youden = sens[["estimate"]] + spec[["estimate"]] - 1)
}

#' Predictive values at a prevalence other than the one observed.
#'
#' PPV and NPV are not properties of a test - they depend on how common the
#' condition is where it is used. A test validated in a referral clinic and
#' deployed for community screening has the same sensitivity and specificity and
#' a very different PPV, which is the single most common misreading of a
#' diagnostic evaluation. Bayes' theorem, applied explicitly.
predictive_values_at <- function(sensitivity, specificity, prevalence) {
  if (!all(is.finite(c(sensitivity, specificity, prevalence))))
    return(c(ppv = NA_real_, npv = NA_real_))
  if (prevalence < 0 || prevalence > 1)
    stop("Prevalence must be between 0 and 1.", call. = FALSE)
  tp <- sensitivity * prevalence
  fp <- (1 - specificity) * (1 - prevalence)
  tn <- specificity * (1 - prevalence)
  fn <- (1 - sensitivity) * prevalence
  c(ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA_real_,
    npv = if ((tn + fn) > 0) tn / (tn + fn) else NA_real_)
}

#' ROC curve and area under it for a continuous score against a binary reference.
#'
#' The AUC is computed as the Mann-Whitney statistic - the probability that a
#' randomly chosen positive scores higher than a randomly chosen negative - which
#' is exact and identical to the trapezoidal area, with ties counted as half.
#' The standard error is Hanley & McNeil's.
#'
#' @param score      Continuous test score (higher = more likely positive, unless
#'                   `direction` says otherwise).
#' @param reference  Binary reference standard.
#' @param positive   Reference value counted as positive.
#' @param direction  ">" (default) when a higher score indicates the condition,
#'                   "<" when lower does.
roc_curve <- function(score, reference, positive = NULL, direction = ">",
                      conf = 0.95) {
  keep <- !is.na(score) & !is.na(reference)
  n_dropped <- sum(!keep)
  score <- suppressWarnings(as.numeric(score[keep])); reference <- reference[keep]
  keep2 <- is.finite(score)
  n_dropped <- n_dropped + sum(!keep2)
  score <- score[keep2]; reference <- reference[keep2]
  lv <- levels(droplevels(as.factor(reference)))
  if (length(lv) != 2L)
    stop(sprintf("An ROC curve needs a reference with exactly two observed values; %d %s found (%s).",
                 length(lv), if (length(lv) == 1L) "was" else "were",
                 paste(utils::head(lv, 5), collapse = ", ")), call. = FALSE)
  pos <- positive %||% lv[[2]]
  if (!pos %in% lv)
    stop(sprintf("'%s' is not one of the reference values (%s).", pos,
                 paste(lv, collapse = ", ")), call. = FALSE)
  if (!direction %in% c(">", "<"))
    stop("direction must be \">\" (higher score = positive) or \"<\".", call. = FALSE)
  s <- if (identical(direction, "<")) -score else score
  x_pos <- s[reference == pos]
  x_neg <- s[reference != pos]
  if (!length(x_pos) || !length(x_neg))
    stop("An ROC curve needs at least one positive and one negative case.",
         call. = FALSE)
  n1 <- length(x_pos); n0 <- length(x_neg)

  # AUC as the Mann-Whitney U statistic, ties counted half - exact, and equal to
  # the trapezoidal area under the empirical curve.
  r <- rank(c(x_pos, x_neg))
  auc <- (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * n0)

  # Hanley & McNeil (1982) standard error.
  q1 <- auc / (2 - auc)
  q2 <- 2 * auc^2 / (1 + auc)
  se <- sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) +
                (n0 - 1) * (q2 - auc^2)) / (n1 * n0))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  ci <- c(max(0, auc - z * se), min(1, auc + z * se))

  # The empirical curve, one point per distinct threshold.
  thr <- sort(unique(s), decreasing = TRUE)
  sens <- vapply(thr, function(t) mean(x_pos >= t), numeric(1))
  spec <- vapply(thr, function(t) mean(x_neg < t), numeric(1))
  youden <- sens + spec - 1
  best <- which.max(youden)
  curve <- data.frame(
    threshold = if (identical(direction, "<")) -thr else thr,
    sensitivity = sens, specificity = spec, youden = youden,
    stringsAsFactors = FALSE, row.names = NULL)

  list(curve = curve, auc = auc, se = se, ci = ci, conf = conf,
       n_positive = n1, n_negative = n0, n_dropped = as.integer(n_dropped),
       positive = pos, direction = direction,
       best_threshold = curve$threshold[best],
       best_sensitivity = sens[best], best_specificity = spec[best],
       best_youden = youden[best])
}

#' Diagnostic accuracy as a guided-test result, in the app's standard contract.
run_diagnostic <- function(test, reference, positive = NULL, alpha = 0.05,
                           prevalences = NULL) {
  a <- diagnostic_accuracy(test, reference, positive = positive,
                           conf = 1 - alpha)
  fmt <- function(v) if (!is.finite(v[["estimate"]])) "not estimable" else
    sprintf("%.3f (%.3f to %.3f)", v[["estimate"]], v[["lower"]], v[["upper"]])
  stats_named <- c(
    "n with both recorded" = a$n,
    "true positives" = a$tp, "false positives" = a$fp,
    "false negatives" = a$fn, "true negatives" = a$tn,
    "sensitivity" = round(a$sensitivity[["estimate"]], 4),
    "specificity" = round(a$specificity[["estimate"]], 4),
    "positive predictive value" = round(a$ppv[["estimate"]], 4),
    "negative predictive value" = round(a$npv[["estimate"]], 4),
    "likelihood ratio +" = round(a$lr_positive[["estimate"]], 3),
    "likelihood ratio -" = round(a$lr_negative[["estimate"]], 3),
    "Youden's J" = round(a$youden, 4),
    "observed prevalence" = round(a$prevalence[["estimate"]], 4))

  interp <- sprintf(
    "Against the reference, sensitivity was %s and specificity %s. In this sample, where %s of cases were truly positive, a positive result had a %s chance of being correct (PPV) and a negative result %s (NPV).",
    fmt(a$sensitivity), fmt(a$specificity),
    sprintf("%.1f%%", 100 * a$prevalence[["estimate"]]),
    fmt(a$ppv), fmt(a$npv))
  interp <- paste(interp,
    "Sensitivity and specificity are properties of the test; the predictive values are not - they depend on how common the condition is where the test is used, so they do not transfer to a population with a different prevalence.")
  if (min(a$tp, a$fp, a$fn, a$tn) == 0)
    interp <- paste(interp,
      "At least one cell of the 2x2 table is empty, so some measures are not estimable and the intervals on the rest are wide - an observed rate of 0 or 1 is not evidence that the true rate is 0 or 1.")
  if (a$n_dropped > 0)
    interp <- paste(interp, sprintf(
      "%d case%s missing the test or the reference and were excluded.",
      a$n_dropped, if (a$n_dropped == 1L) " was" else "s were"))

  primary <- mk_result("Diagnostic accuracy", stats_named, p = NA_real_,
    effect = list("Youden's J" = round(a$youden, 4)),
    interpretation = interp,
    note = sprintf("Positive = '%s'. Proportions use Wilson score intervals, which stay within 0 and 1 near the boundaries where diagnostic measures usually sit.",
                   a$positive))

  # What the test would do elsewhere. This is the guard against the commonest
  # misreading: a PPV quoted from a high-prevalence validation sample being
  # taken as the PPV in a screening programme.
  pv_tab <- NULL
  prevs <- prevalences %||% c(0.001, 0.01, 0.05, 0.10, 0.25, 0.50)
  if (is.finite(a$sensitivity[["estimate"]]) &&
      is.finite(a$specificity[["estimate"]])) {
    pv <- t(vapply(prevs, function(p) predictive_values_at(
      a$sensitivity[["estimate"]], a$specificity[["estimate"]], p), numeric(2)))
    pv_tab <- data.frame(
      `Prevalence` = sprintf("%.1f%%", 100 * prevs),
      `PPV` = round(pv[, "ppv"], 4), `NPV` = round(pv[, "npv"], 4),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  }

  list(primary = primary, alternative = NULL, recommended = "primary",
       assumptions = list(),
       predictive_values = pv_tab,
       rationale = "Sensitivity and specificity describe the test; predictive values describe the test in a population. Both are reported, with the predictive values recomputed across a range of prevalences so the result can be read for the setting it will be used in.")
}
