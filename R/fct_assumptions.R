# fct_assumptions.R ---------------------------------------------------------
# The assumption / distribution checks that reroute recommendations
# (Section 7 of the design doc). Each function returns a small list with a
# machine-readable verdict plus a plain-language note for the UI. Nothing here
# silently changes an analysis - the checks only inform recommendations.

# Format a p-value for display.
p_fmt <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")
}

#' Normality check for a numeric vector.
#'
#' Uses Shapiro-Wilk for 3 <= n <= 5000. For larger n it uses Anderson-Darling
#' when {nortest} is available, otherwise it declines to test and notes that
#' means-based methods are robust at large n (central limit theorem).
#' @return list(ok, test, p, note)
check_normality <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3)
    return(list(ok = NA, test = "none", p = NA_real_,
                note = "Too few values to test normality."))

  if (n <= 5000) {
    p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    test <- "Shapiro-Wilk"
  } else if (requireNamespace("nortest", quietly = TRUE)) {
    p <- tryCatch(nortest::ad.test(x)$p.value, error = function(e) NA_real_)
    test <- "Anderson-Darling"
  } else {
    return(list(ok = TRUE, test = "none", p = NA_real_,
                note = sprintf("n = %d is large; not formally tested. Means-based tests are robust to non-normality at this size (CLT).", n)))
  }

  ok <- !is.na(p) && p >= 0.05
  note <- if (is.na(p)) "Normality could not be tested."
    else if (ok) sprintf("%s p = %s: no strong evidence against normality.", test, p_fmt(p))
    else sprintf("%s p = %s: evidence of non-normality%s.", test, p_fmt(p),
                 if (n >= 30) " (but n >= 30, so means-based tests remain fairly robust)" else "")
  list(ok = ok, test = test, p = p, note = note)
}

#' Normality within each group; overall verdict is the AND across groups.
check_normality_by_group <- function(x, g) {
  g <- as.factor(g)
  res <- lapply(split(x, g), check_normality)
  oks <- vapply(res, function(r) isTRUE(r$ok), logical(1))
  list(ok = all(oks), by_group = res,
       note = if (all(oks)) "Each group is approximately normal."
              else "At least one group departs from normality.")
}

#' Homogeneity of variance: Levene's test (Brown-Forsythe form, centred on the
#' median so it is robust). @return list(ok, test, p, note)
check_variance_homogeneity <- function(x, g) {
  d <- stats::na.omit(data.frame(x = x, g = as.factor(g)))
  if (nlevels(droplevels(d$g)) < 2)
    return(list(ok = NA, test = "Levene", p = NA_real_,
                note = "Need at least two groups."))
  med <- tapply(d$x, d$g, stats::median)
  z <- abs(d$x - med[as.character(d$g)])
  p <- tryCatch(stats::anova(stats::lm(z ~ d$g))$`Pr(>F)`[1],
                error = function(e) NA_real_)
  ok <- !is.na(p) && p >= 0.05
  note <- if (is.na(p)) "Variance homogeneity could not be tested."
    else if (ok) sprintf("Levene p = %s: variances look equal across groups.", p_fmt(p))
    else sprintf("Levene p = %s: variances differ across groups (prefer Welch).", p_fmt(p))
  list(ok = ok, test = "Levene (median-centred)", p = p, note = note)
}

#' Expected-cell-count rule for chi-square vs an exact test.
#' Flags when any expected count < 1, or < 5 in more than 20% of cells.
check_expected_counts <- function(tab) {
  suppressWarnings(exp_counts <- stats::chisq.test(tab)$expected)
  prop_low <- mean(exp_counts < 5)
  any_tiny <- any(exp_counts < 1)
  ok <- !(any_tiny || prop_low > 0.20)
  note <- if (ok)
      "Expected cell counts are adequate for a chi-square test."
    else sprintf("%.0f%% of cells have expected count < 5%s - an exact test is safer.",
                 100 * prop_low, if (any_tiny) " (some < 1)" else "")
  list(ok = ok, min_expected = min(exp_counts), prop_low = prop_low, note = note)
}

#' Check multicollinearity in a fitted model matrix.
#'
#' This is a dependency-free VIF check.  It works on the actual columns passed
#' to the model (including factor contrasts), so the warning is about the fit
#' the analyst is looking at rather than a separate pre-processing guess.
#' A VIF of 5 is treated as a review threshold and 10 as severe, following the
#' conventional diagnostic guidance used by the UI.
check_multicollinearity <- function(fit, threshold = 5) {
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mm))
    return(list(ok = NA, test = "VIF", max_vif = NA_real_, by_term = numeric(0),
                note = "Multicollinearity could not be evaluated for this model."))

  keep <- !grepl("^\\(Intercept\\)$", colnames(mm))
  mm <- mm[, keep, drop = FALSE]
  if (ncol(mm) < 2L)
    return(list(ok = NA, test = "VIF", max_vif = NA_real_, by_term = setNames(1, colnames(mm)),
                note = "Fewer than two predictor terms were available; a VIF comparison is not informative."))

  vif <- vapply(seq_len(ncol(mm)), function(i) {
    other <- mm[, -i, drop = FALSE]
    fit_i <- tryCatch(stats::lm.fit(cbind(1, other), mm[, i]),
                      error = function(e) NULL)
    if (is.null(fit_i)) return(Inf)
    rss <- sum(fit_i$residuals^2)
    centred <- mm[, i] - mean(mm[, i])
    tss <- sum(centred^2)
    if (!is.finite(tss) || tss <= 0) return(Inf)
    r2 <- max(0, min(1, 1 - rss / tss))
    if (r2 >= 1 - .Machine$double.eps) Inf else 1 / (1 - r2)
  }, numeric(1))
  names(vif) <- colnames(mm)
  max_vif <- max(vif, na.rm = TRUE)
  ok <- is.finite(max_vif) && max_vif < threshold
  note <- if (!is.finite(max_vif) || max_vif >= 10) {
    sprintf("Severe multicollinearity detected (maximum VIF = %s); coefficient estimates may be unstable.",
            if (is.finite(max_vif)) formatC(max_vif, digits = 2, format = "f") else "Inf")
  } else if (max_vif >= threshold) {
    sprintf("Multicollinearity needs review (maximum VIF = %.2f; values at or above %.0f are commonly flagged).",
            max_vif, threshold)
  } else {
    sprintf("No concerning multicollinearity was detected (maximum VIF = %.2f).", max_vif)
  }
  list(ok = ok, test = "Variance inflation factor", max_vif = max_vif,
       by_term = vif, note = note)
}
