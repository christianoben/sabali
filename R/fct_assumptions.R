# fct_assumptions.R ---------------------------------------------------------
# The assumption / distribution checks that reroute recommendations
# (Section 7 of the design doc). Nothing here silently changes an analysis -
# the checks only inform recommendations.
#
# ORGANISATION. Every check returns the SAME structured shape, built by
# assumption_result(): an id, a graded verdict, the statistic and p-value behind
# it, a plain-language note, and a remedy naming what to do next. That structure
# is the point. It lets the app count violations, colour them, group them, and
# link a failure to the transformation or the alternative test that fixes it -
# none of which is possible when a check returns only a sentence.
#
# The catalogue below is the single source of truth for what can be checked,
# exactly as planner_test_catalogue() is for tests and transform_catalogue() is
# for transformations. Every check's id appears there with the analyses it
# applies to and the remedies it can suggest.
#
# VERDICTS ARE GRADED, NOT PASS/FAIL:
#   ok        nothing to act on
#   review    tolerable, but say so in the write-up
#   violated  the inference is not trustworthy as it stands
#   untested  could not be evaluated (too little data, missing dependency)
#
# MAGNITUDE, NOT SIGNIFICANCE. Where a formal test exists it is reported, but
# the verdict is driven by EFFECT SIZE. A significance test for normality is the
# wrong instrument at scale: with n = 750, Shapiro-Wilk rejects log10 viral load
# (p < 2e-16) even though the transformation cut its skew from 6.36 to 0.75 and
# did exactly what was wanted. Judging on p would tell the analyst a successful
# transformation had failed.

# Format a p-value for display.
p_fmt <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")
}

# --- vocabulary -------------------------------------------------------------

#' Verdicts, ordered from least to most serious.
ASSUMPTION_SEVERITY <- c("ok", "review", "violated", "untested")

ASSUMPTION_SEVERITY_LABELS <- c(
  ok       = "Satisfied",
  review   = "Worth reviewing",
  violated = "Violated",
  untested = "Not tested")

ASSUMPTION_FAMILY_LABELS <- c(
  distribution = "Distribution of the values or residuals",
  variance     = "Equality and constancy of variance",
  structure    = "Independence and correlation structure",
  form         = "Shape of the relationship",
  counts       = "Cell counts and count-model fit",
  influence    = "Outliers and influential observations",
  collinearity = "Redundancy among predictors",
  power        = "Whether there is enough data")

# Rank a verdict so two can be combined by taking the worse.
.as_rank <- function(s) match(s, c("ok", "review", "violated"), nomatch = 0L)

#' The worse of two verdicts ("untested" loses to any real verdict).
assumption_worse <- function(a, b) {
  ra <- .as_rank(a); rb <- .as_rank(b)
  if (ra == 0L && rb == 0L) return("untested")
  if (ra == 0L) return(b)
  if (rb == 0L) return(a)
  if (ra >= rb) a else b
}

# --- catalogue --------------------------------------------------------------

#' Every assumption the app can check.
#'
#' `applies_to` names the analyses the check is relevant to, using the same
#' vocabulary as the planner's catalogues, so the two layers can be joined.
#' `remedies` is the actionable part: each entry names a fix and, where the app
#' can perform it, the transformation or method id that does so.
assumption_catalogue <- function() {
  a <- function(id, name, family, statistic, test = "", applies_to = character(0),
                remedies = list(), note = "")
    list(id = id, name = name, family = family, statistic = statistic,
         test = test, applies_to = applies_to, remedies = remedies, note = note)

  list(
    a("normality", "Normality of the values", "distribution",
      statistic = "skewness and excess kurtosis", test = "Shapiro-Wilk / Anderson-Darling",
      applies_to = c("one_sample_t", "paired_t", "pearson"),
      remedies = list(
        list(what = "Log-transform a right-skewed variable", transform = "log10"),
        list(what = "Let Box-Cox choose the power", transform = "boxcox"),
        list(what = "Use Yeo-Johnson if there are zeros or negatives", transform = "yeojohnson"),
        list(what = "Switch to the rank-based equivalent", test = "wilcoxon")),
      note = "Means-based methods are robust at moderate n; magnitude matters more than the p-value."),

    a("normality_groups", "Normality within each group", "distribution",
      statistic = "skewness and excess kurtosis per group",
      test = "Shapiro-Wilk / Anderson-Darling",
      applies_to = c("welch_t", "one_way_anova", "factorial_anova", "ancova"),
      remedies = list(
        list(what = "Log-transform the outcome", transform = "log10"),
        list(what = "Use the non-parametric comparison", test = "mann_whitney"),
        list(what = "Use the non-parametric multi-group comparison", test = "kruskal"))),

    a("normality_residuals", "Normality of the residuals", "distribution",
      statistic = "skewness and excess kurtosis of residuals",
      test = "Shapiro-Wilk / Anderson-Darling",
      applies_to = c("ols", "lmm", "gee_gaussian"),
      remedies = list(
        list(what = "Transform the outcome", transform = "log10"),
        list(what = "Fit a robust regression instead", method = "robust"),
        list(what = "Fit a quantile (median) regression instead", method = "quantile"))),

    a("variance_homogeneity", "Equal variance across groups", "variance",
      statistic = "ratio of group variances", test = "Levene (median-centred)",
      applies_to = c("one_way_anova", "factorial_anova", "ancova"),
      remedies = list(
        list(what = "Use Welch's test, which does not assume equal variance", test = "welch_t"),
        list(what = "Log-transform to stabilise the variance", transform = "log10"))),

    a("homoscedasticity", "Constant residual variance", "variance",
      statistic = "Breusch-Pagan statistic", test = "Breusch-Pagan",
      applies_to = c("ols"),
      remedies = list(
        list(what = "Fit a robust regression", method = "robust"),
        list(what = "Log-transform the outcome", transform = "log10"),
        list(what = "Fit a quantile regression", method = "quantile"))),

    a("linearity", "Linear relationship with the outcome", "form",
      statistic = "extra variance explained by curvature",
      test = "F test on quadratic and cubic terms",
      applies_to = c("ols", "pearson", "ancova", "lmm"),
      remedies = list(
        list(what = "Transform the predictor", transform = "log10"),
        list(what = "Bin the predictor into groups", transform = "bin_quantile"),
        list(what = "Use a monotonic (rank) correlation instead", test = "spearman")),
      note = "A curved relationship makes a single slope misleading even when every other assumption holds."),

    a("independence", "Independent observations", "structure",
      statistic = "lag-1 autocorrelation of residuals", test = "Durbin-Watson",
      applies_to = c("ols", "pearson", "one_way_anova"),
      remedies = list(
        list(what = "Fit a mixed model with a subject random effect", method = "lmm"),
        list(what = "Fit a GEE with a working correlation", method = "gee_gaussian"),
        list(what = "Use a time-series regression", method = "ts_regression")),
      note = "Autocorrelation makes standard errors too small and p-values too optimistic."),

    a("sphericity", "Equal variance of the within-subject differences", "structure",
      statistic = "Greenhouse-Geisser epsilon", test = "Mauchly",
      applies_to = c("rm_anova", "mixed_anova"),
      remedies = list(
        list(what = "Apply the Greenhouse-Geisser correction to the degrees of freedom"),
        list(what = "Use the non-parametric repeated-measures test", test = "friedman"),
        list(what = "Fit a mixed model, which does not require sphericity", method = "lmm")),
      note = "Only meaningful with three or more repeated measures; with two it holds automatically."),

    a("expected_counts", "Adequate expected cell counts", "counts",
      statistic = "proportion of cells with expected count below 5",
      test = "chi-square expected counts",
      applies_to = c("chi_square"),
      remedies = list(
        list(what = "Use Fisher's exact test", test = "fisher_exact"),
        list(what = "Group the rare categories together", transform = "collapse_rare"))),

    a("overdispersion", "Count variance matches the mean", "counts",
      statistic = "Pearson dispersion", test = "",
      applies_to = c("poisson", "gee_poisson", "glmm_poisson"),
      remedies = list(
        list(what = "Fit a negative-binomial model", method = "negative_binomial"))),

    a("zero_inflation", "No excess zeros", "counts",
      statistic = "observed zeros divided by expected zeros", test = "",
      applies_to = c("poisson", "negative_binomial", "gee_poisson", "glmm_poisson"),
      remedies = list(
        list(what = "Fit a negative-binomial model, which tolerates more zeros",
             method = "negative_binomial"),
        list(what = "Consider a zero-inflated or hurdle model (outside this app)"),
        list(what = "Check whether a zero means 'none' or 'not recorded'",
             transform = "missing_indicator")),
      note = "More zeros than the model expects usually means two processes are at work."),

    a("influence", "No single observation dominates", "influence",
      statistic = "Cook's distance above 4/n", test = "",
      applies_to = c("ols", "logistic", "poisson", "robust"),
      remedies = list(
        list(what = "Winsorise the extreme values", transform = "winsorise"),
        list(what = "Cap values beyond 3 MADs", transform = "cap_mad"),
        list(what = "Fit a robust regression", method = "robust"))),

    a("multicollinearity", "Predictors are not redundant", "collinearity",
      statistic = "variance inflation factor", test = "",
      applies_to = c("ols", "logistic", "poisson", "lmm", "cox_ph"),
      remedies = list(
        list(what = "Drop one of the near-duplicate predictors"),
        list(what = "Use ridge regression, which tolerates collinearity", method = "ridge"),
        list(what = "Combine the related predictors into one score", transform = "mean_columns"))),

    a("proportional_hazards", "Hazard ratios constant over time", "structure",
      statistic = "Schoenfeld residual correlation with time", test = "cox.zph",
      applies_to = c("cox_ph"),
      remedies = list(
        list(what = "Stratify the model on the offending variable"),
        list(what = "Add a time-varying coefficient"),
        list(what = "Report Kaplan-Meier curves by group instead", method = "kaplan_meier"))),

    a("events_per_variable", "Enough events for the model size", "power",
      statistic = "events per coefficient", test = "",
      applies_to = c("cox_ph", "logistic", "glmm_binomial", "gee_binomial"),
      remedies = list(
        list(what = "Reduce the number of predictors"),
        list(what = "Use a penalised model", method = "lasso")),
      note = "Fewer than ten events per coefficient makes estimates unstable and intervals unreliable."),

    a("singular_fit", "Random-effects structure is supportable", "structure",
      statistic = "singular-fit indicator", test = "",
      applies_to = c("lmm", "glmm_binomial", "glmm_poisson"),
      remedies = list(
        list(what = "Simplify to a random intercept only"),
        list(what = "Fit a GEE instead", method = "gee_gaussian")))
  )
}

#' The catalogue entry for a check id, or NULL.
assumption_check <- function(id) {
  hit <- Filter(function(a) identical(a$id, id), assumption_catalogue())
  if (length(hit)) hit[[1]] else NULL
}

#' Check ids, optionally those relevant to a given analysis id.
assumption_ids <- function(applies_to = NULL) {
  cat <- assumption_catalogue()
  if (!is.null(applies_to))
    cat <- Filter(function(a) any(applies_to %in% a$applies_to), cat)
  vapply(cat, function(a) a$id, character(1))
}

#' Remedies for a check, as short actionable lines. Only returned when there is
#' something to act on, so a satisfied assumption stays quiet.
assumption_remedies <- function(id, severity = "violated") {
  if (identical(severity, "ok") || identical(severity, "untested")) return(list())
  a <- assumption_check(id)
  if (is.null(a)) list() else a$remedies
}

# --- the standard result shape ----------------------------------------------

#' Build a check result. Every check in this file returns one of these.
#'
#' `ok` is kept for the callers that only want a yes/no (TRUE when the verdict
#' is "ok", NA when untested), so the graded verdict is an addition rather than
#' a breaking change. `note` is kept because every existing renderer reads it.
assumption_result <- function(id, severity, note, statistic = NA_real_,
                              p = NA_real_, test = "", detail = list(),
                              scope = "") {
  a <- assumption_check(id)
  list(check = id,
       label = if (is.null(a)) id else a$name,
       family = if (is.null(a)) "distribution" else a$family,
       severity = severity,
       ok = if (identical(severity, "untested")) NA else identical(severity, "ok"),
       statistic = statistic,
       p = p,
       test = test,
       note = note,
       # Which variables this verdict is about. A readiness screen runs the same
       # check several times over different variables, so the result has to say
       # which one it means.
       scope = scope,
       remedy = assumption_remedies(id, severity),
       detail = detail)
}

# Attach a scope to an existing result (the checks themselves do not know which
# variables the caller handed them).
.as_scoped <- function(res, scope) { res$scope <- scope; res }

# --- magnitude helpers ------------------------------------------------------

# Sample skewness (g1) and excess kurtosis (g2). Method-of-moments versions,
# which is what the practical thresholds below were calibrated on.
.as_skewness <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x); s2 <- sum((x - m)^2) / n
  if (s2 <= 0) return(NA_real_)
  (sum((x - m)^3) / n) / s2^1.5
}
.as_kurtosis <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x); s2 <- sum((x - m)^2) / n
  if (s2 <= 0) return(NA_real_)
  (sum((x - m)^4) / n) / s2^2 - 3
}

# Thresholds. |skew| <= 1 with |excess kurtosis| <= 2 is comfortable for
# means-based inference; the wider band (|skew| <= 2, |excess kurtosis| <= 7)
# is the range commonly treated as tolerable in the applied literature, beyond
# which a transformation or a rank method is the honest choice.
ASSUMPTION_SKEW_OK      <- 1
ASSUMPTION_SKEW_REVIEW  <- 2
ASSUMPTION_KURT_OK      <- 2
ASSUMPTION_KURT_REVIEW  <- 7

.as_shape_verdict <- function(skew, kurt) {
  if (!is.finite(skew)) return("untested")
  k <- if (is.finite(kurt)) abs(kurt) else 0
  if (abs(skew) <= ASSUMPTION_SKEW_OK && k <= ASSUMPTION_KURT_OK) "ok"
  else if (abs(skew) <= ASSUMPTION_SKEW_REVIEW && k <= ASSUMPTION_KURT_REVIEW) "review"
  else "violated"
}

# --- shapes the moments cannot see -----------------------------------------
# Skewness and kurtosis describe a distribution's tails, not the number of
# humps in it, and they cannot see a spike of repeated values. The calibration
# study (tools/calibration_study.R) measured both blind spots directly, so both
# are now detected separately.

#' Sarle's bimodality coefficient. Above about 0.555 - the value for a uniform
#' distribution - the data is more consistent with two modes than one.
#'
#' Why this is needed: a symmetric two-mode mixture has skewness near zero and
#' NEGATIVE excess kurtosis, so the moment rule reports it as normal. Measured
#' detection of bimodal data by moments alone was 0% at n >= 60. Real for this
#' app: viral load is genuinely bimodal (suppressed versus failing).
.as_bimodality <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  # Below 30 observations the coefficient is too noisy to act on: it fired on
  # 2-3.5% of genuinely normal samples at n = 15-30 while catching little that
  # the small-sample Shapiro rule does not already catch.
  if (n < 30) return(NA_real_)
  g1 <- .as_skewness(x); g2 <- .as_kurtosis(x)
  if (!is.finite(g1) || !is.finite(g2)) return(NA_real_)
  (g1^2 + 1) / (g2 + 3 * (n - 1)^2 / ((n - 2) * (n - 3)))
}
# Calibrated, and NOT the textbook 0.555. Two measured findings drove this:
#
#  * Sarle's coefficient flags skewed UNIMODAL data 96% of the time, because
#    skewness enters the numerator squared. Used alone it would have labelled
#    nearly every skewed variable in a health dataset "bimodal" - badly wrong
#    advice. It is therefore only consulted for near-symmetric data
#    (|skewness| < 1); skew is already reported separately anyway.
#  * With that gate in place, 0.500 detects a 4-SD separation 100% of the time
#    and a 3-SD separation 31%, against 85% and 0% at 0.555, with an identical
#    false-positive profile (0% on normal, skewed, mildly skewed and
#    heavy-tailed data). The lower threshold strictly dominates.
ASSUMPTION_BIMODAL_THRESHOLD <- 0.500
ASSUMPTION_BIMODAL_MAX_SKEW  <- 1

#' The largest share of the sample sitting on one repeated value, and whether
#' that value is the minimum.
#'
#' A pile-up at the minimum is the signature of a left-censored assay: viral
#' load reported as "<50" or "undetectable" arrives as a spike at the limit.
#' The calibration study found this is the single most damaging shape in the
#' set - a one-sample t-test ran at 26-41% type I error against a nominal 5% -
#' and, worse, a log10 transformation makes the SKEW look fine while the point
#' mass remains, so the app would otherwise report the problem as solved.
.as_point_mass <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 10) return(list(share = NA_real_, at_minimum = FALSE, value = NA_real_))
  tb <- table(x)
  top <- which.max(tb)
  val <- as.numeric(names(tb)[top])
  list(share = unname(tb[top]) / n,
       at_minimum = isTRUE(all.equal(val, min(x))),
       value = val)
}
ASSUMPTION_POINT_MASS_THRESHOLD <- 0.10

# --- distribution -----------------------------------------------------------

#' Normality check for a numeric vector.
#'
#' The verdict comes from the SHAPE of the distribution (skewness and excess
#' kurtosis), not from a significance test - see the header note. Shapiro-Wilk
#' (or Anderson-Darling above n = 5000, when {nortest} is installed) is still
#' computed and reported, because analysts expect to see it and because at small
#' n it carries information the moments do not: below n = 30 there is no
#' central-limit protection, so a rejecting test can only make the verdict
#' worse, never better.
#'
#' @return an assumption_result(); `ok`, `test`, `p` and `note` are retained for
#'   existing callers.
check_normality <- function(x, id = "normality", what = "the values") {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    out <- assumption_result(id, "untested",
                             sprintf("Too few values to assess the shape of %s.", what))
    out$test <- "none"; return(out)
  }

  skew <- .as_skewness(x); kurt <- .as_kurtosis(x)
  verdict <- .as_shape_verdict(skew, kurt)

  if (n <= 5000) {
    p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    test <- "Shapiro-Wilk"
  } else if (requireNamespace("nortest", quietly = TRUE)) {
    p <- tryCatch(nortest::ad.test(x)$p.value, error = function(e) NA_real_)
    test <- "Anderson-Darling"
  } else {
    p <- NA_real_; test <- "none"
  }

  # Below n = 30 the central limit theorem is not doing any work for us, so a
  # rejecting formal test is allowed to worsen the verdict.
  if (n < 30 && !is.na(p) && p < 0.05)
    verdict <- assumption_worse(verdict, "review")

  # Two shapes the moments cannot see. Both were measured, not guessed.
  bc <- .as_bimodality(x)
  pm <- .as_point_mass(x)
  bimodal <- is.finite(bc) && bc > ASSUMPTION_BIMODAL_THRESHOLD &&
    is.finite(skew) && abs(skew) < ASSUMPTION_BIMODAL_MAX_SKEW
  censored <- is.finite(pm$share) && pm$share >= ASSUMPTION_POINT_MASS_THRESHOLD &&
    isTRUE(pm$at_minimum)
  extra <- ""
  if (censored) {
    # The damaging case: escalate, and say plainly that transforming will not
    # remove it, because the skew-based verdict alone would say it had.
    verdict <- "violated"
    extra <- sprintf(" %s of values sit at the lowest value (%s), which is the signature of a detection limit or a floor. Transforming will not remove that spike - model the censoring, or analyse above the limit.",
                     sprintf("%.0f%%", 100 * pm$share), format(pm$value, digits = 6))
  } else if (bimodal) {
    verdict <- assumption_worse(verdict, "review")
    extra <- " The distribution appears to have two peaks, so a single mean does not describe it well; check whether two subgroups are mixed together."
  } else if (is.finite(pm$share) && pm$share >= ASSUMPTION_POINT_MASS_THRESHOLD) {
    extra <- sprintf(" %s of values are identical (%s), so ranks and quantile groupings will be heavily tied.",
                     sprintf("%.0f%%", 100 * pm$share), format(pm$value, digits = 6))
  }

  shape <- sprintf("skewness %.2f, excess kurtosis %.2f", skew,
                   if (is.finite(kurt)) kurt else NA_real_)
  test_bit <- if (identical(test, "none")) ""
              else sprintf(" %s p = %s.", test, p_fmt(p))
  # Lead with "Shape of <what>:" rather than trying to conjugate a verb around a
  # caller-supplied phrase - `what` may be singular or plural.
  subject <- sprintf("Shape of %s", what)
  note <- switch(
    verdict,
    ok = sprintf("%s: close to normal (%s).%s", subject, shape, test_bit),
    review = if (bimodal && !censored)
      sprintf("%s: two peaks rather than one (%s).%s%s", subject, shape, extra, test_bit)
      else sprintf("%s: moderately skewed (%s) - usable for means-based methods at n = %d, but worth noting.%s%s",
                   subject, shape, n, extra, test_bit),
    violated = if (censored)
      sprintf("%s: censored at a limit (%s).%s%s", subject, shape, extra, test_bit)
      else sprintf("%s: departs markedly from normal (%s). Transform it, or use a rank-based method.%s%s",
                   subject, shape, extra, test_bit),
    sprintf("%s could not be assessed.", subject))
  if (identical(verdict, "ok") && nzchar(extra))
    note <- paste0(note, extra)

  out <- assumption_result(id, verdict, note, statistic = skew, p = p, test = test,
                           detail = list(n = n, skewness = skew, kurtosis = kurt,
                                         bimodality = bc, point_mass = pm$share,
                                         censored = censored, bimodal = bimodal))
  out
}

#' Normality within each group; the overall verdict is the worst group's.
check_normality_by_group <- function(x, g) {
  g <- as.factor(g)
  res <- lapply(split(x, g), check_normality, id = "normality_groups")
  if (!length(res))
    return(assumption_result("normality_groups", "untested", "No groups to assess."))
  worst <- Reduce(assumption_worse, vapply(res, function(r) r$severity, character(1)))
  bad <- names(res)[vapply(res, function(r) r$severity %in% c("review", "violated"), logical(1))]
  note <- switch(
    worst,
    ok = "Every group is close to normal in shape.",
    review = sprintf("Moderate skew in: %s. Means-based comparisons remain usable, but say so.",
                     paste(bad, collapse = ", ")),
    violated = sprintf("Marked departure from normality in: %s. Transform the outcome, or compare with a rank-based test.",
                       paste(bad, collapse = ", ")),
    "Group shapes could not be assessed.")
  out <- assumption_result("normality_groups", worst, note,
                           detail = list(by_group = res, flagged = bad))
  out$by_group <- res      # retained: mod_tests reads by_group
  out
}

# --- variance ---------------------------------------------------------------

#' Homogeneity of variance: Levene's test in the Brown-Forsythe form (centred
#' on the median, so it is robust). The verdict is driven by the RATIO of the
#' largest to smallest group variance, with Levene's p reported alongside:
#' a ratio under 2 is benign whatever the p-value at large n, and a ratio above
#' 4 matters even when the test does not reach significance at small n.
check_variance_homogeneity <- function(x, g) {
  d <- stats::na.omit(data.frame(x = x, g = as.factor(g)))
  if (nlevels(droplevels(d$g)) < 2) {
    out <- assumption_result("variance_homogeneity", "untested",
                             "At least two groups are needed to compare variances.")
    out$test <- "Levene"; return(out)
  }
  med <- tapply(d$x, d$g, stats::median)
  z <- abs(d$x - med[as.character(d$g)])
  p <- tryCatch(stats::anova(stats::lm(z ~ d$g))$`Pr(>F)`[1], error = function(e) NA_real_)

  vars <- tapply(d$x, droplevels(d$g), stats::var)
  vars <- vars[is.finite(vars) & vars > 0]
  ratio <- if (length(vars) >= 2) max(vars) / min(vars) else NA_real_

  verdict <- if (!is.finite(ratio)) "untested"
             else if (ratio <= 2) "ok"
             else if (ratio <= 4) "review"
             else "violated"
  pbit <- if (is.na(p)) "" else sprintf(" Levene p = %s.", p_fmt(p))
  note <- switch(
    verdict,
    ok = sprintf("Variances are similar across groups (largest / smallest = %.2f).%s", ratio, pbit),
    review = sprintf("Variances differ moderately (largest / smallest = %.2f); Welch's test is the safer comparison.%s",
                     ratio, pbit),
    violated = sprintf("Variances differ substantially (largest / smallest = %.2f); use Welch's test or transform the outcome.%s",
                       ratio, pbit),
    sprintf("Variance homogeneity could not be assessed.%s", pbit))
  assumption_result("variance_homogeneity", verdict, note, statistic = ratio, p = p,
                    test = "Levene (median-centred)",
                    detail = list(variance_ratio = ratio, variances = vars))
}

# --- counts -----------------------------------------------------------------

#' Expected-cell-count rule for chi-square vs an exact test.
#' Flags when any expected count is below 1, or below 5 in more than 20% of cells.
check_expected_counts <- function(tab) {
  exp_counts <- tryCatch({
    suppressWarnings(stats::chisq.test(tab)$expected)
  }, error = function(e) NULL)
  if (is.null(exp_counts))
    return(assumption_result("expected_counts", "untested",
                             "Expected cell counts could not be computed for this table."))
  prop_low <- mean(exp_counts < 5)
  any_tiny <- any(exp_counts < 1)
  verdict <- if (any_tiny) "violated"
             else if (prop_low > 0.20) "review"
             else "ok"
  note <- switch(
    verdict,
    ok = "Expected cell counts are adequate for a chi-square test.",
    review = sprintf("%.0f%% of cells have an expected count below 5; Fisher's exact test is safer.",
                     100 * prop_low),
    violated = sprintf("%.0f%% of cells have an expected count below 5 and some are below 1; use Fisher's exact test.",
                       100 * prop_low))
  out <- assumption_result("expected_counts", verdict, note, statistic = prop_low,
                           test = "chi-square expected counts",
                           detail = list(min_expected = min(exp_counts), prop_low = prop_low))
  out$min_expected <- min(exp_counts)   # retained for existing callers
  out$prop_low <- prop_low
  out
}

#' Excess zeros in a count model: compare the zeros actually observed with the
#' number the fitted model expects. A ratio well above 1 means the model cannot
#' account for the zeros, which usually signals two processes (never exposed vs
#' exposed-but-zero) rather than one.
#'
#' @param y      the observed counts
#' @param fitted fitted means (from the model); the observed mean is used if absent
#' @param theta  negative-binomial dispersion, when the model is NB
check_zero_inflation <- function(y, fitted = NULL, theta = NULL) {
  y <- y[is.finite(y)]
  if (!length(y) || any(y < 0))
    return(assumption_result("zero_inflation", "untested",
                             "Excess zeros can only be assessed for non-negative counts."))
  mu <- if (is.null(fitted) || !any(is.finite(fitted))) rep(mean(y), length(y))
        else fitted[is.finite(fitted)]
  observed <- sum(y == 0)
  expected <- if (!is.null(theta) && is.finite(theta))
    sum((theta / (theta + mu))^theta) else sum(stats::dpois(0, mu))

  if (!is.finite(expected) || expected < 1e-8) {
    verdict <- if (observed > 0) "violated" else "ok"
    note <- if (observed > 0)
      sprintf("There are %d zero counts, but the fitted model expects essentially none.", observed)
      else "There are no zero counts and none were expected."
    return(assumption_result("zero_inflation", verdict, note,
                             statistic = if (observed > 0) Inf else 1,
                             detail = list(observed = observed, expected = expected)))
  }

  ratio <- observed / expected
  # Calibrated, not assumed. A 1.25 cutoff flagged 17.5% of genuinely
  # Poisson data - the observed zero count is itself a random variable, and at
  # a small expected count its relative sampling error is large. 1.5 brings the
  # false-alarm rate under 10% while still catching a 10% share of structural
  # zeros essentially every time (see tools/calibration/zero_inflation.csv).
  verdict <- if (ratio <= 1.5) "ok" else if (ratio <= 2.5) "review" else "violated"
  note <- switch(
    verdict,
    ok = sprintf("Zero counts are in line with the model (%d observed, %.1f expected).",
                 observed, expected),
    review = sprintf("Slightly more zeros than expected (%d observed, %.1f expected, ratio %.2f); a negative-binomial model may fit better.",
                     observed, expected, ratio),
    violated = sprintf("Far more zeros than the model expects (%d observed, %.1f expected, ratio %.2f); consider a negative-binomial, zero-inflated or hurdle model.",
                       observed, expected, ratio))
  assumption_result("zero_inflation", verdict, note, statistic = ratio,
                    detail = list(observed = observed, expected = expected, ratio = ratio,
                                  model = if (is.null(theta)) "Poisson" else "negative binomial"))
}

# --- form -------------------------------------------------------------------

#' Is the relationship between a predictor and an outcome straight?
#'
#' Adds quadratic and cubic terms and asks how much extra variance they explain.
#' The verdict is that GAIN in R-squared (an effect size that does not grow with
#' n), with the F test reported alongside. A curved relationship makes a single
#' slope misleading however well every other assumption holds, and nothing else
#' in the app was checking for it.
check_linearity <- function(x, y) {
  d <- stats::na.omit(data.frame(x = suppressWarnings(as.numeric(x)),
                                 y = suppressWarnings(as.numeric(y))))
  if (nrow(d) < 15 || length(unique(d$x)) < 6)
    return(assumption_result("linearity", "untested",
                             "Too few distinct predictor values to judge whether the relationship is straight."))
  lin <- tryCatch(stats::lm(y ~ x, data = d), error = function(e) NULL)
  if (is.null(lin))
    return(assumption_result("linearity", "untested",
                             "The straight-line fit could not be evaluated."))
  # Cubic where there is room for it, quadratic otherwise.
  deg <- if (length(unique(d$x)) >= 10) 3 else 2
  cur <- tryCatch(stats::lm(y ~ stats::poly(x, deg), data = d), error = function(e) NULL)
  if (is.null(cur))
    return(assumption_result("linearity", "untested",
                             "The curved comparison fit could not be evaluated."))
  r2_lin <- summary(lin)$r.squared
  r2_cur <- summary(cur)$r.squared
  gain <- max(0, r2_cur - r2_lin)
  p <- tryCatch(stats::anova(lin, cur)$`Pr(>F)`[2], error = function(e) NA_real_)

  verdict <- if (gain <= 0.01) "ok" else if (gain <= 0.05) "review" else "violated"
  pbit <- if (is.na(p)) "" else sprintf(" F test p = %s.", p_fmt(p))
  note <- switch(
    verdict,
    ok = sprintf("The relationship looks straight; curvature adds only %.1f%% to the variance explained.%s",
                 100 * gain, pbit),
    review = sprintf("Mild curvature: allowing a bend adds %.1f%% to the variance explained.%s",
                     100 * gain, pbit),
    violated = sprintf("The relationship is not straight: allowing a bend adds %.1f%% to the variance explained. Transform the predictor, bin it, or use a rank-based measure.%s",
                       100 * gain, pbit))
  assumption_result("linearity", verdict, note, statistic = gain, p = p,
                    test = sprintf("F test on a degree-%d polynomial", deg),
                    detail = list(r2_linear = r2_lin, r2_curved = r2_cur, gain = gain))
}

# --- structure --------------------------------------------------------------

#' Are the residuals serially independent?
#'
#' Reports the Durbin-Watson statistic, and drives the verdict from the lag-1
#' autocorrelation, which is interpretable on its own scale. Autocorrelation
#' makes standard errors too small, so an unchecked violation shows up as
#' p-values that are optimistic rather than as an obviously wrong estimate -
#' which is precisely why it needs checking rather than eyeballing.
#'
#' @param e   residuals (or any series), in the order they were collected
#' @param fit optional fitted model, used for an exact Durbin-Watson p-value
#'   when {lmtest} is installed
check_autocorrelation <- function(e, fit = NULL) {
  e <- suppressWarnings(as.numeric(e))
  e <- e[is.finite(e)]
  n <- length(e)
  if (n < 10)
    return(assumption_result("independence", "untested",
                             "Too few observations in sequence to assess autocorrelation."))
  # A constant series has no variance to correlate; say so rather than letting
  # cor() emit a warning and hand back NA.
  if (stats::sd(e) == 0)
    return(assumption_result("independence", "untested",
                             "Every value in the series is identical, so autocorrelation is undefined."))
  dw <- sum(diff(e)^2) / sum((e - mean(e))^2)
  r1 <- suppressWarnings(stats::cor(utils::head(e, -1), utils::tail(e, -1)))
  p <- NA_real_
  if (!is.null(fit) && requireNamespace("lmtest", quietly = TRUE))
    p <- tryCatch(lmtest::dwtest(fit)$p.value, error = function(e) NA_real_)

  verdict <- if (!is.finite(r1)) "untested"
             else if (abs(r1) <= 0.2) "ok"
             else if (abs(r1) <= 0.4) "review"
             else "violated"
  pbit <- if (is.na(p)) "" else sprintf(" Durbin-Watson p = %s.", p_fmt(p))
  dir <- if (is.finite(r1) && r1 < 0) "negative" else "positive"
  note <- switch(
    verdict,
    ok = sprintf("Successive values look independent (lag-1 correlation %.2f, Durbin-Watson %.2f).%s",
                 r1, dw, pbit),
    review = sprintf("Mild %s autocorrelation (lag-1 correlation %.2f, Durbin-Watson %.2f); standard errors may be slightly optimistic.%s",
                     dir, r1, dw, pbit),
    violated = sprintf("Substantial %s autocorrelation (lag-1 correlation %.2f, Durbin-Watson %.2f). Standard errors are too small; use a mixed model, a GEE, or a time-series regression.%s",
                       dir, r1, dw, pbit),
    "Autocorrelation could not be assessed.")
  assumption_result("independence", verdict, note, statistic = r1, p = p,
                    test = "Durbin-Watson",
                    detail = list(durbin_watson = dw, lag1 = r1, n = n))
}

#' Sphericity for a repeated-measures design.
#'
#' The verdict is the Greenhouse-Geisser epsilon - the quantity that actually
#' determines how much the degrees of freedom need correcting - with Mauchly's
#' test reported alongside. Epsilon of 1 means sphericity holds exactly; the
#' further below 1, the more the uncorrected F test overstates significance.
#'
#' @param x         the numeric measurements, in long format
#' @param subject   subject identifier
#' @param condition the within-subject factor (visit, timepoint, condition)
check_sphericity <- function(x, subject, condition) {
  d <- stats::na.omit(data.frame(x = suppressWarnings(as.numeric(x)),
                                 s = as.character(subject),
                                 c = as.character(condition),
                                 stringsAsFactors = FALSE))
  if (!nrow(d))
    return(assumption_result("sphericity", "untested",
                             "No complete observations with which to assess sphericity."))
  conds <- sort(unique(d$c))
  k <- length(conds)
  if (k < 3)
    return(assumption_result("sphericity", "ok",
      sprintf("With %d repeated measure%s sphericity holds automatically; no correction is needed.",
              k, if (k == 1) "" else "s"),
      statistic = 1))

  # Long to wide, keeping only subjects measured in every condition.
  wide <- tryCatch({
    m <- tapply(d$x, list(d$s, d$c), function(z) z[1])
    m[, conds, drop = FALSE]
  }, error = function(e) NULL)
  if (is.null(wide))
    return(assumption_result("sphericity", "untested",
                             "The repeated measures could not be arranged for a sphericity check."))
  n_all <- nrow(wide)
  wide <- wide[stats::complete.cases(wide), , drop = FALSE]
  # Sphericity needs every subject measured in every condition, so incomplete
  # subjects are dropped. On a real cohort with attrition that can quietly
  # discard most of the sample: the calibration study measured 36% of subjects
  # retained at 40% attrition and 16% at 60%. Saying so is the difference
  # between a defensible epsilon and a confident answer from a biased subset.
  dropped_share <- if (n_all > 0) 1 - nrow(wide) / n_all else 0
  if (nrow(wide) < k + 1)
    return(assumption_result("sphericity", "untested",
      sprintf("Only %d subject%s measured in all %d conditions - too few to assess sphericity.",
              nrow(wide), if (nrow(wide) == 1) "" else "s", k)))

  # Greenhouse-Geisser epsilon from the covariance matrix of orthonormal
  # within-subject contrasts.
  S <- stats::cov(wide)
  M <- qr.Q(qr(cbind(1, stats::contr.helmert(k))))[, -1, drop = FALSE]
  T <- t(M) %*% S %*% M
  eps <- sum(diag(T))^2 / ((k - 1) * sum(T * T))
  eps <- max(1 / (k - 1), min(1, eps))

  p <- tryCatch({
    fitm <- stats::lm(wide ~ 1)
    stats::mauchly.test(fitm, X = ~1)$p.value
  }, error = function(e) NA_real_)

  # Epsilon is a downward-biased estimate: under perfect compound symmetry with
  # 60 subjects and 3 conditions it averages 0.97 and drops below 0.95 about a
  # fifth of the time. A 0.95 cutoff would therefore flag correct data as
  # suspect - the same over-flagging this file avoids for normality. 0.90 keeps
  # the false-flag rate near 2% and matches the applied convention (below 0.75
  # the uncorrected F test is seriously anti-conservative).
  verdict <- if (!is.finite(eps)) "untested"
             else if (eps >= 0.90) "ok"
             else if (eps >= 0.75) "review"
             else "violated"
  pbit <- if (is.na(p)) "" else sprintf(" Mauchly p = %s.", p_fmt(p))
  # Never let the epsilon stand alone when a large share of subjects was dropped.
  dbit <- if (dropped_share >= 0.10)
    sprintf(" Note: %.0f%% of subjects were not measured in every condition and are excluded, so this is based on %d of %d subjects and may not represent the full sample.",
            100 * dropped_share, nrow(wide), n_all) else ""
  note <- switch(
    verdict,
    ok = sprintf("Sphericity holds (Greenhouse-Geisser epsilon = %.2f across %d conditions).%s%s",
                 eps, k, pbit, dbit),
    review = sprintf("Sphericity is mildly violated (epsilon = %.2f); apply the Greenhouse-Geisser correction to the degrees of freedom.%s%s",
                     eps, pbit, dbit),
    violated = sprintf("Sphericity is clearly violated (epsilon = %.2f). Use the Greenhouse-Geisser correction, the Friedman test, or a mixed model.%s%s",
                       eps, pbit, dbit),
    "Sphericity could not be assessed.")
  assumption_result("sphericity", verdict, note, statistic = eps, p = p, test = "Mauchly",
                    detail = list(epsilon = eps, conditions = k, subjects = nrow(wide),
                                  subjects_total = n_all, dropped_share = dropped_share))
}

# --- collinearity -----------------------------------------------------------

#' Check multicollinearity in a fitted model matrix.
#'
#' A dependency-free VIF check computed on the actual columns passed to the
#' model (including factor contrasts), so the warning is about the fit the
#' analyst is looking at rather than a separate pre-processing guess. A VIF of 5
#' is the review threshold and 10 is severe, following conventional guidance.
check_multicollinearity <- function(fit, threshold = 5) {
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mm)) {
    out <- assumption_result("multicollinearity", "untested",
                             "Multicollinearity could not be evaluated for this model.")
    out$test <- "VIF"; out$max_vif <- NA_real_; out$by_term <- numeric(0); return(out)
  }
  keep <- !grepl("^\\(Intercept\\)$", colnames(mm))
  mm <- mm[, keep, drop = FALSE]
  if (ncol(mm) < 2L) {
    out <- assumption_result("multicollinearity", "untested",
      "Fewer than two predictor terms were available; a VIF comparison is not informative.")
    out$test <- "VIF"; out$max_vif <- NA_real_
    out$by_term <- stats::setNames(1, colnames(mm)); return(out)
  }

  vif <- vapply(seq_len(ncol(mm)), function(i) {
    other <- mm[, -i, drop = FALSE]
    fit_i <- tryCatch(stats::lm.fit(cbind(1, other), mm[, i]), error = function(e) NULL)
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

  verdict <- if (!is.finite(max_vif) || max_vif >= 10) "violated"
             else if (max_vif >= threshold) "review" else "ok"
  note <- switch(
    verdict,
    ok = sprintf("No concerning multicollinearity was detected (maximum VIF = %.2f).", max_vif),
    review = sprintf("Multicollinearity needs review (maximum VIF = %.2f; values at or above %.0f are commonly flagged).",
                     max_vif, threshold),
    violated = sprintf("Severe multicollinearity detected (maximum VIF = %s); coefficient estimates may be unstable.",
                       if (is.finite(max_vif)) formatC(max_vif, digits = 2, format = "f") else "Inf"))
  out <- assumption_result("multicollinearity", verdict, note, statistic = max_vif,
                           test = "Variance inflation factor",
                           detail = list(max_vif = max_vif, by_term = vif))
  out$max_vif <- max_vif    # retained for existing callers
  out$by_term <- vif
  out
}

# --- summarising a set of results -------------------------------------------

# --- pre-analysis screening -------------------------------------------------

#' Which catalogue checks can be run BEFORE anything is fitted.
#'
#' The split is not arbitrary. Residual normality, heteroscedasticity, Cook's
#' distance, overdispersion, proportional hazards and singular fit all need a
#' fitted model - there is no residual to examine until something has been
#' fitted - so they stay on the analysis tabs. Everything below is a property
#' of the data itself and can be screened as soon as the analyst has named an
#' outcome and some predictors.
ASSUMPTION_PREFIT <- c("normality", "normality_groups", "variance_homogeneity",
                       "expected_counts", "linearity", "independence",
                       "sphericity", "zero_inflation", "multicollinearity")

#' Checks that only a fitted model can answer, named so the UI can say what is
#' still to come rather than implying the screen is exhaustive.
ASSUMPTION_POSTFIT <- c("normality_residuals", "homoscedasticity", "influence",
                        "overdispersion", "proportional_hazards",
                        "events_per_variable", "singular_fit")

#' Run every pre-analysis check that applies to a chosen outcome and predictors.
#'
#' Nothing is fitted and nothing is changed - this reads the data and reports.
#' Each result carries a `scope` naming the variables it concerns, because the
#' same check runs several times over different predictors.
#'
#' @param df         the data
#' @param types      named measurement types
#' @param outcome    the chosen outcome column, or NULL
#' @param predictors chosen predictor columns
#' @param panel      the panel/role list, for the repeated-measures checks
#' @param max_per_check cap on how many predictors each check is run against, so
#'   a 50-variable dataset does not produce an unreadable wall of results.
readiness_assumptions <- function(df, types = NULL, outcome = NULL,
                                  predictors = character(0), panel = NULL,
                                  max_per_check = 5L) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(list())
  out <- list()

  # Every check is guarded INDIVIDUALLY. A single wrapper round the whole screen
  # meant one failing check returned nothing at all, so the panel read "no
  # assumption applies" while an outcome and ten predictors were selected -
  # indistinguishable from there being genuinely nothing to check. A check that
  # breaks now reports itself as untested, with the reason, and the other
  # fifteen still run.
  add <- function(expr, id = "normality", scope = "") {
    r <- tryCatch(expr, error = function(e)
      assumption_result(id, "untested",
        sprintf("This check could not be run: %s", conditionMessage(e)),
        scope = scope))
    if (!is.null(r)) out[[length(out) + 1L]] <<- r
  }

  ty <- function(v) if (!is.null(types) && v %in% names(types))
    unname(types[[v]]) else NULL
  num <- function(v) suppressWarnings(as.numeric(as.character(df[[v]])))
  is_num <- function(v) { t <- ty(v)
    if (!is.null(t)) is_numeric_type(t) else is.numeric(df[[v]]) }
  is_cat <- function(v) { t <- ty(v)
    if (!is.null(t)) is_categorical_type(t) else !is.numeric(df[[v]]) }
  nlev <- function(v) length(unique(stats::na.omit(as.character(df[[v]]))))

  predictors <- setdiff(intersect(predictors, names(df)), outcome %||% "")
  has_outcome <- !is.null(outcome) && nzchar(outcome) && outcome %in% names(df)

  cat_preds <- utils::head(Filter(function(v) is_cat(v) && nlev(v) >= 2 &&
                                    nlev(v) <= 10, predictors), max_per_check)
  num_preds <- utils::head(Filter(is_num, predictors), max_per_check)

  if (has_outcome && is_num(outcome)) {
    y <- num(outcome)
    add(.as_scoped(check_normality(y, what = sprintf("'%s'", outcome)), outcome),
        "normality", outcome)

    for (g in cat_preds) {
      add(.as_scoped(check_normality_by_group(y, df[[g]]),
                     sprintf("%s by %s", outcome, g)),
          "normality_groups", sprintf("%s by %s", outcome, g))
      add(.as_scoped(check_variance_homogeneity(y, df[[g]]),
                     sprintf("%s by %s", outcome, g)),
          "variance_homogeneity", sprintf("%s by %s", outcome, g))
    }
    for (x in num_preds)
      add(.as_scoped(check_linearity(num(x), y), sprintf("%s vs %s", outcome, x)),
          "linearity", sprintf("%s vs %s", outcome, x))

    # Counts: a Poisson-style outcome with more zeros than its own mean implies.
    if (identical(ty(outcome), "count"))
      add(.as_scoped(check_zero_inflation(y), outcome), "zero_inflation", outcome)
  }

  # Categorical outcome against categorical predictors: the chi-square rule.
  if (has_outcome && is_cat(outcome)) {
    for (g in cat_preds) {
      tab <- tryCatch(table(df[[outcome]], df[[g]]), error = function(e) NULL)
      if (!is.null(tab) && all(dim(tab) >= 2))
        add(.as_scoped(check_expected_counts(tab), sprintf("%s by %s", outcome, g)),
            "expected_counts", sprintf("%s by %s", outcome, g))
    }
  }

  # Structure, from the roles declared on the Data tab.
  if (!is.null(panel) && has_outcome && is_num(outcome)) {
    y <- num(outcome)
    id <- panel$id %||% ""; tm <- panel$time %||% ""
    if (identical(panel$kind %||% "", "repeated") &&
        nzchar(id) && nzchar(tm) && all(c(id, tm) %in% names(df))) {
      # The within-subject factor is the OCCASION (first visit, second, third),
      # not the calendar date. In a real cohort each patient is seen on their
      # own dates - this one has 431 distinct visit dates across 250 patients -
      # so using the date itself would give hundreds of "conditions" and
      # sphericity could never be computed. Ranking time within each subject
      # recovers the occasion whether the time column holds labels or dates.
      add({
        occ <- .as_occasion(df[[id]], df[[tm]])
        .as_scoped(check_sphericity(y, df[[id]], occ),
                   sprintf("%s across repeated visits", outcome))
      }, "sphericity", sprintf("%s across repeated visits", outcome))
    }
    if (identical(panel$kind %||% "", "time_series") && nzchar(tm) &&
        tm %in% names(df)) {
      add({
        ord <- order(df[[tm]])
        .as_scoped(check_autocorrelation(y[ord]), sprintf("%s over %s", outcome, tm))
      }, "independence", sprintf("%s over %s", outcome, tm))
    }
  }

  # Collinearity among the chosen predictors. The variance inflation factor
  # depends only on the predictor matrix, not on the outcome, so it can be
  # computed before anything is modelled - a deterministic dummy response is
  # used purely to build the design matrix.
  if (length(predictors) >= 2) {
    r <- tryCatch({
      mm <- df[, predictors, drop = FALSE]
      keep <- vapply(mm, function(col) length(unique(stats::na.omit(col))) > 1,
                     logical(1))
      mm <- mm[, keep, drop = FALSE]
      if (ncol(mm) < 2) NULL else {
        mm[[".__dummy"]] <- seq_len(nrow(mm))
        fit <- stats::lm(.__dummy ~ ., data = mm)
        check_multicollinearity(fit)
      }
    }, error = function(e) NULL)
    if (!is.null(r)) add(.as_scoped(r, paste(predictors, collapse = ", ")),
                         "multicollinearity", paste(predictors, collapse = ", "))
  }

  out
}

# Each row's visit NUMBER within its subject ("Visit 1", "Visit 2", ...),
# ordered by whatever the time column holds - a date, a label, or a number.
.as_occasion <- function(id, time) {
  id <- as.character(id)
  key <- suppressWarnings(as.numeric(as.Date(as.character(time))))
  if (!any(is.finite(key))) key <- suppressWarnings(as.numeric(as.character(time)))
  if (!any(is.finite(key))) key <- as.numeric(as.factor(time))
  occ <- rep(NA_integer_, length(id))
  for (idx in split(seq_along(id), id)) occ[idx] <- rank(key[idx], ties.method = "first")
  paste("Visit", occ)
}

#' Resolve free text to an assumption id, so the planner's prose becomes checks
#' the app can actually run. Mirrors transform_match_method().
assumption_match_check <- function(text) {
  if (is.null(text) || !length(text)) return("")
  s <- tolower(trimws(as.character(text)[1]))
  if (!nzchar(s)) return("")
  if (s %in% assumption_ids()) return(s)
  patterns <- c(
    normality_residuals = "residual.{0,20}normal|normal.{0,20}residual",
    normality_groups = "normal.{0,25}(within|each|by) group|group.{0,15}normal",
    sphericity = "spheric|mauchly|greenhouse",
    zero_inflation = "zero.?inflat|excess zero",
    overdispersion = "overdispers|dispersion",
    proportional_hazards = "proportional hazard|schoenfeld|cox\\.zph",
    multicollinearity = "collinear|multicollinear|\\bvif\\b|variance inflation",
    expected_counts = "expected (cell )?count|cell count|sparse cell",
    homoscedasticity = "homoscedast|heteroscedast|constant (residual )?variance|breusch",
    variance_homogeneity = "equal variance|homogeneity of variance|levene",
    independence = "independen|autocorrelat|durbin|serial correlation|clustering",
    linearity = "linear(ity)? relationship|linearity|straight line",
    influence = "influential|outlier|cook",
    events_per_variable = "events per|sample size|power",
    singular_fit = "random.?effect|singular",
    normality = "normal")
  for (id in names(patterns)) if (grepl(patterns[[id]], s)) return(id)
  ""
}

#' The assumption ids a plan names, resolved and split into what can be screened
#' now and what has to wait for a fitted model.
plan_assumption_ids <- function(plan) {
  txt <- unlist(plan$assumptions_to_check %||% list())
  ids <- unique(Filter(nzchar, vapply(txt, assumption_match_check, character(1),
                                      USE.NAMES = FALSE)))
  list(prefit = intersect(ids, ASSUMPTION_PREFIT),
       postfit = intersect(ids, ASSUMPTION_POSTFIT),
       unmatched = txt[vapply(txt, function(z) !nzchar(assumption_match_check(z)),
                              logical(1))])
}

#' Counts by verdict, for a headline the analyst reads before the detail.
assumption_summary <- function(results) {
  sev <- vapply(results, function(r) r$severity %||% "untested", character(1))
  counts <- c(ok = sum(sev == "ok"), review = sum(sev == "review"),
              violated = sum(sev == "violated"), untested = sum(sev == "untested"))
  worst <- if (!length(sev)) "untested" else Reduce(assumption_worse, sev)
  headline <- if (!length(sev)) "No assumption checks were available."
    else if (counts[["violated"]] > 0)
      sprintf("%d of %d assumption checks %s violated.", counts[["violated"]],
              length(sev), if (counts[["violated"]] == 1) "is" else "are")
    else if (counts[["review"]] > 0)
      sprintf("%d of %d assumption checks need review; none is clearly violated.",
              counts[["review"]], length(sev))
    else if (counts[["ok"]] > 0)
      sprintf("All %d assumption checks that could be run are satisfied.", counts[["ok"]])
    else "The assumptions could not be checked for this analysis."
  list(counts = counts, worst = worst, headline = headline, n = length(sev))
}

#' Render a set of assumption results for a Shiny tab.
#'
#' Shared by the Tests, Regression and Advanced tabs so a violated assumption
#' looks the same wherever it appears: a headline count, then one line per check
#' with a coloured verdict and, when there is something to do about it, the
#' remedy. Previously every tab printed an unlabelled bullet list of sentences.
assumption_ui <- function(results, empty = "No formal assumption checks for this analysis.") {
  if (!length(results)) return(shiny::tags$em(empty))
  s <- assumption_summary(results)
  colour <- c(ok = "#018786", review = "#8a6d3b", violated = "#a94442",
              untested = "#7b8f8d")

  items <- lapply(results, function(r) {
    sev <- r$severity %||% "untested"
    # Contextual notes are explanations, not verdicts; show them unbadged.
    is_context <- identical(r$check %||% "", "context")
    rem <- r$remedy %||% list()
    shiny::tags$li(
      style = "margin-bottom:6px;",
      if (!is_context) shiny::tags$span(
        class = "label",
        style = sprintf("background:%s;font-weight:600;margin-right:6px;",
                        unname(colour[sev])),
        unname(ASSUMPTION_SEVERITY_LABELS[sev])),
      if (!is_context) shiny::strong(paste0(r$label %||% r$check, ": ")),
      # Name the variables when the same check has been run several times.
      if (!is_context && nzchar(r$scope %||% ""))
        shiny::tags$small(style = "color:#4a6360;",
                          sprintf("[%s] ", r$scope)),
      r$note %||% "",
      if (length(rem)) shiny::tags$div(
        style = "margin-top:2px;",
        shiny::tags$small(style = "color:#4a6360;",
          shiny::tags$em("What to do: "),
          paste(vapply(rem, function(z) z$what %||% "", character(1)),
                collapse = "; "), ".")))
  })

  shiny::tagList(
    shiny::tags$p(shiny::strong(
      style = sprintf("color:%s;", unname(colour[s$worst])), s$headline)),
    shiny::tags$ul(style = "padding-left:18px;", items))
}

#' Assumption results as a display table, with the verdict as its own column and
#' the suggested action alongside - the whole point of grading the checks.
assumption_df <- function(results) {
  if (!length(results))
    return(data.frame(Check = character(0), Variables = character(0),
                      Verdict = character(0), Detail = character(0),
                      `What to do` = character(0),
                      check.names = FALSE, stringsAsFactors = FALSE))
  data.frame(
    Check = vapply(results, function(r) r$label %||% r$check %||% "Diagnostic", character(1)),
    Variables = vapply(results, function(r) r$scope %||% "", character(1)),
    Verdict = vapply(results, function(r)
      unname(ASSUMPTION_SEVERITY_LABELS[r$severity %||% "untested"]) %||% "", character(1)),
    Detail = vapply(results, function(r)
      if (is.null(r$note) || !length(r$note)) "" else as.character(r$note)[1], character(1)),
    `What to do` = vapply(results, function(r) {
      rem <- r$remedy %||% list()
      if (!length(rem)) return("")
      paste(vapply(rem, function(z) z$what %||% "", character(1)), collapse = "; ")
    }, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}
