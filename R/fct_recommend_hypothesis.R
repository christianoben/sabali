# fct_recommend_hypothesis.R ------------------------------------------------
# Routes the user's role choices to the right test family/runner and produces
# the plain-language "here is what we will run and why" preview (Section 5).

# Number of usable (non-missing) levels of a grouping variable.
n_group_levels <- function(g) nlevels(droplevels(as.factor(g[!is.na(g)])))

# ---------------------------------------------------------------------------
# Feasibility / role-eligibility helpers (Section 5). These drive the tests
# tab so that only test families and role variables that make sense for the
# chosen types and conditions are offered — the same discipline the
# Descriptives tab already applies.
# ---------------------------------------------------------------------------

FAMILY_LABELS <- c(
  location       = "Compare groups / factorial designs (means / locations)",
  categorical    = "Association between two categories",
  cochran_q     = "Repeated binary outcomes (Cochran's Q)",
  correlation    = "Correlation between two numbers",
  point_biserial = "Point-biserial correlation (binary vs numeric)",
  proportion     = "One proportion vs a reference"
)

#' Which test families the data can support, given the variable types.
#'
#' @param types Named character vector of measurement types.
#' @param include_proportion Include the one-proportion family when a binary
#'   variable is available. Kept opt-in for compatibility with callers that
#'   use the original three-family contract.
#' @return Subset of c("location","categorical","correlation","proportion").
#'   location    : needs >= 1 numeric variable (an outcome)
#'   correlation : needs >= 2 numeric variables
#'   categorical : needs >= 2 categorical variables
available_test_families <- function(types, include_proportion = FALSE,
                                     include_point_biserial = FALSE,
                                     include_cochran_q = FALSE) {
  n_num <- sum(vapply(types, is_numeric_type, logical(1)))
  n_cat <- sum(vapply(types, is_categorical_type, logical(1)))
  n_binary <- sum(types == "binary")
  fams <- character(0)
  if (n_num >= 1) fams <- c(fams, "location")
  if (n_cat >= 2) fams <- c(fams, "categorical")
  if (n_num >= 2) fams <- c(fams, "correlation")
  if (isTRUE(include_point_biserial) && n_binary >= 1 && n_num >= 1)
    fams <- c(fams, "point_biserial")
  if (isTRUE(include_cochran_q) && n_binary >= 1)
    fams <- c(fams, "cochran_q")
  if (isTRUE(include_proportion) && n_binary >= 1)
    fams <- c(fams, "proportion")
  fams[order(match(fams, c("location", "categorical", "correlation",
                           "cochran_q", "point_biserial", "proportion")))]
}

#' Grouping variables valid for a given between-groups design.
#'
#' @param types Named character vector of measurement types.
#' @param df    The data frame (used to count non-missing levels).
#' @param design "two_independent" (exactly 2 levels) or "multi_independent" (>=3).
valid_grouping_vars <- function(types, df, design) {
  cats <- names(types)[vapply(types, is_categorical_type, logical(1))]
  if (design %in% c("mixed", "ancova"))
    return(valid_factor_vars(types, df))   # any categorical with >= 2 levels
  keep <- vapply(cats, function(v) {
    k <- n_group_levels(df[[v]])
    if (identical(design, "two_independent")) k == 2 else k >= 3
  }, logical(1))
  cats[keep]
}

# Categorical variables suitable for a factorial factor or a between-subjects
# grouping role. Unlike valid_grouping_vars(), this does not impose an exact
# number of levels because a factor may legitimately have 2+ levels.
valid_factor_vars <- function(types, df, exclude = character(0)) {
  cats <- setdiff(names(types)[vapply(types, is_categorical_type, logical(1))], exclude)
  cats[vapply(cats, function(v) n_group_levels(df[[v]]) >= 2, logical(1))]
}

#' Candidate subject-ID variables (paired / repeated designs).
#'
#' Excludes the chosen numeric outcome (a variable cannot be both the outcome
#' and the subject identifier) and free-text columns.
valid_subject_vars <- function(types, outcome = NULL) {
  nm <- names(types)
  nm <- nm[!vapply(types[nm], function(t) identical(t, "text"), logical(1))]
  setdiff(nm, outcome)
}

#' Which location designs the current variables can actually support.
#'
#' This is the idiot-proof base gate: a design is offered only when the data
#' has the variables it needs (a two-level group for a two-group test, a >=3
#' level group for a one-way ANOVA, two factors for a factorial design, a
#' covariate for ANCOVA, repeated structure for paired/mixed). It is
#' deliberately OUTCOME-INDEPENDENT so choosing an outcome does not reset the
#' design menu. `reserved` are columns not usable as ordinary test variables.
feasible_location_designs <- function(types, df, panel = NULL, reserved = character(0)) {
  tp <- types[intersect(setdiff(names(types), reserved), names(df))]
  n_num   <- sum(vapply(tp, is_numeric_type, logical(1)))
  cats2   <- length(valid_factor_vars(tp, df))                       # >= 2-level cats
  n_two   <- length(valid_grouping_vars(tp, df, "two_independent"))  # exactly 2 levels
  n_multi <- length(valid_grouping_vars(tp, df, "multi_independent"))# >= 3 levels
  cats    <- names(tp)[vapply(tp, is_categorical_type, logical(1))]
  within_ok <- length(unique(c(intersect(panel$time %||% character(0), names(df)), cats))) > 0
  subj_ok   <- length(valid_subject_vars(tp)) > 0

  out <- character(0)
  if (n_num  >= 1) out <- c(out, "one_sample")
  if (n_two  >= 1) out <- c(out, "two_independent")
  if (n_multi >= 1) out <- c(out, "multi_independent")
  if (cats2  >= 2) out <- c(out, "factorial")
  if (cats2  >= 1 && n_num >= 2) out <- c(out, "ancova")
  if (within_ok && subj_ok) out <- c(out, "paired_repeated")
  if (within_ok && subj_ok && cats2 >= 1) out <- c(out, "mixed")
  out
}

#' Validate a fully-specified test spec BEFORE running, raising human-readable
#' errors for impossible role combinations (replaces raw stopifnot() failures).
validate_spec <- function(df, spec) {
  fam <- spec$family
  if (identical(fam, "location")) {
    if (is.null(spec$outcome) || !nzchar(spec$outcome))
      stop("Choose a numeric outcome variable for this comparison.")
    design <- spec$design
    if (design %in% c("two_independent", "multi_independent")) {
      if (is.null(spec$group) || !nzchar(spec$group))
        stop("Choose a grouping variable for this comparison.")
      k <- n_group_levels(df[[spec$group]])
      if (identical(design, "two_independent") && k != 2)
        stop(sprintf(
          "A two-group comparison needs a grouping variable with exactly two groups, but '%s' has %d. Pick a two-level variable, or switch to the 'more than two groups' design.",
          spec$group, k))
      if (identical(design, "multi_independent") && k < 3)
        stop(sprintf(
          "A comparison of more than two groups needs a grouping variable with at least three groups, but '%s' has %d.",
          spec$group, k))
    }
    if (design %in% c("two_paired", "repeated")) {
      if (!is.null(spec$subject) && identical(spec$subject, spec$outcome))
        stop("The subject ID must be a different variable from the outcome.")
    }
    if (identical(design, "factorial")) {
      factors <- unique(spec$factors %||% character(0))
      if (length(factors) < 2 || length(factors) > 3)
        stop("A factorial ANOVA needs exactly two or three factor variables.")
      if (any(!factors %in% names(df)) || any(factors == spec$outcome))
        stop("Factor variables must be present in the data and distinct from the outcome.")
      if (any(vapply(factors, function(v) n_group_levels(df[[v]]) < 2, logical(1))))
        stop("Each factorial variable needs at least two observed levels.")
    }
    if (identical(design, "mixed")) {
      roles <- c(spec$outcome, spec$group, spec$within, spec$subject)
      if (any(is.null(roles)) || any(!nzchar(roles)) ||
          any(!roles %in% names(df)) || length(unique(roles)) != 4)
        stop("A mixed-model ANOVA needs four distinct, valid outcome, group, within, and subject variables.")
      if (n_group_levels(df[[spec$group]]) < 2)
        stop("The between-subjects grouping variable needs at least two levels.")
      if (n_group_levels(df[[spec$within]]) < 2)
        stop("The within-subjects variable needs at least two conditions.")
      if (n_group_levels(df[[spec$subject]]) < 2)
        stop("The mixed-model ANOVA needs at least two subjects.")
    }
    if (identical(design, "ancova")) {
      if (is.null(spec$group) || !nzchar(spec$group))
        stop("Choose a grouping variable for the ANCOVA.")
      if (n_group_levels(df[[spec$group]]) < 2)
        stop("ANCOVA needs a grouping variable with at least two groups.")
      covs <- unique((spec$covariates %||% character(0)))
      covs <- covs[nzchar(covs)]
      if (!length(covs))
        stop("Choose at least one continuous covariate to adjust for.")
      if (spec$outcome %in% covs || spec$group %in% covs)
        stop("The covariate must differ from the outcome and the grouping variable.")
    }
  } else if (identical(fam, "point_biserial")) {
    if (is.null(spec$x) || !nzchar(spec$x) || is.null(spec$y) || !nzchar(spec$y))
      stop("Choose a binary variable and a numeric variable for the point-biserial correlation.")
    if (identical(spec$x, spec$y))
      stop("Point-biserial correlation needs two different variables.")
    if (n_group_levels(df[[spec$x]]) != 2)
      stop("Point-biserial correlation needs a binary variable with exactly two observed levels.")
  } else if (identical(fam, "proportion")) {
    if (is.null(spec$outcome) || !nzchar(spec$outcome) || !spec$outcome %in% names(df))
      stop("Choose a valid binary outcome variable for the one-proportion test.")
    if (n_group_levels(df[[spec$outcome]]) != 2)
      stop("A one-proportion test needs a binary outcome with exactly two observed levels.")
    ref <- spec$ref_prop %||% 0.5
    if (!is.numeric(ref) || length(ref) != 1 || !is.finite(ref) || ref <= 0 || ref >= 1)
      stop("The reference proportion must be a single number strictly between 0 and 1.")
  } else if (identical(fam, "cochran_q")) {
    roles <- c(spec$outcome %||% "", spec$within %||% "", spec$subject %||% "")
    if (any(!nzchar(roles)) || any(!roles %in% names(df)) ||
        length(unique(roles)) != 3)
      stop("Cochran's Q needs distinct, valid outcome, condition/time, and subject variables.")
    if (n_group_levels(df[[spec$outcome]]) != 2)
      stop("Cochran's Q needs a binary outcome with exactly two observed levels.")
    if (n_group_levels(df[[spec$within]]) < 2)
      stop("Repeated binary analysis needs at least two conditions or time points.")
    if (n_group_levels(df[[spec$subject]]) < 2)
      stop("Cochran's Q needs at least two subjects.")
  } else if (identical(fam, "correlation")) {
    if (identical(spec$x, spec$y))
      stop("Correlation needs two different variables; X and Y are the same. Choose distinct variables.")
    if (isTRUE(spec$repeated)) {
      roles <- c(spec$x %||% "", spec$y %||% "", spec$subject %||% "")
      if (any(!nzchar(roles)) || any(!roles %in% names(df)) ||
          length(unique(roles)) != 3)
        stop("Repeated-measures correlation needs distinct, valid X, Y, and subject variables.")
      if (n_group_levels(df[[spec$subject]]) < 2)
        stop("Repeated-measures correlation needs at least two subjects.")
    }
  } else if (identical(fam, "categorical")) {
    if (identical(spec$x, spec$y))
      stop("An association test needs two different variables; both roles are the same. Choose distinct variables.")
  }
  invisible(TRUE)
}

#' Preview text for a planned location comparison, before it is run.
suggest_location_test <- function(design, n_levels = NA) {
  switch(design,
    one_sample = list(primary = "One-sample t-test",
                      alt = "Wilcoxon signed-rank test",
                      why = "Comparing one sample's centre to a reference value."),
    two_independent = list(primary = "Welch's t-test",
                      alt = "Mann-Whitney U test",
                      why = "Two independent groups. Welch's t-test is the safe default; the Mann-Whitney U is the nonparametric alternative."),
    two_paired = list(primary = "Paired t-test",
                      alt = "Wilcoxon signed-rank test",
                      why = "Two measurements on the same subjects."),
    multi_independent = list(primary = "One-way ANOVA (Welch if variances differ)",
                      alt = "Kruskal-Wallis test",
                      why = sprintf("%s independent groups.", ifelse(is.na(n_levels), "Several", n_levels))),
    repeated = list(primary = "Repeated-measures ANOVA",
                      alt = "Friedman test",
                      why = "The same subjects measured under 3+ conditions."),
    factorial = list(primary = "Two-/three-way factorial ANOVA",
                      alt = "Aligned Rank Transform ANOVA",
                      why = "Two or three crossed categorical factors; main effects and interactions are reported."),
    mixed = list(primary = "Mixed-model ANOVA (LMM)",
                      alt = "Rank-transformed mixed-model ANOVA",
                      why = "A between-subjects factor and a within-subjects factor are modelled with a subject random intercept."),
    ancova = list(primary = "ANCOVA (covariate-adjusted group comparison)",
                      alt = "Quade rank ANCOVA",
                      why = "Group means on a continuous outcome are compared after adjusting for one or more continuous covariates; a homogeneity-of-slopes check is reported."),
    list(primary = "-", alt = "-", why = "Select a design."))
}

#' Dispatch to the correct runner given a fully-specified test spec.
#'
#' spec fields (by family):
#'   family = "location":  outcome, design, plus group | within+subject | ref_value
#'   family = "categorical": x, y, paired
#'   family = "agreement": x, y, weights
#'   family = "fleiss": raters (3+ columns)
#'   family = "diagnostic": test, reference, positive
#'   family = "effects": outcome, group, positive, exposed
#'   family = "trend": outcome, exposure, positive
#'   family = "proportion": outcome, ref_prop, success
#'   family = "cochran_q": outcome, within, subject, success
#'   family = "correlation": x, y, repeated, subject (when longitudinal)
run_guided_test <- function(df, spec) {
  validate_spec(df, spec)
  fam <- spec$family
  if (fam == "location") {
    y <- df[[spec$outcome]]
    switch(spec$design,
      one_sample        = run_one_sample(y, mu = spec$ref_value %||% 0),
      two_independent   = run_two_independent(y, df[[spec$group]]),
      multi_independent = run_anova_oneway(y, df[[spec$group]]),
      two_paired        = run_two_paired(y, df[[spec$within]], df[[spec$subject]]),
      repeated          = run_repeated(y, df[[spec$within]], df[[spec$subject]]),
      factorial         = run_factorial_anova(df, spec$outcome, spec$factors),
      mixed             = run_mixed_anova(df, spec$outcome, spec$group,
                                          spec$within, spec$subject),
      ancova            = run_ancova(df, spec$outcome, spec$group,
                                     spec$covariates %||% character(0)),
      stop("Unknown location design."))
  } else if (fam == "proportion") {
    run_proportion_test(df[[spec$outcome]], ref_prop = spec$ref_prop %||% 0.5,
                        success = spec$success)
  } else if (fam == "cochran_q") {
    run_repeated_binary(df[[spec$outcome]], df[[spec$within]], df[[spec$subject]],
                        success = spec$success)
  } else if (fam == "point_biserial") {
    run_point_biserial(df[[spec$x]], df[[spec$y]])
  } else if (fam == "effects") {
    run_two_group_effects(df[[spec$outcome]], df[[spec$group]],
                          positive = spec$positive, exposed = spec$exposed)
  } else if (fam == "trend") {
    run_trend_test(df[[spec$outcome]], df[[spec$exposure]],
                   positive = spec$positive)
  } else if (fam == "diagnostic") {
    run_diagnostic(df[[spec$test]], df[[spec$reference]],
                   positive = spec$positive)
  } else if (fam == "fleiss") {
    run_fleiss(df[, spec$raters, drop = FALSE])
  } else if (fam == "agreement") {
    run_agreement(df[[spec$x]], df[[spec$y]],
                  weights = spec$weights %||% "unweighted")
  } else if (fam == "categorical") {
    run_categorical(df[[spec$x]], df[[spec$y]], paired = isTRUE(spec$paired))
  } else if (fam == "correlation") {
    if (isTRUE(spec$repeated))
      run_repeated_correlation(df[[spec$x]], df[[spec$y]], df[[spec$subject]])
    else
      run_correlation(df[[spec$x]], df[[spec$y]])
  } else {
    stop("Unknown test family.")
  }
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
