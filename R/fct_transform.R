# fct_transform.R -----------------------------------------------------------
# Data transformation engine. Derives new variables from existing ones:
# distributional transforms (log, Box-Cox, Yeo-Johnson), scaling, rank-based
# scores, outlier handling, discretisation, categorical recoding, simple
# imputation, row-wise composites, longitudinal change scores and date parts.
#
# This file is the ENGINE ONLY - it has no UI. It is a pure library the Data
# Check page (or any other caller) can drive.
#
# Four contracts hold throughout, because a tool that can hold patient data
# must not quietly alter it:
#
#  1. ADDITIVE. A transform never overwrites its input. It returns a NEW column
#     and leaves the original untouched, so Table 1 and the data contract still
#     describe the values as collected.
#  2. NEVER THROWS. Every entry point returns list(ok, reason, ...). A transform
#     that cannot apply says why, in words for the analyst - matching
#     apply_value_constraints() in fct_clean.R.
#  3. RECORDED. Every application returns a provenance record: method, resolved
#     parameters, how many values went in and how many came back missing. That
#     record is what the audit trail and METHODS_APPENDIX are written from. A
#     transform nobody can see in the write-up is a reproducibility hole.
#  4. INVERTIBLE WHERE THE MATHS ALLOWS. Every record that can be reversed
#     carries what is needed to reverse it, so a model fitted on log10 viral
#     load can be reported as a geometric mean on the original scale. See
#     transform_inverse().
#
# The catalogue below is the single source of truth for what the engine can do,
# exactly as planner_test_catalogue() is for tests. Add a method there and to
# .transform_apply_one(), and every caller picks it up.

# --- families ---------------------------------------------------------------

TRANSFORM_FAMILY_LABELS <- c(
  distribution = "Change the shape of a distribution",
  scaling      = "Put variables on a common scale",
  rank         = "Replace values by their rank position",
  outliers     = "Limit the influence of extreme values",
  discretise   = "Turn a number into groups",
  categorical  = "Reorganise categories",
  missing      = "Fill or flag missing values",
  derived      = "Combine several columns into one",
  longitudinal = "Compare each subject against their own baseline",
  datetime     = "Pull calendar parts out of a date")

# Guard keys a method can declare in `requires`; .transform_guard() enforces them.
#   positive     x > 0 everywhere (log, Box-Cox, reciprocal of a rate)
#   nonnegative  x >= 0 (sqrt, log1p)
#   proportion   0 <= x <= 1 after any percent rescaling (logit, arcsine)
#   any          works on any column, whatever it holds (missingness handling)
#   numeric      coercible to a number
#   categorical  a factor / character / small-cardinality column
#   date         parseable as a date
#   panel        a subject id and a time column (longitudinal change scores)
#   partner      a second column, named in the spec as `with`
#   columns      a set of columns, named in the spec as `columns`

# --- catalogue --------------------------------------------------------------

#' Every transformation the engine can perform.
#'
#' `id` is the stable key a caller passes as spec$method. `params` gives each
#' tunable and its default, so a UI can build its own controls from this list
#' without hard-coding anything.
transform_catalogue <- function() {
  m <- function(id, name, family, requires = "numeric", produces = "continuous",
                params = list(), invertible = FALSE, note = "")
    list(id = id, name = name, family = family, requires = requires,
         produces = produces, params = params, invertible = invertible, note = note)

  list(
    # --- distribution ------------------------------------------------------
    m("log", "Natural log", "distribution", "positive", invertible = TRUE,
      note = "Multiplicative effects become additive; report as geometric means."),
    m("log2", "Log base 2", "distribution", "positive", invertible = TRUE,
      note = "One unit is a doubling - useful for fold changes."),
    m("log10", "Log base 10", "distribution", "positive", invertible = TRUE,
      note = "The convention for viral load and other wide-ranging assay values."),
    m("log1p", "Log of (x + 1)", "distribution", "nonnegative", invertible = TRUE,
      note = "Handles exact zeros without dropping them."),
    m("log_offset", "Log of (x + c)", "distribution", "numeric", invertible = TRUE,
      params = list(offset = NA_real_),
      note = "Offset defaults to half the smallest non-zero value, a common convention for assay data with zeros."),
    m("sqrt", "Square root", "distribution", "nonnegative", invertible = TRUE,
      note = "A mild transform; the classic variance stabiliser for counts."),
    m("cbrt", "Cube root", "distribution", "numeric", invertible = TRUE,
      note = "Like the square root but defined for negative values too."),
    m("reciprocal", "Reciprocal (1 / x)", "distribution", "numeric", invertible = TRUE,
      note = "Strong transform; turns a duration into a rate. Reverses the ordering."),
    m("square", "Square", "distribution", "numeric", invertible = TRUE,
      note = "Expands the upper tail; for left-skewed variables."),
    m("power", "Raise to a power", "distribution", "numeric", invertible = TRUE,
      params = list(exponent = 2),
      note = "A free-form Tukey ladder step when you already know the exponent."),
    m("exp", "Exponentiate", "distribution", "numeric", invertible = TRUE,
      note = "Reverses a log; occasionally wanted on already-logged data."),
    m("boxcox", "Box-Cox (lambda estimated)", "distribution", "positive",
      invertible = TRUE, params = list(lambda = NA_real_),
      note = "Picks the power that maximises normality. Needs strictly positive values."),
    m("yeojohnson", "Yeo-Johnson (lambda estimated)", "distribution", "numeric",
      invertible = TRUE, params = list(lambda = NA_real_),
      note = "Box-Cox that also accepts zero and negative values."),
    m("arcsine", "Arcsine square root", "distribution", "proportion",
      invertible = TRUE, params = list(percent = FALSE),
      note = "Historic transform for proportions; a logit or a binomial model is usually preferable now."),
    m("logit", "Logit", "distribution", "proportion", invertible = TRUE,
      params = list(percent = FALSE, squeeze = 0.001),
      note = "For proportions. Exact 0s and 1s are squeezed inwards, which is recorded."),
    m("probit", "Probit", "distribution", "proportion", invertible = TRUE,
      params = list(percent = FALSE, squeeze = 0.001),
      note = "The normal-quantile alternative to a logit."),

    # --- scaling -----------------------------------------------------------
    m("zscore", "Standardise (z-score)", "scaling", "numeric", invertible = TRUE,
      note = "Mean 0, SD 1. Makes regression coefficients comparable across variables."),
    m("center", "Centre on the mean", "scaling", "numeric", invertible = TRUE,
      note = "Makes a regression intercept the value at the average, not at zero."),
    m("scale_sd", "Divide by the SD", "scaling", "numeric", invertible = TRUE,
      note = "Rescales spread without moving the origin."),
    m("minmax", "Rescale to 0-1", "scaling", "numeric", invertible = TRUE,
      params = list(lower = 0, upper = 1),
      note = "Bounded by the observed range, so it is sensitive to outliers."),
    m("robust_scale", "Robust scale (median / IQR)", "scaling", "numeric",
      invertible = TRUE,
      note = "Like a z-score but unmoved by outliers - the safer default for skewed data."),
    m("maxabs", "Divide by the largest absolute value", "scaling", "numeric",
      invertible = TRUE, note = "Maps into -1..1 while keeping zero at zero."),
    m("index100", "Index against a reference (= 100)", "scaling", "numeric",
      invertible = TRUE, params = list(reference = "mean"),
      note = "Expresses each value as a percentage of the mean, median or first value."),

    # --- rank --------------------------------------------------------------
    m("rank", "Rank", "rank", "numeric",
      params = list(ties = "average"),
      note = "Discards the spacing between values, keeping only their order."),
    m("percentile", "Percentile (0-100)", "rank", "numeric",
      note = "Each value's position in the distribution."),
    m("inverse_normal", "Rank-based inverse-normal score", "rank", "numeric",
      params = list(constant = "blom"),
      note = "Forces an exactly normal shape. Blom, Tukey, Van der Waerden or Rankit offsets."),

    # --- outliers ----------------------------------------------------------
    m("winsorise", "Winsorise the tails", "outliers", "numeric",
      params = list(lower = 0.05, upper = 0.95),
      note = "Pulls extreme values in to a percentile instead of deleting them."),
    m("trim_quantile", "Set the tails to missing", "outliers", "numeric",
      params = list(lower = 0.01, upper = 0.99),
      note = "Deletes extreme values. Say so in the write-up: it changes the sample."),
    m("clip", "Clip to fixed bounds", "outliers", "numeric",
      params = list(min = NA_real_, max = NA_real_),
      note = "For known plausible limits, e.g. a physiological range."),
    m("cap_sd", "Cap beyond k SDs", "outliers", "numeric",
      params = list(k = 3),
      note = "Mean-and-SD based, so extreme values themselves widen the cap."),
    m("cap_mad", "Cap beyond k MADs", "outliers", "numeric",
      params = list(k = 3),
      note = "Median-and-MAD based; the robust alternative to capping on SDs."),

    # --- discretise --------------------------------------------------------
    m("bin_equal_width", "Bins of equal width", "discretise", "numeric",
      produces = "ordinal", params = list(bins = 4),
      note = "Equal value ranges, so group sizes can be very uneven."),
    m("bin_quantile", "Bins of equal size (quantiles)", "discretise", "numeric",
      produces = "ordinal", params = list(bins = 4),
      note = "Tertiles, quartiles, quintiles, deciles. Cutpoints are sample-specific."),
    m("bin_breaks", "Bins at cutpoints you choose", "discretise", "numeric",
      produces = "ordinal",
      params = list(breaks = numeric(0), labels = character(0)),
      note = "For clinical bands (BMI categories, age groups) that must match a guideline."),
    m("dichotomise", "Split at a threshold", "discretise", "numeric",
      produces = "binary", params = list(threshold = NA_real_, at_or_above = TRUE),
      note = "Loses information and power. Prefer keeping the number unless a guideline defines the cutoff."),

    # --- categorical -------------------------------------------------------
    m("collapse_rare", "Group rare categories as 'Other'", "categorical",
      "categorical", produces = "nominal",
      params = list(min_n = 10, min_pct = NA_real_, other_label = "Other"),
      note = "Stops sparse categories destabilising a model."),
    m("recode_levels", "Merge or rename categories", "categorical", "categorical",
      produces = "nominal", params = list(map = list()),
      note = "An explicit old-to-new mapping; anything unmapped is kept as it was."),
    m("relevel", "Set the reference category", "categorical", "categorical",
      produces = "nominal", params = list(reference = NA_character_),
      note = "Chooses what a regression compares the other categories against."),
    m("to_ordinal", "Declare an order", "categorical", "categorical",
      produces = "ordinal", params = list(order = character(0)),
      note = "For genuinely ordered categories such as a WHO clinical stage."),
    m("ordinal_score", "Score an ordered category as a number", "categorical",
      "categorical", params = list(scores = numeric(0)),
      note = "Treats ordered categories as equally spaced, or uses scores you supply."),
    m("dummy", "Indicator (one-hot) columns", "categorical", "categorical",
      produces = "binary", params = list(drop_first = FALSE),
      note = "One 0/1 column per category. R's model formulas do this for you already."),

    # --- missing -----------------------------------------------------------
    m("impute_mean", "Fill missing with the mean", "missing", "numeric",
      note = "Single imputation understates uncertainty and shrinks the variance."),
    m("impute_median", "Fill missing with the median", "missing", "numeric",
      note = "Safer than the mean when skewed, with the same caveat."),
    m("impute_mode", "Fill missing with the commonest value", "missing",
      "categorical", produces = "nominal",
      note = "The categorical counterpart, with the same caveat."),
    m("impute_constant", "Fill missing with a value", "missing", "any",
      params = list(value = 0),
      note = "Only when missing genuinely means a known quantity, e.g. zero doses."),
    m("missing_indicator", "Flag which values were missing", "missing", "any",
      produces = "binary",
      note = "Keeps missingness itself analysable. Pair it with any imputation."),

    # --- derived (row-wise across columns) ---------------------------------
    m("ratio", "Ratio of two columns", "derived", "partner",
      params = list(with = NA_character_),
      note = "Zero denominators become missing rather than infinite."),
    m("difference", "Difference between two columns", "derived", "partner",
      params = list(with = NA_character_)),
    m("sum_columns", "Sum of several columns", "derived", "columns",
      params = list(columns = character(0), na_rm = FALSE),
      note = "For scale scores built from items."),
    m("mean_columns", "Mean of several columns", "derived", "columns",
      params = list(columns = character(0), na_rm = FALSE)),
    m("bmi", "Body mass index from weight and height", "derived", "partner",
      params = list(with = NA_character_, height_unit = "cm"),
      note = "Weight in kilograms divided by height in metres squared."),
    m("zscore_by_group", "Standardise within each group", "derived", "partner",
      params = list(with = NA_character_), invertible = FALSE,
      note = "A z-score computed inside each level, e.g. within each site."),

    # --- longitudinal (needs a subject id and a time column) ---------------
    m("change_from_baseline", "Change from the subject's baseline",
      "longitudinal", "panel", params = list(baseline = NA_character_),
      note = "Each visit minus that subject's first (or nominated) visit."),
    m("pct_change_from_baseline", "Percent change from baseline",
      "longitudinal", "panel", params = list(baseline = NA_character_),
      note = "Undefined where the baseline is zero; those become missing."),
    m("lag_within", "Previous value for the same subject", "longitudinal", "panel",
      params = list(n = 1)),
    m("lead_within", "Next value for the same subject", "longitudinal", "panel",
      params = list(n = 1)),
    m("cumsum_within", "Running total within a subject", "longitudinal", "panel"),
    m("time_since_baseline", "Time elapsed since the subject's first visit",
      "longitudinal", "panel", params = list(units = "days"),
      note = "Turns visit labels into an actual elapsed-time variable for a mixed model."),

    # --- datetime ----------------------------------------------------------
    m("date_year", "Year", "datetime", "date", produces = "count"),
    m("date_month", "Month (1-12)", "datetime", "date", produces = "ordinal"),
    m("date_quarter", "Quarter (1-4)", "datetime", "date", produces = "ordinal"),
    m("date_week", "ISO week", "datetime", "date", produces = "count"),
    m("date_weekday", "Day of the week", "datetime", "date", produces = "nominal"),
    m("date_doy", "Day of the year", "datetime", "date", produces = "count"),
    m("date_diff", "Days between two dates", "datetime", "partner",
      params = list(with = NA_character_, units = "days")),
    m("age_from_date", "Age in years at a reference date", "datetime", "partner",
      params = list(with = NA_character_),
      note = "Completed years between the two dates, not a simple day count over 365.")
  )
}

#' The catalogue entry for a method id, or NULL.
transform_method <- function(id) {
  hit <- Filter(function(m) identical(m$id, id), transform_catalogue())
  if (length(hit)) hit[[1]] else NULL
}

#' Method ids, optionally restricted to one family.
transform_method_ids <- function(family = NULL) {
  cat <- transform_catalogue()
  if (!is.null(family)) cat <- Filter(function(m) identical(m$family, family), cat)
  vapply(cat, function(m) m$id, character(1))
}

#' A transform specification: the method plus any parameters it takes.
#' Unnamed catalogue defaults are filled in, so a caller may pass only what it
#' wants to change.
transform_spec <- function(method, variable = NA_character_, ...) {
  m <- transform_method(method)
  extra <- list(...)
  params <- if (is.null(m)) extra else utils::modifyList(m$params, extra)
  c(list(method = method, variable = variable), params)
}

# --- helpers ----------------------------------------------------------------

# Coerce to numeric, treating Inf/-Inf/NaN as missing. A bad import or an
# upstream division can leave infinities in a column; left alone they propagate
# into NaNs (log(-Inf)) and into every summary statistic. Missing is the honest
# reading: the value is not a number we can work with.
.tf_num <- function(x) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!is.finite(v)] <- NA_real_
  v
}
.tf_ok  <- function(...) list(ok = TRUE, reason = "", ...)
.tf_no  <- function(reason) list(ok = FALSE, reason = reason)

# A finite-values-only view, for estimating parameters.
.tf_finite <- function(v) v[is.finite(v)]

# The commonest non-missing value.
.tf_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  tb <- table(as.character(x))
  names(tb)[which.max(tb)]
}

#' A free column name derived from a variable and a method, e.g. viral_load_log10.
#' Suffixes are deliberately short and readable - these names appear in the
#' analysis menus, model output and the report.
TRANSFORM_SUFFIX <- c(
  log = "log", log2 = "log2", log10 = "log10", log1p = "log1p",
  log_offset = "logc", sqrt = "sqrt", cbrt = "cbrt", reciprocal = "recip",
  square = "sq", power = "pow", exp = "exp", boxcox = "bc",
  yeojohnson = "yj", arcsine = "asin", logit = "logit", probit = "probit",
  zscore = "z", center = "ctr", scale_sd = "sd", minmax = "01",
  robust_scale = "rz", maxabs = "maxabs", index100 = "idx",
  rank = "rank", percentile = "pct", inverse_normal = "int",
  winsorise = "wins", trim_quantile = "trim", clip = "clip",
  cap_sd = "capsd", cap_mad = "capmad",
  bin_equal_width = "bin", bin_quantile = "grp", bin_breaks = "cat",
  dichotomise = "hi", collapse_rare = "coll", recode_levels = "recode",
  relevel = "rel", to_ordinal = "ord", ordinal_score = "score", dummy = "is",
  impute_mean = "impmean", impute_median = "impmed", impute_mode = "impmode",
  impute_constant = "impval", missing_indicator = "wasna",
  ratio = "ratio", difference = "diff", sum_columns = "sum",
  mean_columns = "mean", bmi = "bmi", zscore_by_group = "zby",
  change_from_baseline = "chg", pct_change_from_baseline = "pctchg",
  lag_within = "lag", lead_within = "lead", cumsum_within = "cum",
  time_since_baseline = "t",
  date_year = "year", date_month = "month", date_quarter = "quarter",
  date_week = "week", date_weekday = "weekday", date_doy = "doy",
  date_diff = "days", age_from_date = "age")

transform_output_name <- function(variable, method, taken = character(0)) {
  sfx <- unname(TRANSFORM_SUFFIX[method])
  if (is.na(sfx)) sfx <- gsub("[^a-z0-9]+", "", tolower(method))
  base <- paste0(variable, "_", sfx)
  if (!(base %in% taken)) return(base)
  for (i in 2:99) {
    cand <- paste0(base, "_", i)
    if (!(cand %in% taken)) return(cand)
  }
  paste0(base, "_", length(taken) + 1L)
}

# --- guards -----------------------------------------------------------------

#' Can this method apply to this column? Returns list(ok, reason) and never
#' throws, so a UI can grey out an option and say why.
#'
#' @param x the column
#' @param type its confirmed measurement type (optional; values are checked too)
#' @param spec the transform spec, for parameters a guard depends on
#' @param df the data frame, for methods needing partner/columns
#' @param panel the panel/role list, for longitudinal methods
transform_applicable <- function(method, x, type = NULL, spec = list(),
                                 df = NULL, panel = NULL) {
  m <- transform_method(method)
  if (is.null(m)) return(.tf_no(sprintf("'%s' is not a transformation this app knows.", method)))
  req <- m$requires

  if (identical(req, "panel")) {
    if (is.null(panel) || !nzchar(panel$id %||% "") || !nzchar(panel$time %||% ""))
      return(.tf_no("Tell the app which column is the subject ID and which is the time/visit on the Data tab first."))
    if (is.null(df) || !all(c(panel$id, panel$time) %in% names(df)))
      return(.tf_no("The subject-ID or time column named on the Data tab is not in this dataset."))
    if (!identical(method, "time_since_baseline") && !any(is.finite(.tf_num(x))))
      return(.tf_no("This needs a numeric column to compare against baseline."))
    return(.tf_ok())
  }
  if (identical(req, "partner")) {
    with <- spec$with %||% NA_character_
    if (is.na(with) || !nzchar(with)) return(.tf_no("Choose the second column to combine with."))
    if (is.null(df) || !(with %in% names(df)))
      return(.tf_no(sprintf("Column '%s' is not in this dataset.", with)))
    if (identical(with, spec$variable %||% ""))
      return(.tf_no("Choose a different column - a variable cannot be combined with itself."))
    return(.tf_ok())
  }
  if (identical(req, "columns")) {
    cols <- as.character(spec$columns %||% character(0))
    if (length(cols) < 2) return(.tf_no("Choose at least two columns to combine."))
    if (is.null(df) || !all(cols %in% names(df)))
      return(.tf_no("Some of the chosen columns are not in this dataset."))
    return(.tf_ok())
  }
  if (identical(req, "any")) {
    if (!length(x)) return(.tf_no("This column is empty."))
    return(.tf_ok())
  }
  if (identical(req, "date")) {
    d <- .tf_as_date(x)
    if (!any(!is.na(d))) return(.tf_no("This column does not hold recognisable dates."))
    return(.tf_ok())
  }
  if (identical(req, "categorical")) {
    if (!is.null(type) && !is_categorical_type(type) && !type %in% c("text"))
      return(.tf_no(sprintf("This applies to categories, and '%s' is %s.", spec$variable %||% "the column", type)))
    if (!length(stats::na.omit(unique(as.character(x)))))
      return(.tf_no("This column has no non-missing values."))
    return(.tf_ok())
  }

  # numeric families
  v <- .tf_finite(.tf_num(x))
  if (!length(v)) return(.tf_no("This column has no usable numeric values."))
  if (length(unique(v)) < 2 && method %in% c("zscore", "scale_sd", "minmax",
                                             "robust_scale", "boxcox", "yeojohnson",
                                             "inverse_normal", "bin_quantile"))
    return(.tf_no("Every value is the same, so this transformation has nothing to work with."))

  if (identical(req, "positive") && any(v <= 0))
    return(.tf_no(sprintf("Needs values above zero; %d value(s) here are zero or negative. Try log of (x + 1), log of (x + c), or Yeo-Johnson.",
                          sum(v <= 0))))
  if (identical(req, "nonnegative") && any(v < 0))
    return(.tf_no(sprintf("Needs values of zero or more; %d value(s) here are negative. Try the cube root or Yeo-Johnson.",
                          sum(v < 0))))
  if (identical(req, "proportion")) {
    if (isTRUE(spec$percent)) v <- v / 100
    if (any(v < 0 | v > 1))
      return(.tf_no("Needs proportions between 0 and 1. Tick the percentage option if this column is 0-100."))
  }
  .tf_ok()
}

# --- lambda estimation ------------------------------------------------------

# Profile log-likelihood for Box-Cox at one lambda (Gaussian errors, no
# covariates - the intercept-only case a variable-level transform implies).
.tf_boxcox_ll <- function(v, lambda) {
  n <- length(v)
  y <- if (abs(lambda) < 1e-9) log(v) else (v^lambda - 1) / lambda
  s2 <- sum((y - mean(y))^2) / n
  if (!is.finite(s2) || s2 <= 0) return(-Inf)
  -n / 2 * log(s2) + (lambda - 1) * sum(log(v))
}

#' Box-Cox lambda by maximum likelihood over a grid, refined locally.
transform_estimate_boxcox <- function(x, limits = c(-5, 5)) {
  v <- .tf_finite(.tf_num(x)); v <- v[v > 0]
  if (length(v) < 3) return(NA_real_)
  grid <- seq(limits[1], limits[2], by = 0.05)
  ll <- vapply(grid, function(l) .tf_boxcox_ll(v, l), numeric(1))
  if (!any(is.finite(ll))) return(NA_real_)
  best <- grid[which.max(ll)]
  fine <- seq(best - 0.05, best + 0.05, by = 0.001)
  fl <- vapply(fine, function(l) .tf_boxcox_ll(v, l), numeric(1))
  round(fine[which.max(fl)], 3)
}

.tf_yj <- function(v, lambda) {
  pos <- v >= 0
  out <- numeric(length(v))
  if (abs(lambda) < 1e-9) out[pos] <- log(v[pos] + 1)
  else out[pos] <- ((v[pos] + 1)^lambda - 1) / lambda
  if (abs(lambda - 2) < 1e-9) out[!pos] <- -log(-v[!pos] + 1)
  else out[!pos] <- -(((-v[!pos] + 1)^(2 - lambda)) - 1) / (2 - lambda)
  out
}

.tf_yj_ll <- function(v, lambda) {
  n <- length(v)
  y <- .tf_yj(v, lambda)
  s2 <- sum((y - mean(y))^2) / n
  if (!is.finite(s2) || s2 <= 0) return(-Inf)
  -n / 2 * log(s2) + (lambda - 1) * sum(sign(v) * log(abs(v) + 1))
}

#' Yeo-Johnson lambda by maximum likelihood; accepts zero and negative values.
transform_estimate_yeojohnson <- function(x, limits = c(-5, 5)) {
  v <- .tf_finite(.tf_num(x))
  if (length(v) < 3) return(NA_real_)
  grid <- seq(limits[1], limits[2], by = 0.05)
  ll <- vapply(grid, function(l) .tf_yj_ll(v, l), numeric(1))
  if (!any(is.finite(ll))) return(NA_real_)
  best <- grid[which.max(ll)]
  fine <- seq(best - 0.05, best + 0.05, by = 0.001)
  fl <- vapply(fine, function(l) .tf_yj_ll(v, l), numeric(1))
  round(fine[which.max(fl)], 3)
}

# Offsets used by the rank-based inverse-normal scores.
TRANSFORM_INT_CONSTANTS <- c(blom = 3/8, tukey = 1/3, waerden = 0, rankit = 1/2)

# Parse to Date, reusing the app's own tolerant parser (fct_dates.R), which
# returns a list whose $value carries the parsed vector.
.tf_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (exists("sabali_parse_dates", mode = "function")) {
    p <- tryCatch(sabali_parse_dates(x), error = function(e) NULL)
    if (!is.null(p$value)) return(suppressWarnings(as.Date(p$value)))
  }
  suppressWarnings(as.Date(as.character(x)))
}

# --- the transforms themselves ----------------------------------------------

# Returns list(values, params, note) or a character(1) error message. `values`
# may be a data frame when a method produces several columns (dummy).
.transform_apply_one <- function(method, x, spec, df, panel) {
  v <- .tf_num(x)
  p <- spec
  fin <- .tf_finite(v)

  # Percent-scaled proportions are divided once, here, and recorded.
  prop <- function() {
    pv <- if (isTRUE(p$percent)) v / 100 else v
    sq <- as.numeric(p$squeeze %||% 0)
    squeezed <- 0L
    if (is.finite(sq) && sq > 0) {
      lo <- !is.na(pv) & pv <= 0; hi <- !is.na(pv) & pv >= 1
      squeezed <- sum(lo | hi)
      pv[lo] <- sq; pv[hi] <- 1 - sq
    }
    list(pv = pv, squeezed = squeezed)
  }

  switch(
    method,

    # --- distribution ---
    log    = list(values = log(v),        params = list()),
    log2   = list(values = log2(v),       params = list()),
    log10  = list(values = log10(v),      params = list()),
    log1p  = list(values = log1p(v),      params = list()),
    log_offset = {
      off <- as.numeric(p$offset %||% NA_real_)
      if (!is.finite(off)) {
        nz <- fin[fin > 0]
        off <- if (length(nz)) min(nz) / 2 else 1
        # Anything still non-positive after the offset would be NaN; lift clear.
        if (any(fin + off <= 0)) off <- abs(min(fin)) + off
      }
      list(values = log(v + off), params = list(offset = off),
           note = sprintf("Offset %s added before taking logs.", format(off, digits = 4)))
    },
    sqrt   = list(values = sqrt(v),       params = list()),
    cbrt   = list(values = sign(v) * abs(v)^(1/3), params = list()),
    reciprocal = {
      w <- v; w[!is.na(w) & w == 0] <- NA_real_
      list(values = 1 / w, params = list(),
           note = if (any(!is.na(v) & v == 0)) "Zero values became missing (1/0 is undefined)." else "")
    },
    # An even power throws away the sign, so it can only be reversed when the
    # input was non-negative. That is recorded, and transform_inverse() refuses
    # rather than silently returning the wrong sign.
    square = list(values = v^2,
                  params = list(exponent = 2, sign_lost = any(fin < 0))),
    power  = {
      e <- as.numeric(p$exponent %||% 2)
      if (!is.finite(e)) return("Give a numeric exponent.")
      if (e == 0) return("An exponent of zero makes every value 1.")
      y <- v^e
      list(values = y,
           params = list(exponent = e,
                         sign_lost = (e %% 2 == 0) && any(fin < 0)),
           note = if (any(fin < 0) && e %% 1 != 0)
             "A fractional power of a negative value is undefined; those became missing." else "")
    },
    exp    = list(values = exp(v), params = list()),
    boxcox = {
      lam <- as.numeric(p$lambda %||% NA_real_)
      if (!is.finite(lam)) lam <- transform_estimate_boxcox(v)
      if (!is.finite(lam)) return("Could not estimate a Box-Cox lambda for this column.")
      y <- if (abs(lam) < 1e-9) log(v) else (v^lam - 1) / lam
      list(values = y, params = list(lambda = lam),
           note = sprintf("Lambda %s%s.", format(lam, digits = 4),
                          if (is.finite(as.numeric(p$lambda %||% NA))) " (supplied)"
                          else " estimated by maximum likelihood"))
    },
    yeojohnson = {
      lam <- as.numeric(p$lambda %||% NA_real_)
      if (!is.finite(lam)) lam <- transform_estimate_yeojohnson(v)
      if (!is.finite(lam)) return("Could not estimate a Yeo-Johnson lambda for this column.")
      y <- rep(NA_real_, length(v)); ok <- !is.na(v)
      y[ok] <- .tf_yj(v[ok], lam)
      list(values = y, params = list(lambda = lam),
           note = sprintf("Lambda %s estimated by maximum likelihood.", format(lam, digits = 4)))
    },
    arcsine = {
      pr <- prop()
      list(values = asin(sqrt(pr$pv)),
           params = list(percent = isTRUE(p$percent)),
           note = if (isTRUE(p$percent)) "Values divided by 100 first." else "")
    },
    logit = {
      pr <- prop()
      list(values = stats::qlogis(pr$pv),
           params = list(percent = isTRUE(p$percent), squeeze = as.numeric(p$squeeze %||% 0)),
           note = paste(c(if (isTRUE(p$percent)) "Values divided by 100 first.",
                          if (pr$squeezed > 0)
                            sprintf("%d value(s) at exactly 0 or 1 were moved inwards by %s.",
                                    pr$squeezed, format(p$squeeze))), collapse = " "))
    },
    probit = {
      pr <- prop()
      list(values = stats::qnorm(pr$pv),
           params = list(percent = isTRUE(p$percent), squeeze = as.numeric(p$squeeze %||% 0)),
           note = if (pr$squeezed > 0)
             sprintf("%d value(s) at exactly 0 or 1 were moved inwards.", pr$squeezed) else "")
    },

    # --- scaling ---
    zscore = {
      mu <- mean(fin); sd <- stats::sd(fin)
      if (!is.finite(sd) || sd == 0) return("The standard deviation is zero, so a z-score is undefined.")
      list(values = (v - mu) / sd, params = list(centre = mu, scale = sd))
    },
    center = { mu <- mean(fin); list(values = v - mu, params = list(centre = mu)) },
    scale_sd = {
      sd <- stats::sd(fin)
      if (!is.finite(sd) || sd == 0) return("The standard deviation is zero.")
      list(values = v / sd, params = list(scale = sd))
    },
    minmax = {
      lo <- min(fin); hi <- max(fin)
      if (hi == lo) return("Every value is the same, so there is no range to rescale.")
      a <- as.numeric(p$lower %||% 0); b <- as.numeric(p$upper %||% 1)
      list(values = a + (v - lo) / (hi - lo) * (b - a),
           params = list(min = lo, max = hi, lower = a, upper = b))
    },
    robust_scale = {
      md <- stats::median(fin); iqr <- stats::IQR(fin)
      if (!is.finite(iqr) || iqr == 0)
        return("The interquartile range is zero, so a robust scale is undefined.")
      list(values = (v - md) / iqr, params = list(centre = md, scale = iqr))
    },
    maxabs = {
      mx <- max(abs(fin))
      if (mx == 0) return("Every value is zero.")
      list(values = v / mx, params = list(scale = mx))
    },
    index100 = {
      ref <- as.character(p$reference %||% "mean")
      base <- switch(ref, mean = mean(fin), median = stats::median(fin),
                     first = fin[1], max = max(fin), NA_real_)
      if (!is.finite(base) || base == 0)
        return("The reference value is zero or undefined, so an index cannot be formed.")
      list(values = 100 * v / base, params = list(scale = base / 100, reference = ref),
           note = sprintf("100 = the %s (%s).", ref, format(base, digits = 4)))
    },

    # --- rank ---
    rank = list(values = rank(v, na.last = "keep",
                              ties.method = as.character(p$ties %||% "average")),
                params = list(ties = as.character(p$ties %||% "average"))),
    percentile = {
      r <- rank(v, na.last = "keep", ties.method = "average")
      n <- sum(!is.na(v))
      list(values = 100 * (r - 0.5) / n, params = list(n = n))
    },
    inverse_normal = {
      key <- tolower(as.character(p$constant %||% "blom"))
      cst <- unname(TRANSFORM_INT_CONSTANTS[key])
      if (is.na(cst)) return(sprintf("Unknown offset '%s'. Use blom, tukey, waerden or rankit.", key))
      r <- rank(v, na.last = "keep", ties.method = "average")
      n <- sum(!is.na(v))
      list(values = stats::qnorm((r - cst) / (n - 2 * cst + 1)),
           params = list(constant = key, offset = cst, n = n),
           note = sprintf("%s offset (c = %s).", key, format(cst, digits = 3)))
    },

    # --- outliers ---
    winsorise = {
      lo <- as.numeric(p$lower %||% 0.05); hi <- as.numeric(p$upper %||% 0.95)
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi)
        return("The lower percentile must be below the upper one.")
      q <- stats::quantile(fin, c(lo, hi), names = FALSE, type = 7)
      w <- v; w[!is.na(w) & w < q[1]] <- q[1]; w[!is.na(w) & w > q[2]] <- q[2]
      list(values = w, params = list(lower = lo, upper = hi, at = q),
           note = sprintf("Pulled in to [%s, %s]; %d value(s) changed.",
                          format(q[1], digits = 4), format(q[2], digits = 4),
                          sum(!is.na(v) & (v < q[1] | v > q[2]))))
    },
    trim_quantile = {
      lo <- as.numeric(p$lower %||% 0.01); hi <- as.numeric(p$upper %||% 0.99)
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi)
        return("The lower percentile must be below the upper one.")
      q <- stats::quantile(fin, c(lo, hi), names = FALSE, type = 7)
      w <- v; drop <- !is.na(w) & (w < q[1] | w > q[2]); w[drop] <- NA_real_
      list(values = w, params = list(lower = lo, upper = hi, at = q),
           note = sprintf("%d value(s) outside [%s, %s] set to missing.",
                          sum(drop), format(q[1], digits = 4), format(q[2], digits = 4)))
    },
    clip = {
      lo <- as.numeric(p$min %||% NA_real_); hi <- as.numeric(p$max %||% NA_real_)
      if (!is.finite(lo) && !is.finite(hi)) return("Give a lower bound, an upper bound, or both.")
      if (is.finite(lo) && is.finite(hi) && lo >= hi) return("The lower bound must be below the upper one.")
      w <- v
      if (is.finite(lo)) w[!is.na(w) & w < lo] <- lo
      if (is.finite(hi)) w[!is.na(w) & w > hi] <- hi
      list(values = w, params = list(min = lo, max = hi))
    },
    cap_sd = {
      k <- as.numeric(p$k %||% 3)
      mu <- mean(fin); sd <- stats::sd(fin)
      if (!is.finite(sd) || sd == 0) return("The standard deviation is zero.")
      lo <- mu - k * sd; hi <- mu + k * sd
      w <- v; w[!is.na(w) & w < lo] <- lo; w[!is.na(w) & w > hi] <- hi
      list(values = w, params = list(k = k, at = c(lo, hi)),
           note = sprintf("Capped at %s SDs: [%s, %s].", format(k),
                          format(lo, digits = 4), format(hi, digits = 4)))
    },
    cap_mad = {
      k <- as.numeric(p$k %||% 3)
      md <- stats::median(fin); mad <- stats::mad(fin)
      if (!is.finite(mad) || mad == 0)
        return("The median absolute deviation is zero, so a MAD cap is undefined.")
      lo <- md - k * mad; hi <- md + k * mad
      w <- v; w[!is.na(w) & w < lo] <- lo; w[!is.na(w) & w > hi] <- hi
      list(values = w, params = list(k = k, at = c(lo, hi)),
           note = sprintf("Capped at %s MADs: [%s, %s].", format(k),
                          format(lo, digits = 4), format(hi, digits = 4)))
    },

    # --- discretise ---
    bin_equal_width = {
      k <- as.integer(p$bins %||% 4)
      if (!is.finite(k) || k < 2) return("Ask for at least two bins.")
      list(values = cut(v, breaks = k, include.lowest = TRUE, ordered_result = TRUE),
           params = list(bins = k))
    },
    bin_quantile = {
      k <- as.integer(p$bins %||% 4)
      if (!is.finite(k) || k < 2) return("Ask for at least two bins.")
      br <- unique(stats::quantile(fin, probs = seq(0, 1, length.out = k + 1),
                                   names = FALSE, type = 7))
      if (length(br) < 3)
        return(sprintf("Too many tied values to make %d groups of equal size.", k))
      labs <- .tf_quantile_labels(length(br) - 1L)
      list(values = cut(v, breaks = br, include.lowest = TRUE, labels = labs,
                        ordered_result = TRUE),
           params = list(bins = length(br) - 1L, at = br),
           note = if (length(br) - 1L != k)
             sprintf("Ties collapsed the %d requested groups into %d.", k, length(br) - 1L) else "")
    },
    bin_breaks = {
      br <- as.numeric(p$breaks %||% numeric(0))
      if (length(br) < 2) return("Give at least two cutpoints.")
      br <- sort(unique(br))
      # Open the outer edges so no value falls outside the declared bands.
      if (min(fin) < br[1]) br <- c(-Inf, br)
      if (max(fin) > br[length(br)]) br <- c(br, Inf)
      labs <- as.character(p$labels %||% character(0))
      use <- if (length(labs) == length(br) - 1L) labs else NULL
      list(values = cut(v, breaks = br, include.lowest = TRUE, labels = use,
                        ordered_result = TRUE),
           params = list(breaks = br, labels = use %||% character(0)))
    },
    dichotomise = {
      th <- as.numeric(p$threshold %||% NA_real_)
      if (!is.finite(th)) return("Give the threshold to split at.")
      above <- !isFALSE(p$at_or_above)
      hit <- if (above) v >= th else v > th
      lab <- c(sprintf("< %s", format(th, digits = 4)),
               sprintf("%s %s", if (above) ">=" else ">", format(th, digits = 4)))
      list(values = factor(ifelse(is.na(v), NA, ifelse(hit, lab[2], lab[1])), levels = lab),
           params = list(threshold = th, at_or_above = above))
    },

    # --- categorical ---
    collapse_rare = {
      chr <- as.character(x); chr[!nzchar(trimws(chr))] <- NA
      tb <- table(chr)
      pct <- as.numeric(p$min_pct %||% NA_real_)
      cut_n <- if (is.finite(pct)) pct / 100 * sum(tb) else as.numeric(p$min_n %||% 10)
      other <- as.character(p$other_label %||% "Other")
      rare <- names(tb)[tb < cut_n]
      out <- chr; out[!is.na(out) & out %in% rare] <- other
      keep <- c(setdiff(names(sort(tb, decreasing = TRUE)), rare),
                if (length(rare)) other)
      list(values = factor(out, levels = keep),
           params = list(min_n = cut_n, other_label = other, collapsed = rare),
           note = if (length(rare))
             sprintf("%d categor%s below %s observations grouped as '%s'.",
                     length(rare), if (length(rare) == 1) "y" else "ies",
                     format(cut_n), other)
           else "No category was rare enough to group.")
    },
    recode_levels = {
      map <- p$map %||% list()
      if (!length(map)) return("Give at least one old-to-new mapping.")
      chr <- as.character(x)
      for (from in names(map)) chr[!is.na(chr) & chr == from] <- as.character(map[[from]])
      list(values = factor(chr), params = list(map = map),
           note = sprintf("%d categor%s remapped.", length(map),
                          if (length(map) == 1) "y" else "ies"))
    },
    relevel = {
      ref <- as.character(p$reference %||% NA_character_)
      lv <- factor_levels_of(x)
      if (is.na(ref) || !nzchar(ref)) return("Choose the reference category.")
      if (!(ref %in% lv)) return(sprintf("'%s' is not one of this column's categories.", ref))
      list(values = stats::relevel(factor(as.character(x), levels = lv), ref = ref),
           params = list(reference = ref))
    },
    to_ordinal = {
      ord <- as.character(p$order %||% character(0))
      lv <- factor_levels_of(x)
      if (!length(ord)) ord <- lv                      # already-sorted order
      missing_lv <- setdiff(lv, ord)
      if (length(missing_lv))
        return(sprintf("The order given leaves out: %s.", paste(missing_lv, collapse = ", ")))
      list(values = factor(as.character(x), levels = ord, ordered = TRUE),
           params = list(order = ord))
    },
    ordinal_score = {
      lv <- if (is.factor(x)) levels(x) else factor_levels_of(x)
      sc <- as.numeric(p$scores %||% numeric(0))
      if (!length(sc)) sc <- seq_along(lv)
      if (length(sc) != length(lv))
        return(sprintf("Give one score per category (%d needed, %d given).", length(lv), length(sc)))
      list(values = sc[match(as.character(x), lv)],
           params = list(levels = lv, scores = sc),
           note = if (identical(sc, as.numeric(seq_along(lv))))
             "Scored 1, 2, 3 ... which assumes the categories are equally spaced." else "")
    },
    dummy = {
      lv <- factor_levels_of(x)
      if (length(lv) < 2) return("This column has fewer than two categories.")
      use <- if (isTRUE(p$drop_first)) lv[-1] else lv
      out <- as.data.frame(lapply(use, function(l) as.integer(as.character(x) == l)),
                           stringsAsFactors = FALSE)
      names(out) <- gsub("[^A-Za-z0-9]+", "_", use)
      list(values = out, params = list(levels = use, drop_first = isTRUE(p$drop_first)),
           note = sprintf("%d indicator column(s) created.", length(use)))
    },

    # --- missing ---
    impute_mean = {
      if (!any(is.na(v))) return("Nothing is missing in this column.")
      mu <- mean(fin); w <- v; w[is.na(w)] <- mu
      list(values = w, params = list(value = mu, n_filled = sum(is.na(v))),
           note = sprintf("%d missing value(s) set to the mean (%s). Single imputation understates uncertainty.",
                          sum(is.na(v)), format(mu, digits = 4)))
    },
    impute_median = {
      if (!any(is.na(v))) return("Nothing is missing in this column.")
      md <- stats::median(fin); w <- v; w[is.na(w)] <- md
      list(values = w, params = list(value = md, n_filled = sum(is.na(v))),
           note = sprintf("%d missing value(s) set to the median (%s). Single imputation understates uncertainty.",
                          sum(is.na(v)), format(md, digits = 4)))
    },
    impute_mode = {
      chr <- as.character(x); chr[!nzchar(trimws(chr))] <- NA
      if (!any(is.na(chr))) return("Nothing is missing in this column.")
      mo <- .tf_mode(chr); w <- chr; w[is.na(w)] <- mo
      list(values = factor(w), params = list(value = mo, n_filled = sum(is.na(chr))),
           note = sprintf("%d missing value(s) set to '%s'. Single imputation understates uncertainty.",
                          sum(is.na(chr)), mo))
    },
    impute_constant = {
      val <- p$value %||% 0
      chr_col <- !is.numeric(x) && !any(is.finite(v))
      if (chr_col) {
        w <- as.character(x); n <- sum(is.na(w)); w[is.na(w)] <- as.character(val)
        return(list(values = factor(w), params = list(value = val, n_filled = n),
                    note = sprintf("%d missing value(s) set to '%s'.", n, val)))
      }
      num <- as.numeric(val)
      if (!is.finite(num)) return("Give a numeric fill value for a numeric column.")
      n <- sum(is.na(v)); w <- v; w[is.na(w)] <- num
      list(values = w, params = list(value = num, n_filled = n),
           note = sprintf("%d missing value(s) set to %s.", n, format(num)))
    },
    missing_indicator = {
      miss <- is.na(x) | !nzchar(trimws(as.character(x)))
      list(values = as.integer(miss), params = list(n_missing = sum(miss)),
           note = sprintf("1 = value was missing (%d of %d).", sum(miss), length(miss)))
    },

    # --- derived ---
    ratio = {
      den <- .tf_num(df[[p$with]])
      den[!is.na(den) & den == 0] <- NA_real_
      list(values = v / den, params = list(with = p$with),
           note = "Zero denominators became missing.")
    },
    difference = list(values = v - .tf_num(df[[p$with]]), params = list(with = p$with)),
    sum_columns = {
      cols <- as.character(p$columns)
      mat <- vapply(cols, function(cc) .tf_num(df[[cc]]), numeric(nrow(df)))
      list(values = rowSums(mat, na.rm = isTRUE(p$na_rm)),
           params = list(columns = cols, na_rm = isTRUE(p$na_rm)),
           note = sprintf("Sum of %s.", paste(cols, collapse = " + ")))
    },
    mean_columns = {
      cols <- as.character(p$columns)
      mat <- vapply(cols, function(cc) .tf_num(df[[cc]]), numeric(nrow(df)))
      list(values = rowMeans(mat, na.rm = isTRUE(p$na_rm)),
           params = list(columns = cols, na_rm = isTRUE(p$na_rm)),
           note = sprintf("Mean of %s.", paste(cols, collapse = ", ")))
    },
    bmi = {
      h <- .tf_num(df[[p$with]])
      unit <- tolower(as.character(p$height_unit %||% "cm"))
      hm <- if (identical(unit, "cm")) h / 100 else h
      hm[!is.na(hm) & hm <= 0] <- NA_real_
      list(values = v / hm^2, params = list(with = p$with, height_unit = unit),
           note = sprintf("Weight (kg) / height (m)^2, height read from '%s' in %s.",
                          p$with, unit))
    },
    zscore_by_group = {
      g <- as.character(df[[p$with]])
      out <- rep(NA_real_, length(v))
      for (lev in unique(g[!is.na(g)])) {
        idx <- which(!is.na(g) & g == lev)
        gv <- .tf_finite(v[idx])
        s <- stats::sd(gv)
        if (is.finite(s) && s > 0) out[idx] <- (v[idx] - mean(gv)) / s
      }
      list(values = out, params = list(with = p$with),
           note = sprintf("Standardised within each level of '%s'; groups with no spread are missing.",
                          p$with))
    },

    # --- longitudinal ---
    change_from_baseline = ,
    pct_change_from_baseline = {
      b <- .tf_baseline_values(v, df, panel, p$baseline %||% NA_character_)
      if (is.character(b)) return(b)
      if (identical(method, "change_from_baseline"))
        list(values = v - b$base, params = b$params,
             note = sprintf("Each row minus that subject's %s value.", b$label))
      else {
        den <- b$base; den[!is.na(den) & den == 0] <- NA_real_
        list(values = 100 * (v - den) / den, params = b$params,
             note = sprintf("Percent change from that subject's %s value; zero baselines are missing.",
                            b$label))
      }
    },
    lag_within = ,
    lead_within = ,
    cumsum_within = {
      ord <- .tf_panel_order(df, panel)
      if (is.character(ord)) return(ord)
      n <- as.integer(p$n %||% 1)
      out <- rep(NA_real_, length(v))
      for (idx in ord$groups) {
        vv <- v[idx]
        out[idx] <- switch(
          method,
          lag_within  = c(rep(NA_real_, min(n, length(vv))),
                          utils::head(vv, max(0, length(vv) - n))),
          lead_within = c(utils::tail(vv, max(0, length(vv) - n)),
                          rep(NA_real_, min(n, length(vv)))),
          cumsum_within = cumsum(ifelse(is.na(vv), 0, vv)))
      }
      list(values = out, params = list(n = n, id = panel$id, time = panel$time),
           note = sprintf("Within each '%s', ordered by '%s'.", panel$id, panel$time))
    },
    time_since_baseline = {
      ord <- .tf_panel_order(df, panel)
      if (is.character(ord)) return(ord)
      tt <- .tf_as_date(df[[panel$time]])
      numeric_time <- !any(!is.na(tt))
      base_t <- if (numeric_time) .tf_num(df[[panel$time]]) else as.numeric(tt)
      if (!any(is.finite(base_t)))
        return(sprintf("Column '%s' is neither a date nor a number, so elapsed time cannot be worked out. Use a visit date or a numeric time.",
                       panel$time))
      out <- rep(NA_real_, length(base_t))
      for (idx in ord$groups) out[idx] <- base_t[idx] - min(base_t[idx], na.rm = TRUE)
      u <- tolower(as.character(p$units %||% "days"))
      div <- switch(u, days = 1, weeks = 7, months = 30.4375, years = 365.25, 1)
      list(values = out / div, params = list(units = u, id = panel$id, time = panel$time),
           note = sprintf("Elapsed %s since each subject's first '%s'.", u, panel$time))
    },

    # --- datetime ---
    date_year    = list(values = as.integer(format(.tf_as_date(x), "%Y")), params = list()),
    date_month   = list(values = factor(as.integer(format(.tf_as_date(x), "%m")),
                                        levels = 1:12, ordered = TRUE), params = list()),
    date_quarter = list(values = factor(paste0("Q", (as.integer(format(.tf_as_date(x), "%m")) - 1) %/% 3 + 1),
                                        levels = paste0("Q", 1:4), ordered = TRUE), params = list()),
    date_week    = list(values = as.integer(format(.tf_as_date(x), "%V")), params = list()),
    date_weekday = list(values = factor(weekdays(.tf_as_date(x)),
                                        levels = c("Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday",
                                                   "Sunday")), params = list()),
    date_doy     = list(values = as.integer(format(.tf_as_date(x), "%j")), params = list()),
    date_diff = {
      d1 <- .tf_as_date(x); d2 <- .tf_as_date(df[[p$with]])
      u <- tolower(as.character(p$units %||% "days"))
      div <- switch(u, days = 1, weeks = 7, months = 30.4375, years = 365.25, 1)
      list(values = as.numeric(d2 - d1) / div, params = list(with = p$with, units = u),
           note = sprintf("'%s' minus this column, in %s.", p$with, u))
    },
    age_from_date = {
      d1 <- .tf_as_date(x); d2 <- .tf_as_date(df[[p$with]])
      yrs <- as.integer(format(d2, "%Y")) - as.integer(format(d1, "%Y"))
      before <- (as.integer(format(d2, "%m")) < as.integer(format(d1, "%m"))) |
        (as.integer(format(d2, "%m")) == as.integer(format(d1, "%m")) &
           as.integer(format(d2, "%d")) < as.integer(format(d1, "%d")))
      list(values = yrs - as.integer(before), params = list(with = p$with),
           note = sprintf("Completed years between this column and '%s'.", p$with))
    },

    sprintf("'%s' is not a transformation this app knows.", method))
}

# Ordinal bin labels an analyst will recognise (tertiles, quartiles, ...).
.tf_quantile_labels <- function(k) {
  nm <- c("2" = "Half", "3" = "Tertile", "4" = "Quartile",
          "5" = "Quintile", "6" = "Sextile", "10" = "Decile")
  base <- unname(nm[as.character(k)])
  if (is.na(base)) base <- sprintf("Group")
  paste(base, seq_len(k))
}

# Row order within each subject, for the longitudinal transforms.
.tf_panel_order <- function(df, panel) {
  if (is.null(panel) || !nzchar(panel$id %||% "") || !nzchar(panel$time %||% ""))
    return("This needs a subject-ID and a time column, chosen on the Data tab.")
  if (!all(c(panel$id, panel$time) %in% names(df)))
    return("The subject-ID or time column is not in this dataset.")
  ids <- as.character(df[[panel$id]])
  tt  <- df[[panel$time]]
  key <- if (any(!is.na(.tf_as_date(tt)))) as.numeric(.tf_as_date(tt)) else
    suppressWarnings(as.numeric(as.factor(tt)))
  groups <- lapply(split(seq_len(nrow(df)), ids), function(idx) idx[order(key[idx])])
  list(groups = unname(groups), key = key)
}

# Each row's baseline value for its subject: the first visit, or a named one.
.tf_baseline_values <- function(v, df, panel, baseline) {
  ord <- .tf_panel_order(df, panel)
  if (is.character(ord)) return(ord)
  base <- rep(NA_real_, length(v))
  named <- !is.na(baseline) && nzchar(baseline)
  tlab <- as.character(df[[panel$time]])
  if (named && !(baseline %in% tlab))
    return(sprintf("'%s' is not one of the values in '%s'.", baseline, panel$time))
  for (idx in ord$groups) {
    pick <- if (named) idx[tlab[idx] == baseline][1] else idx[1]
    base[idx] <- if (length(pick) && !is.na(pick)) v[pick] else NA_real_
  }
  list(base = base,
       params = list(id = panel$id, time = panel$time,
                     baseline = if (named) baseline else "first"),
       label = if (named) sprintf("'%s'", baseline) else "first")
}

# --- entry points -----------------------------------------------------------

#' Apply one transformation, returning the widened data frame and a record.
#'
#' Never throws. On failure the data frame comes back untouched with ok = FALSE
#' and a reason the analyst can act on.
#'
#' @param df    the data frame
#' @param spec  a transform_spec(): method, variable, plus parameters
#' @param types named character vector of confirmed measurement types (optional)
#' @param panel the panel/role list from the Data tab (longitudinal methods)
#' @param name  output column name; derived from the variable and method if absent
#' @return list(ok, reason, df, record, added)
apply_transform <- function(df, spec, types = NULL, panel = NULL, name = NULL) {
  fail <- function(reason) list(ok = FALSE, reason = reason, df = df,
                               record = NULL, added = character(0))
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(fail("There is no data to transform."))
  method <- spec$method %||% ""
  var <- spec$variable %||% ""
  m <- transform_method(method)
  if (is.null(m)) return(fail(sprintf("'%s' is not a transformation this app knows.", method)))
  if (!nzchar(var) || !(var %in% names(df)))
    return(fail(sprintf("Column '%s' is not in this dataset.", var)))

  type <- if (!is.null(types) && var %in% names(types)) unname(types[[var]]) else NULL
  gate <- transform_applicable(method, df[[var]], type, spec, df, panel)
  if (!isTRUE(gate$ok)) return(fail(gate$reason))

  res <- tryCatch(.transform_apply_one(method, df[[var]], spec, df, panel),
                  error = function(e) sprintf("This transformation failed: %s", conditionMessage(e)))
  if (is.character(res)) return(fail(res))

  vals <- res$values
  n_in <- sum(!is.na(df[[var]]))

  # A transform that leaves nothing behind has not succeeded, whatever the
  # arithmetic did. The calibration sweep caught two of these reporting success
  # with an entirely empty column (the reciprocal of an all-zero column, and
  # trimming a column that is almost all missing), which would put a useless
  # variable into the analysis menus.
  first_vals <- if (is.data.frame(vals)) vals[[1]] else vals
  if (!length(first_vals) || all(is.na(first_vals)))
    return(fail(paste("This transformation produced no usable values for",
                      sprintf("'%s'.", var))))

  if (is.data.frame(vals)) {                      # dummy: several columns at once
    nms <- transform_output_name(var, method, names(df))
    names(vals) <- paste0(nms, "_", names(vals))
    out <- df
    for (nm in names(vals)) out[[nm]] <- vals[[nm]]
    added <- names(vals)
  } else {
    nm <- name %||% transform_output_name(var, method, names(df))
    if (nm %in% names(df)) nm <- transform_output_name(var, method, names(df))
    out <- df
    out[[nm]] <- vals
    added <- nm
  }

  n_out <- sum(!is.na(if (is.data.frame(vals)) vals[[1]] else vals))
  record <- list(
    method = method, label = m$name, family = m$family, variable = var,
    output = added, produces = m$produces, params = res$params %||% list(),
    n_in = n_in, n_out = n_out, n_lost = max(0L, n_in - n_out),
    invertible = isTRUE(m$invertible),
    note = trimws(paste(c(res$note %||% "", m$note), collapse = " ")))
  list(ok = TRUE, reason = "", df = out, record = record, added = added)
}

#' Apply several transformations in order, each seeing the columns the previous
#' ones added - so log10 then z-score works, and each step is recorded.
#' Failures are collected rather than aborting the run.
apply_transforms <- function(df, specs, types = NULL, panel = NULL) {
  records <- list(); failures <- list(); added <- character(0)
  for (sp in specs) {
    r <- apply_transform(df, sp, types, panel)
    if (isTRUE(r$ok)) {
      df <- r$df
      records[[length(records) + 1L]] <- r$record
      added <- c(added, r$added)
    } else {
      failures[[length(failures) + 1L]] <-
        list(method = sp$method %||% "", variable = sp$variable %||% "", reason = r$reason)
    }
  }
  list(df = df, records = records, failures = failures, added = added,
       ok = length(failures) == 0)
}

#' Reverse a transformation, mapping values on the transformed scale back to the
#' original one. This is what lets a model fitted on log10 viral load be
#' reported as a geometric mean, or a standardised coefficient read in the
#' original units. Returns NULL when the transformation cannot be reversed.
transform_inverse <- function(record, values) {
  if (is.null(record) || !isTRUE(record$invertible)) return(NULL)
  v <- as.numeric(values); p <- record$params %||% list()
  # An even power applied to data containing negatives is not one-to-one; there
  # is no correct answer to give, so give none.
  if (isTRUE(p$sign_lost)) return(NULL)
  back <- switch(
    record$method,
    log = exp(v), log2 = 2^v, log10 = 10^v, log1p = expm1(v),
    log_offset = exp(v) - as.numeric(p$offset),
    sqrt = v^2, cbrt = v^3, reciprocal = 1 / v, square = sqrt(v),
    power = v^(1 / as.numeric(p$exponent)), exp = log(v),
    boxcox = { l <- as.numeric(p$lambda)
               if (abs(l) < 1e-9) exp(v) else (l * v + 1)^(1 / l) },
    yeojohnson = { l <- as.numeric(p$lambda); pos <- v >= 0; o <- numeric(length(v))
                   o[pos] <- if (abs(l) < 1e-9) expm1(v[pos]) else (l * v[pos] + 1)^(1 / l) - 1
                   o[!pos] <- if (abs(l - 2) < 1e-9) 1 - exp(-v[!pos])
                              else 1 - (-(2 - l) * v[!pos] + 1)^(1 / (2 - l))
                   o },
    arcsine = { pr <- sin(v)^2; if (isTRUE(p$percent)) pr * 100 else pr },
    logit   = { pr <- stats::plogis(v); if (isTRUE(p$percent)) pr * 100 else pr },
    probit  = { pr <- stats::pnorm(v); if (isTRUE(p$percent)) pr * 100 else pr },
    zscore  = v * as.numeric(p$scale) + as.numeric(p$centre),
    center  = v + as.numeric(p$centre),
    scale_sd = v * as.numeric(p$scale),
    minmax = { a <- as.numeric(p$lower); b <- as.numeric(p$upper)
               lo <- as.numeric(p$min); hi <- as.numeric(p$max)
               lo + (v - a) / (b - a) * (hi - lo) },
    robust_scale = v * as.numeric(p$scale) + as.numeric(p$centre),
    maxabs = v * as.numeric(p$scale),
    index100 = v * as.numeric(p$scale),
    NULL)
  back
}

# --- reporting --------------------------------------------------------------

#' Transformation records as a display table (Data Check page, report appendix).
transform_record_df <- function(records) {
  if (!length(records))
    return(data.frame(Variable = character(0), Transformation = character(0),
                      `New column` = character(0), Detail = character(0),
                      check.names = FALSE, stringsAsFactors = FALSE))
  data.frame(
    Variable = vapply(records, function(r) r$variable, character(1)),
    Transformation = vapply(records, function(r) r$label, character(1)),
    `New column` = vapply(records, function(r) paste(r$output, collapse = ", "), character(1)),
    Detail = vapply(records, function(r) {
      bits <- c(r$note,
                if (r$n_lost > 0)
                  sprintf("%d value(s) became missing.", r$n_lost))
      trimws(paste(bits, collapse = " "))
    }, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}

#' One methods-section sentence per transformation, for METHODS_APPENDIX.
#' Written so a reader can reproduce the step without seeing the app.
transform_methods_text <- function(records) {
  if (!length(records)) return(character(0))
  vapply(records, function(r) {
    par <- r$params %||% list()
    detail <- if (length(par)) {
      keep <- par[vapply(par, function(z) length(z) >= 1 && length(z) <= 4, logical(1))]
      if (length(keep))
        sprintf(" (%s)", paste(sprintf("%s = %s", names(keep),
                 vapply(keep, function(z) paste(format(unlist(z), digits = 4),
                                                collapse = ", "), character(1))),
                 collapse = "; "))
      else ""
    } else ""
    sprintf("%s was derived from %s by %s%s.",
            paste(r$output, collapse = " and "), r$variable,
            tolower(r$label), detail)
  }, character(1))
}

#' Resolve free text to a catalogue method id.
#'
#' The planner is asked to return catalogue ids, but a model may still describe
#' a transformation in words ("log10 transformation", "ordinal coding"). This
#' maps such a description onto a method the app can actually run, so a
#' suggestion becomes a button rather than a sentence. Returns "" when nothing
#' matches, which the caller shows as advice-only.
transform_match_method <- function(text) {
  if (is.null(text) || !length(text)) return("")
  s <- tolower(trimws(as.character(text)[1]))
  if (!nzchar(s)) return("")
  ids <- transform_method_ids()
  if (s %in% ids) return(s)                       # already an id

  # Ordered longest-first so "log10" wins over "log", "log1p" over "log".
  patterns <- c(
    log10 = "log ?10|log-?10|base ?10|common log",
    log2 = "log ?2|base ?2",
    log1p = "log( of)? ?\\(?x ?\\+ ?1|log1p|log plus one",
    log_offset = "log( of)? ?\\(?x ?\\+ ?c|offset log|log with (an )?offset",
    log = "\\blog\\b|natural log|\\bln\\b|logarithm",
    yeojohnson = "yeo|johnson",
    boxcox = "box ?-? ?cox",
    sqrt = "square root|sqrt",
    cbrt = "cube root|cbrt",
    reciprocal = "reciprocal|inverse of|1 ?/ ?x",
    logit = "logit",
    probit = "probit",
    arcsine = "arcsin|arc-?sine|angular",
    inverse_normal = "inverse[- ]normal|rank[- ]based inverse|van der waerden|blom",
    zscore = "z-?score|standardi[sz]",
    robust_scale = "robust scal|median.{0,10}iqr",
    minmax = "min-?max|rescale to 0|0 ?- ?1 scal",
    center = "centre|center|mean-?cent",
    percentile = "percentile",
    rank = "\\brank\\b",
    winsorise = "winsori[sz]",
    trim_quantile = "trim",
    cap_mad = "cap.{0,12}mad",
    cap_sd = "cap.{0,12}sd|cap.{0,12}standard deviation",
    clip = "clip|clamp",
    bin_quantile = "quantile bin|tertile|quartile|quintile|decile|equal-?size bin",
    bin_equal_width = "equal-?width bin",
    bin_breaks = "cut ?-?point|clinical band|custom bin|categori[sz]e at",
    dichotomise = "dichotomi[sz]|binari[sz]|split at|threshold",
    to_ordinal = "ordinal coding|treat as ordinal|declare an order|ordered factor|ordinal scale",
    ordinal_score = "ordinal score|score the categor",
    collapse_rare = "collapse|group rare|rare categor|other categor",
    recode_levels = "recode|merge categor|relabel",
    relevel = "reference (categor|level)|relevel",
    dummy = "dummy|one-?hot|indicator column",
    impute_median = "impute.{0,15}median|median imput",
    impute_mean = "impute.{0,15}mean|mean imput",
    impute_mode = "impute.{0,15}mode|mode imput",
    impute_constant = "impute.{0,15}constant|fill with",
    missing_indicator = "missing(ness)? (indicator|flag)",
    change_from_baseline = "change from baseline",
    pct_change_from_baseline = "percent change",
    time_since_baseline = "time since baseline|elapsed time",
    bmi = "\\bbmi\\b|body mass",
    ratio = "\\bratio\\b",
    difference = "difference between")
  for (id in names(patterns))
    if (grepl(patterns[[id]], s)) return(id)
  ""
}

#' Which methods could apply to a column right now, with a reason for each that
#' cannot. Lets a UI build an honest menu instead of offering the impossible.
transform_options_for <- function(x, type = NULL, df = NULL, panel = NULL,
                                  variable = NA_character_) {
  lapply(transform_catalogue(), function(m) {
    sp <- transform_spec(m$id, variable = variable)
    g <- transform_applicable(m$id, x, type, sp, df, panel)
    list(id = m$id, name = m$name, family = m$family, note = m$note,
         params = m$params, ok = isTRUE(g$ok), reason = g$reason)
  })
}
