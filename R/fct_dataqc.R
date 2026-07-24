# fct_dataqc.R --------------------------------------------------------------
# Data-readiness engine for Tab 0.  The engine deliberately separates:
#   * raw-import checks (before coercion),
#   * semantic/data-contract checks, and
#   * analysis-readiness checks that require an outcome/predictor context.
#
# It keeps the original `check`, `severity`, `scope`, `message`, and `n`
# columns for compatibility, while adding a scorecard, action fields, review
# status, and privacy-safe evidence.

DQ_SEVERITY_ORDER <- c("error", "warning", "note", "pass")
DQ_SEVERITY_LABEL <- c(error = "ERRORS", warning = "WARNINGS",
                       note = "NOTES", pass = "PASSED CHECKS")
DQ_PRIORITY_ORDER <- c("blocker", "high", "medium", "low", "info")

# The visible scorecard is intentionally explicit.  Some rows contain several
# low-level rules, but every requested data-management domain has a named row.
DQ_SCORECARD <- data.frame(
  point = 1:22,
  id = c(
    "structural_completeness", "schema_integrity", "missingness_levels",
    "missingness_patterns", "subgroup_missingness", "exact_duplicates",
    "key_duplicates", "category_consistency", "sparse_categories",
    "numeric_validity", "expected_ranges", "outliers", "constant_variables",
    "near_zero_variance", "date_integrity", "identifier_integrity",
    "unexpected_types", "correlated_predictors", "class_imbalance",
    "skewed_distributions", "logistic_separation", "modeling_suitability"),
  domain = c(
    "Structure", "Schema", "Missingness", "Missingness", "Missingness",
    "Duplicates", "Duplicates", "Categories", "Categories", "Values",
    "Values", "Values", "Variation", "Variation", "Dates", "Identifiers",
    "Schema", "Model readiness", "Model readiness", "Distributions",
    "Model readiness", "Model readiness"),
  label = c(
    "Structural completeness", "Schema integrity", "Missingness levels",
    "Missingness patterns", "Missingness by subgroup", "Exact duplicate rows",
    "Duplicate keys", "Category consistency", "Empty or sparse categories",
    "Numeric validity", "Expected ranges", "Outliers", "Constant variables",
    "Near-zero variance", "Date integrity", "Identifier integrity",
    "Unexpected data types", "Highly correlated predictors", "Class imbalance",
    "Skewed distributions", "Logistic separation", "Modeling suitability"),
  context = c(rep("always", 4), "contract", rep("always", 12),
              "analysis", "analysis", "always", "analysis", "analysis"),
  stringsAsFactors = FALSE
)

DQ_CHECK_TO_SCORECARD <- c(
  `dataset present` = "structural_completeness",
  `non-empty dataset` = "structural_completeness",
  `empty rows` = "structural_completeness",
  `column names unique` = "schema_integrity",
  `column names valid` = "schema_integrity",
  `required fields` = "schema_integrity",
  `raw coercion loss` = "unexpected_types",
  `invalid text encoding` = "unexpected_types",
  `unexpected data types` = "unexpected_types",
  `overall missingness` = "missingness_levels",
  `column missingness` = "missingness_levels",
  `empty columns` = "missingness_levels",
  `missingness patterns` = "missingness_patterns",
  `missingness by subgroup` = "subgroup_missingness",
  `duplicate rows` = "exact_duplicates",
  `duplicate key` = "key_duplicates",
  `unique id-time` = "key_duplicates",
  `unique subject id` = "key_duplicates",
  `single-observation subjects` = "key_duplicates",
  `redundant columns` = "schema_integrity",
  `disguised missing` = "category_consistency",
  `numbers stored as text` = "unexpected_types",
  `high-cardinality category` = "identifier_integrity",
  `inconsistent categories` = "category_consistency",
  `allowed levels` = "category_consistency",
  `rare categories` = "sparse_categories",
  `empty factor levels` = "sparse_categories",
  `binary has two levels` = "sparse_categories",
  `infinite values` = "numeric_validity",
  `count is non-negative` = "numeric_validity",
  `count is integer` = "numeric_validity",
  `expected ranges` = "expected_ranges",
  `outliers` = "outliers",
  `constant columns` = "constant_variables",
  `near-zero variance` = "near_zero_variance",
  `implausible dates` = "date_integrity",
  `date order` = "date_integrity",
  `identifier integrity` = "identifier_integrity",
  `highly correlated predictors` = "correlated_predictors",
  `class imbalance` = "class_imbalance",
  `skewed distributions` = "skewed_distributions",
  `logistic separation` = "logistic_separation",
  `modeling suitability` = "modeling_suitability"
)

#' Default thresholds for automated checks.  Domain-specific ranges and
#' allowed levels belong in a contract rather than being guessed here.
dq_default_opts <- function() {
  list(
    missing_note = 0.10,
    missing_warn = 0.20,
    missing_error = 0.50,
    rare_level_min = 5,
    high_card_ratio = 0.50,
    high_card_min = 20,
    outlier_k = 3,
    numeric_text_frac = 0.90,
    old_date_year = 1900,
    future_date_tolerance_days = 0,
    nzv_unique_max = 5,
    nzv_freq_prop = 0.95,
    correlation_threshold = 0.90,
    imbalance_min_prop = 0.20,
    skew_note = 1.50,
    skew_warn = 2.50,
    subgroup_gap = 0.20,
    subgroup_min_n = 5,
    pattern_min_pct = 0.10,
    max_correlation_pairs = 30,
    sentinel_values = c("", "na", "n/a", "null", "none", "nil", "missing",
                        "unknown", "unk", "-", "--", "---", ".", "?", "9999",
                        "-99", "-999"),
    include_examples = FALSE
  )
}

#' Build a declarative data contract.
#'
#' `ranges` is a named list with `min` and/or `max`; bounds may be numeric or
#' ISO date strings. `allowed_levels` is a named list of permitted values.
#' `unique_keys` is a list of character vectors, one vector per key.
dq_contract <- function(expected_types = NULL, required = character(),
                        ranges = list(), allowed_levels = list(),
                        identifiers = character(), unique_keys = list(),
                        date_order = list(), subgroup_vars = character(),
                        outcomes = character(), predictors = character()) {
  if (is.character(unique_keys) && length(unique_keys)) unique_keys <- list(unique_keys)
  list(expected_types = expected_types,
       required = unique(as.character(required)),
       ranges = ranges %||% list(),
       allowed_levels = allowed_levels %||% list(),
       identifiers = unique(as.character(identifiers)),
       unique_keys = unique_keys %||% list(),
       date_order = date_order %||% list(),
       subgroup_vars = unique(as.character(subgroup_vars)),
       outcomes = unique(as.character(outcomes)),
       predictors = unique(as.character(predictors)))
}

#' Build analyst-friendly contract defaults from the observed dataset.
#'
#' These are starting values for the Data Check contract editor, not hidden
#' assumptions: every field is initially required, numeric/date bounds cover
#' the observed non-missing values, and categorical levels contain the observed
#' non-missing levels. The editor still lets the analyst clear or change every
#' value before checks are run.
dq_contract_defaults <- function(df, types = NULL) {
  if (!is.data.frame(df)) {
    return(list(required = character(0), ranges = list(),
                allowed_levels = list()))
  }

  nm <- names(df) %||% character(0)
  if (is.null(types) || !length(types)) {
    types <- stats::setNames(
      vapply(seq_along(df), function(i) detect_type(df[[i]], nm[[i]]), character(1)),
      nm)
  }

  display_number <- function(value) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) return("")
    trimws(format(value, digits = 15, trim = TRUE, scientific = FALSE))
  }

  ranges <- list()
  allowed_levels <- list()
  for (col in nm) {
    x <- df[[col]]
    type <- as.character(types[[col]] %||% "")

    if (is_numeric_type(type)) {
      # Never use a factor's integer codes as observed numeric values. A
      # factor may only contribute numeric bounds when its displayed labels
      # are themselves numeric.
      numeric_source <- if (is.factor(x)) as.character(x) else x
      values <- suppressWarnings(as.numeric(numeric_source))
      values <- values[is.finite(values)]
      if (length(values)) {
        ranges[[col]] <- list(min = display_number(min(values)),
                              max = display_number(max(values)))
      }
    } else if (identical(type, "datetime") ||
               inherits(x, c("Date", "POSIXct", "POSIXt"))) {
      values <- .dq_as_date(x)
      values <- values[!is.na(values)]
      if (length(values)) {
        ranges[[col]] <- list(min = as.character(min(values)),
                              max = as.character(max(values)))
      }
    }

    if (is_categorical_type(type)) {
      levels_observed <- factor_levels_of(x)
      if (length(levels_observed)) allowed_levels[[col]] <- levels_observed
    }
  }

  list(required = nm, ranges = ranges, allowed_levels = allowed_levels)
}

#' Build the three editor defaults for one variable and expected type.
#'
#' This is used when an analyst changes a variable's expected type in the
#' interactive contract editor. The values are deliberately recalculated from
#' the observed data so that the controls remain useful after a type change.
dq_contract_column_defaults <- function(df, col, type) {
  empty <- list(min = "", max = "", levels = "")
  if (!is.data.frame(df) || length(col) != 1L || !as.character(col) %in% names(df)) {
    return(empty)
  }

  nm <- names(df)
  types <- stats::setNames(rep("text", length(nm)), nm)
  types[[as.character(col)]] <- as.character(type %||% "text")[[1]]
  observed <- dq_contract_defaults(df, types)
  range <- observed$ranges[[as.character(col)]] %||% list()
  list(
    min = as.character(range$min %||% ""),
    max = as.character(range$max %||% ""),
    levels = paste(observed$allowed_levels[[as.character(col)]] %||% character(0),
                   collapse = ", ")
  )
}

.dq_normalise_contract <- function(contract, df, types, panel) {
  base <- dq_contract()
  if (is.list(contract)) {
    for (field in intersect(names(contract), names(base))) {
      if (!is.null(contract[[field]])) base[[field]] <- contract[[field]]
    }
  }
  nm <- names(df)
  base$required_missing <- setdiff(base$required, nm)
  base$required <- intersect(base$required, nm)
  base$identifiers <- intersect(base$identifiers, nm)
  base$subgroup_vars <- intersect(base$subgroup_vars, nm)
  base$outcomes <- intersect(base$outcomes, nm)
  base$predictors <- intersect(base$predictors, nm)
  if (is.null(base$expected_types)) base$expected_types <- character(0)
  base$expected_types <- base$expected_types[names(base$expected_types) %in% nm]
  base$ranges <- base$ranges[names(base$ranges) %in% nm]
  base$allowed_levels <- base$allowed_levels[names(base$allowed_levels) %in% nm]
  base$unique_keys <- Filter(function(k) all(k %in% nm), base$unique_keys)
  base$date_order <- Filter(function(k) {
    k <- unname(unlist(k)); length(k) >= 2 && all(k[1:2] %in% nm)
  }, base$date_order)

  p <- if (is.list(panel)) panel else list()
  id <- p$id %||% NULL
  time <- p$time %||% NULL
  if (is.character(id) && length(id) == 1 && nzchar(id) && id %in% nm) {
    base$identifiers <- unique(c(base$identifiers, id))
    if (is.character(time) && length(time) == 1 && nzchar(time) && time %in% nm) {
      base$unique_keys <- c(base$unique_keys, list(c(id, time)))
    } else if (identical(p$kind %||% "cross_sectional", "cross_sectional")) {
      base$unique_keys <- c(base$unique_keys, list(id))
    }
  }
  base$unique_keys <- Filter(function(k) length(k) && all(k %in% nm), base$unique_keys)
  if (length(base$expected_types) == 0 && !is.null(types)) {
    # Confirmed types are useful for semantic rules, but they are not treated
    # as an expected raw storage schema unless the caller supplies a contract.
    base$semantic_types <- types
  } else {
    base$semantic_types <- types %||% character(0)
  }
  base
}

# One finding row. Existing consumers may continue to use `check`, `scope`,
# `message`, and `n`; the additional columns support the new workflow.
dq_finding <- function(check, severity, scope, message, n = NA_integer_,
                       denominator = NA_integer_, scorecard_id = NULL,
                       domain = NULL, priority = NULL, issue = NULL,
                       impact = NULL, recommendation = NULL,
                       evidence = NULL, model_scope = NULL) {
  data.frame(
    check = as.character(check)[1], severity = as.character(severity)[1],
    scope = as.character(scope)[1], message = as.character(message)[1],
    n = suppressWarnings(as.integer(n)[1]),
    denominator = suppressWarnings(as.integer(denominator)[1]),
    scorecard_id = as.character(scorecard_id %||% unname(DQ_CHECK_TO_SCORECARD[check]))[1],
    domain = as.character(domain %||% "Data quality")[1],
    priority = as.character(priority %||% "medium")[1],
    issue = as.character(issue %||% "")[1],
    impact = as.character(impact %||% "")[1],
    recommendation = as.character(recommendation %||% "")[1],
    evidence = as.character(evidence %||% message)[1],
    model_scope = as.character(model_scope %||% "")[1],
    stringsAsFactors = FALSE
  )
}

.dq_numeric_parse_frac <- function(x) {
  ch <- trimws(as.character(x))
  ch <- ch[!is.na(ch) & nzchar(ch)]
  if (!length(ch)) return(0)
  mean(!is.na(suppressWarnings(as.numeric(ch))))
}

.dq_nonmissing <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

.dq_slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", as.character(x))
  x <- gsub("^-+|-+$", "", x)
  tolower(x)
}

.dq_type_matches <- function(x, expected) {
  if (is.null(expected) || !nzchar(expected)) return(TRUE)
  if (expected %in% c("continuous", "count")) return(is.numeric(x))
  if (expected == "datetime") return(inherits(x, c("Date", "POSIXct", "POSIXt")) ||
                                        is.character(x))
  if (expected %in% c("binary", "nominal", "ordinal", "identifier", "text"))
    return(is.atomic(x) && !is.list(x))
  TRUE
}

.dq_as_date <- function(x) {
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "Date")) return(x)
  suppressWarnings(as.Date(as.character(x)))
}

.dq_bound <- function(value, x) {
  if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(as.character(value)))
    return(NULL)
  if (inherits(x, c("Date", "POSIXt"))) return(.dq_as_date(value))
  if (is.numeric(x)) return(suppressWarnings(as.numeric(value)))
  as.character(value)
}

.dq_skew <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x); s <- stats::sd(x)
  if (n < 10 || !is.finite(s) || s == 0) return(NA_real_)
  mean((x - mean(x))^3) / s^3
}

.dq_target <- function(scope) {
  if (is.null(scope) || !nzchar(scope) || scope %in% c("dataset", "(engine)"))
    "The dataset"
  else sprintf("Variable '%s'", scope)
}

.dq_guidance <- function(check, severity, scope, message, n, denominator,
                         model_scope = "") {
  target <- .dq_target(scope)
  domain <- switch(check,
    `duplicate rows` = "Duplicates", `duplicate key` = "Duplicates",
    `unique id-time` = "Duplicates", `unique subject id` = "Duplicates",
    `column missingness` = "Missingness", `overall missingness` = "Missingness",
    `missingness patterns` = "Missingness", `missingness by subgroup` = "Missingness",
    `constant columns` = "Variation", `near-zero variance` = "Variation",
    `outliers` = "Values", `expected ranges` = "Values",
    `implausible dates` = "Dates", `date order` = "Dates",
    `identifier integrity` = "Identifiers", `highly correlated predictors` = "Model readiness",
    `class imbalance` = "Model readiness", `logistic separation` = "Model readiness",
    `modeling suitability` = "Model readiness", `high-cardinality category` = "Model readiness",
    `skewed distributions` = "Distributions",
    `rare categories` = "Categories", `empty factor levels` = "Categories",
    `binary has two levels` = "Categories", `inconsistent categories` = "Categories",
    `allowed levels` = "Categories", `disguised missing` = "Categories",
    `numbers stored as text` = "Schema", `raw coercion loss` = "Schema",
    `unexpected data types` = "Schema", `invalid text encoding` = "Schema",
    `column names unique` = "Schema", `column names valid` = "Schema",
    `required fields` = "Schema", `redundant columns` = "Schema",
    `dataset present` = "Structure", `non-empty dataset` = "Structure",
    `empty rows` = "Structure", `empty columns` = "Missingness",
    `infinite values` = "Values", `count is non-negative` = "Values",
    `count is integer` = "Values", "Data quality")

  priority <- if (severity == "pass") "info" else if (severity == "error") "blocker"
              else if (check %in% c("outliers", "rare categories", "empty factor levels",
                                    "skewed distributions", "single-observation subjects")) "medium"
              else if (severity == "warning") "high" else "low"

  issue <- switch(check,
    `column missingness` = sprintf("%s contains missing values.", target),
    `overall missingness` = "The dataset contains a material amount of missing data.",
    `missingness patterns` = "Missing values occur in repeated combinations across variables.",
    `missingness by subgroup` = "Missingness differs materially between subgroups.",
    `duplicate rows` = "Some observations are exact duplicates.",
    `duplicate key` = "The declared observation key is not unique.",
    `unique id-time` = "The subject/time key is not unique.",
    `unique subject id` = "The identifier repeats where one row per subject was expected.",
    `constant columns` = sprintf("%s has no usable variation.", target),
    `near-zero variance` = sprintf("%s is dominated by one value and has near-zero variance.", target),
    `outliers` = sprintf("%s contains statistically unusual values.", target),
    `expected ranges` = sprintf("%s contains values outside the declared range.", target),
    `infinite values` = sprintf("%s contains infinite values.", target),
    `count is non-negative` = sprintf("%s contains negative count values.", target),
    `count is integer` = sprintf("%s contains non-integer count values.", target),
    `implausible dates` = sprintf("%s contains implausible dates.", target),
    `date order` = "One or more date intervals are reversed or have an invalid order.",
    `identifier integrity` = sprintf("%s does not meet the identifier rules.", target),
    `inconsistent categories` = sprintf("%s has category labels that differ only by case or whitespace.", target),
    `allowed levels` = sprintf("%s contains values outside the declared categories.", target),
    `rare categories` = sprintf("%s contains sparse categories.", target),
    `empty factor levels` = sprintf("%s contains factor levels with no observations.", target),
    `binary has two levels` = sprintf("%s is marked binary but does not have exactly two observed levels.", target),
    `class imbalance` = "The selected outcome is imbalanced across classes.",
    `highly correlated predictors` = "Selected predictors are highly correlated.",
    `logistic separation` = "The selected logistic model shows perfect or quasi-separation.",
    `skewed distributions` = sprintf("%s has a strongly skewed distribution.", target),
    `modeling suitability` = sprintf("%s may be unsuitable for the selected analysis.", target),
    `numbers stored as text` = sprintf("%s contains numeric-looking values stored as text.", target),
    `high-cardinality category` = sprintf("%s has many distinct values relative to its row count.", target),
    `raw coercion loss` = sprintf("Values in %s were lost or became missing during type coercion.", target),
    `unexpected data types` = sprintf("%s does not match the expected data type.", target),
    `invalid text encoding` = sprintf("%s contains text that cannot be validated as UTF-8.", target),
    `required fields` = "A required field is missing or contains missing values.",
    `column names unique` = "Column names are duplicated.",
    `column names valid` = "One or more column names may cause lookup or formula problems.",
    `redundant columns` = "Two or more columns contain identical information.",
    `empty columns` = sprintf("%s is entirely missing.", target),
    `empty rows` = "Some rows contain no usable values.",
    `dataset present` = "No active dataset is loaded.",
    `non-empty dataset` = "The dataset has no usable rows or columns.",
    `infinite values` = sprintf("%s contains non-finite numeric values.", target),
    "An automated data-quality issue was detected.")

  impact <- switch(check,
    `column missingness` = "Complete-case analyses may lose observations and estimates may be biased if missingness differs by outcome or subgroup.",
    `overall missingness` = "Many analyses may lose observations or require an explicit missing-data strategy.",
    `missingness patterns` = "Listwise deletion may disproportionately remove records with particular profiles of missing variables.",
    `missingness by subgroup` = "Subgroup comparisons and model estimates may be biased if missingness is related to group membership.",
    `duplicate rows` = "Counts, standard errors, and model weights may be inflated.",
    `duplicate key` = "Repeated observations may be counted as independent or overwrite each other during joins.",
    `unique id-time` = "Repeated-measures and longitudinal models may use the wrong observation structure.",
    `constant columns` = "The variable contributes no information and can cause singular model matrices.",
    `near-zero variance` = "The variable can destabilise models and produce unreliable estimates.",
    `outliers` = "Extreme values can materially change means, regression coefficients, and p-values.",
    `expected ranges` = "The values may be data-entry errors or may invalidate the planned analysis.",
    `class imbalance` = "Accuracy and conventional model diagnostics can be misleading; the minority class may be poorly estimated.",
    `highly correlated predictors` = "Coefficient estimates may be unstable and difficult to interpret.",
    `logistic separation` = "Maximum-likelihood logistic estimates may be infinite, with unreliable standard errors and predictions.",
    `skewed distributions` = "Means, normality-based tests, and linear-model residuals may be sensitive to the distribution.",
    `modeling suitability` = "The selected method may fail, overfit, or produce an interpretation that does not match the variable's role.",
    `identifier integrity` = "Joins, deduplication, and subject-level analyses may be incorrect.",
    `date order` = "Durations and time-to-event analyses may be negative or incorrectly calculated.",
    `numbers stored as text` = "Sorting, summaries, ranges, and statistical models may treat numeric measurements as categories.",
    `high-cardinality category` = "The field may be a hidden identifier or free-text field and may overfit a model.",
    `raw coercion loss` = "The typed dataset no longer represents the uploaded values for those records.",
    `unexpected data types` = "Analyses may use the wrong measurement scale or silently coerce values.",
    `required fields` = "Required records cannot be reliably identified or analysed.",
    `invalid text encoding` = "Names, categories, or values may be corrupted during import or reporting.",
    `rare categories` = "Group comparisons and regression coefficients may be unstable.",
    `empty factor levels` = "Model matrices may contain unused levels and confusing output.",
    `binary has two levels` = "Binary tests and models may fail or use an unintended contrast.",
    `disguised missing` = "Missing observations may be treated as a real category or numeric value.",
    `inconsistent categories` = "Counts and joins may split one real category into several labels.",
    `empty columns` = "The variable cannot contribute to any analysis.",
    `infinite values` = "Most statistical functions cannot process these values correctly.",
    `count is non-negative` = "Negative counts are impossible under the declared measurement definition.",
    `count is integer` = "Fractional counts violate the declared measurement definition.",
    `dataset present` = "No downstream analysis can be performed until data is loaded.",
    `non-empty dataset` = "There are no observations available for analysis.",
    `empty rows` = "These records add no information and may affect row counts.",
    "The issue may affect interpretation or analysis readiness.")

  recommendation <- switch(check,
    `column missingness` = "Review missingness by outcome, time, and subgroup. Pre-specify whether to retain, impute, or exclude this variable.",
    `overall missingness` = "Profile missingness before modelling and document the chosen handling strategy.",
    `missingness patterns` = "Inspect the affected variables together and assess whether a planned imputation or data-collection correction is needed.",
    `missingness by subgroup` = "Investigate the data-collection process for the affected subgroups before comparing or modelling them.",
    `duplicate rows` = "Confirm whether duplicates are legitimate repeated records. Deduplicate only with a documented key and rule.",
    `duplicate key` = "Confirm the observation key and resolve or document conflicting records before modelling.",
    `unique id-time` = "Check the subject and time fields, then resolve duplicate visits or define a valid aggregation rule.",
    `constant columns` = "Exclude it from modelling unless the constant value is a required audit field.",
    `near-zero variance` = "Review the variable's purpose and consider excluding or combining it after subject-matter review.",
    `outliers` = "Verify the source record and domain limits. Correct confirmed entry errors; otherwise use a documented robust or sensitivity analysis.",
    `expected ranges` = "Verify the source record and update the contract only if the declared range is wrong.",
    `class imbalance` = "Report class counts, consider appropriate metrics or methods, and avoid relying on accuracy alone.",
    `highly correlated predictors` = "Review the predictor set, prespecify one variable or a composite, or use a method designed for collinearity.",
    `logistic separation` = "Check sparse levels and coding; consider penalised or exact methods and report the limitation.",
    `skewed distributions` = "Inspect a plot, verify units, and consider transformation, robust methods, or a distribution-appropriate model.",
    `modeling suitability` = "Confirm the variable's role, type, missingness, and measurement meaning before running the selected model.",
    `identifier integrity` = "Check completeness, uniqueness, formatting, and the intended one-row-per-subject rule.",
    `date order` = "Verify date definitions and units, then correct or quarantine invalid intervals.",
    `numbers stored as text` = "Confirm the intended measurement type on the Data tab and inspect failed conversions before proceeding.",
    `high-cardinality category` = "Confirm whether this is an identifier or free text; exclude it from modelling unless a categorical role is substantively justified.",
    `raw coercion loss` = "Return to the raw field, correct the source or conversion rule, and re-run the data check.",
    `unexpected data types` = "Compare the uploaded field with the data contract and correct the source or expected type explicitly.",
    `required fields` = "Complete the field, revise the contract with a documented reason, or exclude affected records under a pre-specified rule.",
    `invalid text encoding` = "Re-save the source as UTF-8 or apply a documented encoding conversion before analysis.",
    `rare categories` = "Verify labels and consider combining levels only when substantively defensible; otherwise use an appropriate sparse-data method.",
    `empty factor levels` = "Drop unused levels after import and confirm the reference level before modelling.",
    `binary has two levels` = "Correct the type or resolve unexpected levels before using binary tests or models.",
    `disguised missing` = "Map confirmed sentinel codes to NA in a separate, audited cleaning step.",
    `inconsistent categories` = "Review and standardise labels in a separate cleaned dataset, retaining the original values.",
    `empty columns` = "Remove it from the analysis dataset or correct the import/source.",
    `infinite values` = "Trace the calculation or import that created them and convert confirmed invalid values to NA under a documented rule.",
    `count is non-negative` = "Verify the source and correct impossible values before modelling counts.",
    `count is integer` = "Verify the source and measurement definition; do not silently round values.",
    `dataset present` = "Load a dataset on the Data tab.",
    `non-empty dataset` = "Load a file containing at least one row and one column.",
    `empty rows` = "Confirm whether these are accidental blank records and remove them only in a separate, audited cleaning step.",
    "Review the source data and document the decision before analysis.")

  if (severity == "pass") {
    issue <- "No issue detected by this rule."
    impact <- "No automated concern was identified."
    recommendation <- "No action is required; retain the pass result in the audit trail."
    priority <- "info"
  }
  list(domain = domain, priority = priority, issue = issue, impact = impact,
       recommendation = recommendation, model_scope = model_scope,
       denominator = denominator, n = n, evidence = message)
}

.dq_enrich <- function(findings, n_rows, n_cols) {
  if (!nrow(findings)) return(findings)
  for (i in seq_len(nrow(findings))) {
    g <- .dq_guidance(findings$check[i], findings$severity[i], findings$scope[i],
                      findings$message[i], findings$n[i], findings$denominator[i],
                      findings$model_scope[i])
    findings$scorecard_id[i] <- unname(DQ_CHECK_TO_SCORECARD[findings$check[i]]) %||%
      findings$scorecard_id[i]
    findings$domain[i] <- g$domain
    findings$priority[i] <- g$priority
    findings$issue[i] <- g$issue
    findings$impact[i] <- g$impact
    findings$recommendation[i] <- g$recommendation
    findings$evidence[i] <- g$evidence
    if (is.na(findings$denominator[i]) || findings$denominator[i] <= 0) {
      if (findings$scope[i] %in% names(findings) ||
          (findings$scope[i] != "dataset" && findings$scope[i] != "(engine)" &&
           findings$scope[i] %in% character(0))) {
        findings$denominator[i] <- n_rows
      } else if (findings$check[i] == "overall missingness") {
        findings$denominator[i] <- n_rows * n_cols
      } else if (findings$check[i] %in% c("column missingness", "outliers",
                                          "expected ranges", "implausible dates",
                                          "missingness by subgroup")) {
        findings$denominator[i] <- n_rows
      }
    }
  }
  findings$affected_n <- findings$n
  findings$affected_pct <- ifelse(!is.na(findings$n) & !is.na(findings$denominator) &
                                    findings$denominator > 0,
                                  100 * findings$n / findings$denominator, NA_real_)
  findings$status <- ifelse(findings$severity == "pass", "passed", "open")
  findings$reviewer <- ""
  findings$review_note <- ""
  findings$reviewed_at <- ""
  key <- paste(findings$check, findings$scope, sep = "::")
  findings$finding_id <- paste0(.dq_slug(key), "-", ave(seq_along(key), key, FUN = seq_along))
  findings
}

.dq_scorecard <- function(findings) {
  do.call(rbind, lapply(seq_len(nrow(DQ_SCORECARD)), function(i) {
    id <- DQ_SCORECARD$id[i]
    f <- findings[!is.na(findings$scorecard_id) & findings$scorecard_id == id,
                  , drop = FALSE]
    if (!nrow(f)) status <- "not evaluated"
    else if (any(f$severity != "pass")) status <- "issues found"
    else status <- "passed"
    data.frame(DQ_SCORECARD[i, , drop = FALSE], status = status,
               findings = sum(f$severity != "pass"), stringsAsFactors = FALSE)
  }))
}

.dq_add_raw_checks <- function(add, mark, raw_df, df, types, contract, n_rows) {
  if (!is.data.frame(raw_df)) return(invisible(NULL))
  safe_raw <- function(check, expr) {
    mark(check)
    tryCatch(expr, error = function(e)
      add(check, "warning", "(engine)",
          sprintf("Raw-import check '%s' could not be evaluated.", check)))
  }
  nm <- intersect(names(raw_df), names(df))
  safe_raw("invalid text encoding", {
    bad <- character(0)
    for (col in nm) if (is.character(raw_df[[col]])) {
      vals <- raw_df[[col]][!is.na(raw_df[[col]])]
      if (length(vals) && any(!validUTF8(vals))) bad <- c(bad, col)
    }
    if (length(bad)) add("invalid text encoding", "error", paste(bad, collapse = ", "),
                          sprintf("Text encoding could not be validated in %d field(s).", length(bad)),
                          length(bad), length(nm))
  })
  expected <- contract$expected_types %||% character(0)
  if (length(expected)) safe_raw("unexpected data types", {
    bad <- character(0)
    for (col in intersect(names(expected), nm)) {
      if (!.dq_type_matches(raw_df[[col]], expected[[col]])) bad <- c(bad, col)
    }
    if (length(bad)) add("unexpected data types", "warning", paste(bad, collapse = ", "),
                          sprintf("The raw storage class does not match the expected measurement type in %d field(s).", length(bad)),
                          length(bad), length(nm))
  })
  if (length(expected)) safe_raw("raw coercion loss", {
    bad <- character(0); lost <- 0L
    for (col in intersect(names(expected), nm)) {
      rx <- raw_df[[col]]; cx <- df[[col]]
      if (length(rx) == length(cx) && expected[[col]] %in% c("continuous", "count", "datetime")) {
        r_ok <- .dq_nonmissing(rx)
        c_na <- is.na(cx)
        n_lost <- sum(r_ok & c_na)
        if (n_lost) { bad <- c(bad, col); lost <- lost + n_lost }
      }
    }
    if (lost) add("raw coercion loss", "error", paste(unique(bad), collapse = ", "),
                  sprintf("%d non-missing raw value(s) became missing after coercion.", lost),
                  lost, n_rows)
  })
  invisible(NULL)
}

#' Run the complete data-readiness battery.
#'
#' @param df active, type-confirmed data frame.
#' @param types named measurement-type vector.
#' @param panel optional layout/role list.
#' @param opts threshold overrides.
#' @param contract optional dq_contract() object.
#' @param raw_df original imported data before coercion.
#' @param analysis list(outcome, predictors, family).
#' @return findings, summary, scorecard, and n_checks.
dq_run_checks <- function(df, types = NULL, panel = NULL,
                          opts = dq_default_opts(), contract = NULL,
                          raw_df = NULL, analysis = NULL) {
  o <- utils::modifyList(dq_default_opts(), opts %||% list())
  out <- list(); ran <- character(0)
  add <- function(check, severity, scope, message, n = NA_integer_,
                  denominator = NA_integer_, ...) {
    out[[length(out) + 1L]] <<- dq_finding(check, severity, scope, message,
                                            n, denominator, ...)
  }
  mark <- function(check) ran <<- unique(c(ran, check))
  safe <- function(check, expr, enabled = TRUE) {
    if (!isTRUE(enabled)) return(invisible(NULL))
    mark(check)
    tryCatch(expr, error = function(e)
      add(check, "warning", "(engine)",
          sprintf("Check '%s' could not be evaluated. Review the technical log before relying on this result.", check)))
  }

  mark("dataset present")
  if (is.null(df) || !is.data.frame(df)) {
    add("dataset present", "error", "dataset", "No active dataset is loaded.")
    return(.dq_finalise(out, ran, 0L, 0L))
  }
  n_rows <- nrow(df); n_cols <- ncol(df); nm <- names(df) %||% character(0)
  mark("non-empty dataset")
  if (n_rows == 0L || n_cols == 0L) {
    add("non-empty dataset", "error", "dataset",
        sprintf("The dataset is empty (%d rows x %d columns).", n_rows, n_cols),
        denominator = max(n_rows * n_cols, 1L))
    return(.dq_finalise(out, ran, n_rows, n_cols))
  }
  panel <- if (is.list(panel)) panel else list()
  con <- .dq_normalise_contract(contract, df, types, panel)
  analysis <- if (is.list(analysis)) analysis else list()
  outcome <- analysis$outcome %||% NULL
  predictors <- intersect(as.character(analysis$predictors %||% con$predictors), nm)
  family <- as.character(analysis$family %||% "")[1]
  if (!is.null(outcome) && length(outcome) == 1 && !nzchar(outcome)) outcome <- NULL
  if (!is.null(outcome) && !outcome %in% nm) outcome <- NULL

  .dq_add_raw_checks(add, mark, raw_df, df, types, con, n_rows)

  # ---- structural and schema checks --------------------------------------
  safe("column names unique", {
    dup <- unique(nm[duplicated(nm)])
    if (length(dup)) add("column names unique", "error", "dataset",
      "Duplicate column names prevent unambiguous variable lookup.", length(dup), n_cols)
  })
  safe("column names valid", {
    blank <- nm[is.na(nm) | !nzchar(trimws(nm))]
    padded <- nm[nzchar(nm) & nm != trimws(nm)]
    nonsyn <- nm[nzchar(trimws(nm)) & make.names(nm) != nm]
    if (length(blank)) add("column names valid", "error", "dataset",
      sprintf("%d column(s) have a blank name.", length(blank)), length(blank), n_cols)
    if (length(padded)) add("column names valid", "warning", "dataset",
      sprintf("%d column name(s) have leading or trailing spaces.", length(padded)), length(padded), n_cols)
    if (length(nonsyn)) add("column names valid", "note", "dataset",
      sprintf("%d column name(s) are non-syntactic and require quoting in formulas.", length(nonsyn)), length(nonsyn), n_cols)
  })
  safe("required fields", {
    missing_cols <- con$required_missing %||% character(0)
    missing_values <- if (length(intersect(con$required, nm)))
      sum(vapply(df[intersect(con$required, nm)], function(x) any(is.na(x)), logical(1))) else 0L
    if (length(missing_cols) || missing_values)
      add("required fields", "error", "dataset",
          sprintf("%d required field(s) are absent or contain missing values.", length(missing_cols) + missing_values),
          length(missing_cols) + missing_values, length(con$required))
  }, enabled = length(con$required) > 0)
  safe("duplicate rows", {
    dup <- sum(duplicated(df))
    if (dup > 0) add("duplicate rows", "warning", "dataset",
      sprintf("%d fully duplicated row(s) detected.", dup), dup, n_rows)
  })
  safe("empty rows", {
    all_na <- sum(vapply(seq_len(n_rows), function(i) all(is.na(df[i, , drop = TRUE])), logical(1)))
    if (all_na > 0) add("empty rows", "warning", "dataset",
      sprintf("%d row(s) are entirely missing.", all_na), all_na, n_rows)
  })
  safe("overall missingness", {
    total <- n_rows * n_cols; miss <- sum(is.na(df)); pct <- miss / total
    if (pct > o$missing_warn) add("overall missingness", "warning", "dataset",
      sprintf("%.1f%% of all cells are missing (%d of %d).", 100 * pct, miss, total), miss, total)
    else if (pct > o$missing_note) add("overall missingness", "note", "dataset",
      sprintf("%.1f%% of all cells are missing (%d of %d).", 100 * pct, miss, total), miss, total)
  })
  safe("redundant columns", {
    if (n_cols >= 2 && n_cols <= 200) for (i in seq_len(n_cols - 1)) for (j in (i + 1):n_cols) {
      if (identical(as.character(df[[i]]), as.character(df[[j]])))
        add("redundant columns", "note", "dataset",
            sprintf("Columns %d and %d contain identical information.", i, j), 2L, n_cols)
    }
  })

  # ---- missingness patterns ----------------------------------------------
  safe("missingness patterns", {
    miss <- is.na(df); multi <- sum(rowSums(miss) >= 2)
    patterns <- if (n_rows) apply(miss, 1, paste0, collapse = "") else character(0)
    common <- if (length(patterns)) max(table(patterns)) else 0L
    if (multi > 0 && common / n_rows >= o$pattern_min_pct)
      add("missingness patterns", "note", "dataset",
          sprintf("%d row(s) have missing values in at least two fields; the most common missingness pattern covers %.1f%% of rows.",
                  multi, 100 * common / n_rows), multi, n_rows)
  })
  subgroups <- intersect(con$subgroup_vars, nm)
  missing_targets <- nm[vapply(df, function(x) any(is.na(x)), logical(1))]
  if (length(subgroups) && length(missing_targets)) safe("missingness by subgroup", {
    for (sg in subgroups) {
      g <- as.character(df[[sg]]); valid_g <- !is.na(g) & nzchar(trimws(g))
      if (sum(valid_g) < 2 * o$subgroup_min_n) next
      for (target in missing_targets) {
        rates <- tapply(is.na(df[[target]][valid_g]), g[valid_g], mean)
        counts <- table(g[valid_g])
        rates <- rates[counts >= o$subgroup_min_n]
        if (length(rates) >= 2 && max(rates) - min(rates) >= o$subgroup_gap)
          add("missingness by subgroup", "warning", paste(target, "by", sg),
              sprintf("Missingness ranges from %.1f%% to %.1f%% across subgroups (gap %.1f percentage points).",
                      100 * min(rates), 100 * max(rates), 100 * (max(rates) - min(rates))),
              round(sum(is.na(df[[target]]))), n_rows)
      }
    }
  }, enabled = length(subgroups) && length(missing_targets))

  # ---- per-column checks --------------------------------------------------
  for (col in nm) {
    x <- df[[col]]
    t <- if (!is.null(types) && col %in% names(types)) unname(types[[col]]) else ""
    n_na <- sum(is.na(x)); n_ok <- n_rows - n_na
    uniq <- unique(x[!is.na(x)])
    safe("empty columns", {
      if (n_ok == 0) add("empty columns", "error", col,
        "Column is entirely missing (no usable values).", n_rows, n_rows)
    })
    if (n_ok == 0) next
    safe("column missingness", {
      pct <- n_na / n_rows
      sev <- if (pct > o$missing_error) "error" else if (pct > o$missing_warn) "warning"
             else if (pct > o$missing_note) "note" else NA_character_
      if (!is.na(sev)) add("column missingness", sev, col,
        sprintf("%.1f%% missing (%d of %d).", 100 * pct, n_na, n_rows), n_na, n_rows)
    })
    safe("constant columns", {
      if (length(uniq) == 1 && n_rows > 1)
        add("constant columns", "warning", col,
            "Only one distinct non-missing value is present; the column carries no information.",
            1L, n_ok)
    })
    safe("near-zero variance", {
      tb <- table(x, useNA = "no")
      if (length(tb) > 1 && length(tb) <= o$nzv_unique_max && max(tb) / sum(tb) >= o$nzv_freq_prop)
        add("near-zero variance", "warning", col,
            sprintf("Only %d distinct value(s) are observed and the most common value represents %.1f%% of usable rows.",
                    length(tb), 100 * max(tb) / sum(tb)), length(tb), n_ok)
    })

    is_cat <- is.factor(x) || is.character(x)
    is_num <- is.numeric(x) || t %in% c("continuous", "count")
    if (is_cat) {
      safe("disguised missing", {
        ch <- tolower(trimws(as.character(x))); hit <- ch %in% o$sentinel_values & !is.na(x)
        if (any(hit)) add("disguised missing", "warning", col,
          sprintf("%d value(s) look like coded missing values but are stored as data.", sum(hit)), sum(hit), n_rows)
      })
      safe("numbers stored as text", {
        if (.dq_numeric_parse_frac(x) >= o$numeric_text_frac && t != "identifier")
          add("numbers stored as text", "warning", col,
              "Nearly all usable values are numeric but the column is text; confirm its type.", n_ok, n_rows)
      })
      safe("high-cardinality category", {
        k <- length(unique(x[!is.na(x)]))
        if (t != "identifier" && k > o$high_card_min && k / n_ok > o$high_card_ratio)
          add("high-cardinality category", "note", col,
              sprintf("%d distinct value(s) occur in %d usable rows.", k, n_ok), k, n_ok)
      })
      safe("inconsistent categories", {
        raw_lv <- unique(as.character(x)[!is.na(x)])
        norm_lv <- unique(tolower(trimws(raw_lv)))
        if (length(norm_lv) < length(raw_lv)) add("inconsistent categories", "warning", col,
          sprintf("%d level(s) differ only by case or whitespace.", length(raw_lv) - length(norm_lv)),
          length(raw_lv) - length(norm_lv), length(raw_lv))
      })
      safe("empty factor levels", {
        if (is.factor(x)) {
          zero <- sum(table(x, useNA = "no") == 0)
          if (zero > 0) add("empty factor levels", "note", col,
            sprintf("%d declared factor level(s) have no observations.", zero), zero, length(levels(x)))
        }
      })
      safe("rare categories", {
        tb <- table(x, useNA = "no"); rare <- sum(tb > 0 & tb < o$rare_level_min)
        if (rare > 0) add("rare categories", "note", col,
          sprintf("%d observed level(s) have fewer than %d observations.", rare, o$rare_level_min), rare, length(tb))
      })
      if (identical(t, "binary")) safe("binary has two levels", {
        if (length(uniq) != 2) add("binary has two levels", "warning", col,
          sprintf("Typed as binary but %d level(s) are observed.", length(uniq)), length(uniq), 2L)
      })
      allowed <- con$allowed_levels[[col]]
      if (length(allowed)) safe("allowed levels", {
        bad <- !is.na(x) & !(as.character(x) %in% as.character(allowed))
        if (any(bad)) add("allowed levels", "error", col,
          sprintf("%d value(s) are outside the declared set of %d allowed level(s).", sum(bad), length(allowed)),
          sum(bad), n_rows)
      })
    }
    if (is_num) {
      xn <- suppressWarnings(as.numeric(x)); finite <- is.finite(xn); xf <- xn[finite]
      safe("infinite values", {
        n_inf <- sum(is.infinite(xn))
        if (n_inf > 0) add("infinite values", "error", col,
          sprintf("%d infinite value(s) (Inf/-Inf) present.", n_inf), n_inf, n_rows)
      })
      safe("outliers", {
        if (length(xf) >= 10) {
          s <- stats::sd(xf); qs <- stats::quantile(xf, c(.25, .75), names = FALSE)
          if (is.finite(s) && s > 0) {
            n_sd <- sum(abs(xf - mean(xf)) / s > o$outlier_k)
            iqr <- qs[2] - qs[1]
            n_iqr <- if (iqr > 0) sum(xf < qs[1] - 1.5 * iqr | xf > qs[2] + 1.5 * iqr) else 0
            if (n_sd > 0 || n_iqr > 0) add("outliers", "note", col,
              sprintf("%d value(s) exceed the SD rule and %d exceed the 1.5xIQR rule; verify them against domain limits.", n_sd, n_iqr),
              max(n_sd, n_iqr), length(xf))
          }
        }
      })
      if (identical(t, "count")) {
        safe("count is non-negative", {
          n_neg <- sum(xf < 0)
          if (n_neg > 0) add("count is non-negative", "error", col,
            sprintf("%d negative value(s) in a count variable.", n_neg), n_neg, n_rows)
        })
        safe("count is integer", {
          n_frac <- sum(abs(xf - round(xf)) > 1e-8)
          if (n_frac > 0) add("count is integer", "warning", col,
            sprintf("%d non-integer value(s) in a count variable.", n_frac), n_frac, n_rows)
        })
      }
      safe("skewed distributions", {
        sk <- .dq_skew(xf)
        if (is.finite(sk) && abs(sk) >= o$skew_note) {
          sev <- if (abs(sk) >= o$skew_warn) "warning" else "note"
          add("skewed distributions", sev, col,
              sprintf("The sample skewness is %.2f.", sk), length(xf), n_rows)
        }
      })
    }

    range_spec <- con$ranges[[col]]
    if (length(range_spec)) safe("expected ranges", {
      date_like <- inherits(x, c("Date", "POSIXt")) || t == "datetime"
      vals <- if (date_like) .dq_as_date(x) else suppressWarnings(as.numeric(x))
      bound_x <- if (date_like) as.Date("2000-01-01") else x
      lo <- .dq_bound(range_spec$min %||% range_spec[["lower"]], bound_x)
      hi <- .dq_bound(range_spec$max %||% range_spec[["upper"]], bound_x)
      bad <- rep(FALSE, length(vals))
      if (!is.null(lo)) bad <- bad | (!is.na(vals) & vals < lo)
      if (!is.null(hi)) bad <- bad | (!is.na(vals) & vals > hi)
      if (any(bad)) add("expected ranges", "error", col,
        sprintf("%d value(s) fall outside the declared range.", sum(bad)), sum(bad), n_rows)
    })

    if (inherits(x, c("Date", "POSIXct", "POSIXt")) || identical(t, "datetime"))
      safe("implausible dates", {
        raw_valid <- .dq_nonmissing(x); dx <- .dq_as_date(x)
        invalid <- sum(raw_valid & is.na(dx)); if (invalid > 0)
          add("implausible dates", "error", col,
              sprintf("%d non-missing value(s) could not be parsed as dates.", invalid), invalid, n_rows)
        valid <- dx[!is.na(dx)]
        if (length(valid)) {
          future <- sum(valid > Sys.Date() + o$future_date_tolerance_days)
          old <- sum(as.integer(format(valid, "%Y")) < o$old_date_year)
          if (future > 0) add("implausible dates", "warning", col,
            sprintf("%d date(s) are after the allowed future tolerance.", future), future, n_rows)
          if (old > 0) add("implausible dates", "note", col,
            sprintf("%d date(s) are before %d.", old, o$old_date_year), old, n_rows)
        }
      })
  }

  # ---- identifiers, keys, and dates --------------------------------------
  id_vars <- unique(c(con$identifiers, panel$id %||% character(0)))
  id_vars <- intersect(id_vars, nm)
  for (id in id_vars) safe("identifier integrity", {
    x <- df[[id]]; bad_missing <- sum(is.na(x) | !nzchar(trimws(as.character(x))))
    if (bad_missing > 0) add("identifier integrity", "error", id,
      sprintf("%d identifier value(s) are blank or missing.", bad_missing), bad_missing, n_rows)
    if (id %in% con$identifiers && !any(vapply(con$unique_keys, function(k) identical(k, id), logical(1)))) {
      dup <- sum(duplicated(x[!is.na(x)]))
      if (dup > 0) add("identifier integrity", "warning", id,
        sprintf("%d repeated identifier value(s) were found.", dup), dup, n_rows)
    }
  })
  keys <- con$unique_keys
  if (length(keys)) for (key in keys) safe("duplicate key", {
    key_df <- df[key]
    dup <- sum(duplicated(key_df))
    if (dup > 0) add("duplicate key", "error", paste(key, collapse = " x "),
      sprintf("%d duplicate observation key(s) were found.", dup), dup, n_rows)
  })
  date_orders <- con$date_order
  p_start <- panel$surv_start %||% NULL; p_end <- panel$surv_end %||% NULL
  if (is.character(p_start) && is.character(p_end) && nzchar(p_start) && nzchar(p_end))
    date_orders <- c(date_orders, list(c(p_start, p_end)))
  if (length(date_orders)) for (pair in date_orders) safe("date order", {
    pair <- unname(unlist(pair)); start <- .dq_as_date(df[[pair[1]]]); end <- .dq_as_date(df[[pair[2]]])
    bad <- !is.na(start) & !is.na(end) & end < start
    if (any(bad)) add("date order", "error", paste(pair[1:2], collapse = " -> "),
      sprintf("%d date interval(s) have an end before the start.", sum(bad)), sum(bad), n_rows)
  })
  if (length(panel$id %||% character(0)) && length(panel$time %||% character(0)) &&
      panel$id %in% nm && panel$time %in% nm) {
    key <- df[c(panel$id, panel$time)]
    safe("unique id-time", {
      dup <- sum(duplicated(key))
      if (dup > 0) add("unique id-time", "error", paste(panel$id, panel$time, sep = " x "),
        sprintf("%d duplicated subject/time combination(s) were found.", dup), dup, n_rows)
    })
    safe("single-observation subjects", {
      counts <- table(df[[panel$id]]); singles <- sum(counts == 1)
      if (singles > 0) add("single-observation subjects", "note", panel$id,
        sprintf("%d subject(s) have only one observation.", singles), singles, length(counts))
    })
  }

  # ---- context-dependent analysis readiness -------------------------------
  if (length(predictors) >= 2) safe("highly correlated predictors", {
    num <- predictors[vapply(predictors, function(v) is.numeric(df[[v]]) ||
                               (types %||% character(0))[[v]] %in% c("continuous", "count"), logical(1))]
    if (length(num) >= 2) {
      cm <- suppressWarnings(stats::cor(df[num], use = "pairwise.complete.obs"))
      hits <- which(abs(cm) >= o$correlation_threshold & upper.tri(cm), arr.ind = TRUE)
      if (nrow(hits)) for (j in seq_len(min(nrow(hits), o$max_correlation_pairs))) {
        a <- rownames(cm)[hits[j, 1]]; b <- colnames(cm)[hits[j, 2]]
        add("highly correlated predictors", "warning", paste(a, b, sep = " x "),
          sprintf("The absolute correlation between two predictors is %.2f (threshold %.2f).",
                  abs(cm[hits[j, 1], hits[j, 2]]), o$correlation_threshold), 1L, 1L,
          model_scope = "selected predictors")
      }
    }
  }, enabled = length(predictors) >= 2)

  if (!is.null(outcome)) safe("class imbalance", {
    y <- df[[outcome]]; tb <- table(y, useNA = "no")
    if (length(tb) == 2 && min(tb) / sum(tb) < o$imbalance_min_prop)
      add("class imbalance", "warning", outcome,
        sprintf("The smallest observed outcome class represents %.1f%% of non-missing outcomes.",
                100 * min(tb) / sum(tb)), min(tb), sum(tb), model_scope = "selected outcome")
  }, enabled = length(outcome) == 1)

  if (length(predictors) && !is.null(outcome)) safe("modeling suitability", {
    for (v in predictors) {
      bad <- FALSE; reason <- character(0)
      if (v %in% id_vars || (types %||% character(0))[[v]] %in% c("identifier", "text", "datetime")) {
        bad <- TRUE; reason <- c(reason, "identifier/text/date role")
      }
      if (mean(is.na(df[[v]])) > o$missing_error) { bad <- TRUE; reason <- c(reason, "excessive missingness") }
      if (length(unique(df[[v]][!is.na(df[[v]])])) <= 1) { bad <- TRUE; reason <- c(reason, "no variation") }
      if (bad) add("modeling suitability", "warning", v,
        sprintf("Selected predictor has a potential %s problem.", paste(reason, collapse = ", ")),
        sum(is.na(df[[v]])), n_rows, model_scope = family %||% "selected analysis")
    }
  }, enabled = length(predictors) && length(outcome) == 1)

  if (identical(family, "logistic") && !is.null(outcome) && length(predictors)) safe("logistic separation", {
    y <- df[[outcome]]; keep <- !is.na(y)
    for (v in predictors) keep <- keep & !is.na(df[[v]])
    y <- droplevels(as.factor(y[keep])); x <- df[keep, predictors, drop = FALSE]
    if (nlevels(y) == 2 && nrow(x) >= 4) {
      mm <- tryCatch(stats::model.matrix(~ ., x), error = function(e) NULL)
      if (!is.null(mm)) {
        fit <- tryCatch(suppressWarnings(stats::glm(y ~ mm[, -1, drop = FALSE], family = stats::binomial())),
                        error = function(e) NULL)
        separated <- FALSE
        if (!is.null(fit)) {
          pr <- tryCatch(stats::fitted(fit), error = function(e) numeric(0))
          separated <- !isTRUE(fit$converged) || any(pr <= 1e-8 | pr >= 1 - 1e-8) ||
            any(abs(stats::coef(fit)) > 20, na.rm = TRUE)
        }
        if (is.null(fit) || separated)
          add("logistic separation", "error", outcome,
            "The selected logistic model shows perfect or quasi-separation.", nrow(x), nrow(x),
            model_scope = paste(c(outcome, predictors), collapse = " ~ "))
      }
    }
  }, enabled = identical(family, "logistic") && !is.null(outcome) && length(predictors))

  .dq_finalise(out, ran, n_rows, n_cols)
}

.dq_finalise <- function(out, ran, n_rows, n_cols) {
  findings <- if (length(out)) do.call(rbind, out) else
    dq_finding(character(0), character(0), character(0), character(0))[0, ]
  flagged <- unique(findings$check)
  passed <- setdiff(ran, flagged)
  if (length(passed)) findings <- rbind(findings,
    do.call(rbind, lapply(passed, function(check)
      dq_finding(check, "pass", "dataset", "No issues detected.", denominator = n_rows))))
  findings <- .dq_enrich(findings, n_rows, n_cols)
  findings$severity <- factor(findings$severity, levels = DQ_SEVERITY_ORDER)
  findings <- findings[order(findings$severity, findings$priority, findings$domain,
                             findings$check, findings$scope), , drop = FALSE]
  findings$severity <- as.character(findings$severity)
  rownames(findings) <- NULL
  counts <- table(factor(findings$severity, levels = DQ_SEVERITY_ORDER))
  scorecard <- .dq_scorecard(findings)
  list(
    findings = findings,
    scorecard = scorecard,
    summary = list(
      n_rows = n_rows, n_cols = n_cols,
      errors = as.integer(counts[["error"]]),
      warnings = as.integer(counts[["warning"]]),
      notes = as.integer(counts[["note"]]),
      passed = as.integer(counts[["pass"]]),
      open_errors = as.integer(counts[["error"]]),
      open_warnings = as.integer(counts[["warning"]]),
      open_notes = as.integer(counts[["note"]]),
      readiness = if (counts[["error"]] > 0) "blocked" else if (counts[["warning"]] > 0)
                  "review required" else if (counts[["note"]] > 0) "ready with notes" else "ready",
      scorecard_issues = sum(scorecard$status == "issues found"),
      scorecard_not_evaluated = sum(scorecard$status == "not evaluated")
    ),
    n_checks = length(ran)
  )
}

#' Merge human review decisions into a data-quality result.
dq_apply_reviews <- function(result, reviews = NULL) {
  if (is.null(result) || !is.data.frame(result$findings) || !nrow(result$findings)) return(result)
  f <- result$findings
  if (is.data.frame(reviews) && nrow(reviews) && "finding_id" %in% names(reviews)) {
    for (i in seq_len(nrow(f))) {
      hit <- which(reviews$finding_id == f$finding_id[i])
      if (length(hit)) {
        r <- reviews[hit[[length(hit)]], , drop = FALSE]
        f$status[i] <- as.character(r$status[[1]] %||% f$status[i])
        f$reviewer[i] <- as.character(r$reviewer[[1]] %||% "")
        f$review_note[i] <- as.character(r$review_note[[1]] %||% "")
        f$reviewed_at[i] <- as.character(r$reviewed_at[[1]] %||% "")
      }
    }
  }
  result$findings <- f
  active <- f[f$severity != "pass" & !f$status %in% c("accepted", "not_applicable"), , drop = FALSE]
  result$summary$open_errors <- sum(active$severity == "error")
  result$summary$open_warnings <- sum(active$severity == "warning")
  result$summary$open_notes <- sum(active$severity == "note")
  result$summary$readiness <- if (result$summary$open_errors > 0) "blocked" else
    if (result$summary$open_warnings > 0) "review required" else
    if (result$summary$open_notes > 0) "ready with notes" else "ready"
  result
}

#' Render an action-oriented text report.
dq_report_text <- function(result, dataset_name = "active dataset",
                           generated_at = as.character(Sys.time())) {
  s <- result$summary; bar <- strrep("=", 78); sub <- strrep("-", 78)
  L <- c(bar, " SABALI - DATA READINESS REPORT", bar,
         sprintf(" Dataset      : %s", dataset_name),
         sprintf(" Generated    : %s", generated_at),
         sprintf(" Dimensions   : %d rows x %d columns", s$n_rows, s$n_cols),
         sprintf(" Checks run   : %d", result$n_checks),
         sprintf(" Readiness    : %s", toupper(s$readiness)),
         sprintf(" Open issues  : %d blocker/error(s), %d warning(s), %d note(s)",
                 s$open_errors, s$open_warnings, s$open_notes), sub, "")
  sc <- result$scorecard
  L <- c(L, "22-POINT SCORECARD", sub)
  for (i in seq_len(nrow(sc)))
    L <- c(L, sprintf(" %02d. %-30s %-16s (%s)", sc$point[i], sc$label[i],
                      sc$status[i], sc$context[i]))
  L <- c(L, "")
  f <- result$findings
  active <- f[f$severity != "pass", , drop = FALSE]
  if (nrow(active)) {
    L <- c(L, "ACTION FINDINGS", sub)
    for (i in seq_len(nrow(active))) {
      affected <- if (!is.na(active$affected_pct[i]))
        sprintf(" Affected    : %d (%.1f%%)", active$affected_n[i], active$affected_pct[i])
      else if (!is.na(active$affected_n[i])) sprintf(" Affected    : %d", active$affected_n[i]) else ""
      L <- c(L, sprintf("[%s/%s] %s", toupper(active$priority[i]), active$status[i], active$issue[i]),
             sprintf(" Scope       : %s", active$scope[i]),
             sprintf(" Impact      : %s", active$impact[i]),
             sprintf(" Recommendation: %s", active$recommendation[i]),
             affected, sprintf(" Evidence    : %s", active$evidence[i]), "")
    }
  } else L <- c(L, "No active data-quality issues were detected.", "")
  L <- c(L, sub, "Notes:",
         " * Automated checks identify structural, statistical, and modelling red flags;",
         "   they do not replace data-owner or subject-matter review.",
         " * Accepted findings remain visible with their review decision.",
         " * The original uploaded data is not modified by this report.", bar,
         " End of report - Sabali, Data check (Tab 0).", bar)
  paste(L, collapse = "\n")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
