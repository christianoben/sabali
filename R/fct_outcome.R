# fct_outcome.R -------------------------------------------------------------
# Candidate-outcome selection and temporary, model-specific outcome coercion.
# The source data and confirmed type metadata are never mutated.

OUTCOME_TARGET_LABELS <- c(
  continuous = "Continuous",
  count = "Count",
  binary = "Binary",
  nominal = "Categorical",
  ordinal = "Ordered categorical"
)

outcome_type_options <- function(x) {
  if (is.null(x)) return(character(0))
  observed <- x[!is.na(x)]
  if (!length(observed)) return(character(0))
  numeric_values <- suppressWarnings(as.numeric(as.character(observed)))
  numeric_ok <- all(is.finite(numeric_values))
  binary_ok <- length(unique(observed)) == 2
  out <- character(0)
  if (numeric_ok) {
    out <- c(out, "continuous")
    if (all(numeric_values >= 0) && all(numeric_values == round(numeric_values)))
      out <- c(out, "count")
  }
  if (binary_ok) out <- c(out, "binary")
  if (is.factor(x) || is.character(x)) {
    out <- c(out, "nominal")
    if (is.ordered(x)) out <- c(out, "ordinal")
  }
  unique(out)
}

outcome_type_options_df <- function(df, vars = names(df)) {
  vars <- intersect(vars, names(df))
  lapply(vars, function(v) outcome_type_options(df[[v]])) |>
    stats::setNames(vars)
}

preferred_outcome_type <- function(x, source_type = NULL) {
  opts <- outcome_type_options(x)
  if (!length(opts)) return(source_type %||% NA_character_)
  if (!is.null(source_type) && source_type %in% c("continuous", "count", "binary") &&
      source_type %in% opts) return(source_type)
  for (candidate in c("continuous", "count", "binary", "nominal", "ordinal"))
    if (candidate %in% opts) return(candidate)
  opts[[1]]
}

default_outcome_candidates <- function(df, types) {
  if (is.null(df) || is.null(types)) return(character(0))
  vars <- intersect(names(df), names(types))
  # Safe defaults preserve the existing cross-sectional behavior. The Data tab
  # still lets the user explicitly select an ID, date, or text column if it is
  # genuinely intended to be an outcome.
  vars <- vars[!types[vars] %in% c("identifier", "text", "datetime")]
  vars[vapply(vars, function(v) length(outcome_type_options(df[[v]])) > 0,
              logical(1))]
}

coerce_outcome_vector <- function(x, target_type, variable = "outcome") {
  if (!target_type %in% names(OUTCOME_TARGET_LABELS))
    stop("Unsupported temporary outcome type: ", target_type, call. = FALSE)
  if (target_type %in% c("continuous", "count")) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    if (any(!is.na(x) & !is.finite(y)))
      stop(sprintf("'%s' cannot be safely converted to numeric values.", variable), call. = FALSE)
    if (target_type == "count" && any(!is.na(y) &
                                       (y < 0 | y != round(y))))
      stop(sprintf("'%s' must contain non-negative integer values for a count model.", variable),
           call. = FALSE)
    return(y)
  }
  if (target_type == "binary") {
    observed <- x[!is.na(x)]
    if (!length(observed) || length(unique(observed)) != 2)
      stop(sprintf("'%s' must have exactly two observed levels for a binary model.", variable),
           call. = FALSE)
    level_order <- if (is.factor(x)) levels(droplevels(x)) else unique(observed)
    return(factor(x, levels = level_order))
  }
  if (target_type %in% c("nominal", "ordinal")) {
    if (is.numeric(x))
      return(if (target_type == "ordinal") factor(x, ordered = TRUE) else factor(x))
    return(if (target_type == "ordinal") factor(x, ordered = TRUE) else factor(x))
  }
  x
}

prepare_outcome_data <- function(df, outcome, target_type, extra_vars = character(0)) {
  if (is.null(outcome) || !nzchar(outcome) || !outcome %in% names(df))
    stop("Choose a valid outcome variable.", call. = FALSE)
  vars <- unique(c(outcome, extra_vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (any(!vars %in% names(df)))
    stop("A selected model variable is not present in the data.", call. = FALSE)
  out <- df
  out[[outcome]] <- coerce_outcome_vector(df[[outcome]], target_type, outcome)
  # Defensive: drop unused factor levels on the model variables so a level that
  # is present in the data's factor definition but has no observations (e.g. one
  # excluded by a Data-contract rule) can never surface as a phantom model term.
  for (v in vars) if (is.factor(out[[v]])) out[[v]] <- droplevels(out[[v]])
  list(data = out, outcome = outcome, target_type = target_type,
       rows = nrow(out), source_type = class(df[[outcome]])[1])
}

prepare_test_data <- function(df, spec) {
  out <- df
  convert <- function(v, target) {
    if (is.null(v) || !nzchar(v) || !v %in% names(out)) return(invisible(NULL))
    out[[v]] <<- coerce_outcome_vector(out[[v]], target, v)
  }
  if (spec$family == "location") {
    convert(spec$outcome, "continuous")
    if (!is.null(spec$within) && nzchar(spec$within) && spec$within %in% names(out))
      out[[spec$within]] <- factor(out[[spec$within]], ordered = TRUE)
    if (identical(spec$design, "ancova"))
      for (cv in spec$covariates %||% character(0)) convert(cv, "continuous")
  } else if (spec$family == "proportion") {
    convert(spec$outcome, "binary")
  } else if (spec$family == "cochran_q") {
    convert(spec$outcome, "binary")
    if (!is.null(spec$within) && nzchar(spec$within) && spec$within %in% names(out))
      out[[spec$within]] <- droplevels(as.factor(out[[spec$within]]))
  } else if (spec$family == "correlation") {
    convert(spec$x, "continuous"); convert(spec$y, "continuous")
  } else if (spec$family == "point_biserial") {
    convert(spec$x, "binary"); convert(spec$y, "continuous")
  } else if (spec$family == "categorical") {
    convert(spec$x, "nominal"); convert(spec$y, "nominal")
  }
  out
}

analysis_outcome_vars <- function(df, types, candidates = NULL, target_types = NULL) {
  vars <- if (is.null(candidates)) names(df) else intersect(candidates, names(df))
  if (is.null(target_types)) target_types <- c("continuous", "count", "binary")
  vars[vapply(vars, function(v) {
    any(outcome_type_options(df[[v]]) %in% target_types)
  }, logical(1))]
}
