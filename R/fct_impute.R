# fct_impute.R ---------------------------------------------------------------
# Multiple imputation for missing data.
#
# Why this exists. The app's default - and every regression's default - is
# complete-case analysis: any row missing any modelled variable is dropped. That
# is unbiased only when the data are missing completely at random, and it throws
# away information even then. With several partially-observed covariates the
# losses compound: five variables each 10% missing can remove a third of the
# sample, and the third removed is rarely a random third.
#
# The app already offers single-value fills (mean, median, mode) on the Data
# check tab. Those are WORSE than complete-case analysis for inference, not
# better: substituting the mean for every missing value pretends to a certainty
# the data does not have, shrinks the variance, and narrows every confidence
# interval that follows. Multiple imputation is the fix - impute several times,
# fit the model in each, and combine with Rubin's rules so the uncertainty about
# the missing values is carried into the standard errors instead of being erased.
#
# The imputation is mice's. What is here is the pooling contract, the guards
# around what is safe to impute, and a missing-data summary that tells the
# analyst whether imputation is even the right move.

#' Summarise the missing data, and what complete-case analysis would cost.
#'
#' The number that decides whether to impute is not the per-variable missingness
#' but how many COMPLETE rows a model would be left with, which is always worse
#' and is what an analyst rarely computes by hand.
missing_summary <- function(df, vars = names(df)) {
  vars <- intersect(vars, names(df))
  if (!length(vars))
    stop("No columns were selected for the missing-data summary.", call. = FALSE)
  d <- df[, vars, drop = FALSE]
  per_var <- data.frame(
    Variable = vars,
    Missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
    `Missing %` = round(100 * vapply(d, function(x) mean(is.na(x)), numeric(1)), 1),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  complete <- sum(stats::complete.cases(d))
  list(per_variable = per_var[order(-per_var$`Missing %`), , drop = FALSE],
       n_rows = nrow(d), n_complete = complete,
       pct_lost = if (nrow(d)) 100 * (1 - complete / nrow(d)) else NA_real_,
       any_missing = any(per_var$Missing > 0))
}

#' Multiply impute, fit a model in each completed dataset, and pool.
#'
#' @param df      Analysis data.
#' @param spec    A model spec as `run_guided_model()` takes it, plus:
#'   `m` (number of imputations), `impute_vars` (columns to include in the
#'   imputation model; defaults to the modelled ones), `seed`.
#' @return The app's model-result contract, with pooled coefficients, the
#'   fraction of missing information per term, and the missing-data summary.
run_imputed_model <- function(df, spec) {
  if (!requireNamespace("mice", quietly = TRUE))
    stop("Multiple imputation needs the 'mice' package. Run install_dependencies.R.",
         call. = FALSE)
  m <- as.integer(spec[["m"]] %||% 20L)
  if (!is.finite(m) || m < 2L)
    stop("Multiple imputation needs at least 2 imputations; 20 or more is usual.",
         call. = FALSE)
  outcome <- spec[["outcome"]]
  predictors <- spec[["predictors"]] %||% character(0)
  if (is.null(outcome) || !nzchar(outcome) || !length(predictors))
    stop("Choose an outcome and at least one predictor.", call. = FALSE)
  model_vars <- unique(c(outcome, predictors))
  extra <- setdiff(spec[["impute_vars"]] %||% character(0), model_vars)
  vars <- unique(c(model_vars, extra))
  missing_cols <- setdiff(vars, names(df))
  if (length(missing_cols))
    stop(sprintf("These columns are not in the data: %s.",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)

  d <- df[, vars, drop = FALSE]
  ms <- missing_summary(d, model_vars)
  if (!ms$any_missing)
    stop("Nothing is missing in the selected variables, so there is nothing to impute - fit the model directly.",
         call. = FALSE)
  # A variable that is almost entirely missing cannot be imputed responsibly:
  # the imputation model would be extrapolating from a handful of cases into a
  # column that then carries the weight of a real covariate.
  heavy <- ms$per_variable$Variable[ms$per_variable$`Missing %` > 60]
  if (length(heavy))
    stop(sprintf("These variables are more than 60%% missing: %s. Imputing them would invent most of their values; drop them from the model, or state a reason for keeping them and impute outside the app.",
                 paste(heavy, collapse = ", ")), call. = FALSE)
  if (is.na(ms$pct_lost) || ms$pct_lost < 1)
    stop("Complete-case analysis would lose less than 1% of the rows, so imputation adds complexity without changing the answer.",
         call. = FALSE)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv),
          add = TRUE)

  imps <- tryCatch(
    mice::mice(d, m = m, printFlag = FALSE, seed = as.integer(spec[["seed"]] %||% 1L)),
    error = function(e) stop("The imputation model could not be fitted: ",
                             conditionMessage(e), call. = FALSE))

  interactions <- validate_interactions(spec[["interactions"]] %||% character(0),
                                        predictors)
  formula <- model_formula(outcome, predictors, interactions)
  method <- spec[["method"]] %||% "ols"
  fam <- switch(method,
    logistic = stats::binomial(), poisson = stats::poisson(), NULL)
  if (!method %in% c("ols", "logistic", "poisson"))
    stop(sprintf("Pooled imputation is available for OLS, logistic and Poisson regression; '%s' is not among them. Rubin's rules apply to the coefficient scale, and the penalised and quantile fits do not report standard errors that can be pooled that way.",
                 method), call. = FALSE)

  # Fit on each completed dataset EXPLICITLY, passing `data =`.
  #
  # `with(imps, lm(formula))` looks like the mice idiom but is a trap when the
  # formula is built dynamically: without `data =`, lm() resolves the variable
  # names through the formula's environment, so if columns of the same name are
  # reachable there - the caller's frame, or the global environment - it fits
  # THOSE instead of the imputed data, silently bypassing the imputation and
  # returning something that looks like a better answer. Caught here because the
  # pooled estimate matched a complete-data fit to four decimal places with a
  # fraction of missing information of 0.002 on 27% missingness, which is not
  # possible.
  environment(formula) <- baseenv()
  fits <- tryCatch(lapply(seq_len(m), function(i) {
    di <- mice::complete(imps, i)
    if (is.null(fam)) stats::lm(formula, data = di)
    else stats::glm(formula, family = fam, data = di)
  }), error = function(e) stop("The model could not be fitted in the imputed data: ",
                               conditionMessage(e), call. = FALSE))
  pooled <- tryCatch(mice::pool(fits),
    error = function(e) stop("The imputed fits could not be pooled: ",
                             conditionMessage(e), call. = FALSE))
  ps <- summary(pooled, conf.int = TRUE, conf.level = 0.95)

  est <- ps$estimate
  se <- ps$std.error
  lo <- ps[["2.5 %"]]; hi <- ps[["97.5 %"]]
  effect_name <- switch(method, logistic = "Odds ratio",
                        poisson = "Rate ratio", NULL)
  coef_tab <- coefficient_table(
    term = as.character(ps$term), estimate = est, std_error = se,
    statistic = ps$statistic, p_value = ps$p.value,
    ci_lower = lo, ci_upper = hi, effect_name = effect_name)

  # fmi is the share of the information about each coefficient that was lost to
  # missingness. It is the number that says whether the imputation mattered, and
  # whether m was large enough: a rule of thumb is m at least 100 * max(fmi).
  pooled_df <- as.data.frame(pooled$pooled)
  fmi <- if ("fmi" %in% names(pooled_df)) pooled_df$fmi else rep(NA_real_, nrow(ps))
  coef_tab$`Fraction missing info` <- round(fmi, 4)

  max_fmi <- suppressWarnings(max(fmi, na.rm = TRUE))
  metrics <- c(
    "imputations (m)" = m,
    "rows" = ms$n_rows,
    "complete cases" = ms$n_complete,
    "rows a complete-case analysis would drop" = ms$n_rows - ms$n_complete,
    "max fraction of missing information" =
      if (is.finite(max_fmi)) round(max_fmi, 4) else NA_real_)

  notes <- c(
    sprintf("Complete-case analysis would have used %s of %s rows, dropping %.1f%%.",
            format(ms$n_complete, big.mark = ","),
            format(ms$n_rows, big.mark = ","), ms$pct_lost),
    "Estimates are pooled across imputations by Rubin's rules, so the standard errors include the uncertainty about the missing values - which a single mean/median fill erases.",
    "Multiple imputation assumes the data are missing at random GIVEN the variables in the imputation model: that whether a value is missing carries no further information once the observed variables are accounted for. That is an assumption about how the data came to be missing, and it cannot be checked from the data.")
  if (is.finite(max_fmi) && m < 100 * max_fmi)
    notes <- c(notes, sprintf(
      "The largest fraction of missing information is %.2f, so %d imputations is on the low side; a common rule of thumb is m of at least %d here.",
      max_fmi, m, ceiling(100 * max_fmi)))

  assumptions <- list(list(
    check = "context", label = "About this method", family = "distribution",
    severity = "ok", ok = TRUE, statistic = NA_real_, p = NA_real_, test = "",
    note = paste(notes, collapse = " "), remedy = list(), detail = list()))

  res <- model_result(
    sprintf("%s, multiply imputed (m = %d)",
            MODEL_METHOD_LABELS[[method]], m),
    method, formula, fits, coef_tab, named_metric_table(metrics), assumptions,
    spec[["outcome_type"]] %||% "continuous", nrow(df), ms$n_rows,
    effect_name = effect_name,
    recommendation = "Pooled across imputations by Rubin's rules; compare with the complete-case fit, and treat a large difference as a sign that the missingness matters rather than as a better number.")
  res$missing_summary <- ms$per_variable
  res$diagnostic_plot <- NULL   # residuals are per-imputation, not pooled
  res$interpretation <- .imputed_interpretation(coef_tab, ms, max_fmi)
  res
}

.imputed_interpretation <- function(coef_tab, ms, max_fmi) {
  sig <- coef_tab$Term[coef_tab$Term != "(Intercept)" &
                         is.finite(coef_tab[["p-value"]]) &
                         coef_tab[["p-value"]] < 0.05]
  bits <- sprintf(
    "Fitted across the imputations and pooled. Complete-case analysis would have dropped %.1f%% of rows (%s of %s); imputation keeps them and carries the uncertainty about the missing values into the standard errors.",
    ms$pct_lost, format(ms$n_rows - ms$n_complete, big.mark = ","),
    format(ms$n_rows, big.mark = ","))
  bits <- c(bits, if (length(sig))
    sprintf("Evidence of an adjusted association for %s.",
            paste(utils::head(sig, 3), collapse = ", "))
    else "No predictor reached p < 0.05 after pooling.")
  if (is.finite(max_fmi))
    bits <- c(bits, sprintf(
      "At most %.0f%% of the information about any coefficient was lost to missingness.",
      100 * max_fmi))
  bits <- c(bits, "This rests on the data being missing at random given the imputation model - an assumption about the missingness mechanism that the data cannot confirm. Associations are not proof of causation.")
  paste(bits, collapse = " ")
}
