# fct_models.R ---------------------------------------------------------------
# Phase 3 regression engine plus the shared clustered-model runners used by the
# Phase 4 Advanced Models tab. Every result follows one contract for the UI and
# Word report.

MODEL_METHOD_LABELS <- c(
  ols = "Ordinary least-squares regression (OLS)",
  robust = "Robust regression (M-estimator)",
  quantile = "Median / quantile regression",
  lasso = "LASSO regression (L1-penalised variable selection)",
  ridge = "Ridge regression (L2-penalised, keeps all predictors)",
  pls = "Partial least squares (PLS) regression",
  logistic = "Logistic GLM (binary outcome)",
  poisson = "Poisson GLM (count outcome)",
  negative_binomial = "Negative binomial GLM (overdispersed count)",
  gee_gaussian = "GEE (Gaussian outcome)",
  gee_binomial = "GEE (logistic)",
  gee_poisson = "GEE (Poisson)",
  lmm = "Linear mixed-effects model (LMM)",
  glmm_binomial = "Generalised linear mixed model (logistic GLMM)",
  glmm_poisson = "Generalised linear mixed model (Poisson GLMM)"
)

MODEL_OUTCOME_TYPES <- c("continuous", "count", "binary")
MODEL_PREDICTOR_TYPES <- c("continuous", "count", "binary", "nominal", "ordinal")

model_assumptions_df <- function(assumptions) {
  if (length(assumptions) == 0)
    return(data.frame(Check = character(0), Note = character(0)))
  data.frame(
    Check = paste("Diagnostic", seq_along(assumptions)),
    Note = vapply(assumptions, function(x) {
      if (is.null(x$note) || !length(x$note)) "" else as.character(x$note)
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

available_model_methods <- function(outcome_type, structure = "independent") {
  if (identical(structure, "clustered")) {
    return(switch(outcome_type,
      continuous = c("gee_gaussian", "lmm"),
      count = c("gee_poisson", "glmm_poisson"),
      binary = c("gee_binomial", "glmm_binomial"),
      character(0)
    ))
  }
  switch(outcome_type,
    continuous = c("ols", "robust", "quantile", "lasso", "ridge", "pls"),
    count = c("poisson", "negative_binomial", "lasso", "ridge"),
    binary = c("logistic", "lasso", "ridge"),
    character(0)
  )
}

suggest_model_plan <- function(outcome_type, structure = "independent") {
  methods <- available_model_methods(outcome_type, structure)
  default <- if (length(methods)) methods[[1]] else NA_character_
  why <- if (structure == "clustered") switch(outcome_type,
    continuous = "A continuous outcome with repeated or clustered observations supports population-average Gaussian GEE or a subject-specific linear mixed model.",
    count = "A clustered count outcome supports Poisson GEE or Poisson GLMM; inspect dispersion before relying on a Poisson variance assumption.",
    binary = "A binary outcome with repeated or clustered observations supports population-average GEE or subject-specific logistic GLMM analysis.",
    "Choose an analysable continuous, count, or binary outcome."
  ) else switch(outcome_type,
    continuous = "A continuous outcome supports OLS as the starting model, with robust and median-regression alternatives when residuals or outliers are a concern.",
    count = "A count outcome supports Poisson regression; inspect dispersion and use negative binomial regression when the Poisson variance assumption is too small.",
    binary = "A binary outcome is modelled with logistic regression; coefficients are also shown as odds ratios.",
    "Choose an analysable continuous, count, or binary outcome."
  )
  list(default = default, methods = methods, why = why)
}

# --- shared helpers --------------------------------------------------------
model_formula <- function(outcome, predictors) {
  if (length(predictors) == 0)
    stop("Choose at least one predictor for the model.", call. = FALSE)
  stats::reformulate(predictors, response = outcome)
}

model_frame <- function(df, formula, outcome, outcome_type,
                        extra_vars = character(0)) {
  needed <- unique(c(all.vars(formula), extra_vars))
  if (any(!needed %in% names(df)))
    stop("The model references a variable that is not present in the data.", call. = FALSE)
  keep <- stats::complete.cases(df[, needed, drop = FALSE])
  for (nm in needed) {
    if (is.numeric(df[[nm]])) keep <- keep & is.finite(df[[nm]])
  }
  mf <- df[keep, , drop = FALSE]

  # Friendly guard: a categorical predictor that collapses to a single level
  # after complete-case filtering would otherwise trigger R's cryptic
  # "contrasts can be applied only to factors with 2 or more levels".
  predictors <- setdiff(all.vars(formula), outcome)
  for (v in intersect(predictors, names(mf))) {
    col <- mf[[v]]
    if ((is.factor(col) || is.character(col)) &&
        nlevels(droplevels(as.factor(col))) < 2)
      stop(sprintf(
        "The predictor '%s' has only one observed category in the complete rows, so it cannot be included in the model. Remove it, or choose a variable with at least two categories.",
        v), call. = FALSE)
  }

  tryCatch(
    stats::model.frame(formula, data = mf, na.action = stats::na.pass),
    error = function(e) stop("Could not build the model data: ",
                             conditionMessage(e), call. = FALSE)
  )
  if (nrow(mf) < 5)
    stop(sprintf("Only %d complete rows are available; at least 5 are needed to fit a model.",
                 nrow(mf)), call. = FALSE)

  y <- mf[[outcome]]
  if (outcome_type == "binary") {
    y <- droplevels(as.factor(y))
    if (nlevels(y) != 2)
      stop(sprintf("A binary model needs exactly two observed outcome levels, but the complete rows contain %d.",
                   nlevels(y)), call. = FALSE)
    mf[[outcome]] <- y
  } else {
    y <- suppressWarnings(as.numeric(y))
    if (any(!is.finite(y)))
      stop("The selected numeric outcome contains no usable finite values.", call. = FALSE)
    mf[[outcome]] <- y
  }
  mf
}

safe_aic <- function(fit) {
  tryCatch(as.numeric(stats::AIC(fit)), error = function(e) NA_real_)
}

named_metric_table <- function(values) {
  data.frame(
    Metric = names(values),
    Value = unname(values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

coefficient_table <- function(term, estimate, std_error, statistic, p_value,
                              ci_lower, ci_upper, effect_name = NULL) {
  tab <- data.frame(
    Term = as.character(term),
    Estimate = as.numeric(estimate),
    `Std. Error` = as.numeric(std_error),
    Statistic = as.numeric(statistic),
    `p-value` = as.numeric(p_value),
    `CI lower` = as.numeric(ci_lower),
    `CI upper` = as.numeric(ci_upper),
    check.names = FALSE, stringsAsFactors = FALSE,
    row.names = NULL
  )
  if (!is.null(effect_name)) {
    tab[[effect_name]] <- exp(tab$Estimate)
    tab[[paste0(effect_name, " CI lower")]] <- exp(tab[["CI lower"]])
    tab[[paste0(effect_name, " CI upper")]] <- exp(tab[["CI upper"]])
  }
  tab
}

matrix_to_coefficient_table <- function(co, ci_lower, ci_upper,
                                        p_col = NULL, statistic_col = 3,
                                        effect_name = NULL, df = NULL) {
  est <- co[, 1]
  se <- co[, 2]
  stat <- if (!is.null(statistic_col)) co[, statistic_col] else est / se
  if (!is.null(p_col) && p_col %in% colnames(co)) {
    p <- co[, p_col]
  } else {
    if (is.null(df)) df <- Inf
    p <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
  }
  coefficient_table(rownames(co), est, se, stat, p,
                    ci_lower, ci_upper, effect_name = effect_name)
}

# summary() omits aliased coefficients from its coefficient matrix, while
# confint() retains them with NA bounds.  Align both objects to coef(fit)'s
# complete term list so rank-deficient models produce an explicit NA row rather
# than a raw "arguments imply differing number of rows" error.
align_model_coefficients <- function(co, fit, ci) {
  terms <- names(stats::coef(fit))
  full <- matrix(NA_real_, nrow = length(terms), ncol = ncol(co),
                 dimnames = list(terms, colnames(co)))
  common <- intersect(rownames(co), terms)
  if (length(common)) full[common, ] <- co[common, , drop = FALSE]
  ci_full <- matrix(NA_real_, nrow = length(terms), ncol = ncol(ci),
                    dimnames = list(terms, colnames(ci)))
  common_ci <- intersect(rownames(ci), terms)
  if (length(common_ci)) ci_full[common_ci, ] <- ci[common_ci, , drop = FALSE]
  list(co = full, ci = ci_full)
}

model_interpretation <- function(coef_table, label, effect_name = NULL) {
  keep <- coef_table$Term != "(Intercept)" &
    is.finite(coef_table[["p-value"]]) & coef_table[["p-value"]] < 0.05
  if (!any(keep))
    return(sprintf("The %s found no predictor with a p-value below 0.05 after adjusting for the other selected predictors. Associations are not proof of causation.",
                   label))
  terms <- coef_table$Term[keep]
  shown <- paste(utils::head(terms, 3), collapse = ", ")
  suffix <- if (length(terms) > 3) " and other terms" else ""
  effect_note <- if (is.null(effect_name))
    "Coefficients are on the model's outcome scale."
  else
    sprintf("Exponentiated coefficients are shown as %s.", effect_name)
  sprintf("The %s identified evidence of an adjusted association for %s%s. %s Associations are not proof of causation.",
          label, shown, suffix, effect_note)
}

diagnostic_plot <- function(fit, method) {
  fitted_values <- tryCatch(as.numeric(stats::fitted(fit)), error = function(e) NULL)
  residual_values <- tryCatch({
    if (inherits(fit, "glm")) stats::residuals(fit, type = "deviance")
    else stats::residuals(fit)
  }, error = function(e) NULL)
  if (is.null(fitted_values) || is.null(residual_values) ||
      length(fitted_values) != length(residual_values)) return(NULL)
  d <- data.frame(fitted = fitted_values, residual = residual_values)
  ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_point(alpha = 0.65, colour = "#018786") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "#D55E00") +
    ggplot2::labs(
      title = paste("Residuals vs fitted values -", MODEL_METHOD_LABELS[[method]]),
      x = "Fitted value", y = "Residual"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

model_assumptions <- function(fit, method, outcome_type) {
  notes <- list()
  add_note <- function(text) notes[[length(notes) + 1]] <<- list(note = text)

  n <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
  if (method == "ols") {
    res <- tryCatch(stats::residuals(fit), error = function(e) numeric(0))
    norm <- check_normality(res)
    add_note(paste("Residual normality:", norm$note))
    bp <- tryCatch(lmtest::bptest(fit), error = function(e) NULL)
    if (is.null(bp)) add_note("Breusch-Pagan heteroscedasticity check could not be evaluated.")
    else add_note(sprintf("Breusch-Pagan p = %s: %s.", p_fmt(bp$p.value),
                          if (bp$p.value < 0.05) "evidence of non-constant variance; consider robust regression" else "no strong evidence of non-constant variance"))
  } else if (method == "robust") {
    add_note("Robust M-estimation reduces the influence of outlying residuals; coefficient p-values use a large-sample t approximation.")
  } else if (method == "quantile") {
    add_note("Median / quantile regression estimates the selected conditional quantile and does not require normally distributed residuals.")
  }

  influence <- if (grepl("^(gee|lmm|glmm)", method)) NULL
               else tryCatch(stats::cooks.distance(fit), error = function(e) NULL)
  if (!is.null(influence) && length(influence) && is.finite(n) && n > 0) {
    threshold <- 4 / n
    flagged <- sum(influence > threshold, na.rm = TRUE)
    add_note(sprintf("Cook's distance flagged %d observation%s above 4/n (threshold %.4f).",
                     flagged, if (flagged == 1) "" else "s", threshold))
  }

  if (outcome_type == "count" && method %in% c("poisson", "negative_binomial",
                                                 "gee_poisson", "glmm_poisson")) {
    dispersion <- tryCatch(
      sum(stats::residuals(fit, type = "pearson")^2, na.rm = TRUE) /
        stats::df.residual(fit), error = function(e) NA_real_)
    if (is.finite(dispersion))
      add_note(sprintf("Pearson dispersion = %.3f; values well above 1 indicate overdispersion.", dispersion))
  }
  # Keep this diagnostic visible for every regression family that exposes a
  # model matrix.  It is especially important for health-data models where a
  # near-duplicate predictor can make an apparently precise estimate unstable.
  vif <- check_multicollinearity(fit)
  if (!is.null(vif$note)) add_note(vif$note)
  if (length(notes) == 0) add_note("No additional formal diagnostics were available for this model.")
  notes
}

mixed_coefficient_table <- function(fit, effect_name = NULL) {
  est <- lme4::fixef(fit)
  vc <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(vc)); stat <- est / se
  p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  ci <- cbind(est - stats::qnorm(0.975) * se,
              est + stats::qnorm(0.975) * se)
  coefficient_table(names(est), est, se, stat, p, ci[, 1], ci[, 2],
                    effect_name = effect_name)
}

clustered_model_assumptions <- function(fit, method, outcome_type) {
  notes <- model_assumptions(fit, method, outcome_type)
  if (grepl("^gee", method))
    notes[[length(notes) + 1]] <- list(note = "GEE estimates population-average effects and uses a robust sandwich variance for within-cluster correlation; review whether the working correlation is plausible.")
  if (grepl("^(lmm|glmm)", method)) {
    singular <- tryCatch(lme4::isSingular(fit, tol = 1e-4), error = function(e) NA)
    notes[[length(notes) + 1]] <- list(note = if (isTRUE(singular))
      "The mixed model appears singular; the random-effects structure may be too complex for the data."
      else "The random-effects structure did not trigger a singular-fit warning.")
    conv <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
    if (!is.null(conv)) notes[[length(notes) + 1]] <- list(note = paste("Optimizer message:", paste(conv, collapse = " ")))
  }
  notes
}

mixed_formula <- function(formula, cluster, random_slope = NULL) {
  cluster_term <- paste0("`", cluster, "`")
  random_term <- if (is.null(random_slope) || !nzchar(random_slope) ||
                     identical(random_slope, "(none)")) {
    sprintf("(1 | %s)", cluster_term)
  } else {
    sprintf("(1 + `%s` | %s)", random_slope, cluster_term)
  }
  stats::as.formula(paste(paste(deparse(formula), collapse = " "), "+", random_term))
}

run_gee_model <- function(mf, formula, method, outcome_type, cluster,
                          corstr, time, n_total) {
  if (!requireNamespace("geepack", quietly = TRUE))
    stop("GEE needs the geepack package. Run install_dependencies.R.", call. = FALSE)
  if (length(unique(mf[[cluster]])) < 2)
    stop("GEE needs at least two observed clusters.", call. = FALSE)
  family <- switch(outcome_type,
    continuous = stats::gaussian(),
    count = stats::poisson(),
    binary = stats::binomial())
  gee_data <- mf
  gee_data[[".__gee_id"]] <- mf[[cluster]]
  use_waves <- identical(corstr, "ar1")
  if (use_waves) {
  if (is.null(time) || !nzchar(time))
      stop("AR-1 GEE needs a time / ordering variable.", call. = FALSE)
    gee_data[[".__gee_waves"]] <- as.numeric(as.factor(mf[[time]]))
  }
  gee_formula <- formula
  if (outcome_type == "binary") {
    y <- mf[[all.vars(formula)[1]]]
    gee_data[[".__gee_y"]] <- as.numeric(y == levels(y)[2])
    rhs <- paste(deparse(formula[[3]]), collapse = " ")
    gee_formula <- stats::as.formula(paste(".__gee_y ~", rhs))
  }
  fit <- tryCatch({
    if (use_waves)
      geepack::geeglm(gee_formula, data = gee_data, id = .__gee_id,
                      waves = .__gee_waves, family = family, corstr = corstr)
    else
      geepack::geeglm(gee_formula, data = gee_data, id = .__gee_id,
                      family = family, corstr = corstr)
  }, error = function(e) stop("GEE could not be fitted: ", conditionMessage(e), call. = FALSE))
  sm <- summary(fit)
  co <- sm$coefficients
  effect_name <- if (outcome_type == "binary") "Odds ratio"
                  else if (outcome_type == "count") "Rate ratio" else NULL
  ci <- cbind(co[, 1] - stats::qnorm(0.975) * co[, 2],
              co[, 1] + stats::qnorm(0.975) * co[, 2])
  coef_tab <- matrix_to_coefficient_table(co, ci[, 1], ci[, 2],
                                          p_col = "Pr(>|W|)",
                                          effect_name = effect_name)
  qic <- tryCatch(geepack::QIC(fit), error = function(e) c(QIC = NA_real_, QICu = NA_real_))
  qic_values <- qic[intersect(c("QIC", "QICu"), names(qic))]
  metrics <- c("n used" = stats::nobs(fit),
               "clusters" = length(unique(mf[[cluster]])),
               "working correlation" = corstr,
               qic_values)
  label <- MODEL_METHOD_LABELS[[method]]
  model_result(label, method, formula, fit, coef_tab, named_metric_table(metrics),
               clustered_model_assumptions(fit, method, outcome_type),
               outcome_type, n_total, stats::nobs(fit),
               effect_name = effect_name,
               recommendation = sprintf("%s estimates population-average effects across %d clusters using a %s working correlation.",
                                         label, length(unique(mf[[cluster]])), corstr))
}

run_lmm_model <- function(mf, formula, cluster, random_slope, n_total) {
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("LMM needs the lme4 package. Run install_dependencies.R.", call. = FALSE)
  mf_formula <- mixed_formula(formula, cluster, random_slope)
  fit <- tryCatch(
    lme4::lmer(mf_formula, data = mf, REML = FALSE),
    error = function(e) stop("LMM could not be fitted: ", conditionMessage(e), call. = FALSE)
  )
  coef_tab <- mixed_coefficient_table(fit)
  stats_tab <- named_metric_table(c(
    "n used" = stats::nobs(fit),
    "clusters" = length(unique(mf[[cluster]])),
    "AIC" = safe_aic(fit),
    "BIC" = tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_),
    "log-likelihood" = as.numeric(stats::logLik(fit))
  ))
  model_result(MODEL_METHOD_LABELS[["lmm"]], "lmm", mf_formula, fit,
               coef_tab, stats_tab,
               clustered_model_assumptions(fit, "lmm", "continuous"),
               "continuous", n_total, stats::nobs(fit),
               recommendation = "LMM estimates subject-specific effects while accounting for correlation within the random-intercept cluster.")
}

run_glmm_model <- function(mf, formula, method, outcome_type, cluster,
                           random_slope, n_total) {
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("GLMM needs the lme4 package. Run install_dependencies.R.", call. = FALSE)
  mf_formula <- mixed_formula(formula, cluster, random_slope)
  family <- if (outcome_type == "binary") stats::binomial() else stats::poisson()
  fit <- tryCatch(
    lme4::glmer(mf_formula, data = mf, family = family,
                control = lme4::glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5))),
    error = function(e) stop("GLMM could not be fitted: ", conditionMessage(e), call. = FALSE)
  )
  effect_name <- if (outcome_type == "binary") "Odds ratio" else "Rate ratio"
  coef_tab <- mixed_coefficient_table(fit, effect_name = effect_name)
  stats_tab <- named_metric_table(c(
    "n used" = stats::nobs(fit),
    "clusters" = length(unique(mf[[cluster]])),
    "AIC" = safe_aic(fit),
    "BIC" = tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_),
    "log-likelihood" = as.numeric(stats::logLik(fit))
  ))
  model_result(MODEL_METHOD_LABELS[[method]], method, mf_formula, fit,
               coef_tab, stats_tab,
               clustered_model_assumptions(fit, method, outcome_type),
               outcome_type, n_total, stats::nobs(fit),
               effect_name = effect_name,
               recommendation = sprintf("%s estimates subject-specific effects with a random intercept for %s.",
                                         MODEL_METHOD_LABELS[[method]], cluster))
}

model_result <- function(label, method, formula, fit, coefficients, fit_stats,
                         assumptions, outcome_type, n_total, n_used,
                         effect_name = NULL, recommendation = NULL, note = NULL) {
  list(
    label = label,
    method = method,
    formula = paste(deparse(formula), collapse = " "),
    fit = fit,
    coefficients = coefficients,
    fit_stats = fit_stats,
    assumptions = assumptions,
    diagnostic_plot = diagnostic_plot(fit, method),
    interpretation = model_interpretation(coefficients, label, effect_name),
    rationale = recommendation %||% sprintf("Fitted %s using %d complete rows out of %d.", label, n_used, n_total),
    note = note %||% "Coefficients are adjusted for the other selected predictors.",
    n_total = n_total,
    n_used = n_used,
    effect_name = effect_name,
    recommendation = recommendation
  )
}

run_ols_model <- function(mf, formula, n_total) {
  fit <- stats::lm(formula, data = mf)
  sm <- summary(fit)
  ci <- stats::confint(fit)
  aligned <- align_model_coefficients(sm$coefficients, fit, ci)
  coef_tab <- matrix_to_coefficient_table(aligned$co, aligned$ci[, 1], aligned$ci[, 2],
                                          p_col = "Pr(>|t|)", df = fit$df.residual)
  stats_tab <- named_metric_table(c(
    "n used" = stats::nobs(fit),
    "R-squared" = sm$r.squared,
    "Adjusted R-squared" = sm$adj.r.squared,
    "Residual standard error" = sm$sigma,
    "AIC" = safe_aic(fit)
  ))
  model_result("OLS linear regression", "ols", formula, fit, coef_tab,
               stats_tab, model_assumptions(fit, "ols", "continuous"),
               "continuous", n_total, stats::nobs(fit),
               recommendation = "OLS is the reference model for a continuous outcome. Review the residual checks before treating the coefficients as reliable.")
}

run_robust_model <- function(mf, formula, n_total) {
  if (!requireNamespace("MASS", quietly = TRUE))
    stop("Robust regression needs the MASS package. Run install_dependencies.R.", call. = FALSE)
  fit <- MASS::rlm(formula, data = mf, psi = MASS::psi.huber)
  sm <- summary(fit)
  co <- sm$coefficients
  df <- max(1, fit$df.residual)
  ci <- cbind(co[, 1] - stats::qt(0.975, df) * co[, 2],
              co[, 1] + stats::qt(0.975, df) * co[, 2])
  coef_tab <- matrix_to_coefficient_table(co, ci[, 1], ci[, 2],
                                          p_col = NULL, df = df)
  y <- mf[[all.vars(formula)[1]]]
  pseudo_r2 <- 1 - sum(stats::residuals(fit)^2) / sum((y - mean(y))^2)
  stats_tab <- named_metric_table(c(
    "n used" = stats::nobs(fit),
    "Pseudo R-squared" = pseudo_r2,
    "AIC" = safe_aic(fit)
  ))
  model_result("Robust M-estimator regression", "robust", formula, fit,
               coef_tab, stats_tab, model_assumptions(fit, "robust", "continuous"),
               "continuous", n_total, stats::nobs(fit),
               recommendation = "Robust regression downweights observations with large residuals; compare its coefficients with OLS and inspect the influence diagnostics.")
}

run_quantile_model <- function(mf, formula, tau, n_total) {
  if (!requireNamespace("quantreg", quietly = TRUE))
    stop("Quantile regression needs the quantreg package. Run install_dependencies.R.", call. = FALSE)
  if (!is.finite(tau) || tau <= 0 || tau >= 1)
    stop("The quantile must be greater than 0 and less than 1.", call. = FALSE)
  fit <- quantreg::rq(formula, data = mf, tau = tau)
  sm <- summary(fit, se = "nid")
  co <- sm$coefficients
  ci <- cbind(co[, 1] - stats::qnorm(0.975) * co[, 2],
              co[, 1] + stats::qnorm(0.975) * co[, 2])
  coef_tab <- matrix_to_coefficient_table(co, ci[, 1], ci[, 2],
                                          p_col = "Pr(>|t|)", statistic_col = 3)
  stats_tab <- named_metric_table(c(
    "n used" = nrow(mf),
    "quantile (tau)" = tau,
    "AIC" = safe_aic(fit)
  ))
  model_result(sprintf("Quantile regression (tau = %.2f)", tau), "quantile",
               formula, fit, coef_tab, stats_tab,
               model_assumptions(fit, "quantile", "continuous"),
               "continuous", n_total, nrow(mf),
               recommendation = "Quantile regression models the selected conditional quantile and is useful when the mean is not representative of the outcome distribution.")
}

# Penalised regression via glmnet, penalty chosen by cross-validation.
# `method` is "lasso" (alpha = 1, performs variable selection - some
# coefficients become exactly zero) or "ridge" (alpha = 0, shrinks all
# coefficients but keeps them). Neither yields valid standard errors or
# p-values, so the coefficient table reports the penalised estimates and the
# effect ratio where meaningful - not SE / p / CI. LASSO additionally reports a
# Selected column.
run_penalised_model <- function(mf, formula, outcome_type, method = "lasso",
                                lambda_choice = "min", n_total, seed = 1L) {
  if (!requireNamespace("glmnet", quietly = TRUE))
    stop("Penalised regression needs the glmnet package. Run install_dependencies.R.", call. = FALSE)
  select <- identical(method, "lasso")
  alpha <- if (select) 1 else 0
  disp <- if (select) "LASSO" else "Ridge"
  lambda_choice <- if (identical(lambda_choice, "1se")) "1se" else "min"

  mm <- stats::model.matrix(formula, data = mf)
  x <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  if (ncol(x) < 2)
    stop(sprintf("%s regression needs at least two predictor terms; add another predictor or use OLS/logistic/Poisson for a single predictor.", disp),
         call. = FALSE)

  outcome <- all.vars(formula)[1]
  family <- switch(outcome_type, continuous = "gaussian",
                   count = "poisson", binary = "binomial")
  if (outcome_type == "binary") {
    yf <- droplevels(as.factor(mf[[outcome]]))
    y <- as.integer(yf == levels(yf)[2])
  } else {
    y <- as.numeric(mf[[outcome]])
  }

  # Deterministic CV folds so the reported model is reproducible across runs.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)

  nfolds <- max(3L, min(10L, nrow(x)))
  cvfit <- tryCatch(
    glmnet::cv.glmnet(x, y, family = family, alpha = alpha, nfolds = nfolds,
                      standardize = TRUE),
    error = function(e) stop(sprintf("%s could not be fitted: ", disp),
                             conditionMessage(e), call. = FALSE))
  lambda <- if (lambda_choice == "1se") cvfit$lambda.1se else cvfit$lambda.min

  co <- as.matrix(stats::coef(cvfit, s = lambda))
  est <- co[, 1]
  effect_name <- switch(outcome_type, binary = "Odds ratio",
                        count = "Rate ratio", NULL)
  coef_tab <- data.frame(
    Term = rownames(co),
    Estimate = as.numeric(est),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  if (select)
    coef_tab$Selected <- ifelse(rownames(co) == "(Intercept)", "-",
                                ifelse(abs(est) > 0, "yes", "no"))
  if (!is.null(effect_name)) coef_tab[[effect_name]] <- exp(coef_tab$Estimate)

  nonzero <- sum(abs(est[names(est) != "(Intercept)"]) > 0)
  idx <- which(cvfit$lambda == lambda)
  dev_ratio <- if (length(idx)) cvfit$glmnet.fit$dev.ratio[idx] else NA_real_
  cv_error <- if (length(idx)) cvfit$cvm[idx] else NA_real_
  metrics <- c("n used" = nrow(x), "predictors offered" = ncol(x))
  if (select) metrics <- c(metrics, "predictors selected" = nonzero)
  metrics <- c(metrics,
    "lambda" = lambda,
    "lambda rule" = if (lambda_choice == "1se") "1-SE" else "minimum CV error",
    "% deviance explained" = round(100 * dev_ratio, 2),
    "CV error" = round(cv_error, 4))
  stats_tab <- named_metric_table(metrics)

  interp <- if (select) {
    kept <- coef_tab$Term[coef_tab$Selected == "yes"]
    if (length(kept))
      sprintf("LASSO retained %d of %d predictor term(s): %s. Coefficients are penalised (shrunk toward zero) for prediction and selection; standard errors and p-values are intentionally not reported because post-selection inference is not valid from the naive fit.",
              length(kept), ncol(x), paste(utils::head(kept, 6), collapse = ", "))
    else
      sprintf("LASSO shrank every one of the %d predictor term(s) to zero at the selected penalty; none carried independent predictive signal after regularisation.",
              ncol(x))
  } else {
    sprintf("Ridge regression shrank all %d predictor coefficients toward zero to stabilise the estimates (useful when predictors are correlated); it keeps every predictor rather than selecting a subset. Standard errors and p-values are intentionally not reported because valid post-shrinkage inference is not available from the naive fit.",
            ncol(x))
  }

  assumptions <- list(
    list(note = sprintf("Penalty chosen by %d-fold cross-validation (%s rule); lambda = %.4g.",
                        nfolds, if (lambda_choice == "1se") "1-SE" else "minimum-CV", lambda)),
    list(note = "Predictors are standardised before fitting; coefficients are returned on the original scale."),
    list(note = sprintf("%s regression is for prediction/stabilisation - it does not provide valid standard errors, confidence intervals, or p-values.", disp)))

  # CV curve as the diagnostic plot.
  cv_df <- data.frame(log_lambda = log(cvfit$lambda), cvm = cvfit$cvm,
                      lo = cvfit$cvlo, up = cvfit$cvup)
  diag_plot <- ggplot2::ggplot(cv_df, ggplot2::aes(x = log_lambda, y = cvm)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = up), colour = "grey75") +
    ggplot2::geom_point(colour = "#018786") +
    ggplot2::geom_vline(xintercept = log(lambda), linetype = "dashed",
                        colour = "#D55E00") +
    ggplot2::labs(title = sprintf("%s cross-validation curve", disp),
                  x = "log(lambda)", y = "Mean cross-validated error") +
    ggplot2::theme_minimal(base_size = 13)

  rationale <- if (select)
    sprintf("LASSO regression selected %d of %d candidate predictor term(s) using cross-validated L1 regularisation.",
            nonzero, ncol(x))
  else
    sprintf("Ridge regression shrank all %d predictor coefficients using cross-validated L2 regularisation.", ncol(x))
  note <- if (select)
    "Retained predictors have non-zero penalised coefficients; dropped predictors were shrunk to exactly zero."
  else
    "All predictors are retained; L2 shrinkage reduces variance from correlated predictors."

  list(
    label = MODEL_METHOD_LABELS[[method]], method = method,
    formula = paste(deparse(formula), collapse = " "),
    fit = cvfit, coefficients = coef_tab, fit_stats = stats_tab,
    assumptions = assumptions, diagnostic_plot = diag_plot,
    interpretation = interp, rationale = rationale, note = note,
    n_total = n_total, n_used = nrow(x), effect_name = effect_name,
    recommendation = NULL)
}

# Partial least squares (PLS) regression for a continuous outcome. Builds
# orthogonal latent components from the predictors (useful when predictors are
# many or highly collinear); the number of components is chosen by CV RMSEP.
# Like the penalised models, PLS does not yield valid SE / p-values.
run_pls_model <- function(mf, formula, n_total, seed = 1L) {
  if (!requireNamespace("pls", quietly = TRUE))
    stop("PLS regression needs the pls package. Run install_dependencies.R.", call. = FALSE)
  mm <- stats::model.matrix(formula, data = mf)
  x <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  if (ncol(x) < 2)
    stop("PLS regression needs at least two predictor terms; use OLS for a single predictor.",
         call. = FALSE)
  max_comp <- min(ncol(x), nrow(mf) - 1L)

  # Deterministic CV segments so the reported component count is reproducible.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  on.exit({ if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) }, add = TRUE)

  fit <- tryCatch(
    pls::plsr(formula, data = mf, scale = TRUE, validation = "CV"),
    error = function(e) stop("PLS could not be fitted: ", conditionMessage(e),
                             call. = FALSE))
  # pls's RMSEP()/MSEP()/R2() (in the installed version) rewrite their inner
  # call and evaluate it in the CALLER's frame, so they fail with "could not
  # find function MSEP / mvrValstats" unless the whole pls package is attached.
  # We only load pls via ::, so call the exported pls::mvrValstats() directly -
  # exactly what those wrappers use - and derive RMSEP (= sqrt(SSE / nobj)) and
  # R2 (= 1 - SSE / SST) ourselves. Index 1 on the model axis = zero components.
  cv_stats <- pls::mvrValstats(fit, estimate = "CV")
  rmsep <- sqrt(cv_stats$SSE[1, 1, ] / cv_stats$nobj[1])
  ncomp <- max(1L, which.min(rmsep) - 1L)

  co <- drop(stats::coef(fit, ncomp = ncomp))
  coef_tab <- data.frame(Term = names(co), Estimate = as.numeric(co),
                         check.names = FALSE, stringsAsFactors = FALSE,
                         row.names = NULL)
  x_var <- unname(cumsum(pls::explvar(fit))[ncomp])
  train_stats <- pls::mvrValstats(fit, estimate = "train")
  r2y <- unname(drop(1 - train_stats$SSE / c(train_stats$SST))[ncomp + 1L])
  stats_tab <- named_metric_table(c(
    "n used" = nrow(mf),
    "predictors" = ncol(x),
    "components used" = ncomp,
    "max components" = max_comp,
    "% X variance explained" = round(x_var, 2),
    "% Y variance explained (R2)" = round(100 * r2y, 2),
    "CV RMSEP" = unname(round(rmsep[ncomp + 1L], 4))))

  comp_df <- data.frame(components = seq_along(rmsep) - 1L, rmsep = rmsep)
  diag_plot <- ggplot2::ggplot(comp_df, ggplot2::aes(x = components, y = rmsep)) +
    ggplot2::geom_line(colour = "grey60") +
    ggplot2::geom_point(colour = "#018786") +
    ggplot2::geom_vline(xintercept = ncomp, linetype = "dashed", colour = "#D55E00") +
    ggplot2::labs(title = "PLS cross-validation (RMSEP by components)",
                  x = "Number of components", y = "CV RMSEP") +
    ggplot2::theme_minimal(base_size = 13)

  list(
    label = MODEL_METHOD_LABELS[["pls"]], method = "pls",
    formula = paste(deparse(formula), collapse = " "),
    fit = fit, coefficients = coef_tab, fit_stats = stats_tab,
    assumptions = list(
      list(note = sprintf("%d component(s) chosen by minimum cross-validated RMSEP (of up to %d).", ncomp, max_comp)),
      list(note = "Predictors are standardised; coefficients are returned on the original outcome scale."),
      list(note = "PLS is a predictive/dimension-reduction method - it does not provide valid standard errors, confidence intervals, or p-values.")),
    diagnostic_plot = diag_plot,
    interpretation = sprintf("PLS regression summarised the %d predictors into %d orthogonal component(s), explaining %.1f%% of the outcome's variance. Coefficients show each predictor's contribution; PLS is used for prediction and collinear data, so no p-values are reported.",
                             ncol(x), ncomp, 100 * r2y),
    rationale = sprintf("PLS regression reduced %d predictors to %d cross-validated component(s).", ncol(x), ncomp),
    note = "PLS is well suited to many or highly correlated predictors.",
    n_total = n_total, n_used = nrow(mf), effect_name = NULL,
    recommendation = NULL)
}

run_glm_model <- function(mf, formula, method, outcome_type, n_total) {
  if (method == "logistic") {
    fit <- stats::glm(formula, data = mf, family = stats::binomial())
    effect_name <- "Odds ratio"
    label <- "Logistic GLM"
  } else if (method == "poisson") {
    fit <- stats::glm(formula, data = mf, family = stats::poisson())
    effect_name <- "Rate ratio"
    label <- "Poisson GLM"
  } else {
    if (!requireNamespace("MASS", quietly = TRUE))
      stop("Negative binomial regression needs the MASS package. Run install_dependencies.R.", call. = FALSE)
    fit <- MASS::glm.nb(formula, data = mf)
    effect_name <- "Rate ratio"
    label <- "Negative binomial GLM"
  }
  sm <- summary(fit)
  co <- sm$coefficients
  ci <- suppressWarnings(stats::confint.default(fit))
  aligned <- align_model_coefficients(co, fit, ci)
  coef_tab <- matrix_to_coefficient_table(aligned$co, aligned$ci[, 1], aligned$ci[, 2],
                                          p_col = "Pr(>|z|)",
                                          effect_name = effect_name)
  null_dev <- fit$null.deviance
  pseudo_r2 <- if (is.finite(null_dev) && null_dev > 0)
    1 - fit$deviance / null_dev else NA_real_
  dispersion <- if (outcome_type == "count")
    sum(stats::residuals(fit, type = "pearson")^2, na.rm = TRUE) /
      stats::df.residual(fit) else NA_real_
  stats_tab <- named_metric_table(c(
    "n used" = stats::nobs(fit),
    "Null deviance" = fit$null.deviance,
    "Residual deviance" = fit$deviance,
    "Pseudo R-squared" = pseudo_r2,
    "AIC" = safe_aic(fit),
    "Pearson dispersion" = dispersion
  ))
  recommendation <- if (method == "poisson" && is.finite(dispersion) && dispersion > 1.5)
    sprintf("Pearson dispersion is %.2f; negative binomial regression is preferable because the Poisson model appears overdispersed.", dispersion)
  else if (method == "poisson")
    "Poisson regression is appropriate when the conditional mean and variance are reasonably aligned; inspect the dispersion diagnostic."
  else if (method == "negative_binomial")
    "Negative binomial regression allows more variability than Poisson regression and is appropriate when counts are overdispersed."
  else
    sprintf("Logistic regression models the odds of the second observed outcome level (%s).", levels(mf[[all.vars(formula)[1]]])[2])
  model_result(label, method, formula, fit, coef_tab, stats_tab,
               model_assumptions(fit, method, outcome_type), outcome_type,
               n_total, stats::nobs(fit), effect_name = effect_name,
               recommendation = recommendation)
}

#' Fit a selected independent regression or clustered model.
#'
#' spec fields: outcome, outcome_type, predictors, method, tau, cluster,
#' corstr, time, random_slope.
run_guided_model <- function(df, spec) {
  if (is.null(spec$outcome) || !nzchar(spec$outcome) ||
      !spec$outcome %in% names(df))
    stop("Choose a valid outcome variable.", call. = FALSE)
  if (is.null(spec$predictors) || length(spec$predictors) == 0)
    stop("Choose at least one predictor.", call. = FALSE)
  if (any(!spec$predictors %in% names(df)))
    stop("One or more selected predictors are not present in the data.", call. = FALSE)
  if (spec$outcome %in% spec$predictors)
    stop("The outcome cannot also be selected as a predictor.", call. = FALSE)
  if (!spec$outcome_type %in% MODEL_OUTCOME_TYPES)
    stop("The selected outcome type is not supported by Phase 3.", call. = FALSE)
  all_methods <- c(available_model_methods(spec$outcome_type, "independent"),
                   available_model_methods(spec$outcome_type, "clustered"))
  if (!spec$method %in% all_methods)
    stop("That model method is not available for the selected outcome type.", call. = FALSE)

  clustered <- grepl("^(gee|lmm|glmm)", spec$method)
  if (clustered) {
    if (is.null(spec$cluster) || !nzchar(spec$cluster) ||
        !spec$cluster %in% names(df))
      stop("Choose a valid subject or cluster ID for this model.", call. = FALSE)
    if (spec$cluster %in% c(spec$outcome, spec$predictors))
      stop("The subject or cluster ID must be different from the outcome and predictors.", call. = FALSE)
    if (length(unique(df[[spec$cluster]][!is.na(df[[spec$cluster]])])) < 2)
      stop("The selected subject or cluster ID has fewer than two observed clusters.", call. = FALSE)
  }
  if (identical(spec$method, "gee_gaussian") || identical(spec$method, "gee_binomial") ||
      identical(spec$method, "gee_poisson")) {
    if (is.null(spec$corstr) || !spec$corstr %in% c("independence", "exchangeable", "ar1"))
      stop("Choose a valid GEE working correlation.", call. = FALSE)
    if (identical(spec$corstr, "ar1") && (is.null(spec$time) || !nzchar(spec$time)))
      stop("AR-1 GEE needs a time / ordering variable.", call. = FALSE)
  }
  random_slope <- spec$random_slope
  if (is.null(random_slope) || identical(random_slope, "(none)")) random_slope <- NULL
  if (!is.null(random_slope) && !random_slope %in% spec$predictors)
    stop("The random slope must be one of the selected predictors.", call. = FALSE)

  formula <- model_formula(spec$outcome, spec$predictors)
  n_total <- nrow(df)
  extras <- if (clustered)
    unique(c(spec$cluster, spec$time, random_slope)) else character(0)
  extras <- extras[!is.na(extras) & nzchar(extras)]
  mf <- model_frame(df, formula, spec$outcome, spec$outcome_type,
                    extra_vars = extras)
  if (clustered && spec$method %in% c("gee_gaussian", "gee_binomial", "gee_poisson"))
    return(run_gee_model(mf, formula, spec$method, spec$outcome_type,
                         spec$cluster, spec$corstr, spec$time, n_total))
  if (identical(spec$method, "lmm"))
    return(run_lmm_model(mf, formula, spec$cluster, random_slope, n_total))
  if (spec$method %in% c("glmm_binomial", "glmm_poisson"))
    return(run_glmm_model(mf, formula, spec$method, spec$outcome_type,
                          spec$cluster, random_slope, n_total))
  if (spec$method == "ols")
    return(run_ols_model(mf, formula, n_total))
  if (spec$method == "robust")
    return(run_robust_model(mf, formula, n_total))
  if (spec$method == "quantile")
    return(run_quantile_model(mf, formula, spec$tau %||% 0.5, n_total))
  if (spec$method %in% c("lasso", "ridge"))
    return(run_penalised_model(mf, formula, spec$outcome_type, spec$method,
                               spec$lambda_choice %||% "min", n_total))
  if (spec$method == "pls")
    return(run_pls_model(mf, formula, n_total))
  run_glm_model(mf, formula, spec$method, spec$outcome_type, n_total)
}

#' Fit a selected Phase 4 clustered/repeated-measures model.
#'
#' Keeping this wrapper separate from run_guided_model makes the phase boundary
#' explicit in the UI while preserving the tested shared fitting routines.
run_guided_clustered_model <- function(df, spec) {
  if (is.null(spec$method) || !grepl("^(gee|lmm|glmm)", spec$method))
    stop("Choose a GEE, LMM, or GLMM method for clustered data.", call. = FALSE)
  run_guided_model(df, spec)
}
