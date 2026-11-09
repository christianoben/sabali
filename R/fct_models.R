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
  glmm_poisson = "Generalised linear mixed model (Poisson GLMM)",
  # Design-based (survey-weighted) regression. Listed here as well as in
  # SURVEY_METHOD_LABELS because every shared consumer - the diagnostic plot
  # title, the report, the planner catalogue - looks the label up in this vector.
  svy_gaussian = "Survey-weighted linear regression (design-based)",
  svy_binomial = "Survey-weighted logistic regression (design-based)",
  svy_poisson = "Survey-weighted Poisson regression (design-based)"
)

MODEL_OUTCOME_TYPES <- c("continuous", "count", "binary")
MODEL_PREDICTOR_TYPES <- c("continuous", "count", "binary", "nominal", "ordinal")

#' Diagnostics as a display table. Named checks with a graded verdict and the
#' suggested action, rather than the old "Diagnostic 1 / Diagnostic 2" listing
#' that threw the verdict away.
model_assumptions_df <- function(assumptions) {
  if (length(assumptions) == 0)
    return(data.frame(Check = character(0), Verdict = character(0),
                      Note = character(0), `What to do` = character(0),
                      check.names = FALSE, stringsAsFactors = FALSE))
  lab <- function(x, i) {
    if (!is.null(x$label) && nzchar(x$label) && !identical(x$check, "context")) x$label
    else if (!is.null(x$label) && identical(x$check, "context")) x$label
    else paste("Diagnostic", i)
  }
  data.frame(
    Check = vapply(seq_along(assumptions),
                   function(i) lab(assumptions[[i]], i), character(1)),
    Verdict = vapply(assumptions, function(x) {
      s <- x$severity %||% "untested"
      unname(ASSUMPTION_SEVERITY_LABELS[s]) %||% ""
    }, character(1)),
    Note = vapply(assumptions, function(x) {
      if (is.null(x$note) || !length(x$note)) "" else as.character(x$note)[1]
    }, character(1)),
    `What to do` = vapply(assumptions, function(x) {
      rem <- x$remedy %||% list()
      if (!length(rem)) return("")
      paste(vapply(rem, function(z) z$what %||% "", character(1)), collapse = "; ")
    }, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
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

# Backtick-quote a column name only when R cannot parse it bare. Real exports
# carry names R will not accept as symbols - the TTC survey workbook has columns
# like `1_1_Household_ID`, and reformulate() fails outright on those - while
# quoting every name would change the Term label of every model already fitted.
.quote_term <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", x, "`"))
}

#' Split interaction specifications into their component variables.
#'
#' An interaction is carried through the app as a single string of
#' colon-separated variable names ("treatment:sex"), which is both what the user
#' picks in the UI and what R's formula notation expects. Returns a list of
#' character vectors, one per interaction.
interaction_parts <- function(interactions) {
  lapply(interactions, function(s) {
    p <- trimws(strsplit(as.character(s), ":", fixed = TRUE)[[1]])
    p[nzchar(p)]
  })
}

#' Canonical, order-insensitive key for an interaction.
#'
#' `a:b` and `b:a` are the same term to R, so the two must compare equal when we
#' de-duplicate them or match a saved selection against a rebuilt choice list.
.interaction_key <- function(interactions) {
  vapply(interaction_parts(interactions),
         function(p) paste(sort(p), collapse = ":"), character(1))
}

#' Build the model formula from the chosen predictors and interactions.
#'
#' Interactions are appended as explicit `a:b` terms rather than written as
#' `a*b`, because the main effects are validated to be present already (see
#' `run_guided_model()`); spelling the product term out keeps the formula, and
#' so the coefficient table, in the order the analyst selected.
model_formula <- function(outcome, predictors, interactions = character(0)) {
  if (length(predictors) == 0)
    stop("Choose at least one predictor for the model.", call. = FALSE)
  terms <- .quote_term(predictors)
  if (length(interactions)) {
    terms <- c(terms, vapply(interaction_parts(interactions),
                             function(p) paste(.quote_term(p), collapse = ":"),
                             character(1)))
  }
  stats::as.formula(
    paste(.quote_term(outcome), "~", paste(terms, collapse = " + ")),
    env = parent.frame())
}

#' Validate a set of interaction specifications against the chosen predictors.
#'
#' Returns the cleaned interactions. The main-effect rule is deliberate: an
#' interaction fitted without both of its main effects is almost always a
#' mis-specification, and its coefficients do not mean what they appear to.
validate_interactions <- function(interactions, predictors) {
  interactions <- interactions[!is.na(interactions) & nzchar(interactions)]
  if (!length(interactions)) return(character(0))
  parts <- interaction_parts(interactions)
  if (any(lengths(parts) < 2L))
    stop("An interaction needs at least two different variables.", call. = FALSE)
  if (any(vapply(parts, function(p) anyDuplicated(p) > 0L, logical(1))))
    stop("An interaction cannot use the same variable twice.", call. = FALSE)
  missing <- setdiff(unlist(parts), predictors)
  if (length(missing))
    stop(sprintf("Every variable in an interaction must also be selected as a predictor so its main effect is in the model. Add: %s.",
                 paste(unique(missing), collapse = ", ")), call. = FALSE)
  if (anyDuplicated(.interaction_key(interactions)) > 0L)
    stop("The same interaction is listed more than once.", call. = FALSE)
  interactions
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
  effect_note <- if (is.null(effect_name))
    "Coefficients are on the model's outcome scale."
  else
    sprintf("Exponentiated coefficients are shown as %s.", effect_name)

  # An interaction coefficient is a DIFFERENCE between effects, not an effect,
  # and once one is in the model the main effects of its components are
  # conditional on the other being at its reference. Reporting either as a plain
  # "adjusted association" would misstate what was estimated, so the two kinds
  # of term get separate sentences.
  is_ix <- grepl(":", terms, fixed = TRUE)
  listed <- function(x) {
    paste0(paste(utils::head(x, 3), collapse = ", "),
           if (length(x) > 3) " and other terms" else "")
  }
  main_part <- if (any(!is_ix))
    sprintf("The %s identified evidence of an adjusted association for %s.",
            label, listed(terms[!is_ix])) else
    sprintf("The %s found no main effect below 0.05.", label)
  ix_part <- if (any(is_ix)) sprintf(
    " The interaction %s %s below 0.05, so the effect of one variable in %s depends on the level of the other - read the main effects of those variables as conditional, not as an overall effect.",
    listed(terms[is_ix]),
    if (sum(is_ix) == 1) "is" else "are",
    if (sum(is_ix) == 1) "it" else "them") else ""

  paste0(main_part, ix_part, " ", effect_note,
         " Associations are not proof of causation.")
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
      title = paste("Residuals vs fitted values -",
                    if (method %in% names(MODEL_METHOD_LABELS))
                      MODEL_METHOD_LABELS[[method]] else method),
      x = "Fitted value", y = "Residual"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

#' Diagnostics for a fitted model.
#'
#' Returns a list of assumption_result()s - graded verdicts, not bare sentences.
#' Each still carries `note`, so every existing renderer keeps working, but the
#' verdict now survives to the UI, which can count violations, colour them and
#' show the remedy. Checks that are merely informational (an explanation of what
#' robust regression does, say) are returned as "ok" so they read as context
#' rather than as a problem.
model_assumptions <- function(fit, method, outcome_type) {
  out <- list()
  add <- function(res) out[[length(out) + 1L]] <<- res
  # A note that is context rather than a check: no id, no remedy, always "ok".
  add_context <- function(text)
    add(list(check = "context", label = "About this method", family = "distribution",
             severity = "ok", ok = TRUE, statistic = NA_real_, p = NA_real_,
             test = "", note = text, remedy = list(), detail = list()))

  n <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
  res <- tryCatch(stats::residuals(fit), error = function(e) numeric(0))

  if (method == "ols") {
    if (length(res)) {
      nr <- check_normality(res, id = "normality_residuals", what = "the residuals")
      add(nr)
    }
    bp <- tryCatch(lmtest::bptest(fit), error = function(e) NULL)
    if (is.null(bp)) {
      add(assumption_result("homoscedasticity", "untested",
                            "The Breusch-Pagan heteroscedasticity check could not be evaluated."))
    } else {
      # Breusch-Pagan has no natural effect size, so its p-value does drive this
      # verdict - graded, so a marginal result is not treated as a failure.
      sev <- if (!is.finite(bp$p.value)) "untested"
             else if (bp$p.value >= 0.05) "ok"
             else if (bp$p.value >= 0.01) "review" else "violated"
      add(assumption_result("homoscedasticity", sev,
        sprintf("Breusch-Pagan p = %s: %s.", p_fmt(bp$p.value),
                switch(sev,
                  ok = "no strong evidence of non-constant variance",
                  review = "some evidence of non-constant variance",
                  violated = "clear evidence of non-constant variance; consider robust regression",
                  "not evaluated")),
        p = bp$p.value, test = "Breusch-Pagan"))
    }
    # Serial dependence in the residuals, in the order the rows were supplied.
    if (length(res) >= 10) add(check_autocorrelation(res, fit))
  } else if (method == "robust") {
    add_context("Robust M-estimation reduces the influence of outlying residuals; coefficient p-values use a large-sample t approximation.")
  } else if (method == "quantile") {
    add_context("Median / quantile regression estimates the selected conditional quantile and does not require normally distributed residuals.")
  }

  influence <- if (grepl("^(gee|lmm|glmm)", method)) NULL
               else tryCatch(stats::cooks.distance(fit), error = function(e) NULL)
  if (!is.null(influence) && length(influence) && is.finite(n) && n > 0) {
    threshold <- 4 / n
    flagged <- sum(influence > threshold, na.rm = TRUE)
    share <- flagged / n
    # 4/n flags ~5% of well-behaved data by construction, so judge the share.
    sev <- if (share <= 0.05) "ok" else if (share <= 0.10) "review" else "violated"
    add(assumption_result("influence", sev,
      sprintf("Cook's distance flagged %d observation%s above 4/n (threshold %.4f)%s.",
              flagged, if (flagged == 1) "" else "s", threshold,
              if (sev == "ok") " - about what is expected by chance"
              else sprintf(", which is %.0f%% of the data", 100 * share)),
      statistic = share, detail = list(flagged = flagged, n = n, threshold = threshold)))
  }

  if (outcome_type == "count" && method %in% c("poisson", "negative_binomial",
                                                 "gee_poisson", "glmm_poisson")) {
    dispersion <- tryCatch(
      sum(stats::residuals(fit, type = "pearson")^2, na.rm = TRUE) /
        stats::df.residual(fit), error = function(e) NA_real_)
    if (is.finite(dispersion)) {
      sev <- if (dispersion <= 1.5) "ok" else if (dispersion <= 2) "review" else "violated"
      add(assumption_result("overdispersion", sev,
        sprintf("Pearson dispersion = %.3f%s.", dispersion,
                switch(sev,
                  ok = "; close enough to 1, so there is no sign of overdispersion",
                  review = "; mild overdispersion",
                  violated = "; substantial overdispersion, so a negative-binomial model is likely to fit better")),
        statistic = dispersion))
    }
    # Excess zeros are a different problem from overdispersion and need saying
    # separately: a Poisson model can have acceptable dispersion and still be
    # unable to account for the zeros.
    y <- tryCatch(stats::model.response(stats::model.frame(fit)), error = function(e) NULL)
    mu <- tryCatch(stats::fitted(fit), error = function(e) NULL)
    th <- tryCatch(fit$theta, error = function(e) NULL)
    if (!is.null(y) && is.numeric(y)) add(check_zero_inflation(y, mu, th))
  }

  # Kept visible for every family that exposes a model matrix. It matters most
  # for health data, where a near-duplicate predictor can make an apparently
  # precise estimate unstable.
  add(check_multicollinearity(fit))

  if (!length(out))
    add(assumption_result("context", "untested",
                          "No additional formal diagnostics were available for this model."))
  out
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

# Random-effects variance components and the intraclass correlation (ICC) for a
# fitted lme4 mixed model.  The fixed effects are reported separately in the
# coefficient table; this decomposes the *unexplained* variation into the
# between-cluster part (the random intercept) and the residual, and converts
# that decomposition into the ICC - the share of variance that sits between
# clusters, i.e. the correlation between two observations in the same cluster.
#
# Scale notes:
#  * LMM (Gaussian): residual variance is on the response scale, ICC is exact.
#  * Binomial GLMM: partitioned on the latent (logit) scale, where the residual
#    variance is fixed at pi^2 / 3 (the latent-scale, Nakagawa/Snijders-Bosker
#    convention).  The ICC is therefore a latent-scale ICC.
#  * Other GLMM families (e.g. Poisson): the residual variance is not on a fixed
#    scale, so only the random-effect variances are shown and no ICC is reported.
#  * With a random slope the ICC is conditional on the covariate, so we report
#    the variance components but omit a single headline ICC.
#
# Returns NULL when lme4 / VarCorr are unavailable, otherwise a list with:
#   table      data.frame(Component, Variance, `Std. Dev.`, `% of variance`)
#   icc        numeric ICC or NA_real_
#   icc_label  e.g. "ICC (subject)" (NULL when no ICC)
#   note       a scale / interpretation caveat, or NULL
mixed_variance_components <- function(fit, outcome_type = "continuous") {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) return(NULL)
  vdf <- tryCatch(as.data.frame(vc), error = function(e) NULL)
  if (is.null(vdf) || !nrow(vdf)) return(NULL)

  # Keep the variance rows (var2 is NA); the correlation rows (var2 set) belong
  # to random-slope models and are not part of the variance partition.
  var_rows <- vdf[is.na(vdf$var2), , drop = FALSE]
  is_resid <- var_rows$grp == "Residual"
  # "(Intercept)" already carries its own parentheses; only wrap bare slope names.
  term_label <- ifelse(grepl("^\\(", var_rows$var1), var_rows$var1,
                        paste0("(", var_rows$var1, ")"))
  component <- ifelse(is_resid, "Residual",
                      ifelse(is.na(var_rows$var1), var_rows$grp,
                             paste0(var_rows$grp, " ", term_label)))
  variance <- var_rows$vcov
  std_dev <- var_rows$sdcor

  note <- NULL
  # lme4 reports a Residual row for Gaussian fits only; supply the latent-scale
  # residual variance for a binomial GLMM so the partition (and ICC) close.
  has_residual <- any(is_resid)
  is_binomial <- identical(outcome_type, "binary") ||
    isTRUE(tryCatch(stats::family(fit)$family == "binomial", error = function(e) FALSE))
  if (!has_residual) {
    if (is_binomial) {
      component <- c(component, "Residual (latent, π²/3)")
      variance <- c(variance, pi^2 / 3)
      std_dev <- c(std_dev, sqrt(pi^2 / 3))
      has_residual <- TRUE
      note <- paste0("Variance is partitioned on the latent (logit) scale, ",
                     "where the residual variance is fixed at π²/3 ≈ 3.29; ",
                     "the ICC is therefore a latent-scale ICC.")
    } else {
      note <- paste0("For this GLMM family the residual variance is not on a ",
                     "fixed scale, so no ICC is reported; only the random-effect ",
                     "variances are shown.")
    }
  }

  total <- sum(variance)
  tab <- data.frame(
    Component = component,
    Variance = variance,
    `Std. Dev.` = std_dev,
    `% of variance` = if (is.finite(total) && total > 0) 100 * variance / total else NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )

  # ICC = between-cluster intercept variance / total variance.  Only well defined
  # for a random-intercept model with an available residual variance; a random
  # slope makes it covariate-conditional, so we omit the headline number.
  re_rows <- var_rows[!is_resid, , drop = FALSE]
  has_slope <- any(!is.na(re_rows$var1) & re_rows$var1 != "(Intercept)")
  intercept_var <- sum(re_rows$vcov[is.na(re_rows$var1) |
                                      re_rows$var1 == "(Intercept)"])
  group_name <- if (nrow(re_rows)) re_rows$grp[1] else NA_character_
  icc <- NA_real_; icc_label <- NULL
  if (has_residual && !has_slope && is.finite(total) && total > 0) {
    icc <- intercept_var / total
    icc_label <- sprintf("ICC (%s)", group_name)
  } else if (has_slope) {
    note <- paste(c(note, paste0("A random slope was fitted, so the ICC is ",
      "conditional on the covariate; the variance components are reported instead ",
      "of a single ICC.")), collapse = " ")
  }

  list(table = tab, icc = icc, icc_label = icc_label,
       group = group_name, note = note)
}

clustered_model_assumptions <- function(fit, method, outcome_type) {
  notes <- model_assumptions(fit, method, outcome_type)
  add <- function(res) notes[[length(notes) + 1L]] <<- res
  if (grepl("^gee", method))
    add(list(check = "context", label = "About this method", family = "structure",
             severity = "ok", ok = TRUE, statistic = NA_real_, p = NA_real_, test = "",
             note = "GEE estimates population-average effects and uses a robust sandwich variance for within-cluster correlation; review whether the working correlation is plausible.",
             remedy = list(), detail = list()))
  if (grepl("^(lmm|glmm)", method)) {
    singular <- tryCatch(lme4::isSingular(fit, tol = 1e-4), error = function(e) NA)
    add(assumption_result("singular_fit",
      if (is.na(singular)) "untested" else if (isTRUE(singular)) "violated" else "ok",
      if (is.na(singular))
        "The random-effects structure could not be checked for a singular fit."
      else if (isTRUE(singular))
        "The mixed model appears singular; the random-effects structure may be too complex for the data."
      else "The random-effects structure did not trigger a singular-fit warning."))
    conv <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
    if (!is.null(conv))
      add(assumption_result("singular_fit", "review",
                            paste("Optimizer message:", paste(conv, collapse = " "))))
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
  # geepack finds the cluster boundaries with diff(as.numeric(id)), so the id
  # has to be numeric AND the rows have to arrive already grouped by cluster.
  # Handing it the raw column breaks both ways on real study exports:
  #  * A character id - household codes like "01/003/03" are the norm in REDCap
  #    and Stata extracts - coerces to NA, the cluster structure collapses, and
  #    the fit comes back with near-zero standard errors and every p-value at 0.
  #    That is a wrong answer reported as a confident one, so it must not be
  #    left to chance.
  #  * Rows in an arbitrary order make the working correlation be estimated
  #    from the wrong pairs, which moves alpha and the p-values.
  # Coding the id to its integer level and sorting on it fixes both. The sort
  # happens once, after every derived column is attached, so nothing misaligns.
  gee_data[[".__gee_id"]] <- as.integer(as.factor(mf[[cluster]]))
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
  # Cluster-major order, and time-ordered within a cluster when AR-1 needs the
  # waves to line up with the lag structure.
  gee_data <- gee_data[if (use_waves)
    order(gee_data[[".__gee_id"]], gee_data[[".__gee_waves"]])
  else order(gee_data[[".__gee_id"]]), , drop = FALSE]
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
  rc <- mixed_variance_components(fit, "continuous")
  metrics <- c(
    "n used" = stats::nobs(fit),
    "clusters" = length(unique(mf[[cluster]])),
    "AIC" = safe_aic(fit),
    "BIC" = tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_),
    "log-likelihood" = as.numeric(stats::logLik(fit)))
  if (!is.null(rc) && !is.null(rc$icc_label) && is.finite(rc$icc))
    metrics[[rc$icc_label]] <- rc$icc
  assumptions <- clustered_model_assumptions(fit, "lmm", "continuous")
  if (!is.null(rc$note)) assumptions[[length(assumptions) + 1]] <- list(note = rc$note)
  model_result(MODEL_METHOD_LABELS[["lmm"]], "lmm", mf_formula, fit,
               coef_tab, named_metric_table(metrics), assumptions,
               "continuous", n_total, stats::nobs(fit),
               random_effects = if (!is.null(rc)) rc$table else NULL,
               recommendation = "LMM estimates subject-specific fixed effects while accounting for correlation within the random-intercept cluster; the random-effect variances are partitioned into an ICC.")
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
  rc <- mixed_variance_components(fit, outcome_type)
  metrics <- c(
    "n used" = stats::nobs(fit),
    "clusters" = length(unique(mf[[cluster]])),
    "AIC" = safe_aic(fit),
    "BIC" = tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_),
    "log-likelihood" = as.numeric(stats::logLik(fit)))
  if (!is.null(rc) && !is.null(rc$icc_label) && is.finite(rc$icc))
    metrics[[rc$icc_label]] <- rc$icc
  assumptions <- clustered_model_assumptions(fit, method, outcome_type)
  if (!is.null(rc$note)) assumptions[[length(assumptions) + 1]] <- list(note = rc$note)
  model_result(MODEL_METHOD_LABELS[[method]], method, mf_formula, fit,
               coef_tab, named_metric_table(metrics), assumptions,
               outcome_type, n_total, stats::nobs(fit),
               effect_name = effect_name,
               random_effects = if (!is.null(rc)) rc$table else NULL,
               recommendation = sprintf("%s estimates subject-specific fixed effects with a random intercept for %s; the random-effect variance is reported with a latent-scale ICC where applicable.",
                                         MODEL_METHOD_LABELS[[method]], cluster))
}

model_result <- function(label, method, formula, fit, coefficients, fit_stats,
                         assumptions, outcome_type, n_total, n_used,
                         effect_name = NULL, recommendation = NULL, note = NULL,
                         random_effects = NULL) {
  list(
    label = label,
    method = method,
    formula = paste(deparse(formula), collapse = " "),
    fit = fit,
    coefficients = coefficients,
    fit_stats = fit_stats,
    random_effects = random_effects,
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
  if (is.null(spec[["outcome"]]) || !nzchar(spec[["outcome"]]) ||
      !spec[["outcome"]] %in% names(df))
    stop("Choose a valid outcome variable.", call. = FALSE)
  if (is.null(spec[["predictors"]]) || length(spec[["predictors"]]) == 0)
    stop("Choose at least one predictor.", call. = FALSE)
  if (any(!spec[["predictors"]] %in% names(df)))
    stop("One or more selected predictors are not present in the data.", call. = FALSE)
  if (spec[["outcome"]] %in% spec[["predictors"]])
    stop("The outcome cannot also be selected as a predictor.", call. = FALSE)
  if (!spec[["outcome_type"]] %in% MODEL_OUTCOME_TYPES)
    stop("The selected outcome type is not supported by Phase 3.", call. = FALSE)
  all_methods <- c(available_model_methods(spec[["outcome_type"]], "independent"),
                   available_model_methods(spec[["outcome_type"]], "clustered"))
  if (!spec[["method"]] %in% all_methods)
    stop("That model method is not available for the selected outcome type.", call. = FALSE)

  clustered <- grepl("^(gee|lmm|glmm)", spec[["method"]])
  if (clustered) {
    if (is.null(spec[["cluster"]]) || !nzchar(spec[["cluster"]]) ||
        !spec[["cluster"]] %in% names(df))
      stop("Choose a valid subject or cluster ID for this model.", call. = FALSE)
    if (spec[["cluster"]] %in% c(spec[["outcome"]], spec[["predictors"]]))
      stop("The subject or cluster ID must be different from the outcome and predictors.", call. = FALSE)
    if (length(unique(df[[spec[["cluster"]]]][!is.na(df[[spec[["cluster"]]]])])) < 2)
      stop("The selected subject or cluster ID has fewer than two observed clusters.", call. = FALSE)
  }
  if (identical(spec[["method"]], "gee_gaussian") || identical(spec[["method"]], "gee_binomial") ||
      identical(spec[["method"]], "gee_poisson")) {
    if (is.null(spec[["corstr"]]) || !spec[["corstr"]] %in% c("independence", "exchangeable", "ar1"))
      stop("Choose a valid GEE working correlation.", call. = FALSE)
    if (identical(spec[["corstr"]], "ar1") && (is.null(spec[["time"]]) || !nzchar(spec[["time"]])))
      stop("AR-1 GEE needs a time / ordering variable.", call. = FALSE)
  }
  random_slope <- spec[["random_slope"]]
  if (is.null(random_slope) || identical(random_slope, "(none)")) random_slope <- NULL
  if (!is.null(random_slope) && !random_slope %in% spec[["predictors"]])
    stop("The random slope must be one of the selected predictors.", call. = FALSE)
  # Requiring both main effects also keeps the outcome out of an interaction:
  # to name it in one, it would have to be selected as a predictor, which the
  # outcome/predictor check above already refuses.
  interactions <- validate_interactions(spec[["interactions"]] %||% character(0),
                                        spec[["predictors"]])

  formula <- model_formula(spec[["outcome"]], spec[["predictors"]], interactions)
  n_total <- nrow(df)
  extras <- if (clustered)
    unique(c(spec[["cluster"]], spec[["time"]], random_slope)) else character(0)
  extras <- extras[!is.na(extras) & nzchar(extras)]
  mf <- model_frame(df, formula, spec[["outcome"]], spec[["outcome_type"]],
                    extra_vars = extras)
  if (clustered && spec[["method"]] %in% c("gee_gaussian", "gee_binomial", "gee_poisson"))
    return(run_gee_model(mf, formula, spec[["method"]], spec[["outcome_type"]],
                         spec[["cluster"]], spec[["corstr"]], spec[["time"]], n_total))
  if (identical(spec[["method"]], "lmm"))
    return(run_lmm_model(mf, formula, spec[["cluster"]], random_slope, n_total))
  if (spec[["method"]] %in% c("glmm_binomial", "glmm_poisson"))
    return(run_glmm_model(mf, formula, spec[["method"]], spec[["outcome_type"]],
                          spec[["cluster"]], random_slope, n_total))
  if (spec[["method"]] == "ols")
    return(run_ols_model(mf, formula, n_total))
  if (spec[["method"]] == "robust")
    return(run_robust_model(mf, formula, n_total))
  if (spec[["method"]] == "quantile")
    return(run_quantile_model(mf, formula, spec[["tau"]] %||% 0.5, n_total))
  if (spec[["method"]] %in% c("lasso", "ridge"))
    return(run_penalised_model(mf, formula, spec[["outcome_type"]], spec[["method"]],
                               spec[["lambda_choice"]] %||% "min", n_total))
  if (spec[["method"]] == "pls")
    return(run_pls_model(mf, formula, n_total))
  run_glm_model(mf, formula, spec[["method"]], spec[["outcome_type"]], n_total)
}

#' Fit a selected Phase 4 clustered/repeated-measures model.
#'
#' Keeping this wrapper separate from run_guided_model makes the phase boundary
#' explicit in the UI while preserving the tested shared fitting routines.
run_guided_clustered_model <- function(df, spec) {
  if (is.null(spec[["method"]]) || !grepl("^(gee|lmm|glmm)", spec[["method"]]))
    stop("Choose a GEE, LMM, or GLMM method for clustered data.", call. = FALSE)
  run_guided_model(df, spec)
}
