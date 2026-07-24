# Statistical validation: independent regression families, precision,
# missingness, determinism and adversarial model inputs.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== OLS golden values ==\n")
ols <- run_guided_model(mtcars, list(outcome = "mpg", outcome_type = "continuous",
                                     predictors = c("wt", "hp"), method = "ols"))
ols_ref <- stats::lm(mpg ~ wt + hp, data = mtcars)
ols_sm <- summary(ols_ref)
ols_ci <- stats::confint(ols_ref)
ok("OLS estimates, SE, t and p match lm()", {
  stopifnot(all.equal(ols$coefficients$Estimate, unname(coef(ols_ref)), tolerance = TOL_STAT),
            all.equal(unname(ols$coefficients[["Std. Error"]]), unname(ols_sm$coefficients[, 2]), tolerance = TOL_STAT),
            all.equal(unname(ols$coefficients$Statistic), unname(ols_sm$coefficients[, 3]), tolerance = TOL_STAT),
            all.equal(unname(ols$coefficients[["p-value"]]), unname(ols_sm$coefficients[, 4]), tolerance = TOL_P),
            all.equal(unname(ols$coefficients[["CI lower"]]), unname(ols_ci[, 1]), tolerance = TOL_CI),
            all.equal(unname(ols$coefficients[["CI upper"]]), unname(ols_ci[, 2]), tolerance = TOL_CI))
})
ok("OLS fit metrics and complete-case n match the reference", {
  expect_close(ols$n_used, stats::nobs(ols_ref), TOL_STAT)
  expect_close(field_value(ols$fit_stats, "R-squared"), ols_sm$r.squared, TOL_STAT)
  expect_close(field_value(ols$fit_stats, "Adjusted R-squared"), ols_sm$adj.r.squared, TOL_STAT)
  expect_close(field_value(ols$fit_stats, "AIC"), stats::AIC(ols_ref), TOL_STAT)
})

cat("\n== GLM and negative-binomial golden values ==\n")
set.seed(31)
log_d <- data.frame(
  y = factor(sample(c("no", "yes"), 60, replace = TRUE), levels = c("no", "yes")),
  x = c(seq(-2, 0, length.out = 32), seq(.1, 2, length.out = 28)),
  z = factor(rep(c("A", "B", "C"), 20)))
log_r <- run_guided_model(log_d, list(outcome = "y", outcome_type = "binary",
                                       predictors = c("x", "z"), method = "logistic"))
log_ref <- stats::glm(y ~ x + z, data = log_d, family = stats::binomial())
log_sm <- summary(log_ref)
log_ci <- stats::confint.default(log_ref)
ok("logistic GLM coefficients, SE, z, p and odds ratios match glm()", {
  stopifnot(all.equal(log_r$coefficients$Estimate, unname(coef(log_ref)), tolerance = TOL_STAT),
            all.equal(unname(log_r$coefficients[["Std. Error"]]), unname(log_sm$coefficients[, 2]), tolerance = TOL_STAT),
            all.equal(unname(log_r$coefficients$Statistic), unname(log_sm$coefficients[, 3]), tolerance = TOL_STAT),
            all.equal(unname(log_r$coefficients[["p-value"]]), unname(log_sm$coefficients[, 4]), tolerance = TOL_P),
            all.equal(unname(log_r$coefficients[["Odds ratio"]]), unname(exp(coef(log_ref))), tolerance = TOL_EFFECT),
            all.equal(unname(log_r$coefficients[["Odds ratio CI lower"]]), unname(exp(log_ci[, 1])), tolerance = TOL_CI),
            all.equal(unname(log_r$coefficients[["Odds ratio CI upper"]]), unname(exp(log_ci[, 2])), tolerance = TOL_CI))
})

warp <- transform(warpbreaks, wool = factor(wool), tension = factor(tension))
pois <- run_guided_model(warp, list(outcome = "breaks", outcome_type = "count",
                                     predictors = c("wool", "tension"), method = "poisson"))
pois_ref <- stats::glm(breaks ~ wool + tension, data = warp, family = stats::poisson())
nb <- run_guided_model(warp, list(outcome = "breaks", outcome_type = "count",
                                   predictors = c("wool", "tension"), method = "negative_binomial"))
nb_ref <- MASS::glm.nb(breaks ~ wool + tension, data = warp)
ok("Poisson coefficients and Pearson dispersion match glm()", {
  expect_close(pois$coefficients$Estimate[1], coef(pois_ref)[1], TOL_STAT)
  disp <- sum(stats::residuals(pois_ref, type = "pearson")^2) / stats::df.residual(pois_ref)
  expect_close(field_value(pois$fit_stats, "Pearson dispersion"), disp, TOL_STAT)
})
ok("negative-binomial coefficients and AIC match MASS::glm.nb()", {
  stopifnot(all.equal(nb$coefficients$Estimate, unname(coef(nb_ref)), tolerance = TOL_STAT))
  expect_close(field_value(nb$fit_stats, "AIC"), stats::AIC(nb_ref), TOL_STAT)
})

cat("\n== robust, quantile and penalised determinism ==\n")
rob <- run_guided_model(mtcars, list(outcome = "mpg", outcome_type = "continuous",
                                      predictors = c("wt", "hp"), method = "robust"))
rob_ref <- MASS::rlm(mpg ~ wt + hp, data = mtcars, psi = MASS::psi.huber)
ok("robust regression matches MASS::rlm()", {
  stopifnot(all.equal(rob$coefficients$Estimate, unname(coef(rob_ref)), tolerance = TOL_STAT))
})
qr <- run_guided_model(mtcars, list(outcome = "mpg", outcome_type = "continuous",
                                    predictors = c("wt", "hp"), method = "quantile", tau = .5))
qr_ref <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = .5)
ok("quantile regression point estimates match quantreg::rq()", {
  stopifnot(all.equal(qr$coefficients$Estimate, unname(coef(qr_ref)), tolerance = TOL_STAT))
})

set.seed(4)
pen <- data.frame(y = rnorm(90), x1 = rnorm(90), x2 = rnorm(90), x3 = rnorm(90))
pen$y <- 1.2 * pen$x1 - .8 * pen$x2 + rnorm(90, 0, .2)
spec_pen <- list(outcome = "y", outcome_type = "continuous",
                 predictors = c("x1", "x2", "x3"), method = "lasso",
                 lambda_choice = "min")
before <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
la <- run_guided_model(pen, spec_pen)
after <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
la2 <- run_guided_model(pen, spec_pen)
ok("LASSO produces identical coefficients on repeated runs", {
  stopifnot(all.equal(la$coefficients, la2$coefficients, tolerance = TOL_STAT))
})
ok("LASSO restores the caller's RNG state", {
  if (!is.null(before)) stopifnot(identical(before, after))
})

cat("\n== missing data and adversarial model inputs ==\n")
miss <- mtcars
miss$mpg[c(1, 4, 7)] <- NA_real_
miss$wt[c(2, 6)] <- NA_real_
miss_r <- run_guided_model(miss, list(outcome = "mpg", outcome_type = "continuous",
                                       predictors = "wt", method = "ols"))
ok("complete-case n is explicit and correct", {
  ref_n <- sum(complete.cases(miss[, c("mpg", "wt")]))
  stopifnot(miss_r$n_used == ref_n, field_value(miss_r$fit_stats, "n used") == ref_n)
})
ok("separation/singular/single-level cases fail or return an explicit diagnostic", {
  sep <- data.frame(y = factor(c(rep("no", 10), rep("yes", 10))), x = c(rep(0, 10), rep(1, 10)))
  sep_result <- tryCatch(run_guided_model(sep, list(outcome = "y", outcome_type = "binary",
                                                      predictors = "x", method = "logistic")),
                         warning = function(w) invokeRestart("muffleWarning"),
                         error = function(e) e)
  stopifnot(is.list(sep_result) || inherits(sep_result, "error"))
  one_level <- data.frame(y = rnorm(12), x = factor(rep("only", 12)))
  e <- friendly_error(run_guided_model(one_level,
        list(outcome = "y", outcome_type = "continuous", predictors = "x", method = "ols")))
  stopifnot(grepl("only one observed category|one observed|predictor", e, ignore.case = TRUE))
})
ok("rank-deficient predictors do not crash and expose the aliased coefficient", {
  singular <- data.frame(y = 1:20, x1 = 1:20, x2 = 1:20)
  r <- run_guided_model(singular, list(outcome = "y", outcome_type = "continuous",
                                       predictors = c("x1", "x2"), method = "ols"))
  stopifnot(is.list(r), anyNA(r$coefficients$Estimate) || any(is.na(coef(r$fit))))
})

report_and_exit()
