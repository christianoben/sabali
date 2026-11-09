# Adversarial assumption checks and small-sample edge cases.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== assumption verdict flips ==\n")
# Seed BEFORE the draw. This was previously seeded only further down, after
# skew_x had already been sampled, so skew_x came from whatever RNG state the
# process happened to start in - and Shapiro-Wilk fails to flag an n = 40
# exponential sample about 2.7% of the time (54 of 2,000 draws), which made the
# whole suite fail at random roughly once every forty runs.
set.seed(91)
normal_x <- qnorm(ppoints(40))
skew_x <- rexp(40, 1)
normal_check <- check_normality(normal_x)
skew_check <- check_normality(skew_x)
ok("normality accepts engineered normal data and rejects skewed data", {
  stopifnot(isTRUE(normal_check$ok), identical(normal_check$test, "Shapiro-Wilk"),
            isFALSE(skew_check$ok))
})

set.seed(91)
equal_d <- data.frame(x = c(rnorm(40, 0, 1), rnorm(40, 0, 1)),
                      g = factor(rep(c("A", "B"), each = 40)))
unequal_d <- data.frame(x = c(rnorm(40, 0, .2), rnorm(40, 0, 5)),
                        g = factor(rep(c("A", "B"), each = 40)))
equal_v <- check_variance_homogeneity(equal_d$x, equal_d$g)
unequal_v <- check_variance_homogeneity(unequal_d$x, unequal_d$g)
ok("homogeneity check accepts equal variances and flags unequal variances", {
  stopifnot(isTRUE(equal_v$ok), isFALSE(unequal_v$ok),
            grepl("Welch", unequal_v$note, ignore.case = TRUE))
})

set.seed(92)
pois_d <- data.frame(y = rpois(160, 4), x = rnorm(160))
over_d <- data.frame(y = rpois(160, exp(rnorm(160, log(4), .9))), x = rnorm(160))
pois_fit <- run_guided_model(pois_d, list(outcome = "y", outcome_type = "count",
                                          predictors = "x", method = "poisson"))
over_fit <- run_guided_model(over_d, list(outcome = "y", outcome_type = "count",
                                          predictors = "x", method = "poisson"))
pois_notes <- paste(vapply(pois_fit$assumptions, function(x) x$note, character(1)), collapse = " ")
over_notes <- paste(vapply(over_fit$assumptions, function(x) x$note, character(1)), collapse = " ")
ok("Poisson dispersion diagnostic distinguishes equidispersed and overdispersed data", {
  d1 <- field_value(pois_fit$fit_stats, "Pearson dispersion")
  d2 <- field_value(over_fit$fit_stats, "Pearson dispersion")
  stopifnot(is.finite(d1), is.finite(d2), d2 > 1.5,
            !grepl("negative binomial regression is preferable", pois_fit$recommendation %||% "", ignore.case = TRUE),
            grepl("overdispersion", over_notes, ignore.case = TRUE))
})

cat("\n== ANCOVA slopes and multicollinearity ==\n")
slopes <- data.frame(group = factor(rep(c("A", "B"), each = 30)),
                     cov = rep(seq(-3, 3, length.out = 30), 2))
slopes$y_parallel <- 2 + 1.5 * slopes$cov + (slopes$group == "B") * .8 +
  rep(c(-.25, -.1, 0, .1, .25), 12)
slopes$y_cross <- 2 + 1.5 * slopes$cov + (slopes$group == "B") * (4 * slopes$cov) +
  rep(c(-.25, -.1, 0, .1, .25), 12)
parallel <- run_ancova(slopes, "y_parallel", "group", "cov")
cross <- run_ancova(slopes, "y_cross", "group", "cov")
parallel_note <- vapply(parallel$assumptions, function(x) x$note, character(1))
cross_note <- vapply(cross$assumptions, function(x) x$note, character(1))
ok("homogeneity-of-slopes check accepts parallel slopes and flags crossing slopes", {
  stopifnot(any(grepl("common-slope ANCOVA is reasonable", parallel_note, fixed = TRUE)),
            any(grepl("Slopes appear to differ", cross_note, fixed = TRUE)))
})

set.seed(93)
vif_d <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
vif_d$x2_collinear <- vif_d$x1 + rnorm(100, 0, .001)
vif_ok <- check_multicollinearity(lm(y ~ x1 + x2, data = vif_d))
vif_bad <- check_multicollinearity(lm(y ~ x1 + x2_collinear, data = vif_d))
ok("VIF check accepts independent predictors and flags near-collinearity", {
  stopifnot(isTRUE(vif_ok$ok), isFALSE(vif_bad$ok), vif_bad$max_vif >= 5,
            grepl("multicollinearity", vif_bad$note, ignore.case = TRUE))
})

cat("\n== adversarial edge cases ==\n")
ok("group n = 1 fails with a clear sample-size message", {
  e <- friendly_error(run_two_independent(c(1, 2, 3), factor(c("A", "B", "B"))))
  stopifnot(grepl("at least two complete observations", e, ignore.case = TRUE))
})
ok("constant data fails instead of producing a misleading p-value", {
  e <- friendly_error(run_one_sample(rep(3, 8)))
  stopifnot(grepl("identical|variation|uncertainty", e, ignore.case = TRUE))
})
ok("more than 50% missing outcome fails with a complete-case explanation", {
  d <- data.frame(y = c(rep(NA_real_, 8), 1:4), x = 1:12)
  e <- friendly_error(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                                predictors = "x", method = "ols")))
  stopifnot(grepl("complete rows|complete observations|at least 5", e, ignore.case = TRUE))
})
ok("ordered-factor predictors are accepted by the model matrix", {
  d <- data.frame(y = rnorm(30), dose = ordered(rep(c("low", "mid", "high"), 10)),
                  x = rnorm(30))
  r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                predictors = c("dose", "x"), method = "ols"))
  stopifnot(is.list(r), nrow(r$coefficients) >= 2L)
})

report_and_exit()
