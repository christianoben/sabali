# tests/test_rates_imputation.R ----------------------------------------------
# Incidence rates over person-time, standardisation, and multiple imputation.
#
# Three things the app could not do, all routine in public health:
#   * a RATE divides events by person-TIME, so it stays interpretable when people
#     are followed for different lengths, which a proportion does not;
#   * STANDARDISATION stops a comparison between populations with different age
#     structures being a comparison of their age structures;
#   * MULTIPLE IMPUTATION replaces complete-case analysis, and replaces the
#     single-value fills the app already offers, which are worse than either
#     because they erase the uncertainty about the missing values.
#
# Intervals are exact (Poisson/gamma), checked against poisson.test; the
# imputation is mice's, so what is tested here is the pooling contract and the
# guards.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

cat("\n== exact Poisson intervals ==\n")

ok("the exact interval matches poisson.test at every scale", {
  for (x in c(0L, 1L, 3L, 10L, 100L)) {
    m <- poisson_exact_ci(x)
    r <- stats::poisson.test(x)$conf.int
    stopifnot(all.equal(m[["lower"]], r[1], tolerance = TOL_STAT),
              all.equal(m[["upper"]], r[2], tolerance = TOL_STAT))
  }
})

ok("zero events gives a lower limit of exactly zero, not a negative one", {
  # A Wald interval on 0 events puts the lower limit below zero, which is not a
  # possible number of events. This is why the interval is exact.
  m <- poisson_exact_ci(0)
  stopifnot(m[["lower"]] == 0, m[["upper"]] > 0)
})

cat("\n== incidence rates ==\n")

ok("the rate is events per person-time, scaled as asked", {
  r <- incidence_rate(15, 2500, per = 1000)
  stopifnot(all.equal(r$rate, 1000 * 15 / 2500), all.equal(r$rate, 6),
            r$lower < r$rate, r$rate < r$upper)
  r2 <- incidence_rate(15, 2500, per = 100000)
  stopifnot(all.equal(r2$rate, 100 * r$rate))
})

ok("zero or missing person-time is refused with an actionable message", {
  stopifnot(grepl("must be a positive total",
                  friendly_error(incidence_rate(5, 0))),
            grepl("must be a positive total",
                  friendly_error(incidence_rate(5, NA))))
})

ok("rate ratios are exact and match the hand calculation", {
  cmp <- rate_comparison(c(30, 15), c(1000, 1500),
                         c("Exposed", "Unexposed"), reference = "Unexposed")
  rr <- cmp$rate_ratios
  stopifnot(all.equal(rr$`Rate ratio`[rr$Group == "Exposed"],
                      (30 / 1000) / (15 / 1500)),
            all.equal(rr$`Rate ratio`[rr$Group == "Exposed"], 3),
            rr$`Rate ratio`[rr$Group == "Unexposed"] == 1,
            rr$`CI lower`[rr$Group == "Exposed"] > 1,
            rr$`p-value`[rr$Group == "Exposed"] < 0.01)
})

ok("a group with no person-time, or a bad reference, is refused by name", {
  stopifnot(grepl("no person-time at risk",
                  friendly_error(rate_comparison(c(5, 5), c(100, 0),
                                                 c("a", "b")))),
            grepl("is not one of the groups",
                  friendly_error(rate_comparison(c(5, 5), c(100, 100),
                                                 c("a", "b"),
                                                 reference = "z"))),
            grepl("at least two groups",
                  friendly_error(rate_comparison(5, 100, "a"))))
})

cat("\n== standardisation ==\n")

# Same stratum rates (0.01 and 0.04), different age structure. The crude rates
# differ twofold purely from structure; the standardised rates must not.
std_pop <- c(1000, 1000)
popA <- list(ev = c(10, 40), pt = c(1000, 1000))   # half in each stratum
popB <- list(ev = c(18, 8),  pt = c(1800, 200))    # mostly low-risk stratum

ok("identical stratum rates give identical standardised rates", {
  a <- direct_standardise(popA$ev, popA$pt, std_pop, per = 1000)
  b <- direct_standardise(popB$ev, popB$pt, std_pop, per = 1000)
  stopifnot(all.equal(a$rate, b$rate, tolerance = TOL_STAT),
            all.equal(a$rate, 25))
})

ok("...while their crude rates differ, which is the point", {
  a <- direct_standardise(popA$ev, popA$pt, std_pop, per = 1000)
  b <- direct_standardise(popB$ev, popB$pt, std_pop, per = 1000)
  stopifnot(all.equal(a$crude, 25), all.equal(b$crude, 13),
            abs(a$crude - b$crude) > 10)
})

ok("the standardised rate is the weighted sum of the stratum rates", {
  a <- direct_standardise(popA$ev, popA$pt, c(3000, 1000), per = 1000)
  manual <- 1000 * (0.75 * (10 / 1000) + 0.25 * (40 / 1000))
  stopifnot(all.equal(a$rate, manual, tolerance = TOL_STAT))
})

ok("strata with no person-time are dropped and counted", {
  a <- direct_standardise(c(10, 40, 5), c(1000, 1000, 0), c(1000, 1000, 1000))
  stopifnot(a$strata_used == 2L, a$strata_dropped == 1L)
})

ok("the SMR compares observed with expected, exactly", {
  s <- indirect_standardise(c(12, 30), c(1000, 500), c(0.01, 0.05))
  expected <- 1000 * 0.01 + 500 * 0.05
  stopifnot(s$observed == 42, all.equal(s$expected, expected),
            all.equal(s$smr, 42 / expected), all.equal(s$smr, 1.2),
            s$lower < s$smr, s$smr < s$upper)
})

ok("an SMR of 1 sits inside its interval; a large excess does not", {
  same <- indirect_standardise(c(35), c(1000), c(0.035))
  stopifnot(all.equal(same$smr, 1), same$lower < 1, same$upper > 1)
  excess <- indirect_standardise(c(100), c(1000), c(0.035))
  stopifnot(excess$smr > 2, excess$lower > 1)
})

ok("mismatched stratum vectors are refused with the counts named", {
  msg <- friendly_error(direct_standardise(c(1, 2), c(10, 20), c(1, 2, 3)))
  stopifnot(grepl("one value per stratum", msg), grepl("3", msg))
  stopifnot(grepl("one value per stratum",
                  friendly_error(indirect_standardise(c(1, 2), c(10, 20), 0.1))))
})

cat("\n== the guided rate analysis ==\n")

rate_df <- data.frame(
  grp = c("A", "B"), ev = c(30, 15), pt = c(1000, 1500),
  stratum_pop = c(1000, 1000), ref_rate = c(0.02, 0.02),
  stringsAsFactors = FALSE)

ok("a plain rate, a grouped comparison and both standardisations all run", {
  plain <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt"))
  stopifnot(grepl("Incidence rate", plain$primary$label),
            grepl("exact", plain$primary$note))
  grouped <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt",
                                             group = "grp"))
  stopifnot(!is.null(grouped$rates), !is.null(grouped$rate_ratios),
            nrow(grouped$rates) == 2L)
  ds <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt",
                                        std_population = "stratum_pop"))
  stopifnot(grepl("Directly standardised", ds$primary$label))
  smr <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt",
                                         reference_rates = "ref_rate"))
  stopifnot(grepl("SMR", smr$primary$label),
            grepl("same time unit", smr$primary$note))
})

ok("the write-up explains why person-time matters", {
  plain <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt"))
  stopifnot(grepl("person-time", plain$rationale))
  ds <- run_rate_analysis(rate_df, list(events = "ev", person_time = "pt",
                                        std_population = "stratum_pop"))
  stopifnot(grepl("different structures", ds$primary$interpretation),
            grepl("use indirect standardisation", ds$primary$note))
})

ok("a missing column is named rather than failing obscurely", {
  stopifnot(grepl("not in the data",
                  friendly_error(run_rate_analysis(rate_df,
                    list(events = "nope", person_time = "pt")))))
})

# --- multiple imputation ----------------------------------------------------
if (!requireNamespace("mice", quietly = TRUE)) {
  cat("\n  SKIP  the 'mice' package is not installed (see install_dependencies.R)\n")
  report_and_exit()
}

cat("\n== multiple imputation ==\n")

# Truth: y = 1 + 0.8x + 0.5z, and x goes missing depending on z alone (missing
# at random). Complete-case analysis is therefore biased; imputation should
# reduce that bias while REPORTING the extra uncertainty.
set.seed(3)
n_imp <- 1200L
z <- stats::rnorm(n_imp)
x <- 0.5 * z + stats::rnorm(n_imp)
y <- 1 + 0.8 * x + 0.5 * z + stats::rnorm(n_imp, 0, 4)
full_df <- data.frame(y = y, x = x, z = z)
miss_df <- full_df
miss_df$x[z > 0.4 & stats::runif(n_imp) < 0.75] <- NA
truth <- unname(stats::coef(stats::lm(y ~ x + z, full_df))[2])
cc <- stats::lm(y ~ x + z, miss_df)
imp_spec <- list(outcome = "y", predictors = c("x", "z"), method = "ols",
                 m = 30L, seed = 7L)

ok("imputation reduces the complete-case bias", {
  r <- run_imputed_model(miss_df, imp_spec)
  est <- r$coefficients$Estimate[r$coefficients$Term == "x"]
  stopifnot(abs(est - truth) < abs(unname(stats::coef(cc)[2]) - truth))
})

ok("it does NOT reproduce the complete-data fit, which would mean a leak", {
  # `with(imps, lm(formula))` looks like the mice idiom but passes no `data =`,
  # so a dynamically built formula resolves its variables through the formula's
  # environment - the caller's frame, or the global environment - and silently
  # fits the COMPLETE data instead of the imputed data. That produced an estimate
  # matching the full-data fit to four decimals with a fraction of missing
  # information of 0.002 on 27% missingness, which is not possible.
  r <- run_imputed_model(miss_df, imp_spec)
  est <- r$coefficients$Estimate[r$coefficients$Term == "x"]
  fmi <- r$coefficients$`Fraction missing info`[r$coefficients$Term == "x"]
  stopifnot(!isTRUE(all.equal(est, truth, tolerance = 1e-4)),
            is.finite(fmi), fmi > 0.05)
})

ok("the standard error grows to carry the imputation uncertainty", {
  r <- run_imputed_model(miss_df, imp_spec)
  se <- r$coefficients$`Std. Error`[r$coefficients$Term == "x"]
  # A single mean-fill is the comparison that matters: it shrinks the SE by
  # pretending the imputed values are known.
  filled <- miss_df
  filled$x[is.na(filled$x)] <- mean(filled$x, na.rm = TRUE)
  se_fill <- summary(stats::lm(y ~ x + z, filled))$coefficients[2, 2]
  stopifnot(se > se_fill)
})

ok("the fit statistics report what complete-case analysis would have cost", {
  r <- run_imputed_model(miss_df, imp_spec)
  stopifnot(field_value(r$fit_stats, "imputations (m)") == 30,
            field_value(r$fit_stats, "complete cases") == sum(stats::complete.cases(miss_df)),
            field_value(r$fit_stats, "rows a complete-case analysis would drop") ==
              n_imp - sum(stats::complete.cases(miss_df)),
            is.data.frame(r$missing_summary))
})

ok("the write-up names the missing-at-random assumption", {
  r <- run_imputed_model(miss_df, imp_spec)
  note <- paste(vapply(r$assumptions, function(a) a$note %||% "", character(1)),
                collapse = " ")
  stopifnot(grepl("missing at random", note),
            grepl("cannot be checked from the data", note),
            grepl("Rubin's rules", note),
            grepl("single mean/median fill erases", note))
})

ok("imputation is refused when it would not change anything", {
  stopifnot(grepl("nothing to impute",
                  friendly_error(run_imputed_model(full_df, imp_spec))))
  barely <- full_df; barely$x[1:3] <- NA          # 0.25% of rows
  stopifnot(grepl("less than 1% of the rows",
                  friendly_error(run_imputed_model(barely, imp_spec))))
})

ok("a variable that is mostly missing is refused, not invented", {
  heavy <- miss_df; heavy$x[stats::runif(n_imp) < 0.8] <- NA
  msg <- friendly_error(run_imputed_model(heavy, imp_spec))
  stopifnot(grepl("more than 60% missing", msg),
            grepl("would invent most of their values", msg))
})

ok("too few imputations, or a method that cannot be pooled, is refused", {
  stopifnot(grepl("at least 2 imputations",
                  friendly_error(run_imputed_model(miss_df,
                    utils::modifyList(imp_spec, list(m = 1L))))),
            grepl("not among them",
                  friendly_error(run_imputed_model(miss_df,
                    utils::modifyList(imp_spec, list(method = "lasso"))))))
})

ok("a logistic model pools too, and reports odds ratios", {
  dbin <- miss_df
  dbin$yb <- factor(as.integer(dbin$y > stats::median(dbin$y)),
                    levels = c(0, 1), labels = c("No", "Yes"))
  r <- run_imputed_model(dbin, list(outcome = "yb", predictors = c("x", "z"),
                                    method = "logistic", outcome_type = "binary",
                                    m = 10L, seed = 2L))
  stopifnot("Odds ratio" %in% names(r$coefficients),
            grepl("multiply imputed", r$label))
})

ok("the result is reproducible for a given seed", {
  a <- run_imputed_model(miss_df, imp_spec)
  b <- run_imputed_model(miss_df, imp_spec)
  stopifnot(all.equal(a$coefficients$Estimate, b$coefficients$Estimate,
                      tolerance = TOL_STAT))
})

report_and_exit()
