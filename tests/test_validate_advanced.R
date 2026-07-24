# Statistical validation: clustered models and survival runners.  The suite
# deliberately uses the same package calls that the maturity plan names as
# trusted references.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

set.seed(66)
ns <- 40
adv <- data.frame(id = factor(rep(seq_len(ns), each = 3)), time = rep(1:3, ns),
                  x = rnorm(ns * 3), z = factor(sample(c("A", "B"), ns * 3, TRUE)))
u <- rnorm(ns, 0, .6)
adv$y <- 1 + .7 * adv$x + .4 * (adv$z == "B") + u[adv$id] + rnorm(nrow(adv), 0, .4)
adv$bin <- factor(rbinom(nrow(adv), 1, plogis(-.3 + .6 * adv$x + u[adv$id])),
                  levels = c(0, 1), labels = c("No", "Yes"))
adv$count <- rpois(nrow(adv), exp(.3 + .2 * adv$x + u[adv$id]))

cat("\n== GEE / LMM / GLMM ==\n")
gee <- run_guided_clustered_model(adv, list(outcome = "y", outcome_type = "continuous",
                                             predictors = "x", method = "gee_gaussian",
                                             cluster = "id", corstr = "exchangeable"))
gee_ref <- geepack::geeglm(y ~ x, id = id, data = adv,
                           family = stats::gaussian(), corstr = "exchangeable")
ok("Gaussian GEE coefficients and robust SE match geeglm()", {
  gref <- summary(gee_ref)$coefficients
  stopifnot(all.equal(gee$coefficients$Estimate, gref[, 1], tolerance = TOL_STAT),
            all.equal(gee$coefficients[["Std. Error"]], gref[, 2], tolerance = TOL_STAT),
            all.equal(gee$coefficients[["p-value"]], gref[, "Pr(>|W|)"], tolerance = TOL_P))
})
lmm <- run_guided_clustered_model(adv, list(outcome = "y", outcome_type = "continuous",
                                             predictors = "x", method = "lmm",
                                             cluster = "id", random_slope = "(none)"))
lmm_ref <- lme4::lmer(y ~ x + (1 | id), data = adv, REML = FALSE)
ok("LMM fixed effects match lme4::lmer()", {
  stopifnot(all.equal(lmm$coefficients$Estimate, unname(lme4::fixef(lmm_ref)), tolerance = TOL_STAT))
})
glmm <- run_guided_clustered_model(adv, list(outcome = "bin", outcome_type = "binary",
                                               predictors = "x", method = "glmm_binomial",
                                               cluster = "id", random_slope = "(none)"))
ok("GLMM returns finite fixed effects and odds ratios", {
  stopifnot(is.list(glmm), all(is.finite(glmm$coefficients$Estimate)),
            all(is.finite(glmm$coefficients[["Odds ratio"]])))
})
glmm2 <- run_guided_clustered_model(adv, list(outcome = "bin", outcome_type = "binary",
                                                predictors = "x", method = "glmm_binomial",
                                                cluster = "id", random_slope = "(none)"))
ok("GLMM optimiser is deterministic for identical input", {
  stopifnot(all.equal(glmm$coefficients$Estimate, glmm2$coefficients$Estimate, tolerance = 1e-6))
})

cat("\n== Kaplan-Meier, log-rank and Cox ==\n")
set.seed(67)
surv <- data.frame(time = rexp(120, .06), status = factor(rbinom(120, 1, .6),
                                                          levels = c(0, 1), labels = c("censored", "event")),
                   group = factor(rep(c("control", "treated"), 60)), x = rnorm(120))
status <- as.integer(surv$status == "event")
surv_ref <- surv
surv_ref$status_num <- status
km <- run_guided_survival(surv, list(method = "kaplan_meier", time = "time",
                                     event = "status", strata = "group"))
km_ref <- survival::survfit(survival::Surv(time, status_num) ~ group, data = surv_ref)
lr_ref <- survival::survdiff(survival::Surv(time, status_num) ~ group, data = surv_ref)
ok("Kaplan-Meier medians and log-rank p match survival package", {
  ref_tab <- survival_summary_table(km_ref)
  stopifnot(all.equal(km$coefficients$`Median survival`, ref_tab$`Median survival`, tolerance = TOL_CI))
  expect_close(field_value(km$fit_stats, "Log-rank p-value"),
               stats::pchisq(lr_ref$chisq, df = length(lr_ref$n) - 1, lower.tail = FALSE), TOL_P)
})
cox <- run_guided_survival(surv, list(method = "cox", time = "time", event = "status",
                                      predictors = c("x", "group")))
cox_ref <- survival::coxph(survival::Surv(time, status_num) ~ x + group,
                           data = surv_ref, ties = "efron")
ok("Cox coefficient, SE, p and hazard ratio match survival::coxph()", {
  sm <- summary(cox_ref)$coefficients
  stopifnot(all.equal(unname(cox$coefficients$Estimate), unname(sm[, "coef"]), tolerance = TOL_STAT),
            all.equal(unname(cox$coefficients[["Std. Error"]]), unname(sm[, "se(coef)"]), tolerance = TOL_STAT),
            all.equal(unname(cox$coefficients[["p-value"]]), unname(sm[, "Pr(>|z|)"]), tolerance = TOL_P),
            all.equal(unname(cox$coefficients[["Hazard ratio"]]), unname(sm[, "exp(coef)"]), tolerance = TOL_EFFECT))
})
ok("survival edge cases are analyst-friendly", {
  no_event <- surv
  no_event$status <- factor(rep("censored", nrow(no_event)),
                            levels = c("censored", "event"))
  e1 <- friendly_error(run_guided_survival(no_event,
    list(method = "kaplan_meier", time = "time", event = "status", strata = "")))
  e2 <- friendly_error(run_guided_survival(surv,
    list(method = "cox", time = "time", event = "time", predictors = "x")))
  stopifnot(grepl("observed event", e1, ignore.case = TRUE),
            grepl("different", e2, ignore.case = TRUE))
})

report_and_exit()
