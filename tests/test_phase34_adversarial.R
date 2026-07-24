# tests/test_phase34_adversarial.R ------------------------------------------
# Adversarial, end-to-end review of Phase 3 (regression) and Phase 4
# (clustered models + survival). Goes beyond "does it run / does the estimate
# match": every column of the coefficient table (Std. Error, Statistic,
# p-value, effect ratio and its CI) is pinned against the reference R fit,
# because a point estimate can be right while the rest of the row is wrong.
# Also exercises the input-guard paths a real user will hit.
#
# Labels:
#   [GUARD] - correct today; must stay green (regression protection).
#   [BUG]   - fails on today's code; documents a defect to fix.
#
# Run:  Rscript tests/test_phase34_adversarial.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(shiny); library(MASS); library(quantreg)
  library(geepack); library(lme4); library(survival)
})

eqcol <- function(a, b, tol = 1e-6) isTRUE(all.equal(unname(a), unname(b), tolerance = tol))

# --- shared synthetic data (clustered structure) ---------------------------
set.seed(2026)
ns <- 80
id <- factor(rep(seq_len(ns), each = 3))
u  <- rnorm(ns, 0, 0.7)
x  <- rnorm(ns * 3)
z  <- factor(sample(c("A", "B"), ns * 3, replace = TRUE))
d <- data.frame(
  id = id, x = x, z = z,
  y   = 1 + 0.7 * x + ifelse(z == "B", 0.5, 0) + u[id] + rnorm(ns * 3, 0, 0.5),
  bin = factor(rbinom(ns * 3, 1, plogis(-0.4 + 0.5 * x + u[id])), labels = c("No", "Yes")),
  count = rpois(ns * 3, exp(0.4 + 0.2 * x + u[id])))

# --- survival data ----------------------------------------------------------
set.seed(74)
n_s <- 180
sv <- data.frame(
  time = rexp(n_s, 0.08),
  event = factor(rbinom(n_s, 1, 0.58), levels = c(0, 1), labels = c("Censored", "Event")),
  group = factor(sample(c("Control", "Intervention"), n_s, replace = TRUE)),
  x = rnorm(n_s), age = rnorm(n_s, 50, 8))
sv_status <- as.integer(sv$event == "Event")

# ===========================================================================
cat("\n== 1. OLS: EVERY coefficient column vs lm() [GUARD] ==\n")
# ===========================================================================
r_ols <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                   predictors = c("x", "z"), method = "ols"))
ref_ols <- lm(y ~ x + z, data = d); s_ols <- summary(ref_ols)$coefficients
ci_ols <- confint(ref_ols)
ok("[GUARD] OLS Estimate",   stopifnot(eqcol(r_ols$coefficients$Estimate, s_ols[, 1])))
ok("[GUARD] OLS Std. Error", stopifnot(eqcol(r_ols$coefficients[["Std. Error"]], s_ols[, 2])))
ok("[GUARD] OLS Statistic (t)", stopifnot(eqcol(r_ols$coefficients$Statistic, s_ols[, 3])))
ok("[GUARD] OLS p-value",    stopifnot(eqcol(r_ols$coefficients[["p-value"]], s_ols[, 4])))
ok("[GUARD] OLS CI lower/upper", stopifnot(eqcol(r_ols$coefficients[["CI lower"]], ci_ols[, 1]),
                                           eqcol(r_ols$coefficients[["CI upper"]], ci_ols[, 2])))

# ===========================================================================
cat("\n== 2. Logistic: SE / z / p / OR / OR-CI vs glm() [GUARD] ==\n")
# ===========================================================================
r_log <- run_guided_model(d, list(outcome = "bin", outcome_type = "binary",
                                   predictors = c("x", "z"), method = "logistic"))
ref_log <- glm(bin ~ x + z, data = d, family = binomial()); s_log <- summary(ref_log)$coefficients
ci_log <- suppressMessages(confint.default(ref_log))
ok("[GUARD] logistic Std. Error", stopifnot(eqcol(r_log$coefficients[["Std. Error"]], s_log[, 2])))
ok("[GUARD] logistic z",          stopifnot(eqcol(r_log$coefficients$Statistic, s_log[, 3])))
ok("[GUARD] logistic p-value",    stopifnot(eqcol(r_log$coefficients[["p-value"]], s_log[, 4])))
ok("[GUARD] logistic Odds ratio", stopifnot(eqcol(r_log$coefficients[["Odds ratio"]], exp(coef(ref_log)))))
ok("[GUARD] logistic OR CI",      stopifnot(eqcol(r_log$coefficients[["Odds ratio CI lower"]], exp(ci_log[, 1])),
                                            eqcol(r_log$coefficients[["Odds ratio CI upper"]], exp(ci_log[, 2]))))

# ===========================================================================
cat("\n== 3. Poisson: SE / RR / dispersion vs glm() [GUARD] ==\n")
# ===========================================================================
r_pois <- run_guided_model(d, list(outcome = "count", outcome_type = "count",
                                    predictors = "x", method = "poisson"))
ref_pois <- glm(count ~ x, data = d, family = poisson()); s_pois <- summary(ref_pois)$coefficients
disp_ref <- sum(residuals(ref_pois, type = "pearson")^2) / ref_pois$df.residual
ok("[GUARD] Poisson Std. Error", stopifnot(eqcol(r_pois$coefficients[["Std. Error"]], s_pois[, 2])))
ok("[GUARD] Poisson Rate ratio", stopifnot(eqcol(r_pois$coefficients[["Rate ratio"]], exp(coef(ref_pois)))))
ok("[GUARD] Poisson Pearson dispersion", {
  v <- as.numeric(r_pois$fit_stats$Value[r_pois$fit_stats$Metric == "Pearson dispersion"])
  stopifnot(eqcol(v, disp_ref))
})

# ===========================================================================
cat("\n== 4. GEE (Gaussian): Estimate + Std. Error vs geeglm() [GUARD] ==\n")
# ===========================================================================
r_gee <- run_guided_clustered_model(d, list(outcome = "y", outcome_type = "continuous",
  predictors = "x", method = "gee_gaussian", cluster = "id", corstr = "exchangeable"))
ref_gee <- geepack::geeglm(y ~ x, id = id, data = d, family = gaussian(), corstr = "exchangeable")
s_gee <- summary(ref_gee)$coefficients
ok("[GUARD] GEE Estimate",   stopifnot(eqcol(r_gee$coefficients$Estimate, s_gee[, 1])))
ok("[GUARD] GEE Std. Error", stopifnot(eqcol(r_gee$coefficients[["Std. Error"]], s_gee[, 2])))

# ===========================================================================
cat("\n== 5. Cox: Estimate / HR / p / HR-CI correct, but SE column WRONG ==\n")
# ===========================================================================
r_cox <- run_guided_survival(sv, list(method = "cox", time = "time",
                                       event = "event", predictors = c("x", "age")))
ref_cox <- survival::coxph(Surv(time, sv_status) ~ x + age, data = sv)
s_cox <- summary(ref_cox)$coefficients          # cols: coef, exp(coef), se(coef), z, Pr(>|z|)
ci_cox <- summary(ref_cox)$conf.int             # cols: exp(coef), exp(-coef), lower .95, upper .95

ok("[GUARD] Cox Estimate (coef)", stopifnot(eqcol(r_cox$coefficients$Estimate, s_cox[, "coef"])))
ok("[GUARD] Cox Hazard ratio",    stopifnot(eqcol(r_cox$coefficients[["Hazard ratio"]], s_cox[, "exp(coef)"])))
ok("[GUARD] Cox p-value",         stopifnot(eqcol(r_cox$coefficients[["p-value"]], s_cox[, "Pr(>|z|)"])))
ok("[GUARD] Cox HR CI vs reference", stopifnot(
  eqcol(r_cox$coefficients[["Hazard ratio CI lower"]], ci_cox[, "lower .95"], tol = 1e-4),
  eqcol(r_cox$coefficients[["Hazard ratio CI upper"]], ci_cox[, "upper .95"], tol = 1e-4)))

# --- the defect: coxph's summary inserts exp(coef) at column 2, so the generic
#     matrix_to_coefficient_table (statistic_col = 3, se = col 2) mislabels the
#     columns: "Std. Error" shows exp(coef) and "Statistic" shows se(coef).
ok("[BUG] Cox 'Std. Error' column equals reference se(coef)",
   stopifnot(eqcol(r_cox$coefficients[["Std. Error"]], s_cox[, "se(coef)"])))
ok("[BUG] Cox 'Statistic' column equals reference z",
   stopifnot(eqcol(r_cox$coefficients$Statistic, s_cox[, "z"])))
ok("[BUG] Cox 'Std. Error' is NOT the hazard ratio (exp(coef))",
   stopifnot(!eqcol(r_cox$coefficients[["Std. Error"]], s_cox[, "exp(coef)"])))

# ===========================================================================
cat("\n== 6. Kaplan-Meier: median survival + log-rank vs survival:: [GUARD] ==\n")
# ===========================================================================
r_km <- run_guided_survival(sv, list(method = "kaplan_meier", time = "time",
                                      event = "event", strata = "group"))
ref_km <- survival::survfit(Surv(time, sv_status) ~ group, data = sv)
ref_med <- survival_summary_table(ref_km)$`Median survival`
ref_lr <- survival::survdiff(Surv(time, sv_status) ~ group, data = sv)
ref_lr_p <- stats::pchisq(ref_lr$chisq, df = length(ref_lr$n) - 1, lower.tail = FALSE)
ok("[GUARD] KM median survival per group", stopifnot(eqcol(r_km$coefficients$`Median survival`, ref_med, tol = 1e-8)))
ok("[GUARD] KM log-rank p vs survdiff()", {
  v <- as.numeric(r_km$fit_stats$Value[r_km$fit_stats$Metric == "Log-rank p-value"])
  stopifnot(eqcol(v, ref_lr_p, tol = 1e-6))
})
ok("[GUARD] KM events + censored == n used", {
  fs <- r_km$fit_stats
  ev <- as.numeric(fs$Value[fs$Metric == "events"])
  ce <- as.numeric(fs$Value[fs$Metric == "censored"])
  nu <- as.numeric(fs$Value[fs$Metric == "n used"])
  stopifnot(ev + ce == nu, ev == sum(sv_status))
})

# ===========================================================================
cat("\n== 7. Input guards / adversarial conditions ==\n")
# ===========================================================================
err_of <- function(expr) tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))

ok("[GUARD] outcome listed as its own predictor is refused",
   stopifnot(is.character(err_of(run_guided_model(d,
     list(outcome = "y", outcome_type = "continuous", predictors = c("y", "x"), method = "ols"))))))
ok("[GUARD] no predictors is refused",
   stopifnot(is.character(err_of(run_guided_model(d,
     list(outcome = "y", outcome_type = "continuous", predictors = character(0), method = "ols"))))))
ok("[GUARD] quantile tau out of (0,1) is refused",
   stopifnot(is.character(err_of(run_guided_model(d,
     list(outcome = "y", outcome_type = "continuous", predictors = "x", method = "quantile", tau = 1.5))))))
ok("[GUARD] GEE with a single cluster is refused", {
  d1 <- d; d1$id <- factor(1)
  stopifnot(is.character(err_of(run_guided_clustered_model(d1,
    list(outcome = "y", outcome_type = "continuous", predictors = "x",
         method = "gee_gaussian", cluster = "id", corstr = "exchangeable")))))
})
ok("[GUARD] AR-1 GEE without a time variable is refused",
   stopifnot(is.character(err_of(run_guided_clustered_model(d,
     list(outcome = "y", outcome_type = "continuous", predictors = "x",
          method = "gee_gaussian", cluster = "id", corstr = "ar1"))))))
ok("[GUARD] cluster ID equal to outcome is refused",
   stopifnot(is.character(err_of(run_guided_clustered_model(d,
     list(outcome = "y", outcome_type = "continuous", predictors = "x",
          method = "lmm", cluster = "y"))))))
ok("[GUARD] Cox with time == event is refused",
   stopifnot(is.character(err_of(run_guided_survival(sv,
     list(method = "cox", time = "time", event = "time", predictors = "x"))))))
ok("[GUARD] survival with zero observed events is refused", {
  bad <- sv; bad$event <- factor("Censored", levels = c("Censored", "Event"))
  stopifnot(is.character(err_of(run_guided_survival(bad,
    list(method = "kaplan_meier", time = "time", event = "event", strata = "")))))
})
ok("[GUARD] negative follow-up time is refused", {
  bad <- sv; bad$time[1] <- -3
  stopifnot(is.character(err_of(run_guided_survival(bad,
    list(method = "kaplan_meier", time = "time", event = "event", strata = "")))))
})

# A single-level factor predictor currently surfaces the raw R message
# "contrasts can be applied only to factors with 2 or more levels".
ok("[BUG] single-level factor predictor gives a plain-language error", {
  d2 <- d; d2$const <- factor("only")
  msg <- err_of(run_guided_model(d2, list(outcome = "y", outcome_type = "continuous",
                                           predictors = c("x", "const"), method = "ols")))
  stopifnot(is.character(msg), !grepl("contrasts can be applied", msg, fixed = TRUE))
})

# ===========================================================================
cat("\n== 8. Advanced-model structure gating [GUARD] ==\n")
# ===========================================================================
# The dataset kind now comes from the user's explicit answer (panel), not from
# heuristics - so no panel / cross-sectional => hidden, and an explicit
# repeated answer with ID + time => longitudinal.
ok("[GUARD] cross-sectional answer -> unavailable", {
  dd <- data.frame(patient_id = 1:30, score = rnorm(30), grp = factor(sample(c("a","b"), 30, TRUE)))
  tp <- c(patient_id = "continuous", score = "continuous", grp = "binary")
  info <- advanced_data_structure(dd, tp, panel = list(kind = "cross_sectional"))
  stopifnot(!isTRUE(info$available), identical(info$kind, "none"))
})
ok("[GUARD] no explicit answer (panel = NULL) -> unavailable", {
  dd <- data.frame(patient_id = rep(1:5, each = 3), visit = rep(c("b","w4","w8"), 5), y = rnorm(15))
  tp <- c(patient_id = "identifier", visit = "nominal", y = "continuous")
  info <- advanced_data_structure(dd, tp, panel = NULL)
  stopifnot(!isTRUE(info$available), identical(info$kind, "none"))
})
ok("[GUARD] real cohort with an explicit repeated answer -> longitudinal", {
  raw <- read_any_format("data/lvct_cohort_repeated.csv", "lvct_cohort_repeated.csv")
  det <- detect_all_types(raw); tps <- stats::setNames(det$type, det$variable)
  dat <- raw; for (nm in names(dat)) dat[[nm]] <- coerce_to_type(dat[[nm]], tps[[nm]])
  info <- advanced_data_structure(dat, tps,
    panel = list(kind = "repeated", id = "patient_id", time = "visit"))
  stopifnot(isTRUE(info$available), identical(info$kind, "longitudinal"))
})

report_and_exit()
