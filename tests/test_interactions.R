# tests/test_interactions.R --------------------------------------------------
# Interaction terms in the regression and clustered-model engines.
#
# The motivating requirement is a real one: the vaccines-and-AMR analysis plan's
# SA2 asks whether the effect of Rotarix on antibiotic prescribing is modified by
# prior PCV-10 vaccination. That is an interaction, and until now the engine
# fitted main effects only - `run_guided_model()` rejected "a:b" as a predictor
# that was "not present in the data".
#
# The contract pinned here:
#   * an interaction is carried as a colon-separated string and appended to the
#     formula as an explicit product term;
#   * both main effects must be selected too, because an interaction without
#     them does not mean what it appears to;
#   * `a:b` and `b:a` are the same term;
#   * non-syntactic column names (the TTC workbook has `1_1_Household_ID`) are
#     backtick-quoted, in interactions and main effects alike;
#   * with no interactions requested, nothing about the old behaviour changes.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(shiny)
  library(geepack)
  library(lme4)
})

# --- fixture: an outcome with a genuine interaction ------------------------
# Shaped like the SA2 question: two vaccine indicators whose joint presence
# changes the prescribing odds beyond either one alone.
set.seed(4242)
n <- 600L
d <- data.frame(
  pcv     = factor(sample(c("no", "yes"), n, replace = TRUE), levels = c("no", "yes")),
  rota    = factor(sample(c("no", "yes"), n, replace = TRUE), levels = c("no", "yes")),
  age     = stats::rnorm(n, 24, 6),
  visits  = stats::rpois(n, 3),
  stringsAsFactors = FALSE
)
lp <- -0.2 - 0.5 * (d$pcv == "yes") - 0.4 * (d$rota == "yes") -
  1.4 * (d$pcv == "yes") * (d$rota == "yes") + 0.02 * (d$age - 24)
d$abx <- factor(stats::rbinom(n, 1, stats::plogis(lp)), levels = c(0, 1),
                labels = c("No", "Yes"))
d$score <- 2 + 0.3 * (d$pcv == "yes") + 0.5 * d$age / 24 +
  1.1 * (d$pcv == "yes") * (d$rota == "yes") + stats::rnorm(n, 0, 0.6)

term_names <- function(res) as.character(res$coefficients$Term)

cat("\n== model_formula() builds the product term ==\n")

ok("interaction is appended as an explicit a:b term", {
  f <- model_formula("abx", c("pcv", "rota"), "pcv:rota")
  stopifnot(identical(paste(deparse(f), collapse = " "), "abx ~ pcv + rota + pcv:rota"))
})

ok("no interactions leaves the formula exactly as before", {
  f <- model_formula("abx", c("pcv", "rota"))
  stopifnot(identical(paste(deparse(f), collapse = " "), "abx ~ pcv + rota"),
            identical(paste(deparse(f), collapse = " "),
                      paste(deparse(stats::reformulate(c("pcv", "rota"),
                                                       response = "abx")),
                            collapse = " ")))
})

ok("non-syntactic names are backtick-quoted in both roles", {
  f <- model_formula("1_out", c("1_1_Household_ID", "sex"), "1_1_Household_ID:sex")
  txt <- paste(deparse(f), collapse = " ")
  stopifnot(identical(txt,
    "`1_out` ~ `1_1_Household_ID` + sex + `1_1_Household_ID`:sex"),
    # and it must actually parse into a usable formula
    setequal(all.vars(f), c("1_out", "1_1_Household_ID", "sex")))
})

ok("a:b and b:a are recognised as the same term", {
  stopifnot(identical(.interaction_key("pcv:rota"), .interaction_key("rota:pcv")))
})

cat("\n== validate_interactions() rejects mis-specifications ==\n")

ok("an interaction without its main effects is refused", {
  msg <- friendly_error(validate_interactions("pcv:rota", "pcv"))
  stopifnot(grepl("must also be selected as a predictor", msg), grepl("rota", msg))
})
ok("a one-variable interaction is refused", {
  stopifnot(grepl("at least two different variables",
                  friendly_error(validate_interactions("pcv", "pcv"))))
})
ok("a self-interaction is refused", {
  stopifnot(grepl("same variable twice",
                  friendly_error(validate_interactions("pcv:pcv", "pcv"))))
})
ok("a duplicated interaction is refused, in either order", {
  stopifnot(grepl("listed more than once",
                  friendly_error(validate_interactions(c("pcv:rota", "rota:pcv"),
                                                       c("pcv", "rota")))))
})
ok("empty and missing interactions are treated as none", {
  stopifnot(identical(validate_interactions(character(0), "pcv"), character(0)),
            identical(validate_interactions(c("", NA_character_), "pcv"),
                      character(0)))
})
ok("the outcome cannot be reached through an interaction", {
  # The main-effect rule closes this off: to name the outcome in an interaction
  # it must also be a predictor, which the outcome/predictor guard refuses.
  stopifnot(grepl("outcome cannot also be selected as a predictor",
    friendly_error(run_guided_model(d, list(outcome = "score",
      outcome_type = "continuous", predictors = c("score", "pcv"),
      interactions = "score:pcv", method = "ols")))),
    grepl("must also be selected as a predictor",
      friendly_error(validate_interactions("score:pcv", "pcv"))))
})

cat("\n== fitted models carry the interaction ==\n")

ok("logistic GLM matches glm() with the same interaction", {
  r <- run_guided_model(d, list(outcome = "abx", outcome_type = "binary",
    predictors = c("pcv", "rota", "age"), interactions = "pcv:rota",
    method = "logistic"))
  ref <- stats::glm(abx ~ pcv + rota + age + pcv:rota, data = d,
                    family = stats::binomial())
  s <- summary(ref)$coefficients
  stopifnot("pcvyes:rotayes" %in% term_names(r),
            all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(r$coefficients[["p-value"]], unname(s[, 4]), tolerance = TOL_P))
})

ok("the planted interaction is actually detected", {
  r <- run_guided_model(d, list(outcome = "abx", outcome_type = "binary",
    predictors = c("pcv", "rota", "age"), interactions = "pcv:rota",
    method = "logistic"))
  row <- match("pcvyes:rotayes", term_names(r))
  stopifnot(!is.na(row), r$coefficients[["p-value"]][row] < 0.05,
            r$coefficients$Estimate[row] < 0)     # planted as -1.4
})

ok("OLS matches lm() with the same interaction", {
  r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota", "age"), interactions = "pcv:rota",
    method = "ols"))
  ref <- stats::lm(score ~ pcv + rota + age + pcv:rota, data = d)
  stopifnot(all.equal(r$coefficients$Estimate, unname(coef(ref)),
                      tolerance = TOL_STAT))
})

ok("LASSO builds a design column for the interaction", {
  r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota", "age", "visits"), interactions = "pcv:rota",
    method = "lasso"))
  stopifnot("pcvyes:rotayes" %in% term_names(r))
})

ok("interaction order does not change the fit", {
  a <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota"), interactions = "pcv:rota", method = "ols"))
  b <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota"), interactions = "rota:pcv", method = "ols"))
  stopifnot(all.equal(sort(a$coefficients$Estimate), sort(b$coefficients$Estimate),
                      tolerance = TOL_STAT))
})

cat("\n== every model method that takes a formula ==\n")

# Interactions reach the fit through model_formula(), so every method that is
# handed one should carry the product term. Asserted for all of them rather than
# inferred from the three that were convenient to check.
method_cases <- list(
  list(m = "robust",            o = "score",  t = "continuous"),
  list(m = "quantile",          o = "score",  t = "continuous"),
  list(m = "ridge",             o = "score",  t = "continuous"),
  list(m = "pls",               o = "score",  t = "continuous"),
  list(m = "poisson",           o = "visits", t = "count"),
  list(m = "negative_binomial", o = "visits", t = "count"),
  list(m = "lasso",             o = "visits", t = "count")
)
for (mc in method_cases) {
  ok(sprintf("%s carries the interaction term", mc$m), {
    r <- run_guided_model(d, list(outcome = mc$o, outcome_type = mc$t,
      predictors = c("pcv", "rota", "age"), interactions = "pcv:rota",
      method = mc$m, tau = 0.5))
    stopifnot("pcvyes:rotayes" %in% term_names(r))
  })
}

cat("\n== every predictor-type combination, not just factor x factor ==\n")

# The UI offers any pair of selected predictors, so the engine has to handle
# every combination of measurement types the app allows as a predictor - not
# only the two-level factors the fixture above happens to use.
d$grade <- factor(sample(c("low", "mid", "high"), n, replace = TRUE),
                  levels = c("low", "mid", "high"), ordered = TRUE)

type_cases <- list(
  list(ix = "age:visits",  pred = c("age", "visits"),   want = "age:visits",
       what = "numeric x numeric"),
  list(ix = "pcv:age",     pred = c("pcv", "age"),      want = "pcvyes:age",
       what = "factor x numeric"),
  list(ix = "age:pcv",     pred = c("age", "pcv"),      want = "age:pcvyes",
       what = "numeric x factor (reversed)"),
  list(ix = "grade:pcv",   pred = c("grade", "pcv"),    want = "grade.L:pcvyes",
       what = "ordered factor x factor"),
  list(ix = "grade:age",   pred = c("grade", "age"),    want = "grade.L:age",
       what = "ordered factor x numeric")
)
for (tc in type_cases) {
  ok(sprintf("%s interaction fits and matches lm()", tc$what), {
    r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
      predictors = tc$pred, interactions = tc$ix, method = "ols"))
    ref <- stats::lm(stats::as.formula(paste("score ~",
             paste(c(tc$pred, tc$ix), collapse = " + "))), data = d)
    stopifnot(tc$want %in% term_names(r),
              all.equal(r$coefficients$Estimate, unname(coef(ref)),
                        tolerance = TOL_STAT))
  })
}

ok("a three-way interaction is accepted by the engine", {
  # The UI only offers pairs, but the spec format allows n-way terms, so the
  # engine must not silently mangle one.
  r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota", "age"), interactions = "pcv:rota:age",
    method = "ols"))
  ref <- stats::lm(score ~ pcv + rota + age + pcv:rota:age, data = d)
  stopifnot("pcvyes:rotayes:age" %in% term_names(r),
            all.equal(r$coefficients$Estimate, unname(coef(ref)),
                      tolerance = TOL_STAT))
})

ok("several interactions can be fitted at once", {
  r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "rota", "age"),
    interactions = c("pcv:rota", "pcv:age"), method = "ols"))
  stopifnot(all(c("pcvyes:rotayes", "pcvyes:age") %in% term_names(r)))
})

ok("a non-syntactic name survives an actual fit, not just the formula", {
  dn <- d
  names(dn)[names(dn) == "pcv"] <- "1_pcv given"
  r <- run_guided_model(dn, list(outcome = "score", outcome_type = "continuous",
    predictors = c("1_pcv given", "age"), interactions = "1_pcv given:age",
    method = "ols"))
  stopifnot(any(grepl("1_pcv given", term_names(r), fixed = TRUE)),
            nrow(r$coefficients) == 4L)
})

cat("\n== clustered models accept interactions too ==\n")

# Repeated measures with the household-style string id the ARCH extracts use.
set.seed(99)
n_hh <- 60L
k <- 6L
hh <- sprintf("01/%03d/05", seq_len(n_hh))
u <- stats::rnorm(n_hh, 0, 0.7)
cl <- data.frame(
  hhid = rep(hh, each = k),
  visit = rep(seq_len(k), n_hh),
  pcv  = factor(rep(sample(c("no", "yes"), n_hh, replace = TRUE), each = k),
                levels = c("no", "yes")),
  rota = factor(sample(c("no", "yes"), n_hh * k, replace = TRUE),
                levels = c("no", "yes")),
  stringsAsFactors = FALSE
)
cl$y <- 1 + 0.4 * (cl$pcv == "yes") - 0.3 * (cl$rota == "yes") +
  0.9 * (cl$pcv == "yes") * (cl$rota == "yes") +
  rep(u, each = k) + stats::rnorm(n_hh * k, 0, 0.5)

ok("GEE fits an interaction and reports its term", {
  r <- run_guided_clustered_model(cl, list(outcome = "y",
    outcome_type = "continuous", predictors = c("pcv", "rota"),
    interactions = "pcv:rota", method = "gee_gaussian",
    cluster = "hhid", corstr = "exchangeable"))
  stopifnot("pcvyes:rotayes" %in% term_names(r),
            all(is.finite(r$coefficients[["Std. Error"]])))
})

ok("LMM fits an interaction alongside the random intercept", {
  r <- run_guided_clustered_model(cl, list(outcome = "y",
    outcome_type = "continuous", predictors = c("pcv", "rota"),
    interactions = "pcv:rota", method = "lmm", cluster = "hhid"))
  stopifnot("pcvyes:rotayes" %in% term_names(r),
            grepl("hhid", r$formula_text %||% paste(deparse(r$formula), collapse = " ")))
})

ok("a random slope still has to be a main-effect predictor", {
  stopifnot(grepl("random slope must be one of the selected predictors",
    friendly_error(run_guided_clustered_model(cl, list(outcome = "y",
      outcome_type = "continuous", predictors = c("pcv", "rota"),
      interactions = "pcv:rota", method = "lmm", cluster = "hhid",
      random_slope = "visit")))))
})

clustered_cases <- list(
  list(m = "gee_binomial", t = "binary"),
  list(m = "gee_poisson",  t = "count"),
  list(m = "glmm_binomial", t = "binary"),
  list(m = "glmm_poisson",  t = "count")
)
cl$bin <- factor(stats::rbinom(nrow(cl), 1, stats::plogis(cl$y - 1)),
                 levels = c(0, 1), labels = c("No", "Yes"))
cl$cnt <- stats::rpois(nrow(cl), exp(0.2 + 0.3 * (cl$pcv == "yes")))
for (mc in clustered_cases) {
  ok(sprintf("%s carries the interaction term", mc$m), {
    r <- run_guided_clustered_model(cl, list(
      outcome = if (mc$t == "binary") "bin" else "cnt", outcome_type = mc$t,
      predictors = c("pcv", "rota"), interactions = "pcv:rota",
      method = mc$m, cluster = "hhid", corstr = "exchangeable"))
    stopifnot("pcvyes:rotayes" %in% term_names(r))
  })
}


cat("\n== the write-up does not call an interaction an association ==\n")

ok("a significant interaction is described as effect modification", {
  r <- run_guided_model(d, list(outcome = "abx", outcome_type = "binary",
    predictors = c("pcv", "rota", "age"), interactions = "pcv:rota",
    method = "logistic"))
  txt <- r$interpretation
  stopifnot(grepl("interaction", txt), grepl("depends on the level", txt),
            grepl("conditional", txt))
})

ok("a model with no interaction keeps the plain association wording", {
  r <- run_guided_model(d, list(outcome = "score", outcome_type = "continuous",
    predictors = c("pcv", "age"), method = "ols"))
  stopifnot(grepl("adjusted association", r$interpretation),
            !grepl("interaction", r$interpretation))
})

cat("\n== an AI plan can ask for an interaction ==\n")

ok("the planner prompt and schema carry an interactions field", {
  # Every column typed, as the app always does; a column the caller forgets is
  # treated as text rather than erroring (see build_planner_metadata).
  tp <- stats::setNames(rep("nominal", ncol(d)), names(d))
  tp[c("age", "score")] <- "continuous"; tp[["visits"]] <- "count"
  tp[c("abx", "pcv", "rota")] <- "binary"; tp[["grade"]] <- "ordinal"
  meta <- build_planner_metadata(d, tp)
  pr <- planner_prompt("Does the vaccine effect differ by prior vaccination?", meta)
  stopifnot(grepl('"interactions"', pr$user, fixed = TRUE),
            grepl("effect modification", pr$system),
            grepl("a:b", pr$system, fixed = TRUE))
})

ok("a planned interaction is read back when its main effects are predictors", {
  plan <- list(predictors = list("pcv", "rota", "age"),
               interactions = list("pcv:rota"))
  stopifnot(identical(plan_interactions(plan), "pcv:rota"))
})

ok("a planned interaction naming a non-predictor is narrowed away", {
  # The engine would refuse it, so the plan is narrowed here rather than
  # failing at fit time.
  stopifnot(identical(plan_interactions(
    list(predictors = list("pcv"), interactions = list("pcv:rota"))),
    character(0)))
  stopifnot(identical(plan_interactions(
    list(predictors = list("pcv", "rota"), interactions = list("pcv:pcv"))),
    character(0)))
  stopifnot(identical(plan_interactions(NULL), character(0)),
            identical(plan_interactions(list(predictors = list("a", "b"))),
                      character(0)))
})

ok("a plan's interactions survive validation and repair untouched", {
  plan <- list(outcome_variable = "abx", outcome_type = "binary",
               predictors = list("pcv", "rota"),
               interactions = list("pcv:rota"),
               regression_method = "logistic", advanced_regression = "none",
               allowable_tests = list(), assumptions_to_check = list())
  stopifnot(length(validate_plan(plan)) == 0L)
  rep <- repair_plan(plan)
  stopifnot(identical(plan_interactions(rep$plan), "pcv:rota"))
})

report_and_exit()
