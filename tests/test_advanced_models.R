# tests/test_advanced_models.R -----------------------------------------------
# Phase 4 clustered/repeated-model tests for GEE, LMM, and GLMM.

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

near <- function(a, b, tol = 1e-6) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

set.seed(2026)
ns <- 80
id <- factor(rep(seq_len(ns), each = 3))
time <- rep(1:3, ns)
u <- rnorm(ns, 0, 0.7)
x <- rnorm(ns * 3)
z <- factor(sample(c("A", "B"), ns * 3, replace = TRUE))
d <- data.frame(
  id = id,
  time = time,
  x = x,
  z = z,
  y = 1 + 0.7 * x + ifelse(z == "B", 0.5, 0) + u[id] + rnorm(ns * 3, 0, 0.5),
  bin = factor(rbinom(ns * 3, 1, plogis(-0.4 + 0.5 * x + u[id])),
               labels = c("No", "Yes")),
  count = rpois(ns * 3, exp(0.4 + 0.2 * x + u[id]))
)

cat("\n== Phase 4 clustered method availability ==\n")
ok("clustered binary methods include GEE and GLMM", {
  stopifnot(setequal(available_model_methods("binary", "clustered"),
                     c("gee_binomial", "glmm_binomial")))
})
ok("clustered continuous methods include GEE and LMM", {
  stopifnot(setequal(available_model_methods("continuous", "clustered"),
                     c("gee_gaussian", "lmm")))
})

cat("\n== GEE, LMM, and GLMM reference checks ==\n")
ok("Gaussian GEE coefficients match geeglm()", {
  r <- run_guided_clustered_model(d, list(outcome = "y", outcome_type = "continuous",
                                          predictors = "x", method = "gee_gaussian",
                                          cluster = "id", corstr = "exchangeable"))
  ref <- geepack::geeglm(y ~ x, id = id, data = d,
                         family = gaussian(), corstr = "exchangeable")
  stopifnot(grepl("GEE", r$label),
            all.equal(r$coefficients$Estimate,
                      unname(coef(ref)), tolerance = 1e-6))
})
ok("binary GEE reports odds ratios", {
  r <- run_guided_clustered_model(d, list(outcome = "bin", outcome_type = "binary",
                                          predictors = "x", method = "gee_binomial",
                                          cluster = "id", corstr = "exchangeable"))
  stopifnot(identical(r$effect_name, "Odds ratio"),
            "Odds ratio" %in% names(r$coefficients))
})
ok("LMM fixed effects match lmer()", {
  r <- run_guided_clustered_model(d, list(outcome = "y", outcome_type = "continuous",
                                          predictors = "x", method = "lmm",
                                          cluster = "id", random_slope = "(none)"))
  ref <- lme4::lmer(y ~ x + (1 | id), data = d, REML = FALSE)
  stopifnot(grepl("LMM", r$label),
            all.equal(r$coefficients$Estimate,
                      unname(lme4::fixef(ref)), tolerance = 1e-6))
})
ok("LMM reports variance components and an ICC matching VarCorr()", {
  r <- run_guided_clustered_model(d, list(outcome = "y", outcome_type = "continuous",
                                          predictors = "x", method = "lmm",
                                          cluster = "id", random_slope = "(none)"))
  ref <- lme4::lmer(y ~ x + (1 | id), data = d, REML = FALSE)
  vd <- as.data.frame(lme4::VarCorr(ref))
  tau <- vd$vcov[vd$grp == "id"]; sigma2 <- vd$vcov[vd$grp == "Residual"]
  icc_ref <- tau / (tau + sigma2)
  stopifnot(!is.null(r$random_effects),
            all(c("Component", "Variance", "Std. Dev.", "% of variance") %in%
                  names(r$random_effects)),
            any(r$random_effects$Component == "Residual"),
            "ICC (id)" %in% r$fit_stats$Metric,
            near(r$fit_stats$Value[r$fit_stats$Metric == "ICC (id)"], icc_ref, 1e-6))
})
ok("logistic GLMM reports odds ratios and a latent-scale ICC", {
  r <- run_guided_clustered_model(d, list(outcome = "bin", outcome_type = "binary",
                                          predictors = "x", method = "glmm_binomial",
                                          cluster = "id", random_slope = "(none)"))
  stopifnot(grepl("GLMM", r$label),
            "Odds ratio" %in% names(r$coefficients),
            !is.null(r$random_effects),
            any(grepl("Residual", r$random_effects$Component)),
            "ICC (id)" %in% r$fit_stats$Metric)
})
ok("a random slope reports variance components but no headline ICC", {
  r <- run_guided_clustered_model(d, list(outcome = "y", outcome_type = "continuous",
                                          predictors = "x", method = "lmm",
                                          cluster = "id", random_slope = "x"))
  stopifnot(!is.null(r$random_effects),
            nrow(r$random_effects) >= 3,
            !("ICC (id)" %in% r$fit_stats$Metric))
})
ok("Poisson GEE and GLMM fit count outcomes", {
  rg <- run_guided_clustered_model(d, list(outcome = "count", outcome_type = "count",
                                           predictors = "x", method = "gee_poisson",
                                           cluster = "id", corstr = "exchangeable"))
  rm <- run_guided_clustered_model(d, list(outcome = "count", outcome_type = "count",
                                           predictors = "x", method = "glmm_poisson",
                                           cluster = "id", random_slope = "(none)"))
  stopifnot("Rate ratio" %in% names(rg$coefficients),
            "Rate ratio" %in% names(rm$coefficients))
})

cat("\n== Advanced Models tab wiring ==\n")
ok("Advanced tab renders a clustered model and adds report items", {
  bag <- list(); rendered <- NULL
  shiny::testServer(mod_phase4_server,
    args = list(data = reactive(d), types = reactive(c(
      id = "identifier", time = "ordinal", x = "continuous", z = "nominal",
      y = "continuous", bin = "binary", count = "count")),
      add_report = function(x) bag[[length(bag) + 1]] <<- x), {
      session$setInputs(analysis = "clustered", cluster_outcome = "y",
                        cluster_method = "gee_gaussian", cluster_predictors = "x",
                        cluster = "id", corstr = "exchangeable")
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      rendered <<- output$coefficients
      session$setInputs(include_diag = TRUE); session$flushReact()
      session$setInputs(add = 1); session$flushReact()
    })
  stopifnot(is.character(rendered), grepl("Estimate", rendered, fixed = TRUE),
            length(bag) >= 3)
})

report_and_exit()
