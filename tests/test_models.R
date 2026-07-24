# tests/test_models.R --------------------------------------------------------
# Phase 3 independent regression tests.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(shiny)
  library(MASS)
  library(quantreg)
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

cat("\n== method availability ==\n")
ok("continuous independent methods", {
  stopifnot(identical(available_model_methods("continuous"),
                       c("ols", "robust", "quantile", "lasso", "ridge", "pls")))
})
ok("continuous methods are independent regressions", {
  stopifnot(identical(available_model_methods("continuous", "independent"),
                       c("ols", "robust", "quantile", "lasso", "ridge", "pls")))
})
ok("binary and count outcomes expose GLM methods", {
  bin_methods <- suggest_model_plan("binary", "independent")$methods
  count_methods <- suggest_model_plan("count", "independent")$methods
  stopifnot(all(c("logistic") %in% bin_methods),
            all(c("poisson", "negative_binomial") %in% count_methods),
            grepl("GLM", MODEL_METHOD_LABELS[["logistic"]]),
            grepl("GLM", MODEL_METHOD_LABELS[["poisson"]]),
            grepl("GLM", MODEL_METHOD_LABELS[["negative_binomial"]]))
})

cat("\n== independent regression families ==\n")
ok("OLS coefficients match lm()", {
  r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                predictors = c("x", "z"), method = "ols"))
  ref <- lm(y ~ x + z, data = d)
  stopifnot(identical(r$label, "OLS linear regression"),
            all.equal(r$coefficients$Estimate,
                      unname(coef(ref)), tolerance = 1e-6))
})
ok("robust coefficients match MASS::rlm()", {
  r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                predictors = c("x", "z"), method = "robust"))
  ref <- MASS::rlm(y ~ x + z, data = d, psi = MASS::psi.huber)
  stopifnot(grepl("Robust", r$label),
            all.equal(r$coefficients$Estimate,
                      unname(coef(ref)), tolerance = 1e-6))
})
ok("quantile coefficients match quantreg::rq()", {
  r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                predictors = c("x", "z"), method = "quantile",
                                tau = 0.5))
  ref <- quantreg::rq(y ~ x + z, data = d, tau = 0.5)
  stopifnot(grepl("Quantile", r$label),
            all.equal(r$coefficients$Estimate,
                      unname(coef(ref)), tolerance = 1e-6))
})
ok("logistic regression reports odds ratios", {
  r <- run_guided_model(d, list(outcome = "bin", outcome_type = "binary",
                                predictors = c("x", "z"), method = "logistic"))
  ref <- glm(bin ~ x + z, data = d, family = binomial())
  stopifnot(grepl("Logistic", r$label),
            all.equal(r$coefficients[["Odds ratio"]],
                      unname(exp(coef(ref))), tolerance = 1e-6))
})
ok("Poisson and negative-binomial regression fit", {
  rp <- run_guided_model(d, list(outcome = "count", outcome_type = "count",
                                 predictors = c("x", "z"), method = "poisson"))
  rn <- run_guided_model(d, list(outcome = "count", outcome_type = "count",
                                 predictors = c("x", "z"), method = "negative_binomial"))
  stopifnot(grepl("Poisson", rp$label), grepl("Negative binomial", rn$label),
            "Rate ratio" %in% names(rp$coefficients),
            "Rate ratio" %in% names(rn$coefficients))
})
ok("selected character outcomes are converted temporarily", {
  d2 <- d
  d2$y_text <- format(round(d2$y, 3), nsmall = 3)
  spec <- list(outcome = "y_text", outcome_type = "continuous",
               predictors = "x", method = "ols")
  prepared <- prepare_outcome_data(d2, spec$outcome, spec$outcome_type,
                                   spec$predictors)
  r <- run_guided_model(prepared$data, spec)
  stopifnot(is.character(d2$y_text), is.numeric(prepared$data$y_text),
            grepl("OLS", r$label))
})

cat("\n== Models tab wiring ==\n")
ok("Models tab shows the GLM choices for binary and count outcomes", {
  bin_html <- count_html <- NULL
  types <- c(id = "identifier", time = "ordinal", x = "continuous", z = "nominal",
             y = "continuous", bin = "binary", count = "count")
  shiny::testServer(mod_models_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("bin", "count")),
                add_report = function(x) NULL), {
      session$setInputs(outcome = "bin")
      session$flushReact()
      bin_html <<- output$method_ui$html
      session$setInputs(outcome = "count")
      session$flushReact()
      count_html <<- output$method_ui$html
    })
  stopifnot(grepl("Logistic GLM", bin_html, fixed = TRUE),
            grepl("Poisson GLM", count_html, fixed = TRUE),
            grepl("Negative binomial GLM", count_html, fixed = TRUE))
})
ok("Models tab explains when candidate outcomes are missing", {
  notice <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(d), types = reactive(c(
      id = "identifier", time = "ordinal", x = "continuous", z = "nominal",
      y = "continuous", bin = "binary", count = "count")),
      outcomes = reactive(character(0)), add_report = function(x) NULL), {
      session$flushReact()
      notice <<- output$outcome_notice$html
    })
  stopifnot(grepl("select one or more candidate outcomes", notice, fixed = TRUE))
})
ok("Models tab renders coefficients and adds report items", {
  bag <- list(); rendered <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(d), types = reactive(c(
      id = "identifier", time = "ordinal", x = "continuous", z = "nominal",
      y = "continuous", bin = "binary", count = "count")),
      add_report = function(x) bag[[length(bag) + 1]] <<- x), {
      session$setInputs(outcome = "y", method = "ols", predictors = "x")
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      rendered <<- output$coefficients
      session$setInputs(add = 1); session$flushReact()
    })
  stopifnot(is.character(rendered), grepl("Estimate", rendered, fixed = TRUE),
            length(bag) >= 3)
})

report_and_exit()
