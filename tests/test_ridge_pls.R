# tests/test_ridge_pls.R ----------------------------------------------------
# Ridge (L2-penalised, glmnet) and PLS (partial least squares) regression on
# the Regression tab: engine correctness vs the reference package fit, the
# deliberate absence of SE / p-values, method availability, guards and wiring.
#
# Run:  Rscript tests/test_ridge_pls.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({ library(shiny); library(glmnet); library(pls) })

set.seed(7)
n <- 200
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
d$y   <- 1 + 2 * d$x1 - 1.5 * d$x2 + rnorm(n)
d$bin <- factor(rbinom(n, 1, plogis(0.5 * d$x1 - d$x2)), labels = c("No", "Yes"))
d$cnt <- rpois(n, exp(0.3 + 0.4 * d$x1))
preds <- c("x1", "x2", "x3", "x4")

cat("\n== method availability ==\n")
ok("Ridge offered for continuous, binary, count", {
  stopifnot("ridge" %in% available_model_methods("continuous"),
            "ridge" %in% available_model_methods("binary"),
            "ridge" %in% available_model_methods("count"))
})
ok("PLS offered for continuous only", {
  stopifnot("pls" %in% available_model_methods("continuous"),
            !"pls" %in% available_model_methods("binary"),
            !"pls" %in% available_model_methods("count"))
})
ok("friendly labels exist",
   stopifnot(grepl("Ridge", MODEL_METHOD_LABELS[["ridge"]]),
             grepl("least squares|PLS", MODEL_METHOD_LABELS[["pls"]])))

cat("\n== Ridge matches glmnet(alpha = 0) ==\n")
r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                              predictors = preds, method = "ridge", lambda_choice = "min"))
mm <- stats::model.matrix(y ~ x1 + x2 + x3 + x4, d); x <- mm[, -1]
set.seed(1)
cv <- glmnet::cv.glmnet(x, d$y, family = "gaussian", alpha = 0, nfolds = 10, standardize = TRUE)
ref <- as.numeric(as.matrix(coef(cv, s = cv$lambda.min)))
ok("ridge coefficients equal direct glmnet fit",
   stopifnot(isTRUE(all.equal(r$coefficients$Estimate, ref, tolerance = 1e-8))))
ok("ridge label is correct", stopifnot(grepl("Ridge", r$label)))
ok("ridge has NO Selected column (keeps all predictors)",
   stopifnot(!"Selected" %in% names(r$coefficients)))
ok("ridge reports no SE / p / CI",
   stopifnot(!any(c("Std. Error", "Statistic", "p-value", "CI lower") %in% names(r$coefficients))))
ok("ridge fit_stats omit 'predictors selected'",
   stopifnot(!"predictors selected" %in% r$fit_stats$Metric,
             "lambda" %in% r$fit_stats$Metric))
ok("ridge is reproducible", {
  r2 <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                 predictors = preds, method = "ridge"))
  stopifnot(isTRUE(all.equal(r$coefficients$Estimate, r2$coefficients$Estimate)))
})
ok("ridge global RNG state restored", {
  set.seed(99); before <- .Random.seed
  invisible(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                     predictors = preds, method = "ridge")))
  stopifnot(identical(before, .Random.seed))
})

cat("\n== Ridge for binary / count reports effect ratios ==\n")
rb <- run_guided_model(d, list(outcome = "bin", outcome_type = "binary",
                               predictors = preds, method = "ridge"))
rc <- run_guided_model(d, list(outcome = "cnt", outcome_type = "count",
                               predictors = preds, method = "ridge"))
ok("binary ridge -> Odds ratio", stopifnot("Odds ratio" %in% names(rb$coefficients)))
ok("count ridge -> Rate ratio", stopifnot("Rate ratio" %in% names(rc$coefficients)))

cat("\n== PLS matches pls::plsr ==\n")
p <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                              predictors = preds, method = "pls"))
set.seed(1)   # matches the deterministic CV seed used inside run_pls_model()
fit <- pls::plsr(y ~ x1 + x2 + x3 + x4, data = d, scale = TRUE, validation = "CV")
# pls::RMSEP()/MSEP() resolve their inner call in the caller frame (fail unless
# pls is attached), so derive CV RMSEP from the exported pls::mvrValstats() -
# exactly as run_pls_model() does.
cv_stats <- pls::mvrValstats(fit, estimate = "CV")
rmsep <- sqrt(cv_stats$SSE[1, 1, ] / cv_stats$nobj[1])
nc <- max(1L, which.min(rmsep) - 1L)
refco <- as.numeric(drop(coef(fit, ncomp = nc)))
ok("PLS coefficients equal direct plsr fit",
   stopifnot(isTRUE(all.equal(p$coefficients$Estimate, refco, tolerance = 1e-6))))
ok("PLS chose the min-RMSEP component count", {
  used <- as.numeric(p$fit_stats$Value[p$fit_stats$Metric == "components used"])
  stopifnot(used == nc)
})
ok("PLS reports variance explained and RMSEP", {
  m <- p$fit_stats$Metric
  stopifnot("% Y variance explained (R2)" %in% m, "CV RMSEP" %in% m)
})
ok("PLS reports no SE / p", stopifnot(!any(c("Std. Error", "p-value") %in% names(p$coefficients))))

cat("\n== guards & diagnostics ==\n")
ok("Ridge with a single predictor is refused", {
  e <- tryCatch(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                         predictors = "x1", method = "ridge")),
                error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("two predictor", e))
})
ok("PLS with a single predictor is refused", {
  e <- tryCatch(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                         predictors = "x1", method = "pls")),
                error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("two predictor", e))
})
ok("both diagnostic plots are ggplots",
   stopifnot(inherits(r$diagnostic_plot, "ggplot"), inherits(p$diagnostic_plot, "ggplot")))

cat("\n== Regression tab wiring ==\n")
ok("Models tab fits ridge and pls and adds report items", {
  types <- c(x1="continuous", x2="continuous", x3="continuous", x4="continuous",
             y="continuous", bin="binary", cnt="count")
  for (mth in c("ridge", "pls")) {
    bag <- list(); rendered <- NULL
    shiny::testServer(mod_models_server,
      args = list(data = reactive(d), types = reactive(types),
                  outcomes = reactive(c("y")), panel = reactive(NULL),
                  add_report = function(x) bag[[length(bag) + 1]] <<- x), {
        session$setInputs(outcome = "y", outcome_type = "continuous",
                          method = mth, lambda_choice = "min", predictors = preds)
        session$flushReact(); session$setInputs(run = 1); session$flushReact()
        rendered <<- output$coefficients
        session$setInputs(add = 1); session$flushReact()
      })
    stopifnot(is.character(rendered), grepl("Estimate", rendered, fixed = TRUE),
              length(bag) >= 3)
  }
})

report_and_exit()
