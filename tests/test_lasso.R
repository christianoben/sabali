# tests/test_lasso.R --------------------------------------------------------
# LASSO (L1-penalised) regression on the Regression tab: engine correctness vs
# glmnet, its selection behaviour, the deliberate ABSENCE of standard errors /
# p-values, and the module wiring (method availability + lambda control).
#
# Run:  Rscript tests/test_lasso.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({ library(shiny); library(glmnet) })

set.seed(7)
n <- 200
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
                x4 = rnorm(n), noise = rnorm(n))
d$y   <- 1 + 2 * d$x1 - 1.5 * d$x2 + rnorm(n)          # x3, x4, noise irrelevant
d$bin <- factor(rbinom(n, 1, plogis(0.5 * d$x1 - d$x2)), labels = c("No", "Yes"))
d$cnt <- rpois(n, exp(0.3 + 0.4 * d$x1))
preds <- c("x1", "x2", "x3", "x4", "noise")

cat("\n== method availability includes LASSO ==\n")
ok("LASSO offered for a continuous outcome",
   stopifnot("lasso" %in% available_model_methods("continuous")))
ok("LASSO offered for a binary outcome",
   stopifnot("lasso" %in% available_model_methods("binary")))
ok("LASSO offered for a count outcome",
   stopifnot("lasso" %in% available_model_methods("count")))
ok("LASSO has a friendly method label",
   stopifnot(grepl("LASSO", MODEL_METHOD_LABELS[["lasso"]])))

cat("\n== continuous LASSO matches glmnet (lambda.min) ==\n")
r <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                              predictors = preds, method = "lasso",
                              lambda_choice = "min"))
mm <- stats::model.matrix(y ~ x1 + x2 + x3 + x4 + noise, d); x <- mm[, -1]
set.seed(1)
cv <- glmnet::cv.glmnet(x, d$y, family = "gaussian", alpha = 1, nfolds = 10,
                        standardize = TRUE)
ref <- as.numeric(as.matrix(coef(cv, s = cv$lambda.min)))
ok("coefficients equal direct glmnet fit",
   stopifnot(isTRUE(all.equal(r$coefficients$Estimate, ref, tolerance = 1e-8))))
ok("label is the LASSO label", stopifnot(grepl("LASSO", r$label)))

cat("\n== coefficient table shape: selection, no SE / p / CI ==\n")
ok("has Term, Estimate, Selected columns",
   stopifnot(all(c("Term", "Estimate", "Selected") %in% names(r$coefficients))))
ok("does NOT report Std. Error / Statistic / p-value / CI",
   stopifnot(!any(c("Std. Error", "Statistic", "p-value", "CI lower", "CI upper")
                  %in% names(r$coefficients))))
ok("true predictors x1 and x2 are retained (Selected = yes)", {
  sel <- r$coefficients$Term[r$coefficients$Selected == "yes"]
  stopifnot(all(c("x1", "x2") %in% sel))
})
ok("intercept row is not marked as a selected predictor",
   stopifnot(r$coefficients$Selected[r$coefficients$Term == "(Intercept)"] == "-"))

cat("\n== fit statistics ==\n")
ok("fit_stats report lambda and predictors selected", {
  m <- r$fit_stats$Metric
  stopifnot("lambda" %in% m, "predictors selected" %in% m, "n used" %in% m)
})
ok("n_used equals complete rows", stopifnot(r$n_used == n))

cat("\n== effect ratios for binary and count outcomes ==\n")
rb <- run_guided_model(d, list(outcome = "bin", outcome_type = "binary",
                               predictors = c("x1","x2","x3","x4"), method = "lasso"))
rc <- run_guided_model(d, list(outcome = "cnt", outcome_type = "count",
                               predictors = c("x1","x2","x3","x4"), method = "lasso"))
ok("binary LASSO reports an Odds ratio column",
   stopifnot(identical(rb$effect_name, "Odds ratio"),
             "Odds ratio" %in% names(rb$coefficients)))
ok("count LASSO reports a Rate ratio column",
   stopifnot(identical(rc$effect_name, "Rate ratio"),
             "Rate ratio" %in% names(rc$coefficients)))
ok("binary LASSO Odds ratio == exp(Estimate)",
   stopifnot(isTRUE(all.equal(rb$coefficients[["Odds ratio"]],
                              exp(rb$coefficients$Estimate)))))

cat("\n== lambda rule, reproducibility, RNG hygiene ==\n")
nz <- function(res) sum(res$coefficients$Selected == "yes")
r_1se <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                  predictors = preds, method = "lasso",
                                  lambda_choice = "1se"))
ok("1-SE rule is no less sparse than minimum-CV rule",
   stopifnot(nz(r_1se) <= nz(r)))
ok("repeated fits are identical (deterministic CV folds)", {
  r2 <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                 predictors = preds, method = "lasso",
                                 lambda_choice = "min"))
  stopifnot(isTRUE(all.equal(r$coefficients$Estimate, r2$coefficients$Estimate)))
})
ok("global RNG state is restored after fitting", {
  set.seed(99); before <- .Random.seed
  invisible(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                     predictors = c("x1","x2","x3"), method = "lasso")))
  stopifnot(identical(before, .Random.seed))
})

cat("\n== guards, interpretation, diagnostics ==\n")
ok("single-predictor LASSO is refused with a clear message", {
  e <- tryCatch(run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                         predictors = "x1", method = "lasso")),
                error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("two predictor", e))
})
ok("interpretation states SE/p are not reported",
   stopifnot(grepl("standard errors and p-values", r$interpretation)))
ok("diagnostic plot is a cross-validation ggplot",
   stopifnot(inherits(r$diagnostic_plot, "ggplot")))

cat("\n== Regression tab wiring ==\n")
ok("Models tab fits a LASSO model and adds report items", {
  bag <- list(); rendered <- NULL
  types <- c(x1="continuous", x2="continuous", x3="continuous", x4="continuous",
             noise="continuous", y="continuous", bin="binary", cnt="count")
  shiny::testServer(mod_models_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("y")), panel = reactive(NULL),
                add_report = function(x) bag[[length(bag) + 1]] <<- x), {
      session$setInputs(outcome = "y", outcome_type = "continuous",
                        method = "lasso", lambda_choice = "min",
                        predictors = preds)
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      rendered <<- output$coefficients
      session$setInputs(include_diag = TRUE); session$flushReact()
      session$setInputs(add = 1); session$flushReact()
    })
  stopifnot(is.character(rendered), grepl("Selected", rendered, fixed = TRUE),
            length(bag) >= 3)
})

report_and_exit()
