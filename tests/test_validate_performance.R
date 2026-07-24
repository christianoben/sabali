# Lightweight scale/UX guard. It is deliberately generous so it catches
# accidental quadratic work without becoming a machine-speed benchmark.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== scale and progress guards ==\n")
set.seed(42)
large <- data.frame(
  y = rnorm(5000),
  x1 = rnorm(5000), x2 = rnorm(5000),
  group = factor(sample(c("A", "B", "C"), 5000, TRUE)))
types <- c(y = "continuous", x1 = "continuous", x2 = "continuous", group = "nominal")
timed <- system.time({
  d <- detect_all_types(large)
  fit <- run_guided_model(large, list(outcome = "y", outcome_type = "continuous",
                                      predictors = c("x1", "x2"), method = "ols"))
})
ok("5,000-row type detection and OLS fit finish within the generous guard", {
  stopifnot(nrow(d) == 4L, is.list(fit), timed[["elapsed"]] < 60)
})
ok("long-running model paths expose progress indicators", {
  a <- paste(readLines(file.path(ROOT, "R/mod_models.R")), collapse = "\n")
  b <- paste(readLines(file.path(ROOT, "R/mod_phase4.R")), collapse = "\n")
  stopifnot(grepl("withProgress", a, fixed = TRUE), grepl("withProgress", b, fixed = TRUE))
})

report_and_exit()
