# tests/test_stale_refit.R --------------------------------------------------
# A fitted analysis must not linger stale after the underlying data changes
# (e.g. a factor level removed via the tab-0 Data contract). The fit is held in
# a reactiveVal and cleared when data() changes; a fresh fit uses the new data.
# Also: prepare_outcome_data() drops unused factor levels so an empty level
# never surfaces as a phantom model term.
#
# Run:  Rscript tests/test_stale_refit.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== prepare_outcome_data drops unused factor levels ==\n")
ok("an empty (0-row) factor level is dropped from the model data", {
  d <- data.frame(y = rbinom(30, 1, 0.4),
                  g = factor(sample(c("Yes", "No"), 30, TRUE),
                             levels = c("Yes", "No", "Unknown")),  # Unknown: 0 rows
                  stringsAsFactors = FALSE)
  pd <- prepare_outcome_data(d, "y", "binary", extra_vars = "g")
  stopifnot(setequal(levels(pd$data$g), c("Yes", "No")),
            !("Unknown" %in% levels(pd$data$g)))
})

cat("\n== Regression fit is invalidated when the data changes ==\n")
ok("editing the data clears a stale fit; re-running uses the new data", {
  set.seed(1)
  with_level_c <- data.frame(
    y = rnorm(60), x = rnorm(60),
    g = factor(sample(c("A", "B", "C"), 60, TRUE)), stringsAsFactors = FALSE)
  # same columns, but level C removed from g and its rows dropped
  no_level_c <- with_level_c[with_level_c$g != "C", , drop = FALSE]
  no_level_c$g <- droplevels(no_level_c$g)

  dv <- shiny::reactiveVal(with_level_c)
  tp <- c(y = "continuous", x = "continuous", g = "nominal")

  shiny::testServer(mod_models_server,
    args = list(data = shiny::reactive(dv()), types = shiny::reactive(tp),
                outcomes = shiny::reactive("y"), add_report = function(x) NULL), {
      session$flushReact()
      session$setInputs(outcome = "y", method = "ols", predictors = c("x", "g"))
      session$flushReact()
      session$setInputs(run = 1); session$flushReact()
      # fitted on data WITH level C -> coefficient gC present
      terms1 <- result()$coefficients$Term
      stopifnot(!is.null(result()), any(grepl("^gC$", terms1)))

      # the data changes (level C removed) -> the stale fit is dropped
      dv(no_level_c); session$flushReact()
      stopifnot(is.null(result()))

      # re-running fits on the new data -> no gC term anywhere
      session$setInputs(run = 2); session$flushReact()
      terms2 <- result()$coefficients$Term
      stopifnot(!is.null(result()), !any(grepl("^gC$", terms2)))
    })
})

report_and_exit()
