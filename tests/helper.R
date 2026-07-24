# tests/helper.R ------------------------------------------------------------
# Shared setup for the test scripts: locate the project root (so the tests run
# from anywhere), load the app's R/ code, and provide a tiny assertion helper.
#
# Run the tests from the project root, e.g.:
#   Rscript tests/test_pipeline.R
#   Rscript tests/test_heatmaps.R

# --- Locate the project root -----------------------------------------------
# Works whether invoked as `Rscript tests/foo.R` or sourced interactively.
.find_root <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  start <- if (length(file_arg) == 1) dirname(normalizePath(file_arg)) else getwd()
  d <- start
  repeat {
    if (file.exists(file.path(d, "app.R")) && dir.exists(file.path(d, "R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("Could not locate project root (app.R + R/).")
    d <- parent
  }
}

ROOT <- .find_root()
setwd(ROOT)

suppressPackageStartupMessages({ library(shiny); library(ggplot2) })

# Source every function/module file, exactly as app.R does.  covr::file_coverage
# has already loaded instrumented copies into its test environment; re-sourcing
# here would replace them with uninstrumented global copies.
if (!isTRUE(getOption("sabali_coverage"))) {
  for (.f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
    source(.f, local = FALSE)
  }
}

# --- Minimal test harness ---------------------------------------------------
.pass <- 0L; .fail <- 0L; .fails <- character(0)

ok <- function(label, expr) {
  res <- tryCatch({ force(expr); TRUE },
                  error = function(e) {
                    .fails[[length(.fails) + 1]] <<- paste0(label, ": ", conditionMessage(e))
                    FALSE
                  })
  if (isTRUE(res)) { .pass <<- .pass + 1L; cat(sprintf("  PASS  %s\n", label)) }
  else             { .fail <<- .fail + 1L; cat(sprintf("  FAIL  %s  -->  %s\n", label, tail(.fails, 1))) }
  invisible(res)
}

# Validation helpers shared by the maturity/adversarial suites.  The app keeps
# p-values and model coefficients at full precision, while several compact
# hypothesis-test display cells are intentionally rounded for readability.
# These helpers make that contract explicit instead of hiding tolerance choices
# in individual tests.
TOL_STAT   <- 1e-6
TOL_P      <- 1e-6
TOL_EFFECT <- 1e-4
TOL_CI     <- 1e-4
TOL_T2     <- 1e-3

field_value <- function(tab, field, value_col = 2L) {
  if (is.null(tab) || !is.data.frame(tab) || ncol(tab) < value_col)
    return(NA_real_)
  hit <- match(field, as.character(tab[[1]]))
  if (is.na(hit)) return(NA_real_)
  suppressWarnings(as.numeric(as.character(tab[[value_col]][hit])))
}

expect_close <- function(actual, expected, tol, label = "value") {
  stopifnot(length(actual) == 1L, length(expected) == 1L,
            is.finite(actual), is.finite(expected),
            abs(actual - expected) <= tol)
  invisible(TRUE)
}

friendly_error <- function(expr) {
  tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
}

html_text <- function(x) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = " "))
  if (is.list(x) && !is.null(x$html)) return(paste(x$html, collapse = " "))
  as.character(x)
}

report_and_exit <- function() {
  cat(sprintf("\n===== RESULT: %d passed, %d failed =====\n", .pass, .fail))
  if (.fail > 0) {
    cat("\nFailures:\n"); for (m in .fails) cat("  -", m, "\n")
    quit(status = 1)
  }
}
