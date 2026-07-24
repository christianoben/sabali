# Coverage gate. The project keeps its historical script-based tests, so this
# check records the configured target and is executed in CI after covr is
# installed. A missing optional package is an explicit local skip.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

TARGET <- 80
cat(sprintf("\n== coverage target: %d%% ==\n", TARGET))
if (!requireNamespace("covr", quietly = TRUE)) {
  cat("  SKIP  covr is not installed; CI installs it for the measured gate\n")
} else {
  ok(sprintf("covr measured source coverage meets the %d%% target", TARGET), {
    files <- list.files(file.path(ROOT, "R"), pattern = "[.]R$", full.names = TRUE)
    stopifnot(length(files) > 0L, file.exists(file.path(ROOT, "tests/coverage_driver.R")))
    cov <- covr::file_coverage(source_files = files,
                               test_files = file.path(ROOT, "tests", "coverage_driver.R"))
    pct <- covr::percent_coverage(cov)
    cat(sprintf("  measured source coverage: %.1f%%\n", pct))
    stopifnot(is.finite(pct), pct >= TARGET)
  })
}
report_and_exit()
