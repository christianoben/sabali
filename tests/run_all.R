# tests/run_all.R -----------------------------------------------------------
# Run every test_*.R file in this directory as a separate R process, so one
# suite crashing cannot mask another, and report an overall pass/fail.
#
# Run from the project root:  Rscript tests/run_all.R

args <- commandArgs(FALSE)
fa   <- sub("^--file=", "", args[grepl("^--file=", args)])
here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"

files <- sort(list.files(here, pattern = "^test_.*\\.[Rr]$", full.names = TRUE))
rscript <- file.path(R.home("bin"), "Rscript")

results <- data.frame(file = basename(files), status = NA_integer_)
for (i in seq_along(files)) {
  cat(sprintf("\n================ %s ================\n", basename(files[i])))
  results$status[i] <- system2(rscript, shQuote(files[i]))
}

cat("\n\n################ SUMMARY ################\n")
for (i in seq_len(nrow(results)))
  cat(sprintf("  %-24s %s\n", results$file[i],
              if (results$status[i] == 0) "PASS" else "FAIL"))

failed <- sum(results$status != 0)
cat(sprintf("\n%d of %d suites passed.\n", nrow(results) - failed, nrow(results)))
if (failed > 0) quit(status = 1)
