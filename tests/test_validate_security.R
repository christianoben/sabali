# Adversarial release checks for lifecycle cleanup, governance claims, and
# session isolation boundaries.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== explicit temporary-path cleanup ==\n")
ok("session_cleanup removes an explicitly registered file", {
  d <- tempfile("sabali-cleanup-"); dir.create(d)
  f <- file.path(d, "upload.csv"); writeLines("a,b\n1,2", f)
  session_cleanup(f)
  stopifnot(!file.exists(f), dir.exists(d))
  unlink(d, recursive = TRUE, force = TRUE)
})
ok("session_cleanup removes an explicitly registered directory", {
  d <- tempfile("sabali-cleanup-dir-"); dir.create(d)
  writeLines("temporary", file.path(d, "plot.png"))
  session_cleanup(d)
  stopifnot(!dir.exists(d))
})
ok("cleanup does not touch an unregistered sibling", {
  d <- tempfile("sabali-cleanup-scope-"); dir.create(d)
  keep <- file.path(d, "keep.txt"); drop <- file.path(d, "drop.txt")
  writeLines("keep", keep); writeLines("drop", drop)
  session_cleanup(drop)
  stopifnot(file.exists(keep), !file.exists(drop))
  unlink(d, recursive = TRUE, force = TRUE)
})

cat("\n== static governance wiring ==\n")
ok("upload lifecycle is explicitly registered on the data module", {
  txt <- paste(readLines(file.path(ROOT, "R/mod_data.R")), collapse = "\n")
  stopifnot(grepl("onSessionEnded", txt, fixed = TRUE),
            grepl("session_uploads", txt, fixed = TRUE),
            grepl("session_cleanup", txt, fixed = TRUE))
})
ok("report plot files are deleted after embedding", {
  txt <- paste(readLines(file.path(ROOT, "R/fct_report.R")), collapse = "\n")
  stopifnot(grepl("on.exit(unlink(tmp", txt, fixed = TRUE))
})
ok("audit records contain no raw values or secrets", {
  x <- audit_record("metadata_sent", c("age", "status"), 12L, 2L)
  stopifnot(identical(names(x), c("timestamp", "event", "fields", "n_rows", "n_columns")),
            identical(x$fields, "age,status"),
            !any(vapply(x, is.list, logical(1))))
})
ok("governance sign-off is explicit and not falsely marked complete", {
  txt <- paste(readLines(file.path(ROOT, "DATA_GOVERNANCE.md")), collapse = "\n")
  stopifnot(grepl("Status: **review required before production**", txt, fixed = TRUE),
            grepl("Raw uploaded rows are processed", txt, fixed = TRUE),
            grepl("Pending", txt, fixed = TRUE))
})
ok("per-session report state is wired through reactiveValues", {
  txt <- paste(readLines(file.path(ROOT, "app.R")), collapse = "\n")
  stopifnot(grepl("report_store <- reactiveValues(items = list()", txt, fixed = TRUE),
            grepl("add_report <- report_enricher(report_store)", txt, fixed = TRUE))
})
ok("dependency scan is wired to the pinned lockfile", {
  txt <- paste(readLines(file.path(ROOT, "tools/dependency_scan.R")), collapse = "\n")
  wf <- paste(readLines(file.path(ROOT, ".github/workflows/dependency-scan.yml")), collapse = "\n")
  stopifnot(grepl("renv.lock", txt, fixed = TRUE),
            grepl("api.osv.dev", txt, fixed = TRUE),
            grepl("dependency_scan.R", wf, fixed = TRUE))
})

report_and_exit()
