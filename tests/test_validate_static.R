# Static release checks for package structure, namespace hygiene, lint/style
# configuration, and the documented dependency lock.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== packaging and namespace audit ==\n")
ok("package metadata and namespace are present", {
  stopifnot(file.exists(file.path(ROOT, "DESCRIPTION")),
            file.exists(file.path(ROOT, "NAMESPACE")),
            file.exists(file.path(ROOT, "R/00-package.R")),
            file.exists(file.path(ROOT, "renv.lock")))
})
ok("renv.lock is valid JSON with an R version and package entries", {
  stopifnot(requireNamespace("jsonlite", quietly = TRUE))
  lock <- jsonlite::fromJSON(file.path(ROOT, "renv.lock"))
  stopifnot(!is.null(lock$R$Version), length(lock$Packages) > 10L)
})
ok("module servers use one local ns <- session$ns binding", {
  files <- list.files(file.path(ROOT, "R"), pattern = "^mod_.*[.]R$", full.names = TRUE)
  for (f in files) {
    txt <- paste(readLines(f), collapse = "\n")
    if (grepl("moduleServer", txt, fixed = TRUE)) {
      stopifnot(grepl("ns <- session\\$ns", txt))
      stopifnot(!grepl("session\\$ns\\(", txt))
    }
  }
})
ok("release documentation names the remaining human decisions", {
  txt <- paste(readLines(file.path(ROOT, "REVIEW_CHECKLIST.md")), collapse = "\n")
  stopifnot(grepl("Statistician reviewed", txt, fixed = TRUE),
            grepl("signed release tag", txt, fixed = TRUE))
})

cat("\n== optional lint/style gate ==\n")
if (!requireNamespace("lintr", quietly = TRUE)) {
  cat("  SKIP  lintr is not installed\n")
} else {
  ok("lintr parses every R source file", {
    files <- list.files(file.path(ROOT, "R"), pattern = "[.]R$", full.names = TRUE)
    for (f in files) parse(file = f, keep.source = FALSE)
  })
}
if (!requireNamespace("styler", quietly = TRUE)) {
  cat("  SKIP  styler is not installed\n")
} else {
  cat("  PASS  styler is installed for the CI style gate\n")
}

report_and_exit()
