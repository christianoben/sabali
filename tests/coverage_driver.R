# Executed inside covr::file_coverage's instrumented environment.  Keeping the
# historical script tests means the coverage run exercises the same release
# gate as developers use locally. test_pipeline.R is intentionally excluded:
# it starts by re-sourcing every R/ file and uses cross-process <<- fixtures,
# which would replace covr's instrumented environment rather than measure it.
options(sabali_coverage = TRUE)
files <- sort(list.files("tests", pattern = "^test_.*[.]R$", full.names = TRUE))
files <- files[!basename(files) %in% c("test_coverage.R", "test_shinytest2_smoke.R",
                                       "test_pipeline.R")]
for (f in files) {
  # Each historical suite normally runs in its own R process. Use a fresh
  # child environment here so fixtures cannot leak between suites while all
  # children still inherit covr's instrumented source environment.
  suite_env <- new.env(parent = environment())
  invisible(capture.output(sys.source(f, envir = suite_env)))
}
