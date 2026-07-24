# tests/test_import.R -------------------------------------------------------
# Import breadth beyond CSV (Gap 1). The import layer's premise is "any
# format", so exercise the .rds / .rda paths, the .read_rda selection and
# error behaviour, and the haven-label stripping.
#
# Run from the project root:  Rscript tests/test_import.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

toy <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)

cat("\n== .rds round-trip ==\n")
ok("read_any_format(.rds) round-trips a data frame", {
  f <- tempfile(fileext = ".rds"); saveRDS(toy, f)
  got <- read_any_format(f, basename(f))
  stopifnot(is.data.frame(got), identical(got, toy))
})

cat("\n== .rda round-trip ==\n")
ok("read_any_format(.rda) round-trips a data frame", {
  f <- tempfile(fileext = ".rda"); my_df <- toy; save(my_df, file = f)
  got <- read_any_format(f, basename(f))
  stopifnot(is.data.frame(got), identical(got, toy))
})

cat("\n== .read_rda selection & error behaviour ==\n")
ok(".read_rda picks the first data frame among mixed objects", {
  f <- tempfile(fileext = ".rda")
  a_df <- toy; z_vec <- 1:10          # alphabetically a_df sorts first
  save(a_df, z_vec, file = f)
  got <- .read_rda(f)
  stopifnot(is.data.frame(got), identical(got, toy))
})
ok(".read_rda errors cleanly on an .rda with no data frame", {
  f <- tempfile(fileext = ".rda"); only_vec <- 1:10; save(only_vec, file = f)
  err <- tryCatch({ .read_rda(f); NA }, error = function(e) conditionMessage(e))
  stopifnot(is.character(err), grepl("no data frame", err, ignore.case = TRUE))
})

cat("\n== haven label stripping ==\n")
if (requireNamespace("haven", quietly = TRUE)) {
  ok(".strip_haven_labels turns a fully-labelled column into a factor", {
    lab <- haven::labelled(c(1, 2, 1, 2), labels = c(no = 1, yes = 2))
    d <- data.frame(id = 1:4); d$flag <- lab
    out <- .strip_haven_labels(d)
    stopifnot(is.factor(out$flag), !inherits(out$flag, "haven_labelled"),
              setequal(levels(out$flag), c("no", "yes")))
  })
  ok(".strip_haven_labels turns a partially-labelled column into numeric", {
    lab <- haven::labelled(c(1, 2, 3, 4), labels = c(no = 1, yes = 2))  # 3,4 unlabelled
    d <- data.frame(id = 1:4); d$score <- lab
    out <- .strip_haven_labels(d)
    stopifnot(is.numeric(out$score), !inherits(out$score, "haven_labelled"),
              identical(out$score, c(1, 2, 3, 4)))
  })
} else {
  cat("  SKIP  haven not installed\n")
}

report_and_exit()
