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

cat("\n== blank / whitespace cells become NA (REDCap-style exports) ==\n")
ok("blank strings in a 2-level field are treated as NA, so it detects as binary", {
  d <- data.frame(
    id = 1:6,
    colonization = c("Colonized", "Not colonized", "", "  ", "Colonized", "Not colonized"),
    stringsAsFactors = FALSE)
  f <- tempfile(fileext = ".csv"); utils::write.csv(d, f, row.names = FALSE, na = "")
  got <- read_any_format(f, basename(f))
  stopifnot(
    setequal(unique(got$colonization[!is.na(got$colonization)]),
             c("Colonized", "Not colonized")),          # no "" level
    sum(is.na(got$colonization)) == 2,                   # the two blanks -> NA
    identical(detect_type(got$colonization, "colonization"), "binary"),
    "binary" %in% outcome_type_options(got$colonization))
})
ok(".blanks_to_na drops an empty factor level", {
  d <- data.frame(g = factor(c("a", "b", "", "a")), stringsAsFactors = FALSE)
  out <- .blanks_to_na(d)
  stopifnot(!("" %in% levels(out$g)), sum(is.na(out$g)) == 1)
})

cat("\n== data-quality issues are advisory, not fatal ==\n")
ok("constant and all-missing columns load with a warning, not an error", {
  d <- data.frame(id = 1:5, konst = rep("x", 5), empty = NA,
                  ok = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  f <- tempfile(fileext = ".csv"); utils::write.csv(d, f, row.names = FALSE, na = "")
  got <- read_any_format(f, basename(f))
  warns <- attr(got, "import_warnings", exact = TRUE)
  stopifnot(is.data.frame(got), ncol(got) == 4L, nrow(got) == 5L,
            any(grepl("constant", warns)), any(grepl("missing values", warns)))
})
ok(".import_warnings is empty for a clean table; .assert_loadable passes it", {
  d <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  stopifnot(length(.import_warnings(d)) == 0L, isTRUE(.assert_loadable(d)))
})
ok(".assert_loadable still rejects a table with no columns", {
  empty <- structure(list(), class = "data.frame", row.names = integer())
  e <- tryCatch({ .assert_loadable(empty); NA }, error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("no data", e, ignore.case = TRUE))
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
