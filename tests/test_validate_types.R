# Type detection, coercion, labelled metadata and import-format adversaries.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== type detector adversaries ==\n")
engineered <- data.frame(
  bin = factor(c("no", "yes", "no", "yes", "no", "yes")),
  count = c(0, 1, 2, 0, 1, 2),
  continuous = c(.1, 1.2, 2.4, 3.8, 4.1, 5.3),
  ordinal = ordered(c("low", "mid", "high", "mid", "low", "high"),
                    levels = c("low", "mid", "high")),
  iso_date = c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04",
               "2024-01-05", "2024-01-06"),
  patient_id = c(10L, 10L, 11L, 11L, 12L, 12L),
  all_missing = rep(NA, 6), stringsAsFactors = FALSE)
det <- detect_all_types(engineered)
got <- setNames(det$type, det$variable)
ok("engineered columns receive exact measurement labels", {
  stopifnot(identical(unname(got[c("bin", "count", "continuous", "ordinal", "iso_date",
                                  "patient_id", "all_missing")]),
                      c("binary", "count", "continuous", "ordinal", "datetime",
                        "identifier", "text")))
})
ok("type coercion preserves the requested analysis representation", {
  stopifnot(is.factor(coerce_to_type(c("a", "b"), "binary")),
            is.numeric(coerce_to_type(c("1.2", "3.4"), "continuous")),
            is.ordered(coerce_to_type(c("low", "high"), "ordinal")),
            identical(coerce_to_type(c("a", "b"), "text"), c("a", "b")))
})
ok("factor level choice is deterministic and relevel-compatible", {
  x <- factor(c("z", "a", "z", "b"))
  stopifnot(identical(factor_levels_of(x), c("a", "b", "z")))
  y <- relevel(x, ref = "z")
  stopifnot(identical(levels(y)[1], "z"))
})

cat("\n== every supported import family with real fixture files ==\n")
toy <- data.frame(id = 1:6, value = c(2, 4, 3, 6, 5, 7),
                  status = factor(c("no", "yes", "no", "yes", "no", "yes")))
td <- tempfile("sabali-import-")
dir.create(td)
csv <- file.path(td, "toy.csv")
utils::write.csv(toy, csv, row.names = FALSE)
rds <- file.path(td, "toy.rds")
saveRDS(toy, rds)
rda <- file.path(td, "toy.rda")
toy_object <- toy
save(toy_object, file = rda)
xlsx <- file.path(td, "toy.xlsx")
openxlsx::write.xlsx(toy, xlsx, overwrite = TRUE)
files <- list(csv = c(csv, "toy.csv"), rds = c(rds, "toy.rds"),
              rda = c(rda, "toy.rda"), xlsx = c(xlsx, "toy.xlsx"))
for (nm in names(files)) {
  f <- files[[nm]][1]
  orig <- files[[nm]][2]
  got_df <- read_any_format(f, orig)
  ok(sprintf("read_any_format reads %s fixture", toupper(nm)), {
    stopifnot(is.data.frame(got_df), nrow(got_df) == nrow(toy), ncol(got_df) == ncol(toy))
  })
}

if (requireNamespace("haven", quietly = TRUE)) {
  labelled <- data.frame(id = 1:6, response = haven::labelled(c(0, 1, 0, 1, 0, 1),
                           labels = c(No = 0, Yes = 1)))
  attr(labelled$response, "label") <- "Currently suppressed"
  sav <- file.path(td, "toy.sav")
  haven::write_sav(labelled, sav)
  dta <- file.path(td, "toy.dta")
  haven::write_dta(labelled, dta)
  xpt <- file.path(td, "toy.xpt")
  haven::write_xpt(labelled, xpt)
  labelled_files <- list(SPSS = c(sav, "toy.sav"), Stata = c(dta, "toy.dta"),
                         SAS_XPORT = c(xpt, "toy.xpt"))
  for (fmt in names(labelled_files)) {
    spec <- labelled_files[[fmt]]
    imported <- read_any_format(spec[1], spec[2])
    ok(sprintf("read_any_format reads %s labelled fixture", fmt), {
      stopifnot(is.data.frame(imported), nrow(imported) == 6L,
                "response" %in% names(imported))
      meta <- build_planner_metadata(imported, c(id = "identifier", response = "binary"),
                                     outcomes = "response", raw = imported)
      v <- Filter(function(x) x$name == "response", meta$variables)[[1]]
      stopifnot(identical(v$label, "Currently suppressed"))
      if (!identical(fmt, "SAS_XPORT"))
        stopifnot(any(grepl("1 = Yes", unlist(v$value_labels))))
    })
  }
} else cat("  SKIP  haven import fixtures\n")

cat("\n== malformed/hostile file messages ==\n")
bad_file <- function(df, ext = ".csv") {
  f <- tempfile(fileext = ext)
  if (identical(ext, ".csv")) utils::write.csv(df, f, row.names = FALSE)
  else saveRDS(df, f)
  f
}
cases <- list(
  empty = structure(list(), class = "data.frame", row.names = integer()),
  single_column = data.frame(only = 1:4),
  all_missing = data.frame(a = 1:4, missing = c(NA, NA, NA, NA)),
  constant = data.frame(a = 1:4, constant = 1L))
for (nm in names(cases)) {
  f <- bad_file(cases[[nm]])
  e <- friendly_error(read_any_format(f, basename(f)))
  ok(sprintf("%s file fails with an analyst-friendly message", nm), {
    stopifnot(is.character(e), !is.na(e), nchar(e) > 10,
              !grepl("subscript out of bounds|object .* not found", e, ignore.case = TRUE))
  })
}
ok("missing source file fails without exposing a raw R path error", {
  e <- friendly_error(read_any_format(file.path(td, "does-not-exist.csv"), "does-not-exist.csv"))
  stopifnot(grepl("could not be found", e, ignore.case = TRUE))
})
ok("non-UTF-8 text is rejected or wrapped as a readable import error", {
  f <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(charToRaw("a,b\n"), as.raw(c(0xff, 0xfe)), charToRaw(",x\n"))), f)
  e <- friendly_error(read_any_format(f, basename(f)))
  stopifnot(is.character(e), !is.na(e),
            grepl("UTF-8|read the data file|parse|constant|observed value", e, ignore.case = TRUE))
})
ok("files above the 200 MB cap fail before parsing", {
  huge <- tempfile(fileext = ".csv")
  con <- file(huge, "wb")
  seek(con, where = 200 * 1024^2 + 1, origin = "start")
  writeBin(as.raw(0), con)
  close(con)
  e <- friendly_error(read_any_format(huge, basename(huge)))
  unlink(huge, force = TRUE)
  stopifnot(grepl("200 MB|upload limit", e, ignore.case = TRUE))
})

unlink(td, recursive = TRUE, force = TRUE)
report_and_exit()
