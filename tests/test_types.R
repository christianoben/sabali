# tests/test_types.R --------------------------------------------------------
# Correctness of measurement-type detection (Gap 2). detect_type() is
# deterministic and core to the guided flow, so pin its behaviour on crafted
# vectors rather than only checking output shape.
#
# Run from the project root:  Rscript tests/test_types.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

eq <- function(label, got, want) ok(label, stopifnot(identical(got, want)))

cat("\n== detect_type() on crafted vectors ==\n")

# Binary: exactly two levels, however typed.
eq("2-level character -> binary",  detect_type(rep(c("a","b"), 5)),      "binary")
eq("2-level numeric   -> binary",  detect_type(rep(c(0, 1), 5)),         "binary")
eq("logical           -> binary",  detect_type(c(TRUE, FALSE, TRUE)),    "binary")

# Count: small non-negative integers, not near-unique.
eq("small non-neg ints -> count",  detect_type(rep(0:5, 20)),            "count")

# Continuous: non-integer numeric with many distinct values.
eq("non-integer numeric -> continuous", detect_type((1:100) / 7),        "continuous")

# Identifier: near-unique integer key, or near-unique high-cardinality text.
eq("near-unique integer -> identifier",   detect_type(1:100),            "identifier")
eq("near-unique text    -> identifier",   detect_type(paste0("id", 1:50)), "identifier")

# Ordinal: ordered factor.
eq("ordered factor -> ordinal",
   detect_type(factor(c("low","med","high"), levels = c("low","med","high"), ordered = TRUE)),
   "ordinal")

# Datetime: Date / POSIXct.
eq("Date    -> datetime", detect_type(as.Date("2024-01-01") + 0:10),     "datetime")
eq("POSIXct -> datetime", detect_type(as.POSIXct("2024-01-01") + 0:10),  "datetime")

# Nominal: a handful of unordered categories.
eq("few unordered categories -> nominal", detect_type(rep(c("x","y","z"), 10)), "nominal")

# Text: everything missing (no information to type on).
eq("all-NA -> text", detect_type(c(NA, NA, NA)), "text")

cat("\n== name-based identifier detection ==\n")

# Longitudinal subject ID: repeats across rows (not unique), so the cardinality
# rule alone would call it a count - the id-like NAME rescues it.
eq("repeating id column (longitudinal) -> identifier",
   detect_type(rep(1:5, each = 4), "patient_id"), "identifier")
eq("same values WITHOUT an id name -> count (name gates the rule)",
   detect_type(rep(1:5, each = 4), "score"), "count")
eq("same values with NO name -> unchanged (count)",
   detect_type(rep(1:5, each = 4)), "count")

# camelCase id names are recognised.
eq("camelCase 'patientID' -> identifier",
   detect_type(rep(1:6, 3), "patientID"), "identifier")

# Classic unique + complete key with an id name.
eq("unique complete key named 'record_id' -> identifier",
   detect_type(1:50, "record_id"), "identifier")

# Guards: an id name must NOT override obviously non-id values.
eq("decimals with an id name -> continuous (not identifier)",
   detect_type(c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6), "weird_id"), "continuous")
eq("two-level column with an id name -> binary (not identifier)",
   detect_type(rep(c(1, 2), 10), "flag_id"), "binary")

# 'id' must match only as a whole token, not any substring.
eq("'width' does not look like an id -> count",
   detect_type(rep(0:4, 6), "width"), "count")

report_and_exit()
