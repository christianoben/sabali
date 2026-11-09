# tests/test_dates.R --------------------------------------------------------
# Flexible date parsing: text like "apr-24" (whole of April 2024 -> month end),
# "9 Apr 2024", "04/2024", ISO date/time and numeric d/m/y triples are turned
# into real Date/POSIXct values. Month-only values land on the last day of the
# month; genuinely ambiguous day/month order is flagged, not guessed silently.
# Also checks detection routing and the end-to-end coercion the app relies on.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

as_chr <- function(x) as.character(x)

cat("\n== Parser: month-only rolls to month end ==\n")

ok("apr-24 / Apr-2024 -> last day of April 2024", {
  stopifnot(as_chr(sabali_parse_dates("apr-24")$value)   == "2024-04-30")
  stopifnot(as_chr(sabali_parse_dates("Apr-2024")$value) == "2024-04-30")
})
ok("Feb-2024 respects the leap year (29th)", {
  stopifnot(as_chr(sabali_parse_dates("Feb-2024")$value) == "2024-02-29")
})
ok("2024-04 and 04/2024 also roll to month end", {
  stopifnot(as_chr(sabali_parse_dates("2024-04")$value) == "2024-04-30")
  stopifnot(as_chr(sabali_parse_dates("04/2024")$value) == "2024-04-30")
})

cat("\n== Parser: full dates keep their day ==\n")

ok("day + month-name + year keeps the exact day, in either order", {
  r <- sabali_parse_dates(c("9 Apr 2024", "10 May 2023", "Apr 9, 2024"))
  stopifnot(as_chr(r$value) == c("2024-04-09", "2023-05-10", "2024-04-09"))
})
ok("ISO date and ISO date-time", {
  stopifnot(as_chr(sabali_parse_dates("2024-09-07")$value) == "2024-09-07")
  r <- sabali_parse_dates("2024-09-07 14:30")
  stopifnot(isTRUE(r$has_time), inherits(r$value, "POSIXct"))
  stopifnot(format(r$value, "%Y-%m-%d %H:%M", tz = "UTC") == "2024-09-07 14:30")
})

cat("\n== Parser: ambiguity and explicit override ==\n")

ok("numeric d/m/y with both parts <= 12 is flagged ambiguous, read day-first", {
  r <- sabali_parse_dates("04/09/2024")               # auto
  stopifnot(isTRUE(r$ambiguous))
  stopifnot(as_chr(r$value) == "2024-09-04")          # 4 September
})
ok("unambiguous day-first triple is not flagged", {
  stopifnot(!isTRUE(sabali_parse_dates("13/02/2024")$ambiguous))
})
ok("explicit month-first (mdy) override flips the reading", {
  stopifnot(as_chr(sabali_parse_dates("04/09/2024", "mdy")$value) == "2024-04-09")
})
ok("explicit day-first (dmy) override", {
  stopifnot(as_chr(sabali_parse_dates("04/09/2024", "dmy")$value) == "2024-09-04")
})

cat("\n== Parser: robustness ==\n")

ok("non-dates parse to NA without error and report ok_pct", {
  r <- sabali_parse_dates(c("hello", "not a date", ""))
  stopifnot(all(is.na(r$value)), r$n_ok == 0)
})
ok("mixed shapes each parse by their own shape", {
  r <- sabali_parse_dates(c("apr-24", "2024-09-07", "13/02/2024", "junk"))
  stopifnot(as_chr(r$value)[1:3] == c("2024-04-30", "2024-09-07", "2024-02-13"))
  stopifnot(is.na(r$value[4]))
  stopifnot(r$n_ok == 3, r$n_total == 4)
})
ok("already-parsed Date is returned unchanged", {
  d <- as.Date(c("2024-01-01", "2024-06-15"))
  stopifnot(identical(sabali_parse_dates(d)$value, d))
})

cat("\n== Detection routing ==\n")

ok("flexible date columns are detected as datetime", {
  stopifnot(detect_type(c("apr-24", "may-24", "dec-23")) == "datetime")
  stopifnot(detect_type(c("9 Apr 2024", "10 May 2023")) == "datetime")
  stopifnot(detect_type(c("04/2024", "12/2023")) == "datetime")
})
ok("codes, bare years and Likert items are NOT mistaken for dates", {
  stopifnot(detect_type(c("AB-12", "CD-34", "EF-56")) != "datetime")
  stopifnot(detect_type(c("2019", "2020", "2021")) != "datetime")
  stopifnot(detect_type(c("1-5", "2-5", "3-5")) != "datetime")
})

cat("\n== End-to-end coercion ==\n")

ok("coerce_to_type('datetime') yields a real Date at month end (auto)", {
  out <- coerce_to_type(c("apr-24", "may-24"), "datetime")
  stopifnot(inherits(out, "Date"))
  stopifnot(as_chr(out) == c("2024-04-30", "2024-05-31"))
})
ok("coerce_to_type('datetime') honours an explicit format", {
  out <- coerce_to_type(c("04/09/2024"), "datetime", format = "mdy")
  stopifnot(as_chr(out) == "2024-04-09")
})

cat("\n== Module: dates flow through as Date, format is honoured ==\n")

make_fixture <- function() {
  df <- data.frame(
    id    = sprintf("P%02d", 1:6),
    month = c("apr-24", "may-24", "jun-24", "jul-24", "aug-24", "sep-24"),
    value = c(10, 12, 9, 15, 11, 13),
    stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  list(path = path, name = basename(path))
}

ok("auto-detected month column becomes month-end Dates downstream", {
  fx <- make_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    stopifnot(identical(types()[["month"]], "datetime"))
    got <- data_coerced()$month
    stopifnot(inherits(got, "Date"))
    stopifnot(as_chr(got)[1:3] == c("2024-04-30", "2024-05-31", "2024-06-30"))
  })
})

ok("an explicit format override re-parses the column", {
  fx <- make_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    # 'month' is the 2nd column. Row inputs are keyed by DETECTION GENERATION
    # as well as row index (see mod_data.R): one file has been loaded, so the
    # generation is 1 and the id is datefmt_1_2. The generation is what stops a
    # choice made against an earlier detection - a previous file, or the types
    # before a REDCap dictionary was applied - being read back as if it applied
    # to the current one.
    # Forcing ymd fails to parse "apr-24", so the column becomes all-NA, which
    # is what proves the override was applied.
    session$setInputs(`datefmt_1_2` = "ymd")
    stopifnot(all(is.na(data_coerced()$month)))
    session$setInputs(`datefmt_1_2` = "bY")
    stopifnot(as_chr(data_coerced()$month)[1] == "2024-04-30")
  })
})

report_and_exit()
