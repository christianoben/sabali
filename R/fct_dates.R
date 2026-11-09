# fct_dates.R ---------------------------------------------------------------
# Flexible date/time parsing. The importer often hands us dates as plain text in
# whatever shape the source used - "apr-24" (April 2024), "Apr-2024",
# "9 Apr 2024", "04/2024", "2024-04", "13/02/2024", ISO timestamps, and so on.
# These helpers turn such text into real Date/POSIXct values so every downstream
# analysis (time series, survival, Table 1) recognises them, while leaving the
# genuinely ambiguous day-first / month-first case for the analyst to confirm.

# Human-facing format choices offered in the "Confirm date formats" editor.
# The value is a key into DATE_ORDER_FAMILIES (or the literal "auto").
DATE_FORMAT_CHOICES <- c(
  "Automatic (detect)"                          = "auto",
  "Year-Month-Day   e.g. 2024-04-09 (ISO)"      = "ymd",
  "Day/Month/Year   e.g. 09/04/2024"            = "dmy",
  "Month/Day/Year   e.g. 04/09/2024 (US)"       = "mdy",
  "Day Month Year   e.g. 9 Apr 2024"            = "dbY",
  "Month Day, Year  e.g. Apr 9, 2024"           = "bdY",
  "Month-Year (month end)   e.g. Apr-2024 / apr-24"  = "bY",
  "Year-Month (month end)   e.g. 2024-04"            = "Ym",
  "Month/Year (month end)   e.g. 04/2024"            = "mY",
  "Date & time      e.g. 2024-04-09 14:30"      = "ymdHM"
)

# lubridate order strings tried for each named format. Several are listed per
# family so a single choice tolerates minor variation (2- vs 4-digit year,
# with/without a time component).
DATE_ORDER_FAMILIES <- list(
  ymd   = c("Ymd HMS", "Ymd HM", "Ymd", "ymd"),
  dmy   = c("dmY HMS", "dmY HM", "dmY", "dmy"),
  mdy   = c("mdY HMS", "mdY HM", "mdY", "mdy"),
  dbY   = c("dbY", "dBY", "dby", "dBy"),
  bdY   = c("bdY", "BdY", "bdy", "Bdy"),
  bY    = c("bY", "by", "Yb", "yb"),
  Ym    = c("Ym", "ym"),
  mY    = c("mY", "my"),
  ymdHM = c("Ymd HMS", "Ymd HM", "ymd HMS", "ymd HM")
)

# Month-only families carry no day, so their values are rolled to the LAST day
# of the month - "apr-24" means the whole of April 2024, i.e. 2024-04-30.
MONTH_ONLY_FAMILIES <- c("bY", "Ym", "mY")

# Last calendar day of the month each date falls in (vectorised, base R, so it
# is correct across leap years without a hard lubridate dependency).
.end_of_month <- function(d) {
  d <- as.Date(d)
  as.Date(cut(as.Date(cut(d, "month")) + 32L, "month")) - 1L
}

# Parse `chr` with a set of lubridate orders, returning a POSIXct vector. Only
# ever called on a shape-homogeneous group, so a single format applies cleanly
# and lubridate cannot greedily borrow digits across differing shapes.
.parse_with_orders <- function(chr, orders) {
  if (requireNamespace("lubridate", quietly = TRUE))
    suppressWarnings(lubridate::parse_date_time(chr, orders = orders,
                                                quiet = TRUE, tz = "UTC"))
  else
    as.POSIXct(suppressWarnings(as.Date(chr)), tz = "UTC")
}

# Classify each trimmed string into a date "family" by its SHAPE, so a
# homogeneous group is parsed with exactly one format. This is what prevents
# greedy mis-reads such as "Apr-2024" -> Apr-20-24. Numeric d/m/y triples are
# routed day-first (UK/metric default); that ambiguity is flagged separately.
# Returns a character vector of family keys, NA where nothing date-shaped fits.
.classify_date_shape <- function(chr) {
  n <- length(chr); fam <- rep(NA_character_, n)
  na <- is.na(chr)
  months_re <- "(?i)(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"
  has_month <- grepl(months_re, chr, perl = TRUE) & !na
  ngroups <- lengths(regmatches(chr, gregexpr("[0-9]+", chr)))
  dig_pos <- regexpr("[0-9]", chr)
  mon_pos <- regexpr(months_re, chr, perl = TRUE)
  set <- function(mask, key) {
    m <- mask & is.na(fam) & !na
    fam[m] <<- key
  }
  set(grepl("^\\d{4}[-/.]\\d{1,2}[-/.]\\d{1,2}[ T]\\d{1,2}:\\d{2}", chr), "ymdHM")
  set(grepl("^\\d{4}[-/.]\\d{1,2}[-/.]\\d{1,2}$", chr), "ymd")
  set(grepl("^\\d{8}$", chr), "ymd")                       # yyyymmdd
  set(has_month & ngroups >= 2 & dig_pos <  mon_pos, "dbY")  # 9 Apr 2024
  set(has_month & ngroups >= 2 & dig_pos >= mon_pos, "bdY")  # Apr 9, 2024
  set(has_month & ngroups == 1, "bY")                        # Apr-2024 / apr-24
  set(grepl("^\\d{1,2}[-/.]\\d{1,2}[-/.]\\d{2,4}$", chr), "dmy")  # day-first
  set(grepl("^\\d{4}[-/.]\\d{1,2}$", chr), "Ym")             # 2024-04
  set(grepl("^\\d{1,2}[-/.]\\d{4}$", chr), "mY")             # 04/2024
  fam
}

# Are the values a numeric d/m/y triple where day and month could be swapped?
# (i.e. some value has both of its first two parts <= 12). Used only to warn.
.dates_ambiguous <- function(chr) {
  chr <- chr[!is.na(chr)]
  if (!length(chr)) return(FALSE)
  m <- grepl("^\\s*\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{2,4}\\s*$", chr)
  if (!any(m)) return(FALSE)
  two <- regmatches(chr[m], regexpr("\\d{1,2}[/.-]\\d{1,2}", chr[m]))
  a <- suppressWarnings(as.integer(sub("[/.-].*$", "", two)))
  b <- suppressWarnings(as.integer(sub("^\\d{1,2}[/.-]", "", two)))
  any(!is.na(a) & !is.na(b) & a <= 12 & b <= 12)
}

#' Parse a vector of dates in a chosen or automatically detected format.
#'
#' @param x A vector (character/factor text, or an already-parsed Date/POSIXct).
#' @param format One of the keys in [DATE_FORMAT_CHOICES] (e.g. "dmy", "bY") or
#'   "auto" to classify each value by shape and parse it accordingly.
#' @return A list: `value` (Date, or POSIXct when a time component is present),
#'   `n_total`, `n_ok`, `ok_pct`, `has_time`, and `ambiguous`.
sabali_parse_dates <- function(x, format = "auto") {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    nn <- sum(!is.na(x))
    return(list(value = x, order = "kept", n_total = nn, n_ok = nn,
                ok_pct = 1, has_time = inherits(x, c("POSIXct", "POSIXt")),
                ambiguous = FALSE))
  }
  chr <- trimws(as.character(x))
  chr[chr == ""] <- NA
  n_total <- sum(!is.na(chr))
  auto <- is.null(format) || !nzchar(format) || identical(format, "auto")

  # Assign each value a family: by shape in auto mode, or the single chosen
  # family otherwise. Then parse each family's values together with its format.
  fam <- if (auto) .classify_date_shape(chr)
         else ifelse(is.na(chr), NA_character_, format)

  parsed <- as.POSIXct(rep(NA_real_, length(chr)), tz = "UTC")
  month_only <- logical(length(chr))
  for (k in unique(fam[!is.na(fam)])) {
    idx <- which(fam == k)
    parsed[idx] <- .parse_with_orders(chr[idx], DATE_ORDER_FAMILIES[[k]] %||% k)
    if (k %in% MONTH_ONLY_FAMILIES) month_only[idx] <- !is.na(parsed[idx])
  }

  n_ok <- sum(!is.na(parsed) & !is.na(chr))
  # Keep a time-of-day only when a value actually carries a non-midnight time;
  # otherwise a plain Date is friendlier for sliders and tables.
  has_time <- any(!is.na(parsed) &
                    format(parsed, "%H:%M:%S", tz = "UTC") != "00:00:00")

  if (has_time) {
    value <- as.POSIXct(parsed, tz = "UTC")
  } else {
    value <- as.Date(parsed, tz = "UTC")
    if (any(month_only)) value[month_only] <- .end_of_month(value[month_only])
  }

  list(value = value, order = if (auto) "auto" else format,
       n_total = n_total, n_ok = n_ok,
       ok_pct = if (n_total > 0) n_ok / n_total else 0,
       has_time = has_time, ambiguous = .dates_ambiguous(chr))
}

# Does a character/factor column look like flexible (non-ISO) dates? Used by
# detect_type. Conservative: the values must be date-shaped (contain a month
# name or a date separator) AND at least 90% must actually parse, so codes,
# IDs, ratios and Likert items are not mistaken for dates. Tested on the unique
# values only, to stay fast on large columns.
.looks_like_flexible_date <- function(x) {
  chr <- unique(trimws(as.character(x[!is.na(x)])))
  chr <- chr[nzchar(chr)]
  if (!length(chr)) return(FALSE)
  months <- "(?i)(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"
  date_ish <- grepl("[/.-]", chr) | grepl(months, chr, perl = TRUE)
  if (mean(date_ish) < 0.9) return(FALSE)
  # Avoid catching bare 4-digit years or integer codes with no separators.
  if (all(grepl("^\\d+$", chr))) return(FALSE)
  isTRUE(sabali_parse_dates(chr, "auto")$ok_pct >= 0.9)
}
