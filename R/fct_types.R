# fct_types.R ---------------------------------------------------------------
# Variable measurement-type detection (Section 2 of the design doc) and the
# helpers that map types to valid analysis roles.

# The measurement types the app understands.
MEASUREMENT_TYPES <- c(
  "continuous", "count", "binary", "nominal",
  "ordinal", "datetime", "identifier", "text"
)

# Friendly labels for the UI.
TYPE_LABELS <- c(
  continuous = "Continuous",
  count      = "Count",
  binary     = "Binary",
  nominal    = "Nominal (categorical)",
  ordinal    = "Ordinal (ordered categories)",
  datetime   = "Date / time",
  identifier = "Identifier (ID)",
  text       = "Free text"
)

# TRUE when a column NAME looks like an identifier, e.g. "id", "patient_id",
# "patientID", "record id", "MRN". Matches "id" only as a whole token (so
# "width", "valid", "candidate" do NOT match) and handles camelCase.
.looks_like_id_name <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(FALSE)
  nm <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", name))   # split camelCase
  grepl("(^|[^a-z])id([^a-z]|$)", nm) ||
    grepl("(^|[^a-z])(identifier|uuid|guid|mrn)([^a-z]|$)", nm)
}

# TRUE when a column's VALUES are plausible identifiers: whole numbers or text
# (never decimals), more than two distinct values, and not mostly missing.
# Deliberately does NOT require uniqueness, so a subject ID that repeats across
# visits in a longitudinal dataset still qualifies.
.id_values_plausible <- function(x) {
  nonmiss <- x[!is.na(x)]
  if (length(nonmiss) == 0) return(FALSE)
  if (mean(is.na(x)) >= 0.5) return(FALSE)          # mostly missing -> not an ID
  if (length(unique(nonmiss)) <= 2) return(FALSE)   # 1-2 distinct -> flag/constant
  if (is.numeric(x) && !all(nonmiss == round(nonmiss))) return(FALSE)  # decimals
  TRUE
}

#' Detect the measurement type of a single vector.
#'
#' Heuristics follow the design doc; every result is user-overridable in the UI.
#' When the column `name` is supplied, an identifier-like name combined with
#' plausible identifier values is treated as an identifier (this catches
#' longitudinal subject IDs that repeat across rows and so slip past the
#' cardinality rule). Called without a name, behaviour is unchanged.
detect_type <- function(x, name = NULL) {
  x_nonmiss <- x[!is.na(x)]
  n  <- length(x)
  ux <- unique(x_nonmiss)
  k  <- length(ux)

  if (k == 0) return("text")                       # all missing
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return("datetime")
  if (is.ordered(x)) return("ordinal")
  if (is.logical(x)) return("binary")

  # Character/factor columns that are really ISO-8601 dates imported as text
  # (e.g. "2024-09-07"). Detect these as datetime so a date is never mistaken
  # for a high-cardinality categorical variable in role/grouping choices.
  if (is.character(x) || is.factor(x)) {
    chr <- as.character(x_nonmiss)
    iso <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?$"
    if (length(chr) > 0 && all(grepl(iso, chr)) &&
        !any(is.na(suppressWarnings(as.Date(chr))))) {
      return("datetime")
    }
    # Non-ISO but still date-shaped text ("apr-24", "9 Apr 2024", "04/2024",
    # "13/02/2024", ...). The analyst confirms/adjusts the exact format in the
    # "Confirm date formats" editor; here we only need to route it to datetime.
    if (.looks_like_flexible_date(x_nonmiss)) return("datetime")
  }

  # Name-based identifier detection (only when a column name is supplied). An
  # id-like name plus plausible id values wins over the count/continuous/nominal
  # rules below, so both a unique key and a repeating longitudinal subject ID
  # are recognised. Binary/decimal columns fall through (guarded above).
  if (.looks_like_id_name(name) && .id_values_plausible(x)) return("identifier")

  if (is.factor(x) || is.character(x)) {
    if (k == 2) return("binary")
    if (n > 0 && k / n > 0.9 && k > 30) return("identifier")
    if (k <= 20) return("nominal")
    return("nominal")
  }

  if (is.numeric(x)) {
    if (k == 2) return("binary")
    is_int <- all(x_nonmiss == round(x_nonmiss))
    if (is_int && n > 0 && k / n > 0.95) return("identifier")
    if (is_int && min(x_nonmiss) >= 0 && k <= 30) return("count")
    return("continuous")
  }

  "text"
}

#' Detect types for every column, returning a tidy data frame for the UI.
detect_all_types <- function(df) {
  data.frame(
    variable = names(df),
    type     = vapply(seq_along(df),
                      function(i) detect_type(df[[i]], names(df)[i]), character(1)),
    distinct = vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1)),
    missing_pct = vapply(df, function(x) round(100 * mean(is.na(x)), 1), numeric(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# --- Type groupings used by recommendation logic ---------------------------
is_numeric_type     <- function(t) t %in% c("continuous", "count")
is_categorical_type <- function(t) t %in% c("binary", "nominal", "ordinal")
is_analysable_type  <- function(t) !t %in% c("identifier", "text")

# Types for which a user-chosen reference (base) level is meaningful in a
# regression. Ordinal factors use ordered contrasts, so a base level does not
# apply to them; only unordered factors have a reference category.
is_relevellable_type <- function(t) t %in% c("binary", "nominal")

#' Candidate reference (base) levels for a column, in the order R would use them
#' when it turns the column into an unordered factor. The first element is R's
#' default reference; letting the user pick any other element is what gives them
#' control over the regression base level.
factor_levels_of <- function(x) {
  lv <- levels(as.factor(x[!is.na(x)]))
  lv[nzchar(lv)]
}

#' Coerce a column to match a (possibly user-corrected) measurement type.
#' Used after the confirm-types screen so downstream code gets consistent types.
coerce_to_type <- function(x, type, format = "auto") {
  switch(
    type,
    continuous = suppressWarnings(as.numeric(x)),
    count      = suppressWarnings(as.numeric(x)),
    binary     = as.factor(x),
    nominal    = as.factor(x),
    ordinal    = factor(x, ordered = TRUE),
    # Parse text dates into real Date/POSIXct using the chosen (or auto) format,
    # so every downstream analysis recognises them. Already-parsed columns are
    # returned unchanged by sabali_parse_dates().
    datetime   = sabali_parse_dates(x, format)$value,
    x                              # identifier / text unchanged
  )
}

# --- wide-data triage -------------------------------------------------------

#' Page size for the confirm-types editor.
#'
#' Wide clinical exports run to well over a thousand columns (a household
#' survey extract measured here had 1,743), and rendering a type and base-level dropdown
#' for every one of them at once builds thousands of widgets and megabytes of
#' HTML before the analyst can touch anything. The editor therefore shows one
#' page at a time.
TYPE_EDITOR_PAGE_SIZE <- 25L

#' Which columns cannot be analysed as they stand.
#'
#' All-missing and constant columns are reported by the importer already; they
#' are separated out here so the "needs a look" filter does not drown in them.
#' Nothing the analyst chooses on the types screen can make them usable, so they
#' are not a decision - just a fact worth being able to find.
types_unusable <- function(detected) {
  if (is.null(detected) || !nrow(detected)) return(logical(0))
  detected$missing_pct >= 100 | detected$distinct <= 1L
}

#' Which columns most likely need a type DECISION before analysis.
#'
#' Most columns in a wide export need no decision at all, so the editor can
#' offer to show only the ones that usually do. This is a PROMPT, not a verdict -
#' every row stays editable and the filter can be switched off. Flagged:
#'
#'  * small sets of whole numbers typed as counts. This is the commonest silent
#'    error on REDCap exports: a radio or dropdown field exported as 1/2/3 is
#'    indistinguishable from a count by shape alone, and left uncorrected it
#'    enters a model as a linear number rather than as categories. Cross-checking
#'    one 237-column electronic-capture export against its own data dictionary
#'    found 61 such fields;
#'  * columns detected as identifiers, which are excluded from analysis and so
#'    are worth confirming.
#'
#' A `from_dictionary` column, when present, suppresses the flag: a type read
#' from a REDCap data dictionary is stated, not inferred.
#'
#' Deliberately NOT flagged: all-missing and constant columns (see
#' `types_unusable()`). Including them made the filter useless on a real
#' 1,743-column extract, where 623 empty columns accounted for most of the 1,058
#' hits and none of them could be acted on.
#'
#' @param detected A `detect_all_types()` data frame.
#' @param max_codes Highest distinct count still treated as a possible code set.
#' @return A logical vector, one element per row of `detected`.
types_needing_attention <- function(detected, max_codes = 10L) {
  if (is.null(detected) || !nrow(detected)) return(logical(0))
  usable <- !types_unusable(detected)
  coded <- detected$type == "count" & detected$distinct <= max_codes
  flag <- usable & (coded | detected$type == "identifier")
  # A column whose type came from a REDCap data dictionary is not a guess, so
  # there is nothing for the analyst to decide about it.
  if (!is.null(detected$from_dictionary))
    flag <- flag & !as.logical(detected$from_dictionary)
  flag
}

#' Short reason a column was flagged, for the UI.
type_attention_reason <- function(type, distinct, missing_pct, max_codes = 10L) {
  if (missing_pct >= 100) return("all values missing")
  if (distinct <= 1L) return("only one observed value")
  if (identical(type, "count") && distinct <= max_codes)
    return(sprintf("%d whole numbers - a coded category?", distinct))
  if (identical(type, "identifier")) return("looks like an ID")
  ""
}

#' Rows of the confirm-types editor that pass the search box and the filter.
#'
#' Split out of the module so the filtering rule is testable on its own. The
#' search is a literal, case-insensitive substring match rather than a pattern:
#' a column name typed with a bracket or a dot should search, not raise a regex
#' error.
#'
#' @param detected A `detect_all_types()` data frame.
#' @param search   Substring to match against the variable name ("" = no filter).
#' @param filter   "all", "attention" (`types_needing_attention()`) or
#'                 "unusable" (`types_unusable()`).
#' @return Integer row indices into `detected`.
type_editor_rows <- function(detected, search = "", filter = "all") {
  if (is.null(detected) || !nrow(detected)) return(integer(0))
  keep <- rep(TRUE, nrow(detected))
  q <- trimws(search %||% "")
  if (nzchar(q))
    keep <- keep & grepl(tolower(q), tolower(detected$variable), fixed = TRUE)
  f <- filter %||% "all"
  if (identical(f, "attention")) keep <- keep & types_needing_attention(detected)
  if (identical(f, "unusable"))  keep <- keep & types_unusable(detected)
  which(keep)
}

#' Number of pages a filtered row set needs.
type_editor_n_pages <- function(rows, page_size = TYPE_EDITOR_PAGE_SIZE) {
  max(1L, as.integer(ceiling(length(rows) / page_size)))
}

#' Slice a filtered row set down to one page.
#'
#' The requested page is clamped into range, so a stale page number left over
#' from a wider filter shows the last page rather than nothing at all.
type_editor_page_rows <- function(rows, page, page_size = TYPE_EDITOR_PAGE_SIZE) {
  if (!length(rows)) return(integer(0))
  n_pages <- type_editor_n_pages(rows, page_size)
  p <- min(max(1L, as.integer(page)), n_pages)
  from <- (p - 1L) * page_size + 1L
  rows[seq.int(from, min(from + page_size - 1L, length(rows)))]
}
