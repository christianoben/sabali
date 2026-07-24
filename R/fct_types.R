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
coerce_to_type <- function(x, type) {
  switch(
    type,
    continuous = suppressWarnings(as.numeric(x)),
    count      = suppressWarnings(as.numeric(x)),
    binary     = as.factor(x),
    nominal    = as.factor(x),
    ordinal    = factor(x, ordered = TRUE),
    datetime   = x,               # leave as-is; import already parsed
    x                              # identifier / text unchanged
  )
}
