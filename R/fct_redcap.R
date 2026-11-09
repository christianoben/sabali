# fct_redcap.R ---------------------------------------------------------------
# Read a REDCap data dictionary (the "Data Dictionary (codebook)" CSV export)
# and use it to type and label the data it describes.
#
# Why this exists. Type detection works from the SHAPE of a column, and on data
# from an electronic capture system shape is not enough: a radio or dropdown
# field exported as 1/2/3 is indistinguishable from a count, so it is detected as
# a count and enters a model as a linear number rather than as categories. This
# is not a quirk of one study - it is how REDCap exports categorical data, so it
# affects every REDCap project. On one 237-column export measured against its own
# dictionary, 61 radio/dropdown fields were typed as numeric that way, and 14 of
# them could not be separated from genuine counts by any shape-based rule: a
# 25-value antibiotic code list and a 19-value duration in days have identical
# form.
#
# The dictionary settles it. It ships beside the data, names each field's type,
# and carries the value labels, so a coded field becomes a properly labelled
# factor and a date-validated text field becomes a date with a known day/month
# order. Nothing here guesses: a column the dictionary does not describe is left
# entirely to the shape-based detector.
#
# See fct_odk.R for the same job on ODK / KoboToolbox XLSForms.

REDCAP_TYPE_MAP <- c(
  yesno       = "binary",
  truefalse   = "binary",
  checkbox    = "binary",   # each expanded ___k column is checked/unchecked
  radio       = "nominal",  # narrowed to binary below when only 2 choices
  dropdown    = "nominal",
  calc        = "continuous",
  slider      = "continuous",
  notes       = "text",
  descriptive = NA_character_,  # no data column at all
  file        = NA_character_
)

# REDCap text-validation -> app measurement type and, for dates, the day/month
# order to hand the date parser (keys of DATE_FORMAT_CHOICES).
REDCAP_VALIDATION_MAP <- list(
  integer        = list(type = "count",      format = NA_character_),
  number         = list(type = "continuous", format = NA_character_),
  number_1dp     = list(type = "continuous", format = NA_character_),
  number_2dp     = list(type = "continuous", format = NA_character_),
  number_3dp     = list(type = "continuous", format = NA_character_),
  number_4dp     = list(type = "continuous", format = NA_character_),
  date_ymd       = list(type = "datetime",   format = "ymd"),
  date_dmy       = list(type = "datetime",   format = "dmy"),
  date_mdy       = list(type = "datetime",   format = "mdy"),
  datetime_ymd   = list(type = "datetime",   format = "ymdHM"),
  datetime_dmy   = list(type = "datetime",   format = "dmy"),
  datetime_mdy   = list(type = "datetime",   format = "mdy"),
  datetime_seconds_ymd = list(type = "datetime", format = "ymdHM"),
  datetime_seconds_dmy = list(type = "datetime", format = "dmy"),
  datetime_seconds_mdy = list(type = "datetime", format = "mdy")
)

# REDCap's yesno fields carry no choice string; the export codes them 1/0.
REDCAP_YESNO_LABELS <- c("0" = "No", "1" = "Yes")
REDCAP_TRUEFALSE_LABELS <- c("0" = "False", "1" = "True")
# An expanded multi-select column (REDCap checkbox, ODK select_multiple) is a
# 0/1 flag for one choice.
MULTI_CHOICE_LABELS <- c("0" = "Unchecked", "1" = "Checked")
REDCAP_CHECKBOX_LABELS <- MULTI_CHOICE_LABELS

#' Parse a REDCap "Choices" string into a value -> label vector.
#'
#' The export format is `1, Yes|2, No`, with the label free to contain commas
#' ("1, Other, specify"), so only the FIRST comma separates code from label.
#' Returns a named character vector (names are the codes), or NULL when the
#' string carries no usable pairs - a `calc` field's formula, for instance,
#' lives in the same column.
redcap_parse_choices <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NULL)
  parts <- strsplit(as.character(x), "|", fixed = TRUE)[[1]]
  parts <- trimws(parts[nzchar(trimws(parts))])
  if (!length(parts)) return(NULL)
  codes <- character(0); labels <- character(0)
  for (p in parts) {
    at <- regexpr(",", p, fixed = TRUE)
    if (at < 1L) next                     # no comma: not a code,label pair
    code <- trimws(substr(p, 1L, at - 1L))
    lab  <- trimws(substr(p, at + 1L, nchar(p)))
    # A REDCap choice code is a short alphanumeric token. Requiring that stops a
    # calc field's formula - "round(([weight_kg]/1),2)" splits on its comma just
    # as happily - from being read as a code/label pair.
    if (!grepl("^[A-Za-z0-9_.-]+$", code)) next
    codes <- c(codes, code)
    labels <- c(labels, if (nzchar(lab)) lab else code)
  }
  # A malformed entry is skipped rather than failing the whole field: real
  # dictionaries contain them (three fields in one real study codebook list an
  # option with no comma at all), and discarding eight good labels over one bad one
  # loses more than it protects. A value whose code was skipped is kept as
  # itself by apply_form_metadata() and reported, never turned into NA.
  # Returning NULL only when NOTHING parsed is what still rejects a formula.
  if (!length(codes) || anyDuplicated(codes)) return(NULL)
  stats::setNames(labels, codes)
}

# The measurement type a `text` field implies, from its validation rule.
.redcap_text_type <- function(validation) {
  v <- tolower(trimws(validation %||% ""))
  if (!nzchar(v) || is.null(REDCAP_VALIDATION_MAP[[v]])) return(NULL)
  REDCAP_VALIDATION_MAP[[v]]
}

#' Read a REDCap data dictionary CSV into a tidy specification.
#'
#' Accepts the export as-is: a UTF-8 BOM on the first header cell and the
#' variable-length trailing columns REDCap has added over the years are both
#' tolerated, and only the columns actually used are required.
#'
#' @param path Path to the dictionary CSV.
#' @return A data frame with one row per field: `field`, `form`, `source_type`,
#'   `label`, `validation`, `type` (app measurement type, NA when the dictionary
#'   makes no claim), `date_format`, and `choices` (a list column of value-label
#'   vectors, NULL where none).
read_redcap_dictionary <- function(path) {
  if (!file.exists(path))
    stop("The data dictionary file could not be found.", call. = FALSE)
  d <- tryCatch(
    utils::read.csv(path, check.names = FALSE, colClasses = "character",
                    stringsAsFactors = FALSE, na.strings = character(0)),
    error = function(e) stop("Could not read the data dictionary: ",
                             conditionMessage(e), call. = FALSE))
  if (!nrow(d) || !ncol(d))
    stop("The data dictionary is empty.", call. = FALSE)
  # Strip a UTF-8 BOM and normalise the header names REDCap uses.
  names(d) <- sub("^﻿", "", names(d))
  pick <- function(...) {
    for (nm in c(...)) if (nm %in% names(d)) return(d[[nm]])
    NULL
  }
  field <- pick("Variable / Field Name", "Variable.Field.Name", "field_name")
  rtype <- pick("Field Type", "field_type")
  if (is.null(field) || is.null(rtype))
    stop("That does not look like a REDCap data dictionary: it needs a 'Variable / Field Name' and a 'Field Type' column.",
         call. = FALSE)
  form  <- pick("Form Name", "form_name") %||% rep("", length(field))
  label <- pick("Field Label", "field_label") %||% rep("", length(field))
  choice_txt <- pick("Choices, Calculations, OR Slider Labels",
                     "select_choices_or_calculations") %||% rep("", length(field))
  valid <- pick("Text Validation Type OR Show Slider Number",
                "text_validation_type_or_show_slider_number") %||%
    rep("", length(field))

  field <- trimws(field)
  rtype <- tolower(trimws(rtype))
  keep <- nzchar(field)
  field <- field[keep]; rtype <- rtype[keep]; form <- form[keep]
  label <- label[keep]; choice_txt <- choice_txt[keep]; valid <- valid[keep]
  if (!length(field))
    stop("The data dictionary lists no field names.", call. = FALSE)

  choices <- lapply(choice_txt, redcap_parse_choices)
  type <- rep(NA_character_, length(field))
  date_format <- rep(NA_character_, length(field))
  for (i in seq_along(field)) {
    if (identical(rtype[i], "text")) {
      tv <- .redcap_text_type(valid[i])
      if (!is.null(tv)) { type[i] <- tv$type; date_format[i] <- tv$format }
      next
    }
    mapped <- REDCAP_TYPE_MAP[[rtype[i]]]
    if (is.null(mapped)) next            # unknown field type: make no claim
    type[i] <- mapped
    # A two-choice radio/dropdown is a binary variable, not a general category.
    if (identical(mapped, "nominal") && length(choices[[i]]) == 2L)
      type[i] <- "binary"
    # A calc field's "choices" cell holds its formula, never value labels.
    # `choices[i] <- list(NULL)`, not `choices[[i]] <- NULL`: the latter DELETES
    # the element and shifts every later index, which silently misaligns the
    # whole table.
    if (rtype[i] %in% c("calc", "slider")) choices[i] <- list(NULL)
    if (identical(rtype[i], "yesno") && is.null(choices[[i]]))
      choices[[i]] <- REDCAP_YESNO_LABELS
    if (identical(rtype[i], "truefalse") && is.null(choices[[i]]))
      choices[[i]] <- REDCAP_TRUEFALSE_LABELS
  }
  out <- data.frame(field = field, form = form, source_type = rtype,
                    label = label, validation = tolower(trimws(valid)),
                    type = type, date_format = date_format,
                    # A "multi" field is one that expands to a 0/1 column per
                    # choice. REDCap's checkbox does; ODK's select_multiple does
                    # too when the export is expanded, so the apply path below is
                    # shared between the two rather than duplicated.
                    multi = rtype == "checkbox",
                    stringsAsFactors = FALSE, row.names = NULL)
  out$choices <- choices
  # How an expanded multi-field's columns are named: REDCap writes field___code.
  attr(out, "expand_sep") <- "___"
  attr(out, "source") <- "REDCap data dictionary"
  out
}

#' Match the columns of a data frame to the fields of a dictionary.
#'
#' Two shapes need handling beyond a plain name match:
#'  * a checkbox field `x` appears in the data as one 0/1 column per choice,
#'    `x___1`, `x___2`, ... (REDCap lowercases and strips punctuation from the
#'    code, so `x___other` occurs too);
#'  * REDCap adds a `<form>_complete` status column per form and the
#'    `redcap_event_name` / `redcap_repeat_*` structure columns, none of which
#'    are in the dictionary. Those stay with the shape-based detector.
#'
#' @return A data frame with one row per column of `df`: `column`, `field` (NA
#'   when unmatched), and `choice_code` (the choice code for an expanded
#'   checkbox column, else NA).
form_match_columns <- function(df, dict) {
  sep <- attr(dict, "expand_sep", exact = TRUE) %||% "___"
  cols <- names(df)
  field <- rep(NA_character_, length(cols))
  code  <- rep(NA_character_, length(cols))
  direct <- match(cols, dict$field)
  field[!is.na(direct)] <- dict$field[direct[!is.na(direct)]]
  # Expanded multi-select columns, longest field name first so `a_b___1` prefers
  # field `a_b` over a field called `a`.
  cb <- .isTRUE_vec(dict$multi)
  cb <- dict$field[cb]
  cb <- cb[order(-nchar(cb))]
  todo <- which(is.na(field))
  for (i in todo) {
    split <- .split_on_last(cols[i], sep)
    if (is.na(split$stem)) next
    hit <- cb[cb == split$stem]
    if (length(hit)) {
      field[i] <- hit[[1]]
      code[i] <- split$code
    }
  }
  # An ODK export names its columns by the full group path
  # (`household/water/source`) while the form defines the bare field `source`.
  # Match a still-unmatched column against the tail of its path, but only when
  # exactly one field claims it: two questions of the same name in different
  # groups are genuinely ambiguous, and guessing between them would silently
  # attach the wrong labels.
  if (!identical(sep, "___")) {
    todo <- which(is.na(field) & grepl(sep, cols, fixed = TRUE))
    for (i in todo) {
      tail_nm <- .split_on_last(cols[i], sep)$code
      if (is.na(tail_nm) || !nzchar(tail_nm)) next
      hit <- which(dict$field == tail_nm)
      if (length(hit) == 1L) field[i] <- dict$field[[hit]]
    }
  }
  data.frame(column = cols, field = field, choice_code = code,
             stringsAsFactors = FALSE, row.names = NULL)
}

# NA-safe logical coercion: `multi` is absent from a hand-built spec.
.isTRUE_vec <- function(x) {
  if (is.null(x)) return(logical(0))
  !is.na(x) & as.logical(x)
}

# Split a column name on the LAST occurrence of a literal separator. Matching is
# fixed, not regular-expression, because the separator is data: ODK's "/" is
# harmless but a separator carrying a metacharacter would otherwise silently
# become a pattern. Taking the LAST occurrence is what makes a nested ODK name
# work - `group/field/choice` splits into `group/field` and `choice`, not
# `group` and `field/choice`.
.split_on_last <- function(x, sep) {
  m <- gregexpr(sep, x, fixed = TRUE)[[1]]
  if (m[[1]] == -1L) return(list(stem = NA_character_, code = NA_character_))
  at <- max(m)
  list(stem = substr(x, 1L, at - 1L),
       code = substr(x, at + nchar(sep), nchar(x)))
}

#' Apply a REDCap dictionary to imported data.
#'
#' Coded fields become labelled factors, so every downstream tab - descriptives,
#' Table 1, charts, the planner metadata, the report - shows the category name
#' rather than the code. Date-validated text becomes a Date parsed with the
#' order the dictionary states, rather than a guessed one. Numeric-validated
#' text becomes numeric.
#'
#' A code present in the data but absent from the dictionary is kept as its own
#' level rather than turned into NA: silently discarding a value the export
#' actually contains would be a data loss, so it is reported instead.
#'
#' @return A list with `data` (the converted data frame), `types` (named
#'   character vector of measurement types the dictionary is authoritative for),
#'   `date_formats` (named character, for datetime columns), `labels` (named
#'   list of the value labels applied), `matched` / `unmatched` (column names),
#'   and `unknown_codes` (a data frame of values not in the dictionary).
apply_form_metadata <- function(df, dict) {
  stopifnot(is.data.frame(df))
  if (is.null(dict) || !nrow(dict))
    return(list(data = df, types = character(0), date_formats = character(0),
                labels = list(), matched = character(0),
                unmatched = names(df), unknown_codes = NULL))
  m <- form_match_columns(df, dict)
  types <- character(0); fmts <- character(0); labels <- list()
  unknown <- list()
  for (i in seq_len(nrow(m))) {
    nm <- m$column[i]
    if (is.na(m$field[i])) next
    spec <- dict[match(m$field[i], dict$field), ]
    # An expanded checkbox column is checked/unchecked, whatever the parent
    # field's own choices say.
    is_cb <- !is.na(m$choice_code[i])
    # A multi-select field matched DIRECTLY - not through an expanded
    # `field/choice` column - means the export was not split, so the column holds
    # a SET of answers ("fever cough"), not one category. Factorising it would
    # invent a level per observed combination, which is worse than leaving it
    # alone: no claim is made and .odk_multi_note() reports it instead.
    if (!is_cb && isTRUE(.isTRUE_vec(spec$multi)[1])) next
    lv <- if (is_cb) MULTI_CHOICE_LABELS else spec$choices[[1]]
    tp <- if (is_cb) "binary" else spec$type
    if (is.na(tp) && is.null(lv)) next            # metadata makes no claim

    x <- df[[nm]]
    # The field's human-readable label travels with the column, the same way a
    # Stata/SPSS label does, so the planner prompt and the report can say
    # "Research Assistant code" instead of `ra_code`.
    field_label <- trimws(spec$label %||% "")
    if (!is.null(lv)) {
      # Coded field: map codes to labels. Compare as trimmed character so "1"
      # and 1 both match, and numeric codes read from a CSV do not surprise.
      chr <- trimws(as.character(x))
      chr[!nzchar(chr)] <- NA_character_
      extra <- setdiff(unique(chr[!is.na(chr)]), names(lv))
      if (length(extra))
        unknown[[length(unknown) + 1L]] <- data.frame(
          column = nm, value = extra, stringsAsFactors = FALSE)
      # Keep unlisted codes as themselves rather than dropping the observation.
      all_codes <- c(names(lv), extra)
      all_labs  <- c(unname(lv), extra)
      df[[nm]] <- factor(chr, levels = all_codes, labels = all_labs)
      df[[nm]] <- droplevels(df[[nm]])
      labels[[nm]] <- lv
      # A field the dictionary calls categorical but which turns out to hold
      # only two observed categories is a binary variable in this extract.
      n_obs <- nlevels(df[[nm]])
      types[[nm]] <- if (identical(tp, "binary")) "binary"
                     else if (n_obs == 2L) "binary" else "nominal"
    } else if (identical(tp, "datetime")) {
      fmt <- if (is.na(spec$date_format)) "auto" else spec$date_format
      parsed <- tryCatch(sabali_parse_dates(x, fmt), error = function(e) NULL)
      if (!is.null(parsed)) df[[nm]] <- parsed$value
      types[[nm]] <- "datetime"
      fmts[[nm]] <- fmt
    } else if (tp %in% c("count", "continuous")) {
      num <- suppressWarnings(as.numeric(trimws(as.character(x))))
      df[[nm]] <- num
      obs <- num[is.finite(num)]
      # The dictionary says numeric; whether it is a count or continuous still
      # follows from the values (an integer validation permits negatives).
      types[[nm]] <- if (identical(tp, "count") && length(obs) &&
                         all(obs >= 0) && all(obs == round(obs)))
        "count" else if (identical(tp, "count")) "continuous" else "continuous"
    } else if (!is.na(tp)) {
      types[[nm]] <- tp
    }
    if (nzchar(field_label) && !is.null(df[[nm]]))
      attr(df[[nm]], "label") <- field_label
  }
  matched <- m$column[!is.na(m$field)]
  out <- list(data = df,
       types = unlist(types) %||% character(0),
       date_formats = unlist(fmts) %||% character(0),
       labels = labels,
       matched = matched,
       unmatched = setdiff(names(df), matched),
       unknown_codes = if (length(unknown)) do.call(rbind, unknown) else NULL)
  attr(out, "source") <- attr(dict, "source", exact = TRUE) %||% "Metadata"
  # Only meaningful for ODK, where an unsplit select_multiple is a real hazard.
  out$multi_note <- tryCatch(.odk_multi_note(out, dict, df),
                             error = function(e) NULL)
  out
}

#' One-line summary of what a dictionary did, for the UI.
form_apply_summary <- function(applied, df) {
  n_col <- ncol(df)
  n_match <- length(applied$matched)
  n_type <- length(applied$types)
  n_lab <- length(applied$labels)
  src <- attr(applied, "source") %||% "Metadata"
  msg <- sprintf(
    "%s matched %s of %s columns; %s type%s set from it and %s coded field%s relabelled.",
    src, format(n_match, big.mark = ","), format(n_col, big.mark = ","),
    format(n_type, big.mark = ","), if (n_type == 1L) "" else "s",
    format(n_lab, big.mark = ","), if (n_lab == 1L) "" else "s")
  if (!is.null(applied$multi_note))
    msg <- paste(msg, applied$multi_note)
  if (!is.null(applied$unknown_codes) && nrow(applied$unknown_codes))
    msg <- paste(msg, sprintf(
      "%d value%s in the data are not listed in the dictionary and were kept as-is (e.g. %s in '%s').",
      nrow(applied$unknown_codes),
      if (nrow(applied$unknown_codes) == 1L) "" else "s",
      applied$unknown_codes$value[1], applied$unknown_codes$column[1]))
  msg
}
