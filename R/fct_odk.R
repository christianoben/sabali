# fct_odk.R ------------------------------------------------------------------
# Read an ODK / XLSForm form definition and use it to type and label the data
# it describes.
#
# Same problem as the REDCap dictionary in fct_redcap.R: an ODK export codes its
# select_one answers as short names or numbers, which no shape-based rule can
# tell from a count, and its dates arrive as text. The form definition states
# both, so it is read rather than guessed at.
#
# The format is XLSForm - the spreadsheet ODK Central, ODK Collect and
# KoboToolbox all build forms from. Two sheets matter:
#
#   survey   one row per question: `type`, `name`, `label`, and the group and
#            repeat structure expressed as begin/end rows.
#   choices  one row per option: `list_name`, `name`, `label`.
#
# The output is the SAME spec contract as read_redcap_dictionary(), so
# apply_form_metadata() handles both without knowing which it was given.
#
# Two export conventions are handled, because both are in the wild:
#   * a select_multiple exported EXPANDED, one 0/1 column per choice, named
#     `field/choice` (ODK Briefcase, and ODK Central's "split select multiples");
#   * a select_multiple exported as ONE column of space-separated choice names,
#     which is ODK Central's default. That is not a categorical variable and is
#     not treated as one - see the note in .odk_multi_note().
#
# Column names in an ODK export are usually the full group path
# (`household/water/source`), so a bare field name is matched against the tail of
# each column as well as the whole of it.

# XLSForm question type -> app measurement type. A type this table does not name
# makes no claim, exactly as a REDCap `text` field does.
ODK_TYPE_MAP <- c(
  integer      = "count",
  decimal      = "continuous",
  range        = "continuous",
  date         = "datetime",
  datetime     = "datetime",
  start        = "datetime",
  end          = "datetime",
  today        = "datetime",
  acknowledge  = "binary",
  # Device / enumerator metadata: identifiers, never analysis variables.
  deviceid     = "identifier",
  subscriberid = "identifier",
  simserial    = "identifier",
  phonenumber  = "identifier",
  username     = "identifier",
  # Deliberately no claim: `text` and `calculate` can hold anything, `time` has
  # no measurement meaning here, and geo/media types are not analysable columns.
  text         = NA_character_,
  time         = NA_character_,
  calculate    = NA_character_,
  note         = NA_character_,
  geopoint     = NA_character_,
  geotrace     = NA_character_,
  geoshape     = NA_character_,
  image        = NA_character_,
  audio        = NA_character_,
  video        = NA_character_,
  file         = NA_character_,
  barcode      = NA_character_
)

# XLSForm date types carry their own format, unlike REDCap's validation strings.
# ODK writes ISO-8601, so the parser is told that rather than left to guess.
ODK_DATE_FORMATS <- c(date = "ymd", datetime = "ymdHM", start = "ymdHM",
                      end = "ymdHM", today = "ymd")

# Guarded lookup in a NAMED ATOMIC vector. `[[` throws "subscript out of
# bounds" for a missing name rather than returning NULL - the same trap that bit
# build_planner_metadata() - so every table lookup here goes through this.
.lookup <- function(tbl, key, default = NA_character_) {
  if (is.null(key) || length(key) != 1L || is.na(key) || !nzchar(key))
    return(default)
  if (!key %in% names(tbl)) return(default)
  tbl[[key]]
}

# Rows that describe structure rather than a question.
ODK_STRUCTURE_TYPES <- c("begin group", "begin_group", "end group", "end_group",
                         "begin repeat", "begin_repeat", "end repeat",
                         "end_repeat")

#' Split an XLSForm `type` cell into its keyword and its choice-list name.
#'
#' `select_one yes_no` and `select_multiple symptoms` carry the list name in the
#' same cell; `select_one_from_file households.csv` names an external file whose
#' choices are not in the workbook, so it is recognised but claims nothing.
odk_parse_type <- function(x) {
  raw <- trimws(tolower(as.character(x %||% "")))
  if (!nzchar(raw)) return(list(kind = "", list_name = NA_character_,
                                multi = FALSE, external = FALSE))
  parts <- strsplit(raw, "[[:space:]]+")[[1]]
  kw <- parts[[1]]
  # XLSForm accepts both the space and underscore spellings of the group and
  # repeat markers.
  kw <- sub("^(begin|end)_", "\\1 ", kw)
  if (kw %in% c("begin", "end") && length(parts) > 1L)
    return(list(kind = paste(kw, parts[[2]]), list_name = NA_character_,
                multi = FALSE, external = FALSE))
  external <- grepl("_from_file$", kw)
  multi <- kw %in% c("select_multiple", "select_multiple_from_file")
  one <- kw %in% c("select_one", "select_one_from_file", "rank")
  list(kind = if (multi) "select_multiple" else if (one) "select_one" else kw,
       list_name = if ((multi || one) && length(parts) > 1L) parts[[2]] else NA_character_,
       multi = multi, external = external)
}

# The label column an XLSForm uses varies with translation: `label`,
# `label::English`, `label:English`. Take the plain one when present, else the
# first translation, so a translated form is not silently label-less.
.odk_label_column <- function(nms) {
  low <- tolower(nms)
  exact <- which(low == "label")
  if (length(exact)) return(nms[[exact[[1]]]])
  cand <- which(startsWith(low, "label:"))
  if (length(cand)) return(nms[[cand[[1]]]])
  NULL
}

.odk_col <- function(d, ...) {
  low <- tolower(names(d))
  for (nm in c(...)) {
    hit <- which(low == tolower(nm))
    if (length(hit)) return(d[[hit[[1]]]])
  }
  NULL
}

#' Read an ODK / XLSForm form definition.
#'
#' @param path Path to the XLSForm workbook (.xlsx / .xls).
#' @return A spec in the same shape `read_redcap_dictionary()` returns: one row
#'   per question with `field`, `form` (the group path), `source_type`, `label`,
#'   `type`, `date_format`, `multi` and a `choices` list column. Carries an
#'   `expand_sep` attribute of "/" so `apply_form_metadata()` finds the expanded
#'   select_multiple columns.
read_odk_form <- function(path) {
  if (!file.exists(path))
    stop("The ODK form definition could not be found.", call. = FALSE)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Reading an XLSForm needs the 'readxl' package. Run install_dependencies.R.",
         call. = FALSE)
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e)
    stop("That file could not be opened as a spreadsheet: ",
         conditionMessage(e), call. = FALSE))
  low <- tolower(trimws(sheets))
  if (!"survey" %in% low)
    stop("That does not look like an ODK XLSForm: it needs a 'survey' sheet listing the questions.",
         call. = FALSE)
  rd <- function(name) {
    hit <- sheets[[which(low == name)[[1]]]]
    as.data.frame(readxl::read_excel(path, sheet = hit, col_types = "text"),
                  stringsAsFactors = FALSE)
  }
  survey <- rd("survey")
  choices <- if ("choices" %in% low) rd("choices") else NULL

  type_col <- .odk_col(survey, "type")
  name_col <- .odk_col(survey, "name")
  if (is.null(type_col) || is.null(name_col))
    stop("The XLSForm's 'survey' sheet needs both a 'type' and a 'name' column.",
         call. = FALSE)
  lab_nm <- .odk_label_column(names(survey))
  label_col <- if (is.null(lab_nm)) rep(NA_character_, nrow(survey)) else survey[[lab_nm]]

  # --- choice lists ---------------------------------------------------------
  lists <- list()
  if (!is.null(choices)) {
    ln <- .odk_col(choices, "list_name", "list name")
    cn <- .odk_col(choices, "name", "value")
    clab_nm <- .odk_label_column(names(choices))
    cl <- if (is.null(clab_nm)) NULL else choices[[clab_nm]]
    if (!is.null(ln) && !is.null(cn)) {
      ln <- trimws(as.character(ln)); cn <- trimws(as.character(cn))
      cl <- if (is.null(cl)) cn else trimws(as.character(cl))
      keep <- !is.na(ln) & nzchar(ln) & !is.na(cn) & nzchar(cn)
      ln <- ln[keep]; cn <- cn[keep]; cl <- cl[keep]
      cl[is.na(cl) | !nzchar(cl)] <- cn[is.na(cl) | !nzchar(cl)]
      for (nm in unique(ln)) {
        idx <- which(ln == nm)
        codes <- cn[idx]; labs <- cl[idx]
        dup <- duplicated(codes)
        # A repeated code in one list is a form error; keep the first so the
        # rest of the list still works, rather than discarding all of it.
        lists[[nm]] <- stats::setNames(labs[!dup], codes[!dup])
      }
    }
  }

  # --- walk the survey sheet, tracking the group path ----------------------
  n <- nrow(survey)
  field <- character(0); grp <- character(0); src <- character(0)
  lab <- character(0); tp <- character(0); fmt <- character(0)
  multi <- logical(0); chs <- list()
  path_stack <- character(0)
  for (i in seq_len(n)) {
    parsed <- odk_parse_type(type_col[[i]])
    nm <- trimws(as.character(name_col[[i]] %||% ""))
    if (parsed$kind %in% ODK_STRUCTURE_TYPES) {
      if (startsWith(parsed$kind, "begin")) path_stack <- c(path_stack, nm)
      else if (length(path_stack)) path_stack <- path_stack[-length(path_stack)]
      next
    }
    if (!nzchar(nm)) next
    is_select <- parsed$kind %in% c("select_one", "select_multiple")
    lv <- if (is_select && !is.na(parsed$list_name))
      lists[[parsed$list_name]] else NULL
    mapped <- if (is_select) {
      # An expanded select_multiple column is a 0/1 flag; the parent field
      # itself is not a single categorical variable.
      if (parsed$multi) NA_character_
      else if (!is.null(lv) && length(lv) == 2L) "binary" else "nominal"
    } else .lookup(ODK_TYPE_MAP, parsed$kind)
    # A select_one whose choices live in an external file cannot be labelled
    # here, but it is still categorical.
    if (is_select && parsed$external && is.null(lv) && !parsed$multi)
      mapped <- "nominal"

    field <- c(field, nm)
    grp <- c(grp, paste(path_stack, collapse = "/"))
    src <- c(src, parsed$kind)
    lab <- c(lab, trimws(as.character(label_col[[i]] %||% "")))
    tp <- c(tp, mapped)
    fmt <- c(fmt, .lookup(ODK_DATE_FORMATS, parsed$kind))
    multi <- c(multi, parsed$multi)
    # `chs[k] <- list(lv)`, not `chs[[k]] <- lv`: assigning NULL through `[[`
    # does not extend the list, so a field with no choice list would be skipped
    # and every later field's choices would attach to the wrong row.
    chs[length(chs) + 1L] <- list(lv)
  }
  if (!length(field))
    stop("The XLSForm's 'survey' sheet lists no questions.", call. = FALSE)

  out <- data.frame(field = field, form = grp, source_type = src,
                    label = ifelse(is.na(lab), "", lab),
                    validation = rep("", length(field)),
                    type = tp, date_format = fmt, multi = multi,
                    stringsAsFactors = FALSE, row.names = NULL)
  out$choices <- chs
  # An expanded select_multiple is written `field/choice`.
  attr(out, "expand_sep") <- "/"
  attr(out, "source") <- "ODK form definition"
  out
}

#' Read whichever supported metadata format a file is.
#'
#' The analyst should not have to tell the app which kind of file they have: a
#' REDCap dictionary is a CSV with a "Field Type" column, an XLSForm is a
#' workbook with a `survey` sheet. Detection is on content, not the extension,
#' because both formats get re-saved and renamed in practice.
read_form_metadata <- function(path, orig_name = path) {
  if (!file.exists(path))
    stop("The metadata file could not be found.", call. = FALSE)
  ext <- tolower(tools::file_ext(orig_name))
  looks_excel <- ext %in% c("xlsx", "xlsm", "xls") ||
    isTRUE(tryCatch("survey" %in% tolower(readxl::excel_sheets(path)),
                    error = function(e) FALSE))
  if (looks_excel) return(read_odk_form(path))
  redcap <- tryCatch(read_redcap_dictionary(path), error = function(e) e)
  if (!inherits(redcap, "error")) return(redcap)
  odk <- tryCatch(read_odk_form(path), error = function(e) e)
  if (!inherits(odk, "error")) return(odk)
  stop("That file was not recognised as a REDCap data dictionary (a CSV with a 'Field Type' column) or an ODK XLSForm (a workbook with a 'survey' sheet). ",
       conditionMessage(redcap), call. = FALSE)
}

#' Note for select_multiple fields the export left unexpanded.
#'
#' ODK Central's default writes a select_multiple as ONE column of
#' space-separated choice names ("fever cough"). That is a set, not a category,
#' and treating it as a factor would invent one level per observed combination -
#' so it is reported instead, with what to do about it.
.odk_multi_note <- function(applied, spec, df) {
  multi_fields <- spec$field[.isTRUE_vec(spec$multi)]
  if (!length(multi_fields)) return(NULL)
  m <- form_match_columns(df, spec)
  # A parent field matched directly (rather than through an expanded
  # `field/choice` column) means the export was not split.
  unexpanded <- intersect(m$column[is.na(m$choice_code) & !is.na(m$field)],
                          multi_fields)
  if (!length(unexpanded)) return(NULL)
  sprintf("%d select_multiple field%s (%s) came through as one column of space-separated answers. Those are sets, not categories: re-export with select multiples split into one column per choice, or derive the indicators you need on the Data-check tab.",
          length(unexpanded), if (length(unexpanded) == 1L) "" else "s",
          paste(utils::head(unexpanded, 3), collapse = ", "))
}
