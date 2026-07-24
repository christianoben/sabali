# fct_import.R --------------------------------------------------------------
# Read any supplied file into a data frame.
#
# Strategy: .rda/.RData and .rds are handled directly; everything else is
# dispatched through {rio}, which itself wraps {foreign}, {haven}, {readxl},
# {data.table}, etc. CSV and RDA are the expected common cases.

#' Read a data file of (almost) any format into a data frame.
#'
#' @param path      Path to the uploaded temp file.
#' @param orig_name Original file name (used to recover the extension, because
#'                  Shiny stores uploads under a temp name with no extension).
#' @return A base data.frame.
read_any_format <- function(path, orig_name = path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path) ||
      !file.exists(path))
    stop("The selected data file could not be found.", call. = FALSE)
  size <- tryCatch(file.info(path)$size, error = function(e) NA_real_)
  if (is.finite(size) && size > 200 * 1024^2)
    stop("The selected file is larger than the 200 MB upload limit. Please export a smaller extract.",
         call. = FALSE)

  ext <- tolower(tools::file_ext(orig_name))
  if (ext == "") ext <- tolower(tools::file_ext(path))

  df <- tryCatch(switch(
      ext,
      "rda"   = ,
      "rdata" = .read_rda(path),
      "rds"   = readRDS(path),
      # default: let rio dispatch (csv, tsv, txt, xlsx, xls, sav, dta, por, ...)
      rio::import(path, setclass = "data.frame")
    ), error = function(e) {
      stop(sprintf("Could not read the data file as %s: %s.",
                   if (nzchar(ext)) toupper(ext) else "a supported format",
                   conditionMessage(e)), call. = FALSE)
    })

  if (!is.data.frame(df)) {
    stop("The file was read but does not contain a table of data.")
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # haven brings in 'labelled' columns from SPSS/Stata; convert to plain
  # factors/numerics so the rest of the app sees ordinary R types.
  df <- .strip_haven_labels(df, keep_labels = TRUE)
  .validate_imported_data(df)
  df
}

# Fail early on files that cannot support a meaningful analysis.  These checks
# deliberately happen after import so the same analyst-friendly messages are
# used for CSV, RDS, Excel and labelled statistical-file inputs.
.validate_imported_data <- function(df) {
  if (!nrow(df) || !ncol(df))
    stop("The file contains no data rows or columns.", call. = FALSE)
  if (ncol(df) < 2L)
    stop("The file contains only one column. Please select a table with at least two columns for analysis.",
         call. = FALSE)

  all_missing <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
  if (length(all_missing))
    stop(sprintf("The following column%s contain only missing values: %s. Remove %s or provide usable values.",
                 if (length(all_missing) == 1) "" else "s",
                 paste(all_missing, collapse = ", "),
                 if (length(all_missing) == 1) "it" else "them"), call. = FALSE)

  constant <- names(df)[vapply(df, function(x) {
    z <- x[!is.na(x)]
    length(z) > 0L && length(unique(z)) <= 1L
  }, logical(1))]
  if (length(constant))
    stop(sprintf("The following column%s have only one observed value: %s. A constant column cannot support a comparison or model.",
                 if (length(constant) == 1) "" else "s",
                 paste(constant, collapse = ", ")), call. = FALSE)

  # Imported text must be representable as UTF-8 before it reaches Shiny or
  # JSON/Word exporters. `sub = NA` turns an invalid byte sequence into an
  # explicit failure rather than silently replacing a category label.
  bad_text <- names(df)[vapply(df, function(x) {
    if (!is.character(x)) return(FALSE)
    y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = NA_character_))
    any(is.na(y) & !is.na(x))
  }, logical(1))]
  if (length(bad_text))
    stop(sprintf("The file contains text that is not valid UTF-8 in column%s: %s. Re-save the file as UTF-8 and try again.",
                 if (length(bad_text) == 1) "" else "s",
                 paste(bad_text, collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}

# Load the first data frame found inside an .rda / .RData file.
.read_rda <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  objs <- ls(e)
  if (length(objs) == 0) stop("The .rda file contained no objects.")
  is_df <- vapply(objs, function(o) is.data.frame(get(o, envir = e)), logical(1))
  if (!any(is_df)) {
    stop("The .rda file contained no data frame.")
  }
  get(objs[is_df][[1]], envir = e)
}

# Convert haven_labelled columns (from SPSS/Stata) to factor or numeric.
.strip_haven_labels <- function(df, keep_labels = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) return(df)
  labelled <- vapply(df, function(x) inherits(x, "haven_labelled"), logical(1))
  for (nm in names(df)[labelled]) {
    x <- df[[nm]]
    labs <- attr(x, "labels")
    variable_label <- attr(x, "label", exact = TRUE)
    # If every value maps to a value label, treat as a factor; else numeric.
    converted <- if (!is.null(labs) && all(x %in% labs | is.na(x))) {
      haven::as_factor(x)
    } else {
      as.numeric(x)
    }
    # Keep the privacy-safe labels on the ordinary factor/numeric result.  The
    # analysis engine ignores these attributes, but the planner can use them
    # to explain coded SPSS/Stata variables without receiving raw rows.
    if (isTRUE(keep_labels)) {
      if (!is.null(variable_label)) attr(converted, "label") <- variable_label
      if (!is.null(labs)) attr(converted, "labels") <- labs
    }
    df[[nm]] <- converted
  }
  df
}

# Human-readable summary of an imported data frame, for the UI banner.
import_summary <- function(df) {
  sprintf("Loaded %s rows and %s columns.",
          format(nrow(df), big.mark = ","),
          format(ncol(df), big.mark = ","))
}
