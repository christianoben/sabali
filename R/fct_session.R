# fct_session.R -------------------------------------------------------------
# Small session-lifecycle helpers.  Only explicit, app-owned temporary paths
# may be passed to session_cleanup; callers must never pass a directory root.

#' Remove explicit temporary files and directories owned by one Shiny session.
#'
#' @param paths Character vector of files or directories created for the
#'   session. Directories are removed recursively only when explicitly listed.
session_cleanup <- function(paths = character()) {
  paths <- unique(as.character(paths))
  paths <- paths[nzchar(paths) & file.exists(paths)]
  for (path in paths) {
    if (dir.exists(path)) {
      unlink(path, recursive = TRUE, force = TRUE)
    } else {
      unlink(path, force = TRUE)
    }
  }
  invisible(NULL)
}

#' Return a privacy-safe audit record for metadata-only activity.
audit_record <- function(event, fields = character(), n_rows = NA_integer_,
                         n_columns = NA_integer_) {
  data.frame(
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
    event = as.character(event)[1],
    fields = paste(sort(unique(as.character(fields))), collapse = ","),
    n_rows = as.integer(n_rows)[1],
    n_columns = as.integer(n_columns)[1],
    stringsAsFactors = FALSE
  )
}
