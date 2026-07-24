# Query OSV for the exact CRAN package versions in renv.lock. This is a
# networked release check; the workflow runs it on demand and weekly.
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("httr2", quietly = TRUE)) {
  stop("dependency_scan.R requires jsonlite and httr2", call. = FALSE)
}

lock_path <- "renv.lock"
if (!file.exists(lock_path)) stop("renv.lock is missing", call. = FALSE)
lock <- jsonlite::read_json(lock_path, simplifyVector = FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
packages <- lock$Packages
queries <- lapply(packages, function(p) list(
  package = list(name = p$Package, ecosystem = "CRAN"),
  version = p$Version
))

response <- httr2::request("https://api.osv.dev/v1/querybatch") |>
  httr2::req_body_json(list(queries = unname(queries)), auto_unbox = TRUE) |>
  httr2::req_timeout(30) |>
  httr2::req_perform()
body <- httr2::resp_body_json(response, simplifyVector = FALSE)
results <- body$results %||% list()
vulnerable <- unlist(lapply(results, function(x) x$vulns %||% list()), recursive = FALSE)

if (length(vulnerable)) {
  cat("Dependency advisories found in the pinned CRAN baseline:\n")
  for (v in vulnerable) {
    cat(" - ", v$id %||% "(unidentified)", ": ",
        v$summary %||% "no summary", "\n", sep = "")
  }
  quit(status = 1)
}
cat("OSV found no advisories for", length(packages), "pinned CRAN packages.\n")
