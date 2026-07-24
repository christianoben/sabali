# Browser smoke harness. It is a real shinytest2 AppDriver when the optional
# browser dependency is installed; local/headless environments without a
# browser report a deliberate skip rather than hiding the release requirement.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== shinytest2 browser smoke ==\n")
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  cat("  SKIP  shinytest2 is not installed; CI installs it for the browser gate\n")
} else {
  run_smoke <- function() {
    # shinytest2 deliberately skips browser startup on CRAN; this local/CI
    # harness is the opposite case and explicitly opts into the real app.
    Sys.setenv(NOT_CRAN = "true")
    # Use a temporary app directory containing R/ so shinytest2 does not
    # interpret the repository's DESCRIPTION as a package-only app. The
    # production app itself remains unchanged; this is only the browser
    # harness boundary.
    smoke_dir <- tempfile("sabali-shinytest2-")
    dir.create(smoke_dir)
    file.copy(file.path(ROOT, "app.R"), smoke_dir)
    file.copy(file.path(ROOT, "R"), smoke_dir, recursive = TRUE)
    on.exit(unlink(smoke_dir, recursive = TRUE, force = TRUE), add = TRUE)
    app <- shinytest2::AppDriver$new(app_dir = smoke_dir,
                                     name = "sabali-smoke",
                                     load_timeout = 30000,
                                     shiny_args = list(port = 3817,
                                                       launch.browser = FALSE))
    on.exit(app$stop(), add = TRUE)
    app$wait_for_idle()
    html <- paste(app$get_html(selector = "body"), collapse = " ")
    stopifnot(grepl("Sabali", html, fixed = TRUE))
    for (value in c("datacheck", "1 · Data", "2 · Descriptives",
                    "3 · Charts", "4 · Tests", "models", "advanced",
                    "7 · Report")) {
      app$click(selector = sprintf("#nav a[data-value='%s']", value))
      app$wait_for_idle()
      stopifnot(nchar(paste(app$get_html(selector = "body"), collapse = " ")) > 500L)
    }
    invisible(TRUE)
  }
  err <- tryCatch({ run_smoke(); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(err) && !nzchar(Sys.getenv("CI")) &&
      grepl("url|chromote|browser|start", err, ignore.case = TRUE)) {
    cat("  SKIP  local shinytest2/browser runtime could not start: ", err, "\n", sep = "")
  } else {
    ok("AppDriver starts and renders every top-level tab", {
      if (!is.null(err)) stop(err, call. = FALSE)
      TRUE
    })
  }
}

report_and_exit()
