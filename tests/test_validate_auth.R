# tests/test_validate_auth.R ------------------------------------------------
# Guards the login layer's security defaults: authentication is OFF unless it is
# explicitly configured (so development and the other suites never get gated),
# and ON once a credential store + passphrase are present, with correct/wrong
# passwords handled. See R/fct_auth.R and run_server.R.

source(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "helper.R"))

# Ensure a clean environment regardless of how the process was launched.
Sys.unsetenv("SABALI_AUTH_DB"); Sys.unsetenv("SABALI_AUTH_PASSPHRASE")

ok("authentication is OFF by default and the wrappers are inert", {
  stopifnot(isFALSE(sabali_auth_enabled()))
  ui <- shiny::fluidPage(shiny::h1("x"))
  srv <- function(input, output, session) NULL
  stopifnot(identical(sabali_wrap_ui(ui), ui))
  stopifnot(identical(sabali_wrap_server(srv), srv))
})

if (!requireNamespace("shinymanager", quietly = TRUE)) {
  cat("  SKIP  shinymanager not installed; the enabled path is exercised in CI\n")
} else {
  ok("a configured store enables auth, wraps the app, and checks credentials", {
    db <- tempfile(fileext = ".sqlite")
    pass <- "test-passphrase-abcdefghij"
    creds <- data.frame(user = "tester", password = "correcthorse123",
                        admin = TRUE, stringsAsFactors = FALSE)
    shinymanager::create_db(credentials_data = creds, sqlite_path = db,
                            passphrase = pass)
    Sys.setenv(SABALI_AUTH_DB = db, SABALI_AUTH_PASSPHRASE = pass)
    on.exit({ Sys.unsetenv("SABALI_AUTH_DB"); Sys.unsetenv("SABALI_AUTH_PASSPHRASE")
              unlink(db) }, add = TRUE)

    stopifnot(isTRUE(sabali_auth_enabled()))
    ui <- shiny::fluidPage(shiny::h1("x"))
    stopifnot(!identical(sabali_wrap_ui(ui), ui))          # wrapped with a login
    srv <- function(input, output, session) NULL
    stopifnot(!identical(sabali_wrap_server(srv), srv))
    chk <- shinymanager::check_credentials(db, passphrase = pass)
    stopifnot(isTRUE(chk(user = "tester", password = "correcthorse123")$result))
    stopifnot(isFALSE(chk(user = "tester", password = "nope")$result))
  })
}

# Leave the environment clean for any suite that shares this process.
Sys.unsetenv("SABALI_AUTH_DB"); Sys.unsetenv("SABALI_AUTH_PASSPHRASE")
report_and_exit()
