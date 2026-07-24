# fct_auth.R -----------------------------------------------------------------
# Optional login layer for serving the app over a network. Uses {shinymanager}
# to put a username/password screen in front of the whole app and to keep each
# user's session separate.
#
# Authentication is OFF unless it is explicitly configured, so local development
# and the automated tests run without a login prompt. The production launcher
# (run_server.R) REQUIRES it before binding to any non-local address, so patient
# data is never served unauthenticated by accident.
#
# Configuration (environment variables, typically via the project .Renviron):
#   SABALI_AUTH_DB          path to the encrypted shinymanager credential store
#                           created by setup_auth.R
#   SABALI_AUTH_PASSPHRASE  passphrase that decrypts that store (never committed)

#' Resolve the login configuration from the environment.
#' @return list(db, passphrase, enabled) - `enabled` is TRUE only when a store
#'   file exists, a passphrase is set, and shinymanager is installed.
sabali_auth_config <- function() {
  db <- Sys.getenv("SABALI_AUTH_DB", "")
  passphrase <- Sys.getenv("SABALI_AUTH_PASSPHRASE", "")
  enabled <- nzchar(db) && file.exists(db) && nzchar(passphrase) &&
    requireNamespace("shinymanager", quietly = TRUE)
  list(db = db, passphrase = passphrase, enabled = isTRUE(enabled))
}

sabali_auth_enabled <- function() isTRUE(sabali_auth_config()$enabled)

#' Wrap a Shiny UI with the login screen when authentication is configured;
#' otherwise return the UI unchanged.
sabali_wrap_ui <- function(ui) {
  cfg <- sabali_auth_config()
  if (!cfg$enabled) return(ui)
  shinymanager::secure_app(
    ui,
    tags_top = shiny::tags$div(
      shiny::tags$h3("Sabali", style = "color:#018786;margin-bottom:0;"),
      shiny::tags$p("Please sign in. Authorised users only.")),
    choose_language = FALSE)
}

#' Wrap a Shiny server function with the login check when authentication is
#' configured; otherwise return it unchanged. The wrapped server runs the login
#' gate first and only then the app's own server logic.
sabali_wrap_server <- function(server) {
  # Force the original function before returning a deferred callback.  Without
  # this, R's lazy argument promise can resolve the global `server` symbol
  # later, after app.R has replaced it with this wrapper, causing recursive
  # authentication-server construction on every flush.
  server_fun <- force(server)
  cfg <- sabali_auth_config()
  if (!cfg$enabled) return(server_fun)
  function(input, output, session) {
    # Do not initialise the full analysis server while an unauthenticated
    # session is still showing the login form. Besides wasting work, registering
    # every module before authentication lets an expensive or misconfigured
    # reactive path monopolise the Shiny event loop and makes Login appear to do
    # nothing. secure_server() exposes the authenticated user after its token is
    # established; start the application exactly once after that point.
    #
    # The handoff is deliberately scheduled with onFlushed(). Calling server()
    # directly from this observer would make every reactive read performed while
    # the modules are being registered a dependency of this gate observer. That
    # can create a runaway invalidation loop as soon as the authenticated app
    # starts reacting to inputs.
    auth_state <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
        cfg$db, passphrase = cfg$passphrase))
    server_started <- FALSE
    shiny::observe({
      user <- auth_state$user
      if (isTRUE(server_started) || length(user) != 1L ||
          !nzchar(as.character(user))) return()
      server_started <<- TRUE
      session$onFlushed(
        function() server_fun(input, output, session),
        once = TRUE)
    })
  }
}
