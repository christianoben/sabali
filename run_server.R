#!/usr/bin/env Rscript
# run_server.R --------------------------------------------------------------
# Production launcher for serving Sabali over a network.
#
#   Rscript run_server.R
#
# It reads the host/port and login configuration from the environment (see
# .Renviron.example) and starts the Shiny server. Crucially it REFUSES to serve
# on a non-local address unless a login has been configured, so a tool that can
# hold patient data is never exposed unauthenticated by mistake.
#
# Environment:
#   SABALI_HOST  interface to bind (default 127.0.0.1 = this machine only).
#                Set to 0.0.0.0 to accept connections from other machines.
#   SABALI_PORT  TCP port (default 3838).
#   SABALI_AUTH_DB / SABALI_AUTH_PASSPHRASE  the login store (see setup_auth.R).

# Rscript normally loads .Renviron before this file. Only load it here when
# required values are still missing; readRenviron() overwrites existing values
# and could otherwise clobber an explicit recovery-store override.
if (file.exists(".Renviron") &&
    (!nzchar(Sys.getenv("SABALI_AUTH_DB")) ||
     !nzchar(Sys.getenv("SABALI_AUTH_PASSPHRASE")))) {
  readRenviron(".Renviron")
}
suppressPackageStartupMessages(library(shiny))
source("R/fct_auth.R")

host <- Sys.getenv("SABALI_HOST", "127.0.0.1")
port <- suppressWarnings(as.integer(Sys.getenv("SABALI_PORT", "3838")))
if (is.na(port) || port < 1L || port > 65535L)
  stop("SABALI_PORT must be a valid TCP port number.", call. = FALSE)

is_local <- host %in% c("127.0.0.1", "localhost", "::1")
auth <- sabali_auth_config()

# Specific, actionable message when a store path is configured but the file is
# missing (a common working-directory / file-sync surprise), instead of the
# generic "without a login" text below.
db_path <- Sys.getenv("SABALI_AUTH_DB", "")
if (nzchar(db_path) && !file.exists(db_path)) {
  stop(sprintf(paste0(
    "Login store not found.\n\n",
    "  SABALI_AUTH_DB points to: %s\n",
    "  Looked relative to      : %s\n\n",
    "Run setup_auth.R from THIS directory (it prints the absolute path of the\n",
    "store it writes), or set SABALI_AUTH_DB to that absolute path in .Renviron."),
    db_path, getwd()), call. = FALSE)
}

# Safety gate: never bind to a network interface without a login in place.
if (!is_local && !isTRUE(auth$enabled)) {
  stop(paste0(
    "Refusing to serve on ", host, ":", port, " without a login.\n\n",
    "This app can hold patient data, so authentication is required before it is\n",
    "reachable from other machines. Configure a login first:\n\n",
    "  1. Pick a long random passphrase and export it (do NOT commit it):\n",
    "       export SABALI_AUTH_PASSPHRASE='...'\n",
    "  2. Create at least one user:\n",
    "       Rscript setup_auth.R\n",
    "  3. Put SABALI_AUTH_DB and SABALI_AUTH_PASSPHRASE in .Renviron.\n\n",
    "See DEPLOYMENT.md for the full walkthrough."), call. = FALSE)
}

# Production hardening: never surface raw R errors or file paths to remote
# users, keep the 200 MB upload cap, and disable Shiny's package-style R/
# autoload (app.R already sources R/ explicitly; DESCRIPTION/NAMESPACE would
# otherwise make Shiny double-load it).
options(shiny.sanitize.errors = TRUE,
        shiny.maxRequestSize = 200 * 1024^2,
        shiny.autoload.r = FALSE)

message(sprintf("Starting Sabali on http://%s:%d  (%s)", host, port,
                if (isTRUE(auth$enabled)) "login required"
                else "no login - local machine only"))
if (!is_local) {
  message("")
  message("!! Reachable from other machines. Patient data and passwords are only")
  message("!! protected in transit if clients connect over HTTPS. Do NOT share a")
  message("!! plain http://<ip>:<port> link for real patient data over the open")
  message("!! internet - use the HTTPS tunnel or reverse-proxy setup in DEPLOYMENT.md.")
  message("")
}

shiny::runApp(".", host = host, port = port, launch.browser = FALSE)
