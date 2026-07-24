#!/usr/bin/env Rscript
# setup_auth.R --------------------------------------------------------------
# Create (or recreate) the encrypted login store used by run_server.R.
# Passwords are hashed with scrypt by {shinymanager}; the SQLite store is
# encrypted with your passphrase. Nothing here is committed to source control.
#
# Usage:
#   export SABALI_AUTH_PASSPHRASE='a-long-random-passphrase'   # required
#   Rscript setup_auth.R                 # interactive: add one or more users
#
# Non-interactive (e.g. first admin from a script):
#   SABALI_AUTH_PASSPHRASE=... SABALI_NEW_USER=alice SABALI_NEW_PASSWORD=... \
#     Rscript setup_auth.R
#
# Re-running REPLACES the whole user list with the users entered in that run.

# Rscript normally loads .Renviron before this file. Only load it here when a
# required value is still missing; readRenviron() overwrites existing variables
# and could otherwise clobber an explicit recovery-store override.
if (file.exists(".Renviron") &&
    (!nzchar(Sys.getenv("SABALI_AUTH_DB")) ||
     !nzchar(Sys.getenv("SABALI_AUTH_PASSPHRASE")))) {
  readRenviron(".Renviron")
}
if (!requireNamespace("shinymanager", quietly = TRUE))
  stop("Install shinymanager first: install.packages('shinymanager')", call. = FALSE)

passphrase <- Sys.getenv("SABALI_AUTH_PASSPHRASE", "")
if (!nzchar(passphrase))
  stop(paste0("Set SABALI_AUTH_PASSPHRASE to a long random passphrase first, e.g.\n",
              "  export SABALI_AUTH_PASSPHRASE=\"$(openssl rand -base64 24)\"\n",
              "Keep it secret and out of version control."), call. = FALSE)
db <- Sys.getenv("SABALI_AUTH_DB", "sabali_auth.sqlite")

# Read one line from the terminal. Opens a single stdin connection and reuses it
# so sequential reads don't lose buffered input (a fresh readLines("stdin") each
# call would discard the lines it buffered but didn't return). Works under
# `Rscript`, where readline() does not block. Returns "" on end-of-input.
.stdin_con <- NULL
read_line <- function(prompt, trim = TRUE) {
  if (is.null(.stdin_con)) {
    .stdin_con <<- file("stdin")
    tryCatch(open(.stdin_con, "r"), error = function(e) NULL)
  }
  cat(prompt)
  line <- tryCatch(readLines(.stdin_con, n = 1, warn = FALSE),
                   error = function(e) character(0))
  if (length(line) == 0) ""
  else if (isTRUE(trim)) trimws(line)
  else line[[1L]]
}

ask_user <- function() {
  env <- Sys.getenv("SABALI_NEW_USER", "")
  if (nzchar(env)) env else read_line("Username: ")
}

# The password is read without echo when {getPass} is installed; otherwise it is
# typed visibly on the terminal, with a warning. Always returns a length-1
# string (never NULL), so validation cannot trip on a cancelled helper.
ask_password <- function() {
  env <- Sys.getenv("SABALI_NEW_PASSWORD", "")
  if (nzchar(env)) return(env)
  if (requireNamespace("getPass", quietly = TRUE)) {
    pw <- tryCatch(getPass::getPass("Password (>= 12 chars): "), error = function(e) NULL)
    if (!is.null(pw) && nzchar(pw)) return(pw)
  }
  message("(install the 'getPass' package for hidden entry; for now the password is visible as you type)")
  read_line("Password (>= 12 chars): ", trim = FALSE)
}

users <- character(0); passwords <- character(0)
env_driven <- nzchar(Sys.getenv("SABALI_NEW_USER", ""))
repeat {
  u <- ask_user()
  p <- ask_password()
  if (!nzchar(u) || !nzchar(p)) stop("Username and password are both required.", call. = FALSE)
  if (nchar(p) < 12) stop("Choose a password of at least 12 characters.", call. = FALSE)
  if (u %in% users) stop(sprintf("Duplicate username '%s'.", u), call. = FALSE)
  users <- c(users, u); passwords <- c(passwords, p)
  # A single run when driven by env vars; otherwise offer to add more users.
  if (env_driven) break
  more <- tolower(read_line("Add another user? [y/N]: "))
  if (!identical(more, "y")) break
}

creds <- data.frame(user = users, password = passwords,
                    admin = TRUE, stringsAsFactors = FALSE)

# Preserve the previous encrypted store before replacing it. This makes a
# recovery/reconfiguration reversible without exposing credentials in output.
if (file.exists(db)) {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  backup <- paste0(db, ".backup-", stamp)
  suffix <- 1L
  while (file.exists(backup)) {
    backup <- paste0(db, ".backup-", stamp, "-", suffix)
    suffix <- suffix + 1L
  }
  if (!file.copy(db, backup, overwrite = FALSE))
    stop(sprintf("Could not back up the existing login store to '%s'. No changes were made.",
                 backup), call. = FALSE)
  message(sprintf("Existing login store backed up to: %s", normalizePath(backup)))
}

shinymanager::create_db(credentials_data = creds, sqlite_path = db,
                        passphrase = passphrase)

# Verify the exact credentials just written without logging an authentication
# attempt. Setup must never report success for an unverifiable store.
verify_credentials <- shinymanager::check_credentials(db, passphrase = passphrase)
verified <- vapply(seq_along(users), function(i) {
  result <- verify_credentials(users[[i]], passwords[[i]])
  isTRUE(result$result)
}, logical(1))
if (!all(verified))
  stop("The login store was written but could not verify the supplied credentials.",
       call. = FALSE)

# Confirm the store really landed on disk and report its ABSOLUTE path, so a
# working-directory or file-sync surprise is caught here rather than later when
# run_server.R cannot find it.
if (!file.exists(db))
  stop(sprintf("create_db reported success but no file exists at '%s' (working dir: %s). Check permissions / that this folder is not offloaded by a file-sync service.",
               db, getwd()), call. = FALSE)
abs_db <- normalizePath(db)
message(sprintf("\nSaved %d user(s).", nrow(creds)))
message(sprintf("Store: %s  (passwords scrypt-hashed, store encrypted)", abs_db))
message(sprintf("Working directory: %s", getwd()))
if (!identical(dirname(abs_db), getwd()))
  message("NOTE: the store is NOT in your working directory. Run run_server.R from the same directory, or set SABALI_AUTH_DB to this absolute path in .Renviron.")
message("Now run run_server.R from this directory.")
