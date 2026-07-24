# run_dev.R -----------------------------------------------------------------
# Launch the app in DEVELOPMENT mode with hot reload.
#
# With auto-reload on, saving any file in this project (app.R or the R/*.R
# function and module files) makes the running app re-run itself and the
# browser refresh automatically - no manual stop/restart needed. Because app.R
# re-sources ./R on every run, your edits are picked up each reload.
#
# How to use:
#   * RStudio: open this file and click "Source" (or run the two lines in the
#     console), then edit code and save - the app tab refreshes on its own.
#   * Terminal: Rscript run_dev.R
#
# Note: a reload restarts the session, so anything you had loaded in the app
# (uploaded data, selections) resets. That is normal for hot reload.
# Do NOT use this launcher in production; use app.R / your normal deploy path.

options(shiny.autoreload = TRUE)   # watch files and reload on save
# shiny::devmode(TRUE)             # optional: enables autoreload + extra dev checks

shiny::runApp(".", launch.browser = TRUE)
