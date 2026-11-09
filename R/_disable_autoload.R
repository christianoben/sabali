# Presence of this file disables Shiny's automatic sourcing of the R/ directory
# (see `?shiny::loadSupport`). This project ships a DESCRIPTION and NAMESPACE, so
# Shiny would otherwise treat the app directory as a package, print
#
#   "Loading R/ subdirectory for Shiny application, but this directory appears
#    to contain an R package. Sourcing files in R/ may cause unexpected
#    behavior."
#
# and source every R/*.R file a SECOND time (app.R already sources them
# explicitly). Disabling autoload here fixes both, and does so for every launch
# path - RStudio "Run App", run_dev.R, runApp() and run_server.R alike - rather
# than relying on each launcher to set options(shiny.autoload.r = FALSE).
#
# Intentionally empty apart from this note.
