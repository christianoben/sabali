# app.R ---------------------------------------------------------------------
# Sabali - guided statistics tool - Phase 4 prototype
# (Data import -> variable-type confirmation -> guided descriptives ->
#  ggplot2 chart studio -> one-click Word report.)
#
# R scripts in ./R are sourced automatically by Shiny (>= 1.5.0), so the
# function and module files do not need to be sourced here explicitly.

library(shiny)
library(ggplot2)

# Load the project-local .Renviron explicitly. R only auto-reads ONE .Renviron
# at startup, so a pre-existing ~/.Renviron - or launching R outside the project
# directory (e.g. RStudio "Run App") - means the project file, which holds the
# optional AI-interpretation key, is otherwise silently skipped. Reading it here
# makes the key load however the app is launched.
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Shiny (>= 1.5.0) auto-sources ./R, but we source explicitly too so the app
# works regardless of Shiny version and load order.
for (.f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(.f, local = FALSE)
}

# Fail early with a friendly message if a hard dependency is missing.
.required <- c("shiny", "ggplot2", "DT", "rio", "officer", "MASS",
               "quantreg", "glmnet", "pls", "lmtest", "geepack", "lme4", "survival")
.missing  <- .required[!vapply(.required, requireNamespace, logical(1),
                               quietly = TRUE)]
if (length(.missing) > 0) {
  stop("Missing required packages: ", paste(.missing, collapse = ", "),
       ".\nRun install_dependencies.R first.", call. = FALSE)
}

# Allow larger uploads (default is 5 MB).
options(shiny.maxRequestSize = 200 * 1024^2)

ui <- navbarPage(
  title = "Sabali",
  id = "nav",
  header = tags$head(
    tags$style(HTML(
      ".well{background:#f7faf9;border-color:#dfeceb;}
       .navbar-default{background:#018786;border:0;}
       .navbar-default .navbar-brand,.navbar-default .navbar-nav>li>a{color:#fff;}
       .navbar-default .navbar-nav>.active>a{background:#016b6a;color:#fff;}")),
    tags$script(HTML("\n      Shiny.addCustomMessageHandler('advanced-tab-visibility', function(message) {\n        var link = document.querySelector('#nav a[data-value=\\\"advanced\\\"]');\n        if (!link) return;\n        var tab = link.parentElement;\n        tab.style.display = message.visible ? '' : 'none';\n        if (!message.visible && tab.classList.contains('active')) {\n          var dataLink = document.querySelector('#nav a[data-value=\\\"data\\\"]');\n          if (dataLink) $(dataLink).tab('show');\n        }\n      });\n    "))
  ),
  tabPanel("0 · Data check",   value = "datacheck", mod_datacheck_ui("datacheck")),
  tabPanel("1 · Data",         value = "data",   mod_data_ui("data")),
  tabPanel("2 · Descriptives", value = "desc",   mod_descriptives_ui("desc")),
  tabPanel("3 · Charts",       value = "charts", mod_charts_ui("charts")),
  tabPanel("4 · Tests",        value = "tests",  mod_tests_ui("tests")),
  tabPanel("5 · Regression",      value = "models",  mod_models_ui("models")),
  tabPanel("6 · Advanced models", value = "advanced", mod_phase4_ui("phase4")),
  tabPanel("7 · Report",       value = "report", mod_report_ui("report"))
)

server <- function(input, output, session) {
  # Per-session report store (keeps sessions independent for many users).
  report_store <- reactiveValues(items = list())
  add_report <- function(item) {
    report_store$items <- c(report_store$items, list(item))
  }

  ds <- mod_data_server("data")
  mod_datacheck_server("datacheck", data = ds$data, types = ds$types,
                       panel = ds$panel)
  mod_descriptives_server("desc", data = ds$data, types = ds$types,
                          outcomes = ds$outcomes, panel = ds$panel,
                          add_report = add_report, audit = ds$audit_record)
  mod_charts_server("charts", data = ds$data, types = ds$types,
                    outcomes = ds$outcomes, panel = ds$panel,
                    add_report = add_report)
  mod_tests_server("tests", data = ds$data, types = ds$types,
                   outcomes = ds$outcomes, panel = ds$panel,
                   plan_rec = ds$plan, add_report = add_report,
                   audit = ds$audit_record)
  mod_models_server("models", data = ds$data, types = ds$types,
                    outcomes = ds$outcomes, panel = ds$panel,
                    plan_rec = ds$plan, add_report = add_report,
                    audit = ds$audit_record)
  mod_phase4_server("phase4", data = ds$data, types = ds$types,
                    outcomes = ds$outcomes, panel = ds$panel,
                    plan_rec = ds$plan, add_report = add_report,
                    audit = ds$audit_record)
  observe({
    structure <- tryCatch(advanced_data_structure(ds$data(), ds$types(), ds$panel()),
                           error = function(e) list(available = FALSE))
    session$sendCustomMessage("advanced-tab-visibility",
                              list(visible = isTRUE(structure$available)))
  })
  mod_report_server("report", report_store = report_store)
}

shinyApp(ui, server)
