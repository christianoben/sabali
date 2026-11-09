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
# makes the key load however the app is launched. Preserve explicitly supplied
# authentication values: run_server.R uses these for recovery/test stores and
# readRenviron() would otherwise silently replace them with the project store.
.explicit_auth_db <- Sys.getenv("SABALI_AUTH_DB", "")
.explicit_auth_passphrase <- Sys.getenv("SABALI_AUTH_PASSPHRASE", "")
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (nzchar(.explicit_auth_db))
  Sys.setenv(SABALI_AUTH_DB = .explicit_auth_db)
if (nzchar(.explicit_auth_passphrase))
  Sys.setenv(SABALI_AUTH_PASSPHRASE = .explicit_auth_passphrase)
rm(.explicit_auth_db, .explicit_auth_passphrase)

# Shiny's automatic sourcing of ./R is disabled (see R/_disable_autoload.R):
# this directory ships a DESCRIPTION/NAMESPACE, so autoload would warn and
# double-source. We therefore source the R/*.R files ourselves here, skipping
# the leading-underscore sentinel (and any other private helper file).
for (.f in list.files("R", pattern = "^[^_].*[.][Rr]$", full.names = TRUE)) {
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

# Warm a small background worker pool so the slow AI calls (planner /
# interpreter) run off the main session thread and the app stays responsive.
# Only when AI is actually configured - no point spawning workers otherwise.
if (ai_available()) ai_async_setup()

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
  # next_id gives every item a stable handle so the background write-up worker
  # can find the item it was launched for and patch its prose back in.
  report_store <- reactiveValues(items = list(), next_id = 0L)

  # Background AI write-up enrichment: when an item carrying an `ai` payload is
  # added, a two-pass AI call (interpret-if-needed, then a Results-section prose
  # write-up) runs off-thread and its result is stitched back onto the item.
  # See report_enricher() in R/mod_report.R.
  add_report <- report_enricher(report_store)

  # The tab-0 Data contract can ENFORCE its allowed levels / numeric ranges:
  # out-of-contract values are coerced to NA in the data every analysis tab
  # uses. The contract is built from the PRE-enforcement data (ds$data_base) so
  # there is no reactive cycle; its derived rules flow back through this shared
  # reactiveVal into mod_data, which produces the enforced ds$data.
  enforce_rules <- reactiveVal(list())
  # Derived-variable specs travel the same way as the contract rules: tab 0 owns
  # the list, mod_data applies it once, and every analysis tab sees the result.
  derive_specs <- reactiveVal(list())
  ds <- mod_data_server("data", enforce = enforce_rules, derive = derive_specs)
  dc <- mod_datacheck_server("datacheck", data = ds$data_base, raw_data = ds$raw,
                       data_name = ds$data_name, types = ds$types_base,
                       outcomes = ds$outcomes, panel = ds$panel,
                       plan = ds$plan,
                       audit = ds$audit, audit_record = ds$audit_record)
  observe(enforce_rules(dc$enforcement()))
  observe(derive_specs(dc$transforms()))
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
                    survey = ds$survey,
                    plan_rec = ds$plan, add_report = add_report,
                    audit = ds$audit_record)
  mod_phase4_server("phase4", data = ds$data, types = ds$types,
                    outcomes = ds$outcomes, panel = ds$panel,
                    survey = ds$survey,
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

# Optional login layer (see R/fct_auth.R). When SABALI_AUTH_DB / _PASSPHRASE are
# configured the whole app is placed behind a shinymanager sign-in screen; with
# no configuration (local development, automated tests) it runs unwrapped.
ui <- sabali_wrap_ui(ui)
server <- sabali_wrap_server(server)

shinyApp(ui, server)
