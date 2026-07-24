# Governance, planner catalogue, AI-off controls, all-tab UI rendering and
# explicit data-layout smoke coverage.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(htmltools))
.clear_ai <- function() Sys.unsetenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
                                      "AI_INTERPRET_MODEL", "OPENCODE_API_KEY"))
.clear_ai()

cat("\n== planner catalogue integrity ==\n")
test_cat <- planner_test_catalogue()
test_ids <- vapply(test_cat, function(x) x$id, character(1))
reg_cat <- planner_regression_catalogue()
reg_ids <- vapply(reg_cat, function(x) x$id, character(1))
adv_cat <- planner_advanced_catalogue()
adv_ids <- vapply(adv_cat, function(x) x$id, character(1))
ok("every planner test id maps to a known Tests-tab family", {
  stopifnot(!anyDuplicated(test_ids), all(test_ids %in% names(PLANNER_TEST_FAMILY)),
            all(nzchar(unname(PLANNER_TEST_FAMILY[test_ids]))))
})
ok("every standard regression id maps to a model label or explicit none", {
  stopifnot(!anyDuplicated(reg_ids), all(reg_ids %in% c(names(MODEL_METHOD_LABELS), "none")))
})
ok("every advanced catalogue id has a supported family or explicit no-op", {
  supported <- c(names(MODEL_METHOD_LABELS), "cox_ph", "kaplan_meier", "time_series", "none")
  stopifnot(!anyDuplicated(adv_ids), all(adv_ids %in% supported))
})
ok("malformed plan ids are rejected without silent fallback", {
  bad <- list(allowable_tests = list(list(test = "DROP_TABLES")),
              recommended_test = "DROP_TABLES", regression_method = "not-a-model",
              advanced_regression = "not-a-model")
  errs <- validate_plan(bad)
  stopifnot(length(errs) >= 4L, all(grepl("unknown", errs)))
})

cat("\n== privacy-safe metadata and AI-off paths ==\n")
safe_df <- data.frame(outcome = factor(c("yes", "no", "yes", "no")),
                      age = c(22, 31, 44, 58),
                      record_id = c(101, 102, 103, 104))
meta <- build_planner_metadata(safe_df,
  c(outcome = "binary", age = "continuous", record_id = "identifier"),
  outcomes = "outcome")
prompt <- planner_prompt("What is associated with outcome?", meta)
ok("planner metadata contains aggregate structure, not row-level values", {
  stopifnot(grepl("n_rows", prompt$user, fixed = TRUE),
            !grepl("101|102|103|104", prompt$user),
            !grepl("patient record|row-level data", prompt$user, ignore.case = TRUE))
})
ok("AI is off and direct AI calls fail clearly when no endpoint is configured", {
  stopifnot(!ai_available())
  e1 <- friendly_error(ai_interpret(list(results = data.frame(p = .2)), list(title = "x")))
  e2 <- friendly_error(ai_plan("q", safe_df,
    c(outcome = "binary", age = "continuous", record_id = "identifier")))
  stopifnot(grepl("no AI endpoint", e1, ignore.case = TRUE),
            grepl("no AI endpoint", e2, ignore.case = TRUE))
})
ok("AI interpretation UI is visibly disabled when off", {
  html <- htmltools::renderTags(mod_interpret_ui("interpret"))$html
  stopifnot(grepl("disabled", html, fixed = TRUE),
            grepl("no AI endpoint is configured", html, fixed = TRUE),
            !grepl("actionButton", html, fixed = TRUE))
})
ok("planner UI is visibly disabled when off", {
  html <- NULL
  shiny::testServer(mod_data_server, {
    session$flushReact()
    html <<- html_text(output$planner_controls)
  })
  stopifnot(grepl("disabled", html, fixed = TRUE),
            grepl("no AI endpoint is configured", html, fixed = TRUE))
})

cat("\n== all tabs and declared data layouts render ==\n")
ui_functions <- list(
  datacheck = mod_datacheck_ui, data = mod_data_ui,
  descriptives = mod_descriptives_ui, charts = mod_charts_ui,
  tests = mod_tests_ui, models = mod_models_ui,
  advanced = mod_phase4_ui, report = mod_report_ui)
for (nm in names(ui_functions)) {
  ok(sprintf("tab UI renders: %s", nm), {
    html <- htmltools::renderTags(ui_functions[[nm]](nm))$html
    stopifnot(nchar(html) > 100L, !grepl("object .* not found|could not find function", html))
  })
}
ok("full navbar renders with every tab", {
  app_env <- new.env(parent = globalenv())
  sys.source(file.path(ROOT, "app.R"), envir = app_env)
  html <- htmltools::renderTags(app_env$ui)$html
  stopifnot(all(vapply(c("datacheck", "data", "desc", "charts", "tests",
                         "models", "advanced", "report"),
                 function(x) grepl(x, html, fixed = TRUE), logical(1))))
})

layout_df <- data.frame(id = rep(1:12, each = 3),
                        visit = rep(1:3, 12),
                        date = as.Date("2024-01-01") + rep(0:35, 1),
                        y = seq_len(36), event = rep(c(0, 1), 18))
layout_types <- c(id = "identifier", visit = "ordinal", date = "datetime",
                  y = "continuous", event = "binary")
layouts <- list(
  cross_sectional = list(kind = "cross_sectional"),
  repeated = list(kind = "repeated", id = "id", time = "visit"),
  time_series = list(kind = "time_series", time = "date"),
  survival = list(kind = "cross_sectional", survival = TRUE,
                  surv_mode = "duration", surv_time = "y", surv_event = "event"))
for (nm in names(layouts)) {
  ok(sprintf("declared %s layout reaches structure gate", nm), {
    s <- advanced_data_structure(layout_df, layout_types, layouts[[nm]])
    if (nm == "cross_sectional") stopifnot(!isTRUE(s$available))
    if (nm == "repeated") stopifnot(isTRUE(s$available), isTRUE(s$clustered))
    if (nm == "time_series") stopifnot(isTRUE(s$available), isTRUE(s$time_series))
    if (nm == "survival") stopifnot(isTRUE(s$available), isTRUE(s$survival))
  })
}

ok("upload cap is configured to 200 MB", {
  stopifnot(getOption("shiny.maxRequestSize") == 200 * 1024^2 ||
            grepl("200", paste(readLines(file.path(ROOT, "app.R")), collapse = " ")))
})

report_and_exit()
