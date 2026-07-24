# tests/test_structure.R ----------------------------------------------------
# Data-structure gating for the Advanced models tab and the revised UI.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== Advanced-model gate is driven by the EXPLICIT answer, not heuristics ==\n")
ok("explicit cross-sectional hides Advanced models", {
  d <- data.frame(id = 1:20, outcome = rnorm(20))
  tp <- c(id = "continuous", outcome = "continuous")
  info <- advanced_data_structure(d, tp, panel = list(kind = "cross_sectional"))
  stopifnot(!isTRUE(info$available), identical(info$kind, "none"))
})
ok("explicit repeated (ID + time) enables longitudinal analyses", {
  d <- data.frame(patient_id = rep(1:5, each = 3),
                  visit = rep(c("baseline", "week4", "week8"), 5),
                  outcome = rnorm(15))
  tp <- c(patient_id = "continuous", visit = "nominal", outcome = "continuous")
  info <- advanced_data_structure(d, tp,
    panel = list(kind = "repeated", id = "patient_id", time = "visit"))
  stopifnot(isTRUE(info$available), identical(info$kind, "longitudinal"),
            grepl("visit", info$reason, fixed = TRUE))
})
ok("explicit time series (time only) enables time-series analyses", {
  d <- data.frame(date = as.Date("2025-01-01") + 0:9, outcome = rnorm(10))
  tp <- c(date = "datetime", outcome = "continuous")
  info <- advanced_data_structure(d, tp,
    panel = list(kind = "time_series", time = "date"))
  stopifnot(isTRUE(info$available), identical(info$kind, "time_series"))
})
ok("repeated chosen but columns not yet selected -> hidden with guidance", {
  d <- data.frame(patient_id = rep(1:5, each = 3), outcome = rnorm(15))
  tp <- c(patient_id = "continuous", outcome = "continuous")
  info <- advanced_data_structure(d, tp, panel = list(kind = "repeated"))
  stopifnot(!isTRUE(info$available), grepl("select", info$reason, ignore.case = TRUE))
})
ok("NO auto-detection: repeated-looking data with no explicit answer stays hidden", {
  # patient_id repeats and 'visit' varies within subject - the old heuristic
  # would have flagged this as longitudinal. Without an explicit answer it must
  # NOT be auto-enabled any more.
  d <- data.frame(patient_id = rep(1:5, each = 3),
                  visit = rep(c("b", "w4", "w8"), 5), outcome = rnorm(15))
  tp <- c(patient_id = "identifier", visit = "nominal", outcome = "continuous")
  info <- advanced_data_structure(d, tp, panel = NULL)
  stopifnot(!isTRUE(info$available), identical(info$kind, "none"))
})
ok("suggest_panel_roles heuristic has been removed", {
  stopifnot(!exists("suggest_panel_roles", mode = "function"))
})

cat("\n== Data tab wiring: panel() comes from the explicit question ==\n")
ok("data_kind + role inputs produce the right panel roles", {
  suppressPackageStartupMessages(library(shiny))
  # repeated -> id + time (+ event); cross-sectional -> none; time_series -> time only
  shiny::testServer(mod_data_server, {
    session$setInputs(data_kind = "repeated", id_var = "pid",
                      time_var = "visit", event_var = "")
    p <- session$returned$panel()
    stopifnot(identical(p$kind, "repeated"), identical(p$id, "pid"),
              identical(p$time, "visit"), is.null(p$event))

    session$setInputs(data_kind = "cross_sectional")
    p2 <- session$returned$panel()
    stopifnot(identical(p2$kind, "cross_sectional"),
              is.null(p2$id), is.null(p2$time))

    session$setInputs(data_kind = "time_series", time_var = "date")
    p3 <- session$returned$panel()
    stopifnot(identical(p3$kind, "time_series"), is.null(p3$id),
              identical(p3$time, "date"))
  })
})
ok("panel variables are classified as time-varying or invariant", {
  d <- data.frame(id = rep(1:4, each = 3), time = rep(1:3, 4),
                  varying = c(1, 2, 3, 2, 2, 2, 4, 5, 6, 7, 7, 7),
                  invariant = rep(c("A", "B", "C", "D"), each = 3))
  v <- panel_variable_status(d, "id", "time", "varying")
  i <- panel_variable_status(d, "id", "time", "invariant")
  stopifnot(identical(v$status, "time-varying"),
            identical(i$status, "time-invariant"))
})
ok("panel summaries include within/between, missingness, and delta outputs", {
  d <- data.frame(id = rep(1:3, each = 3), time = rep(1:3, 3),
                  y = c(1, 2, 4, 2, NA, 5, 3, 3, 3))
  wb <- panel_within_between_summary(d, "id", "time", "y")
  miss <- panel_missingness_table(d, "time", "y")
  delta <- panel_delta_scores(d, "id", "time", "y")
  stopifnot(all(c("Overall mean", "Between-person SD", "Within-person SD") %in% wb$Measure),
            sum(miss$Missing) == 1,
            "Mean change" %in% delta$Measure,
            nrow(attr(delta, "individual_changes")) == 3)
})
ok("panel and time-series chart helpers render", {
  d <- data.frame(id = rep(1:4, each = 3), time = rep(1:3, 4),
                  y = rnorm(12), group = factor(rep(c("A", "B"), 6)))
  p1 <- build_panel_chart(d, list(x = "time", y = "y", group = "group",
                                  facet = NULL, title = "", subtitle = "",
                                  caption = "", base_size = 13),
                          list(id = "id", time = "time"), TRUE, TRUE)
  p2 <- build_time_series_chart(d, list(x = "time", y = "y", group = NULL,
                                        facet = NULL, title = "", subtitle = "",
                                        caption = "", base_size = 13),
                                list(time = "time"), TRUE)
  stopifnot(inherits(p1, "ggplot"), inherits(p2, "ggplot"))
})

cat("\n== Revised model-tab UI ==\n")
ok("formula sections are not visible in either model tab", {
  regression_html <- htmltools::renderTags(mod_models_ui("models"))$html
  advanced_html <- htmltools::renderTags(mod_phase4_ui("advanced"))$html
  stopifnot(!grepl("Model formula", regression_html, fixed = TRUE),
            !grepl("Analysis formula", advanced_html, fixed = TRUE))
})

report_and_exit()
