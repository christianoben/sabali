# tests/test_report_publication.R -------------------------------------------
# Publication-ready report layer: table trimming (publication_table_df),
# prose write-up rendering beneath tables/figures, the two-pass AI write-up
# entry point, chart data-labels, and chart summary tables that feed the AI.
#
# Run:  Rscript tests/test_report_publication.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# --- publication_table_df: paper-style trimming -----------------------------
cat("\n== publication_table_df trimming ==\n")

ok("drops the baked-in Interpretation row from Field/Value test tables", {
  tb <- data.frame(Field = c("Test", "statistic", "p-value", "Interpretation"),
                   Value = c("t-test", "2.1", "0.04", "means differ"),
                   stringsAsFactors = FALSE)
  out <- publication_table_df(tb)
  stopifnot(!("Interpretation" %in% out$Field),
            all(c("Test", "statistic", "p-value") %in% out$Field))
})

ok("drops Std. Error and Statistic columns from coefficient tables", {
  co <- data.frame(Term = c("(Intercept)", "x"),
                   Estimate = c(1.2, 0.5), `Std. Error` = c(0.3, 0.1),
                   Statistic = c(4, 5), `p-value` = c(0.001, 0.2),
                   `CI lower` = c(0.6, 0.3), `CI upper` = c(1.8, 0.7),
                   check.names = FALSE)
  out <- publication_table_df(co)
  stopifnot(!("Std. Error" %in% names(out)), !("Statistic" %in% names(out)),
            all(c("Term", "Estimate", "CI lower", "CI upper") %in% names(out)))
})

ok("drops redundant log-scale Estimate/CI when an exponentiated effect is present", {
  co <- data.frame(Term = "x", Estimate = 0.7, `Std. Error` = 0.1,
                   Statistic = 7, `p-value` = 0.01,
                   `CI lower` = 0.5, `CI upper` = 0.9,
                   `Odds ratio` = 2.01, `Odds ratio CI lower` = 1.6,
                   `Odds ratio CI upper` = 2.5, check.names = FALSE)
  out <- publication_table_df(co)
  stopifnot(!("Estimate" %in% names(out)), !("CI lower" %in% names(out)),
            !("CI upper" %in% names(out)),
            all(c("Odds ratio", "Odds ratio CI lower") %in% names(out)))
})

ok("formats p-values via p_fmt and keeps counts as integers", {
  d <- data.frame(Metric = c("n used", "p-value"), value = c(320, 0.0004),
                  check.names = FALSE)
  # emulate the two-column-with-p shape used across the app
  co <- data.frame(Term = "x", Estimate = 0.123456, `p-value` = 0.0004,
                   `n used` = 320, check.names = FALSE)
  out <- publication_table_df(co)
  stopifnot(identical(out[["p-value"]], "< 0.001"),
            identical(out[["n used"]], "320"),
            identical(out[["Estimate"]], "0.12"))
})

# --- write-up rendering beneath the table -----------------------------------
cat("\n== write-up prose in the report ==\n")

ok("an item's write-up is rendered as prose beneath its table", {
  f <- tempfile(fileext = ".docx")
  items <- list(list(kind = "table", caption = "Golden table",
                     data = data.frame(Field = "n", Value = "4"),
                     writeup = "The sample comprised four participants."))
  export_word_report(items, f, title = "WU", subtitle = "Fixture")
  s <- officer::docx_summary(officer::read_docx(f))
  txt <- paste(s$text, collapse = " ")
  stopifnot(grepl("The sample comprised four participants", txt, fixed = TRUE),
            grepl("AI-generated summary", txt, fixed = TRUE))
  unlink(f, force = TRUE)
})

# --- two-pass AI write-up: AI-only, no offline fallback ---------------------
cat("\n== ai_report_writeup (AI-only) ==\n")
Sys.unsetenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
               "AI_INTERPRET_MODEL", "AI_INTERPRET_THINKING", "OPENCODE_API_KEY"))

ok("ai_report_writeup ERRORS when no AI endpoint is configured", {
  err <- tryCatch({ ai_report_writeup(list(T = data.frame(a = 1)),
                                      context = list(title = "L")); NA },
                  error = function(e) conditionMessage(e))
  stopifnot(is.character(err), grepl("no AI endpoint", err, ignore.case = TRUE))
})

ok("report_finalise_writeups is a no-op when AI is unconfigured", {
  items <- list(list(kind = "table", data = data.frame(a = 1),
                     ai = list(tables = list(T = data.frame(a = 1)),
                               context = list(title = "L"))))
  out <- report_finalise_writeups(items)
  stopifnot(is.null(out[[1]]$writeup))
})

ok("interp_reuse_text reuses only a same-analysis interpretation", {
  pre <- list(text = "cached", payload = list(context = list(title = "Model A")))
  stopifnot(identical(interp_reuse_text(pre, list(title = "Model A")), "cached"),
            is.null(interp_reuse_text(pre, list(title = "Model B"))),
            is.null(interp_reuse_text(NULL, list(title = "Model A"))))
})

ok("the 'writeup' async job kind is wired and stays AI-only", {
  out <- ai_worker_dispatch("writeup",
    list(tables = list(T = data.frame(a = 1)), context = list(title = "L")),
    ctx = list())
  stopifnot(is.list(out), isFALSE(out$ok),
            grepl("no AI endpoint", out$error, ignore.case = TRUE))
})

# --- chart data labels ------------------------------------------------------
cat("\n== chart data labels ==\n")
df <- data.frame(
  grp = factor(rep(c("A", "B", "C"), c(5, 3, 2))),
  val = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  yr  = 1:10)

label_layers <- function(p)
  sum(vapply(p$layers, function(l) inherits(l$geom, "GeomText") ||
             inherits(l$geom, "GeomLabel"), logical(1)))

ok("count bar gains a data-label layer when data_labels = TRUE", {
  s <- default_chart_spec(); s$type <- "bar"; s$x <- "grp"
  p <- build_chart(df, s)
  stopifnot(label_layers(p) >= 1)
  ggplot2::ggplot_build(p)   # renders without error
})

ok("count bar has no data-label layer when data_labels = FALSE", {
  s <- default_chart_spec(); s$type <- "bar"; s$x <- "grp"; s$data_labels <- FALSE
  stopifnot(label_layers(build_chart(df, s)) == 0)
})

ok("pie renders with labels (regression guard for captured plot data)", {
  s <- default_chart_spec(); s$type <- "pie"; s$x <- "grp"
  p <- build_chart(df, s)
  stopifnot(label_layers(p) >= 1)
  ggplot2::ggplot_build(p)
})

ok("line chart gains value labels and renders", {
  s <- default_chart_spec(); s$type <- "line"; s$x <- "yr"; s$y <- "val"
  p <- build_chart(df, s)
  stopifnot(label_layers(p) >= 1)
  ggplot2::ggplot_build(p)
})

# --- chart summary tables (aggregate that feeds the AI) ---------------------
cat("\n== chart_summary_tables ==\n")

ok("categorical chart summarises counts and percentages", {
  s <- default_chart_spec(); s$type <- "bar"; s$x <- "grp"
  tb <- chart_summary_tables(df, s)
  stopifnot(length(tb) >= 1)
  first <- tb[[1]]
  stopifnot(all(c("count", "percent") %in% names(first)), sum(first$count) == 10)
})

ok("scatter summarises n and a correlation, never raw rows", {
  s <- default_chart_spec(); s$type <- "scatter"; s$x <- "yr"; s$y <- "val"
  tb <- chart_summary_tables(df, s)
  flat <- do.call(rbind, lapply(tb, function(d) if ("statistic" %in% names(d)) d else NULL))
  stopifnot(!is.null(flat), "Pearson r" %in% flat$statistic)
})

ok("chart_summary_tables never throws on odd input", {
  s <- default_chart_spec(); s$type <- "bar"; s$x <- "does_not_exist"
  stopifnot(is.list(chart_summary_tables(df, s)))
})

report_and_exit()
