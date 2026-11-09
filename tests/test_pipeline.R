# tests/test_pipeline.R -----------------------------------------------------
# Headless end-to-end test of the Phase 1 pipeline. Exercises every layer
# without a browser: parse/source -> import -> type detection -> coercion ->
# descriptives (all types + grouped) -> chart recommendation ->
# EVERY chart type builds & renders -> Word (.docx) report export.
#
# Run from the project root:  Rscript tests/test_pipeline.R

# Locate helper.R relative to this script, so the test runs from any cwd.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

SAMPLE <- file.path(ROOT, "data/lvct_cohort_repeated.csv")

cat("\n== 1. Source all R/ files (parse + eval) ==\n")
if (!isTRUE(getOption("sabali_coverage"))) {
  for (f in list.files("R", pattern = "^[^_].*[.][Rr]$", full.names = TRUE))
    ok(paste("source", basename(f)), source(f, local = FALSE))
} else {
  ok("instrumented source files are already loaded", TRUE)
}

cat("\n== 2. Build the Shiny UI (all modules assemble) ==\n")
ok("navbarPage with all modules", {
  ui <- navbarPage("t", tabPanel("1", mod_data_ui("data")),
                   tabPanel("2", mod_descriptives_ui("desc")),
                   tabPanel("3", mod_charts_ui("charts")),
                   tabPanel("4", mod_tests_ui("tests")),
                   tabPanel("5", mod_report_ui("report")))
  stopifnot(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

cat("\n== 3. Import sample data ==\n")
dat <- NULL
ok("read_any_format(csv)", {
  dat <<- read_any_format(SAMPLE, basename(SAMPLE))
  stopifnot(is.data.frame(dat), nrow(dat) == 750, ncol(dat) == 13)
})
cat("   ", import_summary(dat), "\n")

cat("\n== 4. Detect + coerce types for every column ==\n")
types_df <- NULL
ok("detect_all_types()", {
  types_df <<- detect_all_types(dat)
  stopifnot(nrow(types_df) == ncol(dat))
})
print(types_df)
tv <- setNames(types_df$type, types_df$variable)
dat_c <- dat
ok("coerce_to_type() all columns",
   for (nm in names(dat_c)) dat_c[[nm]] <- coerce_to_type(dat[[nm]], tv[[nm]]))

cat("\n== 5. Descriptives for every measurement type ==\n")
for (nm in names(dat_c))
  ok(sprintf("describe_variable(%s) [%s]", nm, tv[[nm]]),
     { d <- describe_variable(dat_c[[nm]], tv[[nm]]); stopifnot(is.data.frame(d)) })
ok("describe_numeric_by_group(cd4 by county)",
   { g <- describe_numeric_by_group(dat_c$cd4_count, dat_c$county); stopifnot(is.data.frame(g)) })

cat("\n== 6. Chart recommendations ==\n")
ok("recommend 1 numeric",     stopifnot(recommend_charts("continuous")$default == "histogram"))
ok("recommend 1 categorical", stopifnot(recommend_charts("nominal")$default == "bar"))
ok("recommend 2 numeric",     stopifnot(recommend_charts(c("continuous","continuous"))$default == "scatter"))
ok("recommend num+cat",       stopifnot(recommend_charts(c("continuous","nominal"))$default == "box_by_group"))
ok("recommend 2 cat",         stopifnot(recommend_charts(c("nominal","binary"))$default == "grouped_bar"))
ok("recommend 3+ numeric",    stopifnot(recommend_charts(rep("continuous",3))$default == "corr_heatmap"))

cat("\n== 7. Build & render EVERY chart type ==\n")
render <- function(p) { f <- tempfile(fileext = ".png")
  suppressWarnings(ggplot2::ggsave(f, p, width = 6, height = 4, dpi = 72))
  stopifnot(file.exists(f), file.info(f)$size > 0); unlink(f) }

num <- "cd4_count"; num2 <- "viral_load"; cat1 <- "county"; grp <- "sex"

for (ty in c("histogram","density","boxplot","violin","raincloud","qq","ecdf"))
  ok(sprintf("chart %s", ty), render(build_chart(dat_c, list(type = ty, x = num))))

for (ty in c("bar","pareto","pie"))
  ok(sprintf("chart %s", ty), render(build_chart(dat_c, list(type = ty, x = cat1))))

ok("chart scatter",   render(build_chart(dat_c, list(type = "scatter", x = num, y = num2, group = grp))))
ok("chart density2d", render(build_chart(dat_c, list(type = "density2d", x = num, y = num2))))

for (ty in c("box_by_group","violin_by_group","raincloud_by_group","mean_ci_bar","jitter"))
  ok(sprintf("chart %s", ty), render(build_chart(dat_c, list(type = ty, x = num, group = grp))))

for (ty in c("grouped_bar","stacked_bar","mosaic","heatmap"))
  ok(sprintf("chart %s", ty), render(build_chart(dat_c, list(type = ty, x = cat1, group = grp))))

agg <- aggregate(cd4_count ~ visit_date, data = dat_c, FUN = mean)
agg$visit_date <- as.Date(agg$visit_date)
ok("chart line",      render(build_chart(agg, list(type = "line", x = "visit_date", y = "cd4_count"))))
ok("chart run_chart", render(build_chart(agg, list(type = "run_chart", x = "visit_date", y = "cd4_count"))))

ok("chart corr_heatmap",   render(build_chart(dat_c, list(type = "corr_heatmap"))))
ok("chart scatter_matrix", render(build_chart(dat_c[, c("cd4_count","viral_load","age_years")],
                                              list(type = "scatter_matrix"))))

cat("\n== 8. Word report export ==\n")
ok("export_word_report() populated", {
  tbl <- describe_variable(dat_c$cd4_count, "continuous")
  plt <- suppressWarnings(build_chart(dat_c, list(type = "histogram", x = num, title = "CD4")))
  out <- tempfile(fileext = ".docx")
  export_word_report(
    items = list(list(kind = "table", caption = "CD4 summary", data = tbl),
                 list(kind = "plot",  caption = "CD4 histogram", plot = plt)),
    file = out, title = "Test Report", subtitle = "sample")
  stopifnot(file.exists(out), file.info(out)$size > 0)
})
ok("export_word_report() empty",
   { out <- tempfile(fileext = ".docx"); export_word_report(list(), out); stopifnot(file.exists(out)) })

report_and_exit()
