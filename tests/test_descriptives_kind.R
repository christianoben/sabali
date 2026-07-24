# tests/test_descriptives_kind.R --------------------------------------------
# The descriptives shown must match the declared dataset KIND:
#   * cross-sectional -> the plain type-appropriate table (no within/between);
#   * longitudinal    -> per-subject summary (time-invariant) OR panel
#                        decomposition (time-varying); never a pooled table;
#   * time series     -> a time-series summary; never a pooled iid-style table.
# Driven by descriptive_tables() so the display / report / interpreter agree.
#
# Run:  Rscript tests/test_descriptives_kind.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))
tbl_names <- function(info) names(info$tables)
has_pooled <- function(info) any(grepl("^Descriptives -", tbl_names(info)))

set.seed(1); ns <- 40
long <- data.frame(
  id    = rep(seq_len(ns), each = 3),
  visit = rep(c("v1", "v2", "v3"), ns),
  sex   = rep(sample(c("F", "M"), ns, TRUE), each = 3),   # time-invariant
  y     = rnorm(ns * 3, 10, 3))                            # time-varying
lpanel <- list(kind = "repeated", id = "id", time = "visit")

# ===========================================================================
cat("\n== Cross-sectional: plain table only, no within/between ==\n")
cs <- descriptive_tables(long, "y", "continuous", list(kind = "cross_sectional"))
ok("cross-sectional shows the pooled type table", {
  stopifnot(cs$kind == "cross_sectional", has_pooled(cs),
            "n valid" %in% cs$tables[["Descriptives - y"]]$statistic)
})
ok("cross-sectional shows NO within/between or by-time tables", {
  stopifnot(!any(grepl("within|between|time", tbl_names(cs), ignore.case = TRUE)))
})
ok("cross-sectional grouping adds a '<var> by <group>' table", {
  g <- descriptive_tables(long, "y", "continuous", list(kind = "cross_sectional"),
                          group = "sex")
  stopifnot("y by sex" %in% tbl_names(g))
})

# ===========================================================================
cat("\n== Longitudinal, time-varying: panel decomposition, NO pooled table ==\n")
lv <- descriptive_tables(long, "y", "continuous", lpanel)
ok("status detected as time-varying", stopifnot(identical(lv$status, "time-varying")))
ok("NO pooled across-rows 'Descriptives -' table", stopifnot(!has_pooled(lv)))
ok("shows structure, by-time, within/between, change, missingness", {
  stopifnot(all(c("Observation structure", "Summary by time point",
                  "Within- and between-person variation",
                  "Baseline-to-follow-up change",
                  "Missingness / attrition by time") %in% tbl_names(lv)))
})
ok("within/between ICC matches a manual variance-component estimate", {
  wb <- lv$tables[["Within- and between-person variation"]]
  pm <- tapply(long$y, long$id, mean)
  b <- stats::sd(pm); w <- stats::sd(long$y - pm[as.character(long$id)])
  icc <- b^2 / (b^2 + w^2)
  got <- wb$Value[wb$Measure == "Intraclass correlation (ICC)"]
  stopifnot(abs(as.numeric(got) - icc) < 1e-6)
})

# ===========================================================================
cat("\n== Longitudinal, time-invariant: ONE row per subject ==\n")
li <- descriptive_tables(long, "sex", "binary", lpanel)
ok("status detected as time-invariant", stopifnot(identical(li$status, "time-invariant")))
ok("summarised per subject, not pooled across rows", {
  stopifnot(!has_pooled(li), length(tbl_names(li)) == 1,
            grepl("Per-subject", tbl_names(li)))
  tab <- li$tables[[1]]
  # binary summary counts must sum to the number of SUBJECTS (40), not rows (120)
  stopifnot(sum(tab$count) == ns)
})
ok("subject_level_values returns one value per subject", {
  v <- subject_level_values(long, "id", "sex")
  stopifnot(length(v) == ns)
})

# ===========================================================================
cat("\n== Time series: time-series summary, NO pooled table ==\n")
set.seed(2)
ts <- data.frame(t = 1:50, val = cumsum(rnorm(50)))
tsr <- descriptive_tables(ts, "val", "continuous", list(kind = "time_series", time = "t"))
ok("time series shows a time-series summary, not a pooled table", {
  stopifnot(!has_pooled(tsr), "Time-series summary" %in% tbl_names(tsr),
            "Missingness by time" %in% tbl_names(tsr))
})
ok("time-series net change = last - first value", {
  s <- tsr$tables[["Time-series summary"]]
  net <- as.numeric(s$value[s$statistic == "net change (last - first)"])
  stopifnot(abs(net - (ts$val[50] - ts$val[1])) < 1e-3)   # values rounded to 4 dp
})

# ===========================================================================
cat("\n== Incomplete longitudinal roles fall back to a plain table ==\n")
ok("repeated chosen but no id/time yet -> cross-sectional table (no crash)", {
  info <- descriptive_tables(long, "y", "continuous", list(kind = "repeated"))
  stopifnot(info$kind == "cross_sectional", has_pooled(info))
})

# ===========================================================================
cat("\n== Module wiring: content follows the declared kind ==\n")
ok("mod_descriptives shows longitudinal content (no pooled table) for repeated data", {
  headline <- NULL; names_seen <- NULL; k <- NULL
  shiny::testServer(mod_descriptives_server,
    args = list(data = reactive(long),
                types = reactive(c(id = "identifier", visit = "nominal",
                                   sex = "binary", y = "continuous")),
                panel = reactive(lpanel), add_report = function(x) NULL), {
      session$setInputs(var = "y")
      session$flushReact()
      info <- descr()
      headline <<- info$headline; names_seen <<- names(info$tables); k <<- kind()
    })
  stopifnot(identical(k, "repeated"), grepl("Longitudinal", headline),
            !any(grepl("^Descriptives -", names_seen)),
            "Within- and between-person variation" %in% names_seen)
})
ok("mod_descriptives hides the Group-by control for longitudinal data", {
  gui <- NULL
  shiny::testServer(mod_descriptives_server,
    args = list(data = reactive(long),
                types = reactive(c(id = "identifier", visit = "nominal",
                                   sex = "binary", y = "continuous")),
                panel = reactive(lpanel), add_report = function(x) NULL), {
      session$setInputs(var = "y"); session$flushReact()
      gui <<- output$group_ui
    })
  stopifnot(is.null(gui) || !nzchar(as.character(gui)) ||
            !grepl("Group by", as.character(gui)))
})

report_and_exit()
