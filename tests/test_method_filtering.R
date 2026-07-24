# tests/test_method_filtering.R --------------------------------------------
# ISSUE 2: "Filtration of variables / available methods based on the type of
# variables you choose and the conditions you choose is not wired in for all
# tabs."
#
# The Descriptives tab already filters (analysable / grouping vars). This suite
# specifies the SAME discipline for the Chart Studio and the Hypothesis-testing
# tab: the methods offered, and the variables offered to each role, must be
# restricted to those valid for the chosen types and conditions
# (design doc Sections 4 & 5).
#
# Tests are labelled:
#   [GUARD]  - current-correct behaviour; must stay green.
#   [BUG]    - target behaviour that FAILS on today's code; defines the fix.
#
# Run from the project root:  Rscript tests/test_method_filtering.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# Real, fully type-coerced dataset (exactly as mod_data hands it over).
raw   <- read_any_format("data/lvct_cohort_repeated.csv", "lvct_cohort_repeated.csv")
det   <- detect_all_types(raw)
types <- stats::setNames(det$type, det$variable)
dat   <- raw
for (nm in names(dat)) dat[[nm]] <- coerce_to_type(dat[[nm]], types[[nm]])

# Numeric-only-ish / categorical-only datasets for feasibility gating.
df_one_numeric <- data.frame(
  grp = factor(rep(c("a","b"), 20)),
  cat = factor(rep(c("x","y","z","w"), 10)),
  num = rnorm(40))
types_one_numeric <- c(grp = "binary", cat = "nominal", num = "continuous")

df_cats_only <- data.frame(
  a = factor(rep(c("p","q"), 20)),
  b = factor(rep(c("r","s","t","u"), 10)))
types_cats_only <- c(a = "binary", b = "nominal")

# ===========================================================================
# PART 1 - CHART STUDIO: the chart-type dropdown must be filtered by type
# ===========================================================================

# Drive the chart module and return the chart-type keys actually offered in the
# "type" dropdown, plus the recommended option set (the intended allowed set).
chart_probe <- function(vars, d = dat, tp = types) {
  res <- list(offered = NULL, reco = NULL)
  shiny::testServer(mod_charts_server,
    args = list(data = reactive(d), types = reactive(tp),
                add_report = function(x) NULL), {
      real <- .subset2(session, "parent")
      cap <- new.env()
      real$sendInputMessage <- function(inputId, message) cap[[inputId]] <- message
      session$setInputs(vars = vars)
      session$flushReact()
      html <- cap[["type"]]$options
      res$offered <<- regmatches(html,
        gregexpr("(?<=value=\")[^\"]+", html, perl = TRUE))[[1]]
      res$reco <<- reco()$options
    })
  res
}

cat("\n== 1a. recommend_charts() is the source of truth [GUARD] ==\n")
ok("[GUARD] 1 numeric -> histogram default, 7 distribution charts",
   stopifnot(recommend_charts("continuous")$default == "histogram",
             setequal(recommend_charts("continuous")$options,
                      c("histogram","density","boxplot","violin","raincloud","qq","ecdf"))))
ok("[GUARD] 1 categorical -> bar/pareto/pie only",
   stopifnot(setequal(recommend_charts("nominal")$options, c("bar","pareto","pie"))))
ok("[GUARD] 2 categorical -> grouped/stacked/mosaic/heatmap",
   stopifnot(setequal(recommend_charts(c("nominal","binary"))$options,
                      c("grouped_bar","stacked_bar","mosaic","heatmap"))))
ok("[GUARD] num + cat -> box/violin/raincloud/mean_ci/jitter",
   stopifnot(recommend_charts(c("continuous","nominal"))$default == "box_by_group"))

cat("\n== 1b. Dropdown must offer ONLY the type-appropriate charts [BUG] ==\n")
# Today the module appends setdiff(all_types, opts) -> all 25 charts are always
# offered, so a single categorical variable can be sent to a scatter/Q-Q/etc.
NUMERIC_ONLY <- c("histogram","density","boxplot","violin","raincloud","qq",
                  "ecdf","scatter","density2d","corr_heatmap","scatter_matrix")
CATEG_ONLY   <- c("bar","pareto","pie","grouped_bar","stacked_bar","mosaic","heatmap")

p_cat <- chart_probe("county")
ok("[BUG] single categorical: dropdown == recommended set (not all 25)",
   stopifnot(setequal(p_cat$offered, p_cat$reco)))
ok("[BUG] single categorical: no numeric-only charts offered",
   stopifnot(length(intersect(p_cat$offered, NUMERIC_ONLY)) == 0))

p_num <- chart_probe("age_years")
ok("[BUG] single numeric: no categorical-only charts offered",
   stopifnot(length(intersect(p_num$offered, CATEG_ONLY)) == 0))
ok("[BUG] single numeric: dropdown == recommended set",
   stopifnot(setequal(p_num$offered, p_num$reco)))

p_2cat <- chart_probe(c("sex","county"))
ok("[BUG] two categoricals: no scatter/histogram/qq offered",
   stopifnot(length(intersect(p_2cat$offered, c("scatter","histogram","qq"))) == 0))

p_mix <- chart_probe(c("cd4_count","county"))
ok("[BUG] numeric + categorical: dropdown == recommended set",
   stopifnot(setequal(p_mix$offered, p_mix$reco)))

cat("\n== 1c. Recommended default is still correct today [GUARD] ==\n")
ok("[GUARD] single categorical default is a bar chart",
   stopifnot(chart_probe("county")$reco[1] == "bar"))

# ===========================================================================
# PART 2 - HYPOTHESIS TESTING: family + role filtering by type and condition
# ===========================================================================

# Read the tests module's internal choice-source reactives.
tests_probe <- function(family, d = dat, tp = types, extra = list()) {
  res <- list()
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(d), types = reactive(tp),
                add_report = function(x) NULL), {
      do.call(session$setInputs, c(list(family = family), extra))
      session$flushReact()
      res$num <<- num_vars()
      res$cat <<- cat_vars()
      res$id  <<- id_vars()
    })
  res
}

cat("\n== 2a. Role variable sources on the tests tab [GUARD] ==\n")
pr <- tests_probe("location")
ok("[GUARD] numeric-outcome source lists only numeric types",
   stopifnot(all(vapply(types[pr$num], is_numeric_type, logical(1)))))
ok("[GUARD] grouping source lists only categorical types",
   stopifnot(all(vapply(types[pr$cat], is_categorical_type, logical(1)))))

cat("\n== 2b. Valid specs run end-to-end [GUARD] ==\n")
ok("[GUARD] two independent groups (sex, 2 levels) runs",
   stopifnot(is.null(run_guided_test(dat,
     list(family="location", outcome="cd4_count", design="two_independent", group="sex"))$error)))
ok("[GUARD] >2 groups (county, 5 levels) runs as multi",
   stopifnot(!is.null(run_guided_test(dat,
     list(family="location", outcome="cd4_count", design="multi_independent", group="county"))$primary)))
ok("[GUARD] correlation of two distinct numerics runs",
   stopifnot(!is.null(run_guided_test(dat,
     list(family="correlation", x="age_years", y="cd4_count"))$primary)))
ok("[GUARD] categorical association of two distinct factors runs",
   stopifnot(!is.null(run_guided_test(dat,
     list(family="categorical", x="sex", y="county", paired=FALSE))$primary)))

cat("\n== 2c. Family feasibility gating by available types [BUG] ==\n")
# Target helper the fix should provide: available_test_families(types) returns
# the subset of c('location','categorical','correlation') the data supports.
#   location    : needs >=1 numeric (outcome)
#   correlation : needs >=2 numeric
#   categorical : needs >=2 categorical
ok("[BUG] available_test_families() exists and gates correlation (needs 2 numerics)", {
  fam <- available_test_families(types_one_numeric)   # only 1 numeric
  stopifnot(!"correlation" %in% fam, "location" %in% fam, "categorical" %in% fam)
})
ok("[BUG] categorical-only data -> only 'categorical' family offered", {
  fam <- available_test_families(types_cats_only)
  stopifnot(setequal(fam, "categorical"))
})
ok("[BUG] full dataset -> all three families offered", {
  fam <- available_test_families(types)
  stopifnot(setequal(fam, c("location","categorical","correlation")))
})

cat("\n== 2d. Grouping-variable choices gated by level count [BUG] ==\n")
# Target helper: valid_grouping_vars(types, df, design)
#   two_independent   -> categorical vars with EXACTLY 2 levels
#   multi_independent -> categorical vars with >=3 levels
ok("[BUG] two_independent grouping choices are 2-level factors only", {
  gv <- valid_grouping_vars(types, dat, "two_independent")
  stopifnot(setequal(gv, c("sex","on_art")))       # not county/regimen/visit
})
ok("[BUG] multi_independent grouping choices are >=3-level factors only", {
  gv <- valid_grouping_vars(types, dat, "multi_independent")
  stopifnot(setequal(gv, c("visit","county","regimen")))
})

cat("\n== 2e. Condition-based guards on the chosen roles [BUG] ==\n")
ok("[BUG] two_independent with a >2-level group is refused with a clear message", {
  e <- tryCatch(run_guided_test(dat,
    list(family="location", outcome="cd4_count", design="two_independent", group="county")),
    error = function(e) conditionMessage(e))
  # today: raw "nlevels(d$g) == 2 is not TRUE"; target: human-readable + no raw assert
  stopifnot(is.character(e), !grepl("is not TRUE|nlevels", e),
            grepl("group|level|two", e, ignore.case = TRUE))
})
ok("[BUG] correlation refuses x == y", {
  e <- tryCatch(run_guided_test(dat, list(family="correlation", x="age_years", y="age_years")),
                error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("same|itself|different|distinct", e, ignore.case = TRUE))
})
ok("[BUG] categorical association refuses x == y", {
  e <- tryCatch(run_guided_test(dat, list(family="categorical", x="county", y="county", paired=FALSE)),
                error = function(e) conditionMessage(e))
  stopifnot(is.character(e), grepl("same|itself|different|distinct", e, ignore.case = TRUE))
})
ok("[BUG] paired/repeated subject-ID choices exclude the chosen outcome", {
  # id_vars() currently returns every column, including numeric outcomes.
  # Contract: a subject ID must not be selectable as the outcome too.
  sid <- valid_subject_vars(types, outcome = "cd4_count")
  stopifnot(!"cd4_count" %in% sid)
})

cat("\n== 2f. Posthoc results are wired into the Tests tab ==\n")
ok("multi-group run renders posthoc HTML and adds it to the report", {
  bag <- list(); rendered <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(dat), types = reactive(types),
                add_report = function(x) bag[[length(bag) + 1]] <<- x), {
      session$setInputs(family = "location", outcome = "cd4_count",
                        design = "multi_independent", group = "county")
      session$flushReact()
      session$setInputs(run = 1)
      session$flushReact()
      rendered <<- output$posthoc_tbl
      session$setInputs(add = 1)
      session$flushReact()
    })
  stopifnot(is.character(rendered), grepl("p-adjusted", rendered, fixed = TRUE),
            length(bag) == 3, grepl("Posthoc", bag[[3]]$caption))
})

report_and_exit()
