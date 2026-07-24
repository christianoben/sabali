# tests/test_descriptives_grouping.R ---------------------------------------
# ISSUE 1: "Group by in descriptives doesn't work."
#
# This suite specifies the COMPLETE intended behaviour of grouped descriptives
# (design doc Section 3: "if a grouping factor is also selected, all of the
# above are produced per group side by side"). It drives the real Shiny module
# with shiny::testServer so it tests the wiring, not just the helpers.
#
# Tests are labelled:
#   [GUARD]  - current-correct behaviour; must stay green.
#   [BUG]    - target behaviour that FAILS on today's code; defines the fix.
#
# Run from the project root:  Rscript tests/test_descriptives_grouping.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# --- Real, fully type-coerced dataset (exactly as mod_data hands it over) ----
raw   <- read_any_format("data/lvct_cohort_repeated.csv", "lvct_cohort_repeated.csv")
det   <- detect_all_types(raw)
types <- stats::setNames(det$type, det$variable)
dat   <- raw
for (nm in names(dat)) dat[[nm]] <- coerce_to_type(dat[[nm]], types[[nm]])

# --- testServer drivers ------------------------------------------------------
# Return the "<var> by <group>" grouped table the module produces for a given
# main variable + group (cross-sectional grouping lives in descriptive_tables()).
grouped_for <- function(var, group, d = dat, tp = types) {
  out <- NULL
  shiny::testServer(mod_descriptives_server,
    args = list(data = reactive(d), types = reactive(tp),
                add_report = function(x) NULL), {
      session$setInputs(var = var, group = group)
      info <- tryCatch(descr(), error = function(e) NULL)
      out <<- if (is.null(info)) NULL else info$tables[[paste(var, "by", group)]]
    })
  out
}

# Capture every item pushed to the report when "Add to report" is clicked.
added_on_add <- function(var, group, d = dat, tp = types) {
  bag <- list()
  add_report <- function(x) bag[[length(bag) + 1]] <<- x
  shiny::testServer(mod_descriptives_server,
    args = list(data = reactive(d), types = reactive(tp), add_report = add_report), {
      session$setInputs(var = var, group = group)
      session$setInputs(add = 1)   # fire observeEvent(input$add)
      session$flushReact()
    })
  bag
}

is_grouped_table <- function(g, min_rows = 2) {
  is.data.frame(g) && nrow(g) >= min_rows
}

# ===========================================================================
cat("\n== A. describe_numeric_by_group(): statistical correctness [GUARD] ==\n")
# ===========================================================================
g_sex <- describe_numeric_by_group(dat$cd4_count, dat$sex)

ok("[GUARD] returns one row per group",
   stopifnot(is.data.frame(g_sex), nrow(g_sex) == nlevels(dat$sex)))
ok("[GUARD] expected columns present",
   stopifnot(all(c("group","n","mean","sd","median","IQR","min","max") %in% names(g_sex))))
ok("[GUARD] per-group n matches tapply(non-missing)", {
  ref <- tapply(dat$cd4_count, dat$sex, function(v) sum(!is.na(v)))
  stopifnot(all(g_sex$n == as.integer(ref[g_sex$group])))
})
ok("[GUARD] per-group mean matches base R (missing excluded)", {
  ref <- tapply(dat$cd4_count, dat$sex, function(v) round(mean(v, na.rm = TRUE), 3))
  stopifnot(all(abs(g_sex$mean - as.numeric(ref[g_sex$group])) < 1e-6))
})
ok("[GUARD] grouped n never counts missing outcome values", {
  # cd4_count has missing values; grouped n must sum to complete cases only.
  stopifnot(sum(g_sex$n) == sum(!is.na(dat$cd4_count[!is.na(dat$sex)])))
})
ok("[GUARD] single-observation group -> sd NA, no crash", {
  x <- c(10, 20, 30, 99); g <- factor(c("a","a","a","b"))
  r <- describe_numeric_by_group(x, g)
  stopifnot(is.data.frame(r), r$n[r$group == "b"] == 1, is.na(r$sd[r$group == "b"]))
})

# ===========================================================================
cat("\n== B. Module wiring: numeric main variable [GUARD] ==\n")
# ===========================================================================
ok("[GUARD] group = (none) -> no grouped table (NULL)",
   stopifnot(is.null(grouped_for("cd4_count", "(none)"))))
ok("[GUARD] numeric main var + factor group -> grouped table",
   stopifnot(is_grouped_table(grouped_for("cd4_count", "county"),
                              min_rows = nlevels(dat$county))))
ok("[GUARD] count type (who_stage) treated as numeric and groups",
   stopifnot(is_grouped_table(grouped_for("who_stage", "sex"))))

# ===========================================================================
cat("\n== C. Module wiring: categorical main variable [BUG] ==\n")
# The design doc requires grouped descriptives for ALL analysable types, not
# just numeric. Today the module returns NULL for binary/nominal/ordinal,
# so selecting a group produces nothing -> this is the reported failure.
# ===========================================================================
ok("[BUG] binary main var (on_art) + group -> non-empty grouped table",
   stopifnot(is_grouped_table(grouped_for("on_art", "county"))))
ok("[BUG] nominal main var (regimen) + group -> non-empty grouped table",
   stopifnot(is_grouped_table(grouped_for("regimen", "county"))))
ok("[BUG] nominal main var (county) grouped by binary (sex) -> non-empty table",
   stopifnot(is_grouped_table(grouped_for("county", "sex"))))

# Ordinal: build a small ordinal dataset (who_stage forced ordinal).
ord_types <- c(stage = "ordinal", arm = "nominal")
ord_df <- data.frame(
  stage = factor(rep(c("I","II","III"), 8), levels = c("I","II","III"), ordered = TRUE),
  arm   = factor(rep(c("A","B"), 12)))
ok("[BUG] ordinal main var + group -> non-empty grouped table",
   stopifnot(is_grouped_table(grouped_for("stage", "arm", d = ord_df, tp = ord_types))))

# ===========================================================================
cat("\n== D. Grouped 'Add to report' behaviour ==\n")
# ===========================================================================
ok("[GUARD] numeric var + group + Add -> main table AND grouped table added", {
  bag <- added_on_add("cd4_count", "county")
  stopifnot(length(bag) == 2,
            grepl("by county", bag[[2]]$caption))
})
ok("[BUG] categorical var + group + Add -> grouped table also added", {
  bag <- added_on_add("regimen", "county")
  stopifnot(length(bag) == 2)   # today only the ungrouped table is added
})

# ===========================================================================
cat("\n== E. Grouping edge conditions ==\n")
# ===========================================================================
ok("[GUARD] missing values in the grouping variable do not crash", {
  d2 <- dat; d2$sex[1:20] <- NA
  g <- grouped_for("cd4_count", "sex", d = d2)
  stopifnot(is_grouped_table(g))
})
ok("[BUG] group has an explicit NA category -> either reported or documented", {
  # Rows whose grouping value is missing are currently dropped silently.
  # Contract: the grouped output must account for them (an NA/'(missing)' row)
  # OR the note must state how many rows were dropped. Fails today (silent).
  d2 <- dat; d2$sex[1:20] <- NA
  g <- grouped_for("cd4_count", "sex", d = d2)
  has_na_row <- any(is.na(g$group) | g$group %in% c("(missing)", "NA"))
  note <- attr(g, "note")
  stopifnot(has_na_row || (!is.null(note) && grepl("missing|dropped", note, ignore.case = TRUE)))
})

report_and_exit()
