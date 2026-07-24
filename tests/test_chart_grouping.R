# tests/test_chart_grouping.R ----------------------------------------------
# Regression guard for the "Colour / group by does not work" fix: the group /
# colour-by variable must be applied as a fill/colour aesthetic across the
# common distribution and bar charts, not just scatter / *_by_group.
#
# Run from the project root:  Rscript tests/test_chart_grouping.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(ggplot2))

SAMPLE <- "data/lvct_cohort_repeated.csv"
dat <- read_any_format(SAMPLE, basename(SAMPLE))
det <- detect_all_types(dat)
tv  <- stats::setNames(det$type, det$variable)
for (nm in names(dat)) dat[[nm]] <- coerce_to_type(dat[[nm]], tv[[nm]])

# Plot-level aesthetic mapping, flattened to text (the fix maps group at the
# ggplot() level, so this is where a group aesthetic will appear).
map_text <- function(p) {
  m <- p$mapping
  if (length(m) == 0) return("")
  paste(vapply(m, rlang::as_label, character(1)), collapse = " | ")
}

renders <- function(p) {
  f <- tempfile(fileext = ".png")
  suppressWarnings(ggplot2::ggsave(f, p, width = 6, height = 4, dpi = 72))
  ok_file <- file.exists(f) && file.info(f)$size > 0
  unlink(f); ok_file
}

# numeric outcome grouped by a categorical
NUM <- "cd4_count"; GRP <- "sex"; CATVAR <- "county"

cat("\n== group / colour-by is applied to distribution charts [GUARD] ==\n")
for (ty in c("histogram", "density", "boxplot", "violin", "qq", "ecdf")) {
  ok(sprintf("[GUARD] %s honours group (maps %s) and renders", ty, GRP), {
    p <- build_chart(dat, list(type = ty, x = NUM, group = GRP))
    stopifnot(grepl(GRP, map_text(p), fixed = TRUE), renders(p))
  })
}

cat("\n== group / colour-by turns a bar into a grouped bar [GUARD] ==\n")
for (ty in c("bar", "pareto")) {
  ok(sprintf("[GUARD] %s honours group (maps %s) and renders", ty, GRP), {
    p <- build_chart(dat, list(type = ty, x = CATVAR, group = GRP))
    stopifnot(grepl(GRP, map_text(p), fixed = TRUE), renders(p))
  })
}

cat("\n== without a group the base chart is unchanged [GUARD] ==\n")
ok("[GUARD] histogram without group does NOT map a group aesthetic", {
  p <- build_chart(dat, list(type = "histogram", x = NUM))
  stopifnot(!grepl(GRP, map_text(p), fixed = TRUE), renders(p))
})
ok("[GUARD] bar without group still renders a single-series bar", {
  p <- build_chart(dat, list(type = "bar", x = CATVAR))
  stopifnot(renders(p))
})

cat("\n== faceting fails safely and never crashes [GUARD] ==\n")
ok("[GUARD] facet on a per-observation chart works without a group", {
  # histogram carries the full data, so it CAN be faceted with no group set.
  p <- build_chart(dat, list(type = "histogram", x = NUM, facet = CATVAR))
  stopifnot(renders(p))
})
ok("[GUARD] facet on a summary bar (no group) renders a safe message, not an error", {
  # plain bar aggregates to totals -> must not raise a raw ggplot facet error.
  p <- build_chart(dat, list(type = "bar", x = CATVAR, facet = GRP))
  stopifnot(renders(p))
})
ok("[GUARD] facet on a grouped bar works (full data retains the facet column)", {
  p <- build_chart(dat, list(type = "bar", x = CATVAR, group = GRP, facet = "regimen"))
  stopifnot(renders(p))
})

report_and_exit()
