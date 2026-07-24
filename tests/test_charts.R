# tests/test_charts.R -------------------------------------------------------
# assign_chart_roles() variable->x/y/group mapping (Gap 4) and palette-branch
# coverage generalising the heatmap fix (Gap 5).
#
# Run from the project root:  Rscript tests/test_charts.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== assign_chart_roles() ==\n")
# Named type vector, as the module supplies it.
types <- c(a = "continuous", b = "continuous", g = "nominal", h = "binary")

ok("scatter -> two numerics as x and y", {
  r <- assign_chart_roles(c("a", "b"), types, "scatter")
  stopifnot(r$x %in% c("a","b"), r$y %in% c("a","b"), r$x != r$y)
})
ok("box_by_group -> numeric x, categorical group", {
  r <- assign_chart_roles(c("a", "g"), types, "box_by_group")
  stopifnot(identical(r$x, "a"), identical(r$group, "g"))
})
ok("grouped_bar -> two categoricals as x and group", {
  r <- assign_chart_roles(c("g", "h"), types, "grouped_bar")
  stopifnot(r$x %in% c("g","h"), r$group %in% c("g","h"), r$x != r$group)
})
ok("bar -> categorical on x", {
  r <- assign_chart_roles(c("g"), types, "bar")
  stopifnot(identical(r$x, "g"))
})

cat("\n== palette branches of .apply_palette (Gap 5) ==\n")
sdf <- data.frame(
  cat = factor(rep(c("p", "q", "r"), 10)),
  n1  = as.numeric(rep(1:10, 3)),
  n2  = as.numeric(rep(10:1, 3))
)

has_fill_scale <- function(p)
  any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))

# Render while capturing errors and the two warnings the fix concerns.
render_clean <- function(p) {
  warns <- character(0); err <- NULL
  withCallingHandlers(
    tryCatch({
      f <- tempfile(fileext = ".png")
      ggplot2::ggsave(f, p, width = 5, height = 3.5, dpi = 72)
      stopifnot(file.exists(f) && file.info(f)$size > 0); unlink(f)
    }, error = function(e) err <<- conditionMessage(e)),
    warning = function(w) { warns[[length(warns)+1]] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  bad <- grep("already present|discrete scale", warns, value = TRUE)
  is.null(err) && length(bad) == 0
}

# Discrete-fill chart: SHOULD receive a fill scale, on both palette branches.
ok("discrete bar (manual palette) gets a fill scale & renders", {
  p <- build_chart(sdf, list(type = "bar", x = "cat", palette = "Default"))
  stopifnot(has_fill_scale(p), render_clean(p))
})
ok("discrete bar (Viridis / NULL-palette branch) gets a fill scale & renders", {
  p <- build_chart(sdf, list(type = "bar", x = "cat", palette = "Viridis"))
  stopifnot(has_fill_scale(p), render_clean(p))
})

# Continuous-fill chart: must NOT get a discrete scale, on either palette.
ok("corr_heatmap (manual palette) renders with no discrete-scale conflict", {
  p <- build_chart(sdf[, c("n1","n2")], list(type = "corr_heatmap", palette = "Default"))
  stopifnot(render_clean(p))
})
ok("corr_heatmap (Viridis) renders with no discrete-scale conflict", {
  p <- build_chart(sdf[, c("n1","n2")], list(type = "corr_heatmap", palette = "Viridis"))
  stopifnot(render_clean(p))
})

report_and_exit()
