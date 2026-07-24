# tests/test_heatmaps.R -----------------------------------------------------
# Regression test for the continuous-fill chart types (heatmap, corr_heatmap).
#
# These two charts map a *continuous* value (cell count / correlation r) to
# fill and define their own gradient scale. A discrete palette scale must NOT
# be layered on top. This test confirms that both charts render AND that
# neither the "discrete scale" error nor the "Scale for fill is already
# present" warning is emitted.
#
# Run from the project root:  Rscript tests/test_heatmaps.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

SAMPLE <- "data/lvct_cohort_repeated.csv"

dat <- read_any_format(SAMPLE, basename(SAMPLE))
tv  <- setNames(detect_all_types(dat)$type, names(dat))
for (nm in names(dat)) dat[[nm]] <- coerce_to_type(dat[[nm]], tv[[nm]])

# Render to PNG while collecting every warning and any error.
check <- function(label, spec) {
  warns <- character(0); err <- NULL
  withCallingHandlers(
    tryCatch({
      p <- build_chart(dat, spec)
      f <- tempfile(fileext = ".png")
      ggplot2::ggsave(f, p, width = 6, height = 4, dpi = 72)
      stopifnot(file.exists(f) && file.info(f)$size > 0); unlink(f)
    }, error = function(e) err <<- conditionMessage(e)),
    warning = function(w) { warns[[length(warns) + 1]] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  bad <- grep("already present|discrete scale", warns, value = TRUE)
  drew <- is.null(err)
  clean <- drew && length(bad) == 0

  cat(sprintf("\n[%s]\n", label))
  cat("  rendered           :", if (drew) "YES" else paste("NO ->", err), "\n")
  cat("  offending warnings :", if (length(bad)) paste(bad, collapse = " | ") else "NONE", "\n")
  ok(label, stopifnot(clean))
  invisible(clean)
}

check("Contingency heatmap renders with no discrete-scale conflict",
      list(type = "heatmap", x = "county", group = "sex"))
check("Correlation heatmap renders with no discrete-scale conflict",
      list(type = "corr_heatmap"))

report_and_exit()
