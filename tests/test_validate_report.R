# Golden structural test for the Word report output.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== Word report golden structure ==\n")
ok("empty report has a stable title and empty-state paragraph", {
  f <- tempfile(fileext = ".docx")
  export_word_report(list(), f, title = "Golden title", subtitle = "Golden source")
  stopifnot(file.exists(f), file.info(f)$size > 0)
  s <- officer::docx_summary(officer::read_docx(f))
  txt <- paste(s$text, collapse = " ")
  stopifnot(grepl("Golden title", txt, fixed = TRUE),
            grepl("Golden source", txt, fixed = TRUE),
            grepl("No items were added", txt, fixed = TRUE))
  unlink(f, force = TRUE)
})
ok("table and plot report contain captions, table text, and embedded media", {
  f <- tempfile(fileext = ".docx")
  p <- ggplot2::ggplot(data.frame(x = 1:4, y = c(2, 4, 3, 5)),
                       ggplot2::aes(x, y)) + ggplot2::geom_point()
  items <- list(
    list(kind = "table", caption = "Golden table", data = data.frame(
      Field = c("n", "mean"), Value = c("4", "3.5"))),
    list(kind = "plot", caption = "Golden plot", plot = p))
  export_word_report(items, f, title = "Structure", subtitle = "Fixture")
  s <- officer::docx_summary(officer::read_docx(f))
  txt <- paste(s$text, collapse = " ")
  media <- utils::unzip(f, list = TRUE)$Name
  stopifnot(grepl("Golden table", txt, fixed = TRUE),
            grepl("Golden plot", txt, fixed = TRUE),
            grepl("mean", txt, fixed = TRUE),
            any(grepl("^word/media/", media)))
  unlink(f, force = TRUE)
})
ok("report output rejects no-data table structure only through normal rendering", {
  f <- tempfile(fileext = ".docx")
  export_word_report(list(list(kind = "table", caption = "Empty",
                               data = data.frame(Field = character(0), Value = character(0)))), f)
  stopifnot(file.exists(f), file.info(f)$size > 0)
  unlink(f, force = TRUE)
})

report_and_exit()
