# fct_report.R --------------------------------------------------------------
# One-click Word (.docx) export of collected tables and charts, using {officer}
# for the document and {flextable} for nicely formatted tables.
#
# The report is assembled from a list of "items". Each item is a list:
#   list(kind = "table", caption = "...", data = <data.frame>)
#   list(kind = "plot",  caption = "...", plot = <ggplot>, spec = <chart spec>)

#' Export a Word report from collected report items.
#'
#' @param items    List of report items (see above).
#' @param file     Output .docx path.
#' @param title    Report title.
#' @param subtitle Optional subtitle / source line.
export_word_report <- function(items, file, title = "Statistics Report",
                               subtitle = NULL) {
  stopifnot(requireNamespace("officer", quietly = TRUE))
  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle))
    doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  doc <- officer::body_add_par(
    doc, paste("Generated", format(Sys.time(), "%d %b %Y %H:%M")),
    style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  if (length(items) == 0) {
    doc <- officer::body_add_par(doc, "No items were added to the report.",
                                 style = "Normal")
    print(doc, target = file)
    return(invisible(file))
  }

  for (i in seq_along(items)) {
    it <- items[[i]]
    cap <- it$caption %||% sprintf("Item %d", i)
    doc <- officer::body_add_par(doc, cap, style = "heading 2")

    if (identical(it$kind, "table")) {
      doc <- .add_table(doc, it$data)
    } else if (identical(it$kind, "plot")) {
      doc <- .add_plot(doc, it$plot)
    }
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  print(doc, target = file)
  invisible(file)
}

.add_table <- function(doc, data) {
  data <- as.data.frame(data)
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(data)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    return(flextable::body_add_flextable(doc, ft))
  }
  officer::body_add_table(doc, data, style = "table_template")
}

.add_plot <- function(doc, plot) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  ggplot2::ggsave(tmp, plot = plot, width = 7, height = 4.5, dpi = 150,
                  bg = "white")
  officer::body_add_img(doc, src = tmp, width = 6.5, height = 4.2)
}

# local null-coalescing helper (also defined in fct_charts.R; safe to redefine)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
