# fct_report.R --------------------------------------------------------------
# One-click Word (.docx) export of collected tables and charts, using {officer}
# for the document and {flextable} for nicely formatted tables.
#
# Tables and figures are rendered to APA style and numbered in the order they
# were added (Table 1, Table 2, ..., Figure 1, Figure 2, ...), so the downloaded
# report is publication-ready. Numbering happens HERE, at export time - callers
# just add items in order.
#
# The report is assembled from a list of "items". Each item is a list:
#   list(kind = "table",  caption = "...", data = <data.frame>)
#   list(kind = "plot",   caption = "...", plot = <ggplot>, spec = <chart spec>)
#   list(kind = "table1", title = "...",  t1 = <build_table_one() object>)

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

  table_n <- 0L; figure_n <- 0L
  for (it in items) {
    if (identical(it$kind, "plot")) {
      figure_n <- figure_n + 1L
      doc <- .apa_caption(doc, "Figure", figure_n,
                          it$caption %||% sprintf("Figure %d", figure_n),
                          above = TRUE)
      doc <- .add_plot(doc, it$plot)
    } else if (identical(it$kind, "table1")) {
      table_n <- table_n + 1L
      doc <- .apa_caption(doc, "Table", table_n,
                          it$title %||% it$caption %||% "Summary table")
      doc <- .add_table1(doc, it$t1)
    } else if (identical(it$kind, "table")) {
      table_n <- table_n + 1L
      doc <- .apa_caption(doc, "Table", table_n,
                          it$caption %||% sprintf("Table %d", table_n))
      doc <- .add_table(doc, it$data)
    }
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  print(doc, target = file)
  invisible(file)
}

# APA caption block: "Table N" (bold) on one line, the italic title on the next.
# For figures APA also places the caption above the image.
.apa_caption <- function(doc, kind, n, title, above = FALSE) {
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(sprintf("%s %d", kind, n),
                   officer::fp_text(bold = TRUE, font.size = 12))))
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(title, officer::fp_text(italic = TRUE, font.size = 12))))
  doc
}

# A plain data-frame table, APA-themed.
.add_table <- function(doc, data) {
  data <- as.data.frame(data)
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(data)
    ft <- flextable::theme_apa(ft)
    ft <- flextable::autofit(ft)
    return(flextable::body_add_flextable(doc, ft))
  }
  officer::body_add_table(doc, data, style = "table_template")
}

# Build a polished flextable for a Table 1 object. Used by BOTH the Word report
# and the in-app preview so what the analyst sees is what downloads.
#
# Look: a clean "booktabs" style (horizontal rules only - APA/Chicago-friendly),
# left-aligned characteristics with indented category levels and bold variable
# rows, centred value columns, "name (n = …)" column headers, a spanning header
# over the outcome groups when the table is split, an italic p header, and the
# tests/definitions Note as a footnote.
tableone_flextable <- function(t1) {
  stopifnot(requireNamespace("flextable", quietly = TRUE))
  df <- t1$table
  value_cols <- setdiff(names(df), "Characteristic")
  ft <- flextable::flextable(df)

  labels <- as.list(gsub("\n", " ", t1$columns))
  names(labels) <- t1$col_names
  labels[["Characteristic"]] <- "Characteristic"
  if (isTRUE(t1$has_p)) labels[["p"]] <- "p-value"
  ft <- do.call(flextable::set_header_labels, c(list(ft), labels))

  # Spanning header naming the outcome over its group columns (not Overall / p).
  group_cols <- intersect(setdiff(t1$col_names, "Overall"), names(df))
  if (isTRUE(t1$has_p) && !is.null(t1$strata) && length(group_cols) >= 1L) {
    top <- rep("", ncol(df)); names(top) <- names(df)
    top[group_cols] <- t1$strata
    ft <- flextable::add_header_row(ft, values = unname(top), top = TRUE)
    ft <- flextable::merge_h(ft, i = 1, part = "header")
    gi <- match(group_cols, names(df))
    ft <- flextable::hline(ft, i = 1, j = gi, part = "header",
                           border = officer::fp_border(color = "grey40", width = 1))
    ft <- flextable::align(ft, i = 1, part = "header", align = "center")
  }

  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = "Characteristic", align = "left", part = "all")
  ft <- flextable::align(ft, j = value_cols, align = "center", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3,
                           padding.left = 6, padding.right = 6, part = "all")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")

  bold_rows <- which(t1$row_kind %in% c("cont", "cat_header", "meta"))
  if (length(bold_rows))
    ft <- flextable::bold(ft, i = bold_rows, j = 1, part = "body")
  level_rows <- which(t1$row_kind == "cat_level")
  if (length(level_rows))
    ft <- flextable::padding(ft, i = level_rows, j = 1, padding.left = 22, part = "body")
  if (isTRUE(t1$has_p))
    ft <- flextable::italic(ft, j = "p", part = "header")

  if (!is.null(t1$note) && nzchar(t1$note)) {
    ft <- flextable::add_footer_lines(ft, values = paste0("Note. ", t1$note))
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
    ft <- flextable::italic(ft, i = 1, part = "footer")
    ft <- flextable::color(ft, color = "grey30", part = "footer")
  }
  flextable::autofit(ft)
}

.add_table1 <- function(doc, t1) {
  if (is.null(t1) || is.null(t1$table)) return(doc)
  if (!requireNamespace("flextable", quietly = TRUE))
    return(.add_table(doc, t1$table))   # graceful fallback
  flextable::body_add_flextable(doc, tableone_flextable(t1))
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
