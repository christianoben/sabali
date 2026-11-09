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
    # The AI write-up (if any) sits as prose directly beneath its table/figure,
    # never inside the table - so the table itself stays small and self-contained.
    doc <- .add_writeup(doc, it$writeup)
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

# A plain data-frame table, trimmed to publication columns/rows and rendered
# compact (single-spaced, tight padding, autofit so it stays small).
.add_table <- function(doc, data) {
  data <- publication_table_df(data)
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(data)
    ft <- flextable::theme_booktabs(ft)
    ft <- .style_flextable_compact(ft)
    ft <- flextable::autofit(ft)
    if (ncol(data) > 8L)
      ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
    return(flextable::body_add_flextable(doc, ft))
  }
  officer::body_add_table(doc, data, style = "table_template")
}

# Shared compact styling: single line spacing, tight vertical padding, a small
# clean font. Applied to every report flextable so tables auto-fit and read as
# publication-ready rather than loosely double-spaced.
.style_flextable_compact <- function(ft) {
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1,
                           padding.left = 5, padding.right = 5, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft
}

# Render an AI write-up as prose directly beneath a table or figure. Splits on
# blank lines into paragraphs and closes with a small attribution note. A NULL
# or empty write-up adds nothing.
.add_writeup <- function(doc, text) {
  if (is.null(text) || !nzchar(trimws(text))) return(doc)
  paras <- strsplit(trimws(text), "\n[[:space:]]*\n")[[1]]
  for (p in paras) {
    p <- gsub("[ \t]*\n[ \t]*", " ", trimws(p))
    if (nzchar(p)) doc <- officer::body_add_par(doc, p, style = "Normal")
  }
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("AI-generated summary; verify against the table above.",
                   officer::fp_text(italic = TRUE, font.size = 8,
                                    color = "grey40"))))
  doc
}

#' Trim a data-frame table to what a paper would actually report.
#'
#' Rules (all safe no-ops when a column/row is absent):
#'  * hypothesis-test Field/Value tables drop the baked-in "Interpretation" row
#'    (it belongs in prose beneath the table, not inside it);
#'  * coefficient tables drop raw "Std. Error" and "Statistic" columns;
#'  * when an exponentiated effect (OR/RR/IRR/HR) carries its own CI, the
#'    redundant log-scale "Estimate"/"CI lower"/"CI upper" are dropped;
#'  * numbers are formatted for display - p-values via p_fmt(), counts as
#'    integers, everything else to a tidy fixed precision.
publication_table_df <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || !ncol(df)) return(df)

  if (ncol(df) == 2L && all(c("Field", "Value") %in% names(df))) {
    df <- df[!df[["Field"]] %in% c("Interpretation"), , drop = FALSE]
    rownames(df) <- NULL
    return(df)
  }

  drop <- intersect(c("Std. Error", "Statistic"), names(df))
  eff_lo <- grep(" CI lower$", names(df), value = TRUE)
  eff_names <- sub(" CI lower$", "", eff_lo)
  eff_names <- eff_names[eff_names %in% names(df) & nzchar(eff_names)]
  if (length(eff_names) && "Estimate" %in% names(df))
    drop <- union(drop, intersect(c("Estimate", "CI lower", "CI upper"), names(df)))
  if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]

  for (nm in names(df)) {
    v <- df[[nm]]
    if (!is.numeric(v)) next
    if (grepl("p-value|p\\.value|^p$|Pr\\(", nm))
      df[[nm]] <- vapply(v, function(x) if (is.finite(x)) p_fmt(x) else "",
                         character(1))
    else
      df[[nm]] <- vapply(v, .num_fmt, character(1))
  }
  rownames(df) <- NULL
  df
}

# Tidy display formatting for one numeric value: integers stay integers, other
# numbers get a sensible fixed number of decimals.
.num_fmt <- function(x) {
  if (!is.finite(x)) return("")
  if (x == round(x) && abs(x) < 1e7)
    return(formatC(x, format = "d", big.mark = ","))
  ax <- abs(x)
  digits <- if (ax >= 1e4 || ax < 1e-2) 3 else 2
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

#' Fill in any missing write-ups synchronously (blocking). Used at export time
#' as a completeness backstop: the on-add background worker usually finishes
#' first, but if the analyst exports while a job is still in flight (or one
#' errored, or AI became available only later), this guarantees the downloaded
#' report still carries the prose. A no-op when AI is unconfigured.
report_finalise_writeups <- function(items) {
  if (!ai_available()) return(items)
  for (i in seq_along(items)) {
    it <- items[[i]]
    if (is.null(it$ai)) next
    if (!is.null(it$writeup) && nzchar(it$writeup)) next
    out <- tryCatch(
      ai_report_writeup(it$ai$tables, it$ai$context %||% list(),
                        it$ai$interpretation),
      error = function(e) NULL)
    if (!is.null(out)) items[[i]]$writeup <- out$text
  }
  items
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
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1,
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
  ft <- flextable::autofit(ft)
  # Wide across-time tables (a column per visit) would otherwise run past the
  # page margins in Word; constrain them to fill - and stay within - the page.
  if (ncol(df) > 8L)
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
  ft
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
