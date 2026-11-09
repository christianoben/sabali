# fct_tableone.R -------------------------------------------------------------
# "Table 1" builder: a multi-variable descriptive summary with rows = variables
# (categorical shown as a header plus one indented row per level; continuous as a
# single row) and columns = an optional Overall column plus one column per level
# of a splitting/outcome variable. Cells are n (%) (column percentages) for
# categorical variables and M (SD) or Median [IQR] for continuous ones.
#
# When (and only when) a splitting variable is supplied, a p-value column is
# added. The test is CHOSEN by the app's existing assumption-aware engines
# (fct_tests.R) and the RECOMMENDED test's p-value is reported; the tests used
# are listed in a footnote. With no splitting variable the table is a plain
# one-dimensional summary with no p-value column and no footnote of tests.
#
# The engine returns a structured object (display data frame + row/column
# metadata + title + note). Publication (APA) formatting and table numbering
# happen later, at report-download time (see fct_report.R); the Descriptives tab
# only previews this object and sends it to the report.

# --- formatting helpers -----------------------------------------------------

# APA-style p-value: "< .001", otherwise 2-3 dp with no leading zero (e.g. ".03").
p_apa <- function(p) {
  if (length(p) != 1L || is.na(p) || !is.finite(p)) return("—")
  if (p < 0.001) return("< .001")
  sub("^(-?)0", "\\1", formatC(round(p, 3), format = "f", digits = 3))
}

.t1_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  sprintf("%.*f (%.*f)", digits, mean(x), digits, stats::sd(x))
}

fmt_median_iqr <- function(x, digits = 2) {
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
  sprintf("%.*f [%.*f, %.*f]", digits, q[2], digits, q[1], digits, q[3])
}

fmt_n_pct <- function(count, denom, pct_digits = 1) {
  if (!length(denom) || denom == 0) return("—")
  sprintf("%d (%.*f%%)", as.integer(count), pct_digits, 100 * count / denom)
}

.t1_is_continuous <- function(tp) tp %in% c("continuous", "count")

# --- assumption-aware p-value via the existing test engines -----------------

# Decide and compute the p-value comparing a characteristic across the split.
# Reuses the Tests-tab runners so the choice (and value) match the rest of the
# app: continuous x 2 groups -> Welch t / Mann-Whitney; continuous x 3+ groups
# -> ANOVA / Kruskal-Wallis; categorical -> chi-square / Fisher. The runner's
# own assumption checks pick the recommended test; we report that one.
# @return list(p, test) - test is NA when a p-value is not applicable.
tableone_pvalue <- function(x, group, x_type) {
  group <- droplevels(as.factor(group))
  if (nlevels(group) < 2L) return(list(p = NA_real_, test = NA_character_))
  res <- tryCatch({
    if (.t1_is_continuous(x_type)) {
      xn <- .t1_num(x)
      if (nlevels(group) == 2L) run_two_independent(xn, group)
      else run_anova_oneway(xn, group)
    } else {
      run_categorical(as.factor(x), group, paired = FALSE)
    }
  }, error = function(e) NULL)
  if (is.null(res)) return(list(p = NA_real_, test = NA_character_))
  chosen <- res[[res$recommended]]
  list(p = chosen$p_value %||% NA_real_, test = chosen$label %||% NA_character_)
}

# For "auto" continuous display: match the descriptive statistic to the chosen
# test - Median [IQR] when the recommended test is rank-based, else M (SD).
.t1_cont_style <- function(cont_style, x, group, has_split, test_label) {
  if (!identical(cont_style, "auto")) return(cont_style)
  if (has_split && !is.na(test_label) &&
      grepl("Mann-Whitney|Kruskal|Wilcoxon", test_label)) return("median_iqr")
  if (!has_split) {
    nrm <- tryCatch(check_normality(.t1_num(x)), error = function(e) list(ok = TRUE))
    if (isFALSE(nrm$ok)) return("median_iqr")
  }
  "mean_sd"
}

.t1_fmt_cont <- function(x, style, digits) {
  if (identical(style, "median_iqr")) fmt_median_iqr(x, digits) else fmt_mean_sd(x, digits)
}

# --- the builder ------------------------------------------------------------

#' Build a Table 1 summary object.
#'
#' @param df      data frame (already type-coerced).
#' @param vars    character vector of characteristic variables (table rows).
#' @param strata  optional splitting/outcome variable (table columns). NULL or ""
#'                gives a one-dimensional summary with no p-value column.
#' @param types   named character vector: variable -> measurement type.
#' @param opts    list: cont_style ("auto"|"mean_sd"|"median_iqr"), digits,
#'                pct_digits, show_overall, show_missing, show_p, title.
#' @return list(table, row_kind, columns, has_p, note, title, tests_used, apa).
build_table_one <- function(df, vars, strata = NULL, types = NULL, opts = list()) {
  if (!length(vars)) stop("Choose at least one variable for the summary table.", call. = FALSE)
  if (any(!vars %in% names(df)))
    stop("The summary table references a variable that is not in the data.", call. = FALSE)
  types <- types %||% stats::setNames(rep("nominal", length(vars)), vars)
  digits      <- opts$digits %||% 2L
  pct_digits  <- opts$pct_digits %||% 1L
  cont_style  <- opts$cont_style %||% "auto"
  show_overall <- opts$show_overall %||% TRUE
  show_missing <- isTRUE(opts$show_missing)
  show_p       <- (opts$show_p %||% TRUE)

  has_split <- !is.null(strata) && length(strata) == 1L && nzchar(strata) &&
    strata %in% names(df) && !strata %in% vars
  if (has_split) {
    sf <- droplevels(as.factor(df[[strata]]))
    if (nlevels(sf) < 2L) has_split <- FALSE
  }
  has_p <- has_split && isTRUE(show_p)

  # Column definitions: named list of row-index vectors. When a strata_order is
  # supplied (e.g. chronological visits) the columns follow it; otherwise the
  # factor levels are used.
  cols <- list()
  if (show_overall || !has_split) cols[["Overall"]] <- which(rep(TRUE, nrow(df)))
  if (has_split) {
    lv_order <- levels(sf)
    if (!is.null(opts$strata_order)) {
      ord <- intersect(as.character(opts$strata_order), lv_order)
      lv_order <- c(ord, setdiff(lv_order, ord))
    }
    for (lv in lv_order) cols[[lv]] <- which(!is.na(sf) & sf == lv)
  }
  col_names <- names(cols)
  col_headers <- vapply(col_names, function(nm)
    sprintf("%s\n(n = %d)", nm, length(cols[[nm]])), character(1))

  rows_char <- character(0); rows_kind <- character(0)
  cell_mat <- vector("list", 0); p_col <- character(0)
  tests_used <- list()

  push_row <- function(char, cells, kind, p_text = "") {
    rows_char[[length(rows_char) + 1L]] <<- char
    rows_kind[[length(rows_kind) + 1L]] <<- kind
    cell_mat[[length(cell_mat) + 1L]]   <<- cells
    p_col[[length(p_col) + 1L]]         <<- p_text
  }

  # Optional attrition/retention row (repeated-measures "over time" tables):
  # participants assessed in each column, as a percentage of the first column.
  if (isTRUE(opts$retention) && length(col_names) >= 1L) {
    base_n <- length(cols[[col_names[1]]])
    cells <- vapply(col_names, function(nm) {
      n <- length(cols[[nm]])
      if (base_n > 0) sprintf("%d (%.0f%%)", n, 100 * n / base_n) else as.character(n)
    }, character(1))
    push_row("Participants, n (% of first column)", cells, "meta", "")
  }

  for (v in vars) {
    tp <- types[[v]] %||% "nominal"
    x <- df[[v]]
    pinfo <- if (has_p) tableone_pvalue(x, sf, tp) else list(p = NA_real_, test = NA_character_)
    if (has_p && !is.na(pinfo$test)) tests_used[[v]] <- pinfo$test
    ptxt <- if (has_p) p_apa(pinfo$p) else ""

    if (.t1_is_continuous(tp)) {
      style <- .t1_cont_style(cont_style, x, if (has_split) sf else NULL, has_split, pinfo$test)
      label <- sprintf("%s, %s", v, if (identical(style, "median_iqr")) "Median [IQR]" else "M (SD)")
      cells <- vapply(col_names, function(nm)
        .t1_fmt_cont(.t1_num(x)[cols[[nm]]], style, digits), character(1))
      push_row(label, cells, "cont", ptxt)
      if (show_missing) {
        miss <- vapply(col_names, function(nm)
          as.character(sum(!is.finite(.t1_num(x)[cols[[nm]]]))), character(1))
        push_row("Missing", miss, "cat_level", "")
      }
    } else {
      xf <- as.factor(x)
      levs <- levels(droplevels(xf))
      push_row(sprintf("%s, n (%%)", v),
               stats::setNames(rep("", length(col_names)), col_names),
               "cat_header", ptxt)
      for (lv in levs) {
        cells <- vapply(col_names, function(nm) {
          xi <- xf[cols[[nm]]]
          fmt_n_pct(sum(!is.na(xi) & xi == lv), sum(!is.na(xi)), pct_digits)
        }, character(1))
        push_row(lv, cells, "cat_level", "")
      }
      if (show_missing) {
        miss <- vapply(col_names, function(nm) {
          xi <- xf[cols[[nm]]]; as.character(sum(is.na(xi)))
        }, character(1))
        push_row("Missing", miss, "cat_level", "")
      }
    }
  }

  # Assemble the display data frame.
  body <- do.call(rbind, lapply(cell_mat, function(cc) cc[col_names]))
  colnames(body) <- col_names
  display <- data.frame(Characteristic = rows_char, body,
                        check.names = FALSE, stringsAsFactors = FALSE)
  if (has_p) display[["p"]] <- p_col

  note_txt <- .t1_note(tests_used, has_p, cont_style, strata, has_split)
  if (isTRUE(opts$retention))
    note_txt <- paste(note_txt,
      "The first row is the number of participants assessed at each time point (percentage relative to the first column); declining counts reflect attrition or missed visits.")

  list(
    table = display,
    row_kind = rows_kind,          # meta | cont | cat_header | cat_level (indent/bold)
    columns = col_headers,         # display headers with N (parallel to col_names)
    col_names = col_names,
    has_p = has_p,
    strata = if (has_split) strata else NULL,
    title = opts$title %||% "Participant characteristics",
    note = note_txt,
    tests_used = tests_used,
    apa = TRUE)
}

# Chronological order of a time/visit variable. Numeric or date times sort
# naturally; an ordered factor carries its own declared order (e.g. an ordinal
# visit "Baseline" < "6 months" < ...), which we respect regardless of row
# order; a plain labelled visit has no sortable order, so we fall back to its
# order of first appearance in the data - long-format panel data is stored
# chronologically within subject by convention.
.t1_time_order <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt")) || is.numeric(x))
    as.character(sort(unique(x[!is.na(x)])))
  else if (is.ordered(x))
    intersect(levels(x), as.character(unique(x[!is.na(x)])))
  else
    unique(as.character(x[!is.na(x)]))
}

# One row per subject at their earliest time point (baseline). For numeric/date
# times this is the minimum; for an ordered factor it is the lowest declared
# level; for a plain labelled visit it is the subject's first row in data order
# (the chronological convention above).
tableone_baseline <- function(df, id, time) {
  if (is.null(id) || is.null(time) || !all(c(id, time) %in% names(df)))
    stop("A baseline table needs the subject-ID and time columns (declare them on the Data tab).",
         call. = FALSE)
  tx <- df[[time]]
  ord <- if (inherits(tx, c("Date", "POSIXct", "POSIXt")) || is.numeric(tx))
    order(df[[id]], tx)
  else if (is.ordered(tx))
    order(df[[id]], as.integer(tx))            # earliest declared level per subject
  else
    order(df[[id]])                            # order() is stable -> keeps data order
  d <- df[ord, , drop = FALSE]
  d[!duplicated(d[[id]]), , drop = FALSE]
}

# Footnote text: statistic definitions, and (when split) the tests used.
.t1_note <- function(tests_used, has_p, cont_style, strata, has_split) {
  parts <- c("Categorical variables are summarised as n (column %); continuous variables as M (SD) or Median [IQR] as labelled.")
  if (has_p && length(tests_used)) {
    by_test <- split(names(tests_used), unlist(tests_used))
    used <- vapply(names(by_test), function(tst)
      sprintf("%s (%s)", tst, paste(by_test[[tst]], collapse = ", ")),
      character(1))
    parts <- c(parts,
      sprintf("p-values compare across %s and are unadjusted; the test was selected from assumption checks (normality, variance homogeneity, expected cell counts). Tests used: %s.",
              strata, paste(used, collapse = "; ")))
  }
  paste(parts, collapse = " ")
}
