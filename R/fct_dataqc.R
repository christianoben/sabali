# fct_dataqc.R --------------------------------------------------------------
# Tab 0 "Data check" engine. Runs every data-quality test that can be checked
# by code against the currently active (type-confirmed) dataset and returns a
# tidy findings table plus a plain-text report for download.
#
# Design goals:
#   * pure functions (no Shiny), so the whole battery is unit-testable;
#   * defensive - a single pathological column must never abort the report;
#   * type-aware - uses the confirmed measurement types where available.

# Severity ranking (most to least serious) used for sorting and the summary.
DQ_SEVERITY_ORDER <- c("error", "warning", "note", "pass")
DQ_SEVERITY_LABEL <- c(error = "ERRORS", warning = "WARNINGS",
                       note = "NOTES", pass = "PASSED CHECKS")

#' Default thresholds for the data-quality checks. Overridable by the caller.
dq_default_opts <- function() {
  list(
    missing_note   = 0.10,   # >10% missing -> note
    missing_warn   = 0.20,   # >20% missing -> warning
    missing_error  = 0.50,   # >50% missing -> error
    rare_level_min = 5,      # category with < 5 obs -> rare
    high_card_ratio = 0.5,   # distinct / n above this (and > min) -> possible ID
    high_card_min  = 20,
    outlier_k      = 3,      # |z| > k -> SD outlier
    numeric_text_frac = 0.9, # >= this fraction of a text column parses numeric
    old_date_year  = 1900,   # dates before this are implausible
    sentinel_values = c("", "na", "n/a", "null", "none", "nil", "missing",
                        "unknown", "unk", "-", "--", "---", ".", "?", "9999",
                        "-99", "-999")
  )
}

# One finding row. `n` is the count of affected cells/rows/levels (NA if n/a).
dq_finding <- function(check, severity, scope, message, n = NA_integer_) {
  data.frame(check = check, severity = severity, scope = scope,
             message = message, n = as.integer(n),
             stringsAsFactors = FALSE)
}

# Numbers stored as text: fraction of non-missing entries that parse as numeric.
.dq_numeric_parse_frac <- function(x) {
  ch <- trimws(as.character(x))
  ch <- ch[!is.na(ch) & nzchar(ch)]
  if (!length(ch)) return(0)
  mean(!is.na(suppressWarnings(as.numeric(ch))))
}

#' Run the full data-quality battery.
#'
#' @param df    the active (coerced) data frame.
#' @param types named character vector variable -> measurement type (optional).
#' @param panel list(id, time, event) role assignment (optional).
#' @param opts  threshold overrides (see dq_default_opts()).
#' @return list(findings = data.frame, summary = list, n_checks = integer).
dq_run_checks <- function(df, types = NULL, panel = NULL, opts = dq_default_opts()) {
  o <- utils::modifyList(dq_default_opts(), opts %||% list())
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- dq_finding(...)
  ran <- character(0)
  mark <- function(check) ran <<- union(ran, check)
  safe <- function(check, expr) {
    mark(check)
    tryCatch(expr, error = function(e)
      add(check, "warning", "(engine)",
          sprintf("Check '%s' could not be evaluated: %s", check, conditionMessage(e))))
  }

  # ---- dataset is present at all -----------------------------------------
  mark("dataset present")
  if (is.null(df) || !is.data.frame(df)) {
    add("dataset present", "error", "dataset", "No active dataset is loaded.")
    return(.dq_finalise(out, ran, 0L, 0L))
  }
  n_rows <- nrow(df); n_cols <- ncol(df)
  mark("non-empty dataset")
  if (n_rows == 0L || n_cols == 0L) {
    add("non-empty dataset", "error", "dataset",
        sprintf("The dataset is empty (%d rows x %d columns).", n_rows, n_cols))
    return(.dq_finalise(out, ran, n_rows, n_cols))
  }

  nm <- names(df)

  # ---- column names -------------------------------------------------------
  safe("column names unique", {
    dup <- unique(nm[duplicated(nm)])
    if (length(dup))
      add("column names unique", "error", paste(dup, collapse = ", "),
          sprintf("Duplicate column name(s): %s. Rename so every column is unique.",
                  paste(dup, collapse = ", ")), length(dup))
  })
  safe("column names valid", {
    blank <- nm[is.na(nm) | !nzchar(trimws(nm))]
    if (length(blank))
      add("column names valid", "error", "dataset",
          sprintf("%d column(s) have a blank name.", length(blank)), length(blank))
    padded <- nm[nzchar(nm) & nm != trimws(nm)]
    if (length(padded))
      add("column names valid", "warning", paste(padded, collapse = ", "),
          "Column name(s) have leading/trailing spaces, which cause silent lookup errors.",
          length(padded))
    nonsyn <- nm[nzchar(trimws(nm)) & make.names(nm) != nm]
    if (length(nonsyn))
      add("column names valid", "note", paste(nonsyn, collapse = ", "),
          "Column name(s) are non-syntactic (spaces/symbols); they still work but must be back-quoted in formulas.",
          length(nonsyn))
  })

  # ---- whole-dataset row checks ------------------------------------------
  safe("duplicate rows", {
    dup <- sum(duplicated(df))
    if (dup > 0)
      add("duplicate rows", "warning", "dataset",
          sprintf("%d fully duplicated row(s) detected.", dup), dup)
  })
  safe("empty rows", {
    all_na <- sum(apply(df, 1, function(r) all(is.na(r))))
    if (all_na > 0)
      add("empty rows", "warning", "dataset",
          sprintf("%d row(s) are entirely missing.", all_na), all_na)
  })
  safe("overall missingness", {
    total <- prod(dim(df)); miss <- sum(is.na(df))
    pct <- if (total) miss / total else 0
    if (pct > o$missing_warn)
      add("overall missingness", "warning", "dataset",
          sprintf("%.1f%% of all cells are missing.", 100 * pct), miss)
    else if (pct > o$missing_note)
      add("overall missingness", "note", "dataset",
          sprintf("%.1f%% of all cells are missing.", 100 * pct), miss)
  })

  # ---- duplicate (identical) columns -------------------------------------
  safe("redundant columns", {
    if (n_cols >= 2 && n_cols <= 200) {
      for (i in seq_len(n_cols - 1)) for (j in (i + 1):n_cols) {
        a <- df[[i]]; b <- df[[j]]
        if (identical(as.character(a), as.character(b)))
          add("redundant columns", "note", paste(nm[i], nm[j], sep = " = "),
              sprintf("Columns '%s' and '%s' are identical; one is redundant.",
                      nm[i], nm[j]))
      }
    }
  })

  # ---- per-column checks --------------------------------------------------
  for (col in nm) {
    x <- df[[col]]
    t <- if (!is.null(types) && col %in% names(types)) unname(types[[col]]) else NA_character_
    n_na <- sum(is.na(x)); n_ok <- n_rows - n_na
    uniq <- unique(x[!is.na(x)])

    safe("empty columns", {
      if (n_ok == 0)
        add("empty columns", "error", col, "Column is entirely missing (no usable values).", n_rows)
    })
    if (n_ok == 0) next

    safe("column missingness", {
      pct <- n_na / n_rows
      sev <- if (pct > o$missing_error) "error"
             else if (pct > o$missing_warn) "warning"
             else if (pct > o$missing_note) "note" else NA
      if (!is.na(sev))
        add("column missingness", sev, col,
            sprintf("%.1f%% missing (%d of %d).", 100 * pct, n_na, n_rows), n_na)
    })

    safe("constant columns", {
      if (length(uniq) == 1 && n_rows > 1)
        add("constant columns", "warning", col,
            sprintf("Only one distinct value ('%s'); the column carries no information.",
                    as.character(uniq[1])))
    })

    is_num <- is.numeric(x)
    is_cat <- is.factor(x) || is.character(x)

    # -- disguised-missing sentinels (text/factor) --
    if (is_cat) {
      safe("disguised missing", {
        ch <- tolower(trimws(as.character(x)))
        hit <- ch %in% o$sentinel_values & !is.na(x)
        if (any(hit)) {
          vals <- unique(as.character(x)[hit])
          add("disguised missing", "warning", col,
              sprintf("%d value(s) look like coded missing (%s) but are stored as data.",
                      sum(hit), paste(utils::head(vals, 5), collapse = ", ")), sum(hit))
        }
      })
      safe("numbers stored as text", {
        if (.dq_numeric_parse_frac(x) >= o$numeric_text_frac && !identical(t, "identifier"))
          add("numbers stored as text", "warning", col,
              "Nearly all values are numeric but the column is text; confirm its type on the Data tab.",
              n_ok)
      })
      safe("inconsistent categories", {
        raw_lv <- unique(as.character(x)[!is.na(x)])
        norm_lv <- unique(tolower(trimws(as.character(x)[!is.na(x)])))
        if (length(norm_lv) < length(raw_lv))
          add("inconsistent categories", "warning", col,
              sprintf("%d level(s) differ only by case/whitespace and likely mean the same thing.",
                      length(raw_lv) - length(norm_lv)), length(raw_lv) - length(norm_lv))
      })
      safe("high-cardinality category", {
        k <- length(uniq)
        if (!identical(t, "identifier") && k > o$high_card_min && k / n_ok > o$high_card_ratio)
          add("high-cardinality category", "note", col,
              sprintf("%d distinct values in %d rows; this may really be an identifier or free text.",
                      k, n_ok))
      })
      safe("rare categories", {
        tb <- table(x)
        rare <- names(tb)[tb > 0 & tb < o$rare_level_min]
        if (length(rare))
          add("rare categories", "note", col,
              sprintf("%d level(s) have fewer than %d observations (e.g. %s).",
                      length(rare), o$rare_level_min,
                      paste(utils::head(rare, 5), collapse = ", ")), length(rare))
      })
      if (identical(t, "binary")) safe("binary has two levels", {
        if (length(uniq) != 2)
          add("binary has two levels", "warning", col,
              sprintf("Typed as binary but %d level(s) are observed.", length(uniq)),
              length(uniq))
      })
    }

    # -- numeric quality --
    if (is_num) {
      safe("infinite values", {
        n_inf <- sum(is.infinite(x))
        if (n_inf > 0)
          add("infinite values", "error", col,
              sprintf("%d infinite value(s) (Inf/-Inf) present.", n_inf), n_inf)
      })
      xf <- x[is.finite(x)]
      safe("outliers", {
        if (length(xf) >= 10) {
          m <- mean(xf); s <- stats::sd(xf)
          if (is.finite(s) && s > 0) {
            n_sd <- sum(abs(xf - m) / s > o$outlier_k)
            qs <- stats::quantile(xf, c(0.25, 0.75), names = FALSE)
            iqr <- qs[2] - qs[1]
            n_iqr <- if (iqr > 0) sum(xf < qs[1] - 1.5 * iqr | xf > qs[2] + 1.5 * iqr) else 0
            if (n_sd > 0 || n_iqr > 0)
              add("outliers", "note", col,
                  sprintf("%d value(s) beyond %g SD and %d beyond the 1.5xIQR fence; check for entry errors.",
                          n_sd, o$outlier_k, n_iqr), max(n_sd, n_iqr))
          }
        }
      })
      if (identical(t, "count")) {
        safe("count is non-negative", {
          n_neg <- sum(xf < 0)
          if (n_neg > 0)
            add("count is non-negative", "error", col,
                sprintf("%d negative value(s) in a count variable.", n_neg), n_neg)
        })
        safe("count is integer", {
          n_frac <- sum(abs(xf - round(xf)) > 1e-8)
          if (n_frac > 0)
            add("count is integer", "warning", col,
                sprintf("%d non-integer value(s) in a count variable.", n_frac), n_frac)
        })
      }
    }

    # -- date quality --
    if (inherits(x, c("Date", "POSIXct", "POSIXt")) || identical(t, "datetime")) {
      safe("implausible dates", {
        dx <- suppressWarnings(as.Date(x))
        dx <- dx[!is.na(dx)]
        if (length(dx)) {
          today <- Sys.Date()
          n_future <- sum(dx > today)
          n_old <- sum(as.integer(format(dx, "%Y")) < o$old_date_year)
          if (n_future > 0)
            add("implausible dates", "warning", col,
                sprintf("%d date(s) are in the future (after %s).", n_future, today), n_future)
          if (n_old > 0)
            add("implausible dates", "note", col,
                sprintf("%d date(s) are before %d and may be data-entry errors.",
                        n_old, o$old_date_year), n_old)
        }
      })
    }
  }

  # ---- ID / panel structure ----------------------------------------------
  id_var <- panel$id %||% NULL
  time_var <- panel$time %||% NULL
  if (!is.null(id_var) && nzchar(id_var) && id_var %in% nm) {
    if (!is.null(time_var) && nzchar(time_var) && time_var %in% nm) {
      safe("unique id-time", {
        key <- paste(df[[id_var]], df[[time_var]], sep = "\r")
        dup <- sum(duplicated(key))
        if (dup > 0)
          add("unique id-time", "error", paste(id_var, time_var, sep = " x "),
              sprintf("%d duplicated (subject, time) combination(s); repeated measures must be unique.",
                      dup), dup)
      })
      safe("single-observation subjects", {
        counts <- table(df[[id_var]])
        singles <- sum(counts == 1)
        if (singles > 0)
          add("single-observation subjects", "note", id_var,
              sprintf("%d subject(s) have only one observation; they contribute no within-subject information.",
                      singles), singles)
      })
    } else {
      safe("unique subject id", {
        dup <- sum(duplicated(df[[id_var]][!is.na(df[[id_var]])]))
        if (dup > 0)
          add("unique subject id", "note", id_var,
              sprintf("Subject ID repeats on %d row(s); expected if this is panel data, an error if it should be one row per subject.",
                      dup), dup)
      })
    }
  }

  .dq_finalise(out, ran, n_rows, n_cols)
}

.dq_finalise <- function(out, ran, n_rows, n_cols) {
  findings <- if (length(out)) do.call(rbind, out) else
    dq_finding(character(0), character(0), character(0), character(0))[0, ]
  # add explicit PASS rows for every check that ran and produced no finding.
  flagged <- unique(findings$check)
  passed <- setdiff(ran, flagged)
  if (length(passed))
    findings <- rbind(findings,
      do.call(rbind, lapply(passed, function(c)
        dq_finding(c, "pass", "dataset", "No issues detected."))))
  # order by severity then check
  findings$severity <- factor(findings$severity, levels = DQ_SEVERITY_ORDER)
  findings <- findings[order(findings$severity, findings$check), , drop = FALSE]
  findings$severity <- as.character(findings$severity)
  rownames(findings) <- NULL

  counts <- table(factor(findings$severity, levels = DQ_SEVERITY_ORDER))
  list(
    findings = findings,
    summary = list(
      n_rows = n_rows, n_cols = n_cols,
      errors = as.integer(counts[["error"]]),
      warnings = as.integer(counts[["warning"]]),
      notes = as.integer(counts[["note"]]),
      passed = as.integer(counts[["pass"]])
    ),
    n_checks = length(ran)
  )
}

#' Render the findings as a plain-text (.txt) notebook report.
#'
#' @param result   output of dq_run_checks().
#' @param dataset_name label for the header.
#' @param generated_at a timestamp string (injectable for reproducible tests).
dq_report_text <- function(result, dataset_name = "active dataset",
                           generated_at = as.character(Sys.time())) {
  s <- result$summary
  bar <- strrep("=", 64)
  sub <- strrep("-", 64)
  L <- c(
    bar,
    " SABALI - DATA QUALITY REPORT",
    bar,
    sprintf(" Dataset      : %s", dataset_name),
    sprintf(" Generated    : %s", generated_at),
    sprintf(" Dimensions   : %d rows x %d columns", s$n_rows, s$n_cols),
    sprintf(" Checks run   : %d", result$n_checks),
    sprintf(" Result       : %d error(s), %d warning(s), %d note(s)",
            s$errors, s$warnings, s$notes),
    sub, ""
  )
  f <- result$findings
  for (sev in DQ_SEVERITY_ORDER) {
    rows <- f[f$severity == sev, , drop = FALSE]
    if (!nrow(rows)) next
    L <- c(L, sprintf("[%s]  (%d)", DQ_SEVERITY_LABEL[[sev]], nrow(rows)))
    if (sev == "pass") {
      for (i in seq_len(nrow(rows)))
        L <- c(L, sprintf("  - %s", rows$check[i]))
    } else {
      for (i in seq_len(nrow(rows)))
        L <- c(L, sprintf("  - [%s] %s: %s", rows$scope[i], rows$check[i], rows$message[i]))
    }
    L <- c(L, "")
  }
  if (s$errors == 0 && s$warnings == 0 && s$notes == 0)
    L <- c(L, "No data-quality issues were detected by the automated checks.", "")
  L <- c(L,
    sub,
    " Notes:",
    "  * Automated checks catch structural and statistical red flags only;",
    "    they are not a substitute for subject-matter review of the data.",
    "  * 'Notes' are informational; 'warnings' and 'errors' should be resolved",
    "    or explicitly justified before analysis.",
    bar,
    " End of report - Sabali, Data check (Tab 0).",
    bar)
  paste(L, collapse = "\n")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
