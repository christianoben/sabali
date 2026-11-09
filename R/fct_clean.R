# fct_clean.R ---------------------------------------------------------------
# Value constraints: per-variable validity rules the analyst declares once on
# the Data step, enforced CENTRALLY so the confirmed type/override actually
# takes effect in the data every downstream tab uses (Descriptives, Charts,
# Tests, Regression, Advanced, Report).
#
# The problem this solves: the analysis tabs decide what a variable can be used
# for from its ACTUAL values (via outcome_type_options), not from the type label
# in the editor. So a field the analyst calls "binary" but which still holds a
# stray third value (a blank, a typo, an out-of-range code) is treated as
# categorical and hidden from binary/continuous/count analyses. Declaring the
# valid values coerces everything else to NA, so the data matches the intent.
#
# A constraint is a small list, keyed by variable name:
#   list(kind = "levels", keep = <character levels to keep>)
#   list(kind = "range",  min = <num|NA>, max = <num|NA>, integer = <lgl>)
# A variable with no entry (or NULL) is unconstrained.

#' Apply value constraints to a data frame, coercing out-of-scope values to NA.
#' Safe no-op for variables with no constraint or not present. Never throws.
apply_value_constraints <- function(df, constraints = list()) {
  if (!length(constraints)) return(df)
  for (nm in intersect(names(constraints), names(df))) {
    c1 <- constraints[[nm]]
    if (is.null(c1) || is.null(c1$kind)) next
    x <- df[[nm]]
    if (identical(c1$kind, "levels")) {
      keep <- as.character(c1$keep %||% character(0))
      chr <- as.character(x)
      chr[!is.na(chr) & !(chr %in% keep)] <- NA
      if (is.factor(x)) {
        lev <- intersect(levels(x), keep)              # preserve declared order
        df[[nm]] <- factor(chr, levels = lev, ordered = is.ordered(x))
      } else {
        df[[nm]] <- chr
      }
    } else if (identical(c1$kind, "range")) {
      v <- suppressWarnings(as.numeric(as.character(x)))
      lo <- c1$min; hi <- c1$max
      if (!is.null(lo) && is.finite(lo)) v[!is.na(v) & v < lo] <- NA
      if (!is.null(hi) && is.finite(hi)) v[!is.na(v) & v > hi] <- NA
      if (isTRUE(c1$integer)) v[!is.na(v) & v != round(v)] <- NA
      df[[nm]] <- v
    }
  }
  df
}

#' A permissive default constraint for one column given its confirmed type -
#' the starting point the UI shows (keeps every observed level / the full
#' observed range). Returns NULL for types where a value constraint does not
#' apply (identifier, text, datetime).
default_value_constraint <- function(x, type) {
  if (is.null(type) || type %in% c("identifier", "text", "datetime"))
    return(NULL)
  if (type %in% c("binary", "nominal", "ordinal")) {
    lv <- levels(as.factor(x[!is.na(x)]))
    lv <- lv[nzchar(trimws(lv))]
    return(list(kind = "levels", keep = lv))
  }
  if (type %in% c("continuous", "count")) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    v <- v[is.finite(v)]
    if (!length(v)) return(list(kind = "range", min = NA_real_, max = NA_real_,
                                integer = identical(type, "count")))
    return(list(kind = "range", min = min(v), max = max(v),
                integer = identical(type, "count")))
  }
  NULL
}

#' TRUE when a constraint would actually change the data relative to the full
#' observed set (used to list only the constraints the analyst has tightened).
constraint_is_active <- function(c1, x, type) {
  d <- default_value_constraint(x, type)
  if (is.null(c1) || is.null(c1$kind)) return(FALSE)
  if (identical(c1$kind, "levels"))
    return(!setequal(as.character(c1$keep %||% character(0)),
                     as.character(d$keep %||% character(0))))
  if (identical(c1$kind, "range")) {
    same <- function(a, b) (is.na(a) && is.na(b)) ||
      (is.finite(a) && is.finite(b) && isTRUE(all.equal(a, b)))
    return(!(same(c1$min, d$min) && same(c1$max, d$max)))
  }
  FALSE
}

#' A short human-readable description of a constraint, for the "active rules"
#' list. `x`/`type` give the baseline so a levels rule can say how many dropped.
constraint_summary <- function(nm, c1, x = NULL, type = NULL) {
  if (is.null(c1) || is.null(c1$kind)) return(sprintf("%s: (no rule)", nm))
  if (identical(c1$kind, "levels")) {
    keep <- as.character(c1$keep %||% character(0))
    kept <- paste(utils::head(keep, 6), collapse = ", ")
    if (length(keep) > 6) kept <- paste0(kept, ", ...")
    return(sprintf("%s: keep %d level%s (%s); others -> missing", nm,
                   length(keep), if (length(keep) == 1) "" else "s", kept))
  }
  if (identical(c1$kind, "range")) {
    fmt1 <- function(z) trimws(formatC(z, format = "g", digits = 6))
    lo <- if (is.null(c1$min) || is.na(c1$min)) "-inf" else fmt1(c1$min)
    hi <- if (is.null(c1$max) || is.na(c1$max)) "+inf" else fmt1(c1$max)
    ints <- if (isTRUE(c1$integer)) "; whole numbers only" else ""
    return(sprintf("%s: keep %s to %s%s; outside -> missing", nm, lo, hi, ints))
  }
  sprintf("%s: (no rule)", nm)
}

#' Translate a tab-0 Data contract into value constraints for enforcement.
#'
#' The contract (see dq_contract) declares, per variable, an expected type,
#' allowed levels (categorical) and a min/max range (numeric). This turns those
#' declarations into the constraint list apply_value_constraints() understands,
#' so the contract can be ENFORCED (out-of-contract values -> NA), not just
#' checked. Only categorical `allowed_levels` and numeric `ranges` are enforced;
#' date ranges and other contract facets are left to the checks.
#'
#' @param contract A dq_contract() list (or NULL).
#' @param df       The data frame the contract applies to.
#' @param types    Named confirmed types (fallback when the contract omits one).
dq_contract_to_constraints <- function(contract, df, types = NULL) {
  out <- list()
  if (is.null(contract) || !is.data.frame(df)) return(out)
  al <- contract$allowed_levels %||% list()
  rg <- contract$ranges %||% list()
  et <- contract$expected_types %||% character(0)
  lookup <- function(x, nm) if (!is.null(x) && nm %in% names(x)) x[[nm]] else NULL
  for (nm in names(df)) {
    tp <- as.character(lookup(et, nm) %||% lookup(types, nm) %||% "")
    if (tp %in% c("binary", "nominal", "ordinal")) {
      lv <- al[[nm]]
      if (!is.null(lv) && length(lv))
        out[[nm]] <- list(kind = "levels", keep = as.character(lv))
    } else if (tp %in% c("continuous", "count")) {
      r <- rg[[nm]]
      if (!is.null(r)) {
        lo <- suppressWarnings(as.numeric(r$min))
        hi <- suppressWarnings(as.numeric(r$max))
        if (is.finite(lo) || is.finite(hi))
          out[[nm]] <- list(kind = "range",
                            min = if (is.finite(lo)) lo else NA_real_,
                            max = if (is.finite(hi)) hi else NA_real_,
                            integer = identical(tp, "count"))
      }
    }
  }
  out
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
