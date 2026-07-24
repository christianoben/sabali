# fct_descriptives.R --------------------------------------------------------
# Descriptive statistics by measurement type (Section 3 of the design doc).
# Each function returns a tidy data frame that is easy to render and export.

# 95% Wilson confidence interval for a proportion.
wilson_ci <- function(successes, n, conf = 0.95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  phat <- successes / n
  denom <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(max(0, centre - half), min(1, centre + half))
}

skewness <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x); s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - m)^3) / n) / (s^3)
}

kurtosis_excess <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x); s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - m)^4) / n) / (s^4) - 3
}

# --- Numeric (continuous / count) ------------------------------------------
describe_numeric <- function(x) {
  x_ok <- x[!is.na(x)]; n <- length(x_ok)
  if (n == 0) return(data.frame(statistic = "n valid", value = 0))
  m  <- mean(x_ok); s <- stats::sd(x_ok)
  se <- if (n > 1) s / sqrt(n) else NA_real_
  ci <- if (n > 1) m + c(-1, 1) * stats::qt(0.975, n - 1) * se else c(NA, NA)
  qs <- stats::quantile(x_ok, c(0.25, 0.5, 0.75), names = FALSE)

  data.frame(
    statistic = c("n valid", "missing", "mean", "SD", "SE", "95% CI lower",
                  "95% CI upper", "median", "Q1", "Q3", "IQR", "min", "max",
                  "range", "skewness", "excess kurtosis"),
    value = round(c(n, sum(is.na(x)), m, s, se, ci[1], ci[2], qs[2], qs[1],
                    qs[3], qs[3] - qs[1], min(x_ok), max(x_ok),
                    max(x_ok) - min(x_ok), skewness(x_ok), kurtosis_excess(x_ok)), 4),
    stringsAsFactors = FALSE
  )
}

# --- Binary ----------------------------------------------------------------
describe_binary <- function(x) {
  tb <- table(x, useNA = "no")
  n  <- sum(tb)
  lv <- names(tb)
  out <- data.frame(
    level = lv,
    count = as.integer(tb),
    percent = round(100 * as.integer(tb) / n, 2),
    stringsAsFactors = FALSE
  )
  # Wilson CI for the first level's proportion.
  ci <- wilson_ci(out$count[1], n)
  attr(out, "note") <- sprintf(
    "Proportion '%s' = %.3f (95%% Wilson CI %.3f to %.3f); valid n = %d, missing = %d.",
    lv[1], out$count[1] / n, ci[1], ci[2], n, sum(is.na(x)))
  out
}

# --- Categorical (nominal / ordinal) ---------------------------------------
describe_categorical <- function(x, ordered = FALSE) {
  tb <- table(x, useNA = "no")
  n  <- sum(tb)
  out <- data.frame(
    level = names(tb),
    count = as.integer(tb),
    percent = round(100 * as.integer(tb) / n, 2),
    cumulative_pct = round(100 * cumsum(as.integer(tb)) / n, 2),
    stringsAsFactors = FALSE
  )
  attr(out, "note") <- sprintf("valid n = %d, missing = %d, categories = %d.",
                               n, sum(is.na(x)), nrow(out))
  out
}

#' Dispatch descriptives by measurement type.
describe_variable <- function(x, type) {
  if (is_numeric_type(type))     return(describe_numeric(x))
  if (type == "binary")          return(describe_binary(x))
  if (is_categorical_type(type)) return(describe_categorical(x))
  if (type == "datetime") {
    x_ok <- x[!is.na(x)]
    return(data.frame(
      statistic = c("n valid", "missing", "earliest", "latest"),
      value = c(length(x_ok), sum(is.na(x)),
                as.character(min(x_ok)), as.character(max(x_ok))),
      stringsAsFactors = FALSE))
  }
  data.frame(statistic = "note", value = "No descriptives for this type.")
}

# Note describing how many rows were dropped for a missing grouping value.
# Rows whose group is NA are never silently ignored: either the caller shows a
# '(missing)' row (categorical) or this note is attached (numeric/datetime).
.missing_group_note <- function(g) {
  n_missing <- sum(is.na(g))
  if (n_missing == 0) return(NULL)
  sprintf("%d row%s had a missing group value and were dropped from the per-group summary.",
          n_missing, if (n_missing == 1) "" else "s")
}

#' Grouped descriptives: numeric summary of `x` split by factor `g`.
describe_numeric_by_group <- function(x, g) {
  sp <- split(x, g)   # rows with g == NA are excluded by split()
  rows <- lapply(names(sp), function(grp) {
    xi <- sp[[grp]]; xi <- xi[!is.na(xi)]
    if (length(xi) == 0) return(NULL)
    data.frame(group = grp, n = length(xi),
               mean = round(mean(xi), 3), sd = round(stats::sd(xi), 3),
               median = round(stats::median(xi), 3),
               IQR = round(stats::IQR(xi), 3),
               min = round(min(xi), 3), max = round(max(xi), 3),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(out)) attr(out, "note") <- .missing_group_note(g)
  out
}

#' Grouped descriptives for a categorical/binary/ordinal variable.
#'
#' Tidy long layout: one row per (group x level) with the within-group count and
#' column percentage. Rows with a missing group value are surfaced as an explicit
#' '(missing)' group so nothing is dropped silently.
describe_categorical_by_group <- function(x, g, ordered = FALSE) {
  g2 <- as.factor(g)
  if (anyNA(g2)) {
    lab <- "(missing)"
    if (lab %in% levels(g2)) lab <- "(missing group)"
    g2 <- factor(g2, levels = c(levels(g2), lab))
    g2[is.na(g2)] <- lab
  }
  g2 <- droplevels(g2)

  xf <- if (ordered) factor(x, ordered = TRUE) else as.factor(x)

  rows <- lapply(levels(g2), function(gl) {
    xi <- xf[g2 == gl]
    tb <- table(xi, useNA = "no")
    n  <- sum(tb)
    if (n == 0) return(NULL)
    data.frame(group = gl,
               level = names(tb),
               count = as.integer(tb),
               percent = round(100 * as.integer(tb) / n, 2),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(out)) attr(out, "note") <- .missing_group_note(g)
  out
}

#' Grouped descriptives for a datetime variable: n / earliest / latest per group.
describe_datetime_by_group <- function(x, g) {
  sp <- split(x, g)
  rows <- lapply(names(sp), function(grp) {
    xi <- sp[[grp]]; xi <- xi[!is.na(xi)]
    if (length(xi) == 0) return(NULL)
    data.frame(group = grp, n = length(xi),
               earliest = as.character(min(xi)),
               latest = as.character(max(xi)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(out)) attr(out, "note") <- .missing_group_note(g)
  out
}

#' Type-aware dispatcher for grouped descriptives.
#'
#' Produces a per-group summary appropriate to the main variable's measurement
#' type, for ANY analysable type (numeric, binary, nominal, ordinal, datetime).
#' Returns NULL only for non-analysable types (identifier / text).
describe_by_group <- function(x, g, type) {
  if (is_numeric_type(type))     return(describe_numeric_by_group(x, g))
  if (is_categorical_type(type)) return(describe_categorical_by_group(
                                          x, g, ordered = identical(type, "ordinal")))
  if (identical(type, "datetime")) return(describe_datetime_by_group(x, g))
  NULL
}
