# fct_panel.R ---------------------------------------------------------------
# Longitudinal/panel summaries and chart helpers. These are opt-in wrappers;
# cross-sectional callers continue to use the existing descriptives/charts.

panel_active <- function(panel) {
  !is.null(panel) && is.list(panel) &&
    ((!is.null(panel$id) && nzchar(panel$id)) ||
       (!is.null(panel$time) && nzchar(panel$time)))
}

panel_has_subject_time <- function(panel) {
  panel_active(panel) && !is.null(panel$id) && nzchar(panel$id) &&
    !is.null(panel$time) && nzchar(panel$time)
}

panel_variable_status <- function(df, id_var, time_var, var) {
  if (any(!c(id_var, time_var, var) %in% names(df)))
    stop("Panel status needs valid ID, time, and selected-variable columns.", call. = FALSE)
  keep <- stats::complete.cases(df[, c(id_var, time_var, var), drop = FALSE])
  d <- df[keep, c(id_var, time_var, var), drop = FALSE]
  if (!nrow(d))
    return(list(status = "not assessable", by_id = data.frame()))
  split_var <- split(d[[var]], d[[id_var]])
  split_time <- split(d[[time_var]], d[[id_var]])
  rows <- lapply(names(split_var), function(id) {
    vals <- split_var[[id]]
    data.frame(
      ID = id,
      Observations = length(vals),
      Time_points = length(unique(split_time[[id]])),
      Distinct_values = length(unique(vals)),
      Status = if (length(unique(split_time[[id]])) < 2) "single time point"
               else if (length(unique(vals)) <= 1) "time-invariant"
               else "time-varying",
      stringsAsFactors = FALSE
    )
  })
  by_id <- do.call(rbind, rows)
  repeated <- by_id$Time_points >= 2
  status <- if (!any(repeated)) "not enough repeated observations"
            else if (all(by_id$Status[repeated] == "time-invariant")) "time-invariant"
            else "time-varying"
  list(status = status, by_id = by_id)
}

panel_numeric_frame <- function(df, id_var, time_var, var) {
  keep_vars <- c(id_var, time_var, var)
  if (any(!keep_vars %in% names(df))) return(NULL)
  d <- df[, keep_vars, drop = FALSE]
  d[[".__panel_value"]] <- suppressWarnings(as.numeric(as.character(d[[var]])))
  d <- d[!is.na(d[[id_var]]) & !is.na(d[[time_var]]) &
           is.finite(d[[".__panel_value"]]), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d
}

panel_within_between_summary <- function(df, id_var, time_var, var) {
  d <- panel_numeric_frame(df, id_var, time_var, var)
  if (is.null(d))
    return(data.frame(Note = "Within/between summaries require a numeric outcome with repeated observations."))
  ids <- d[[id_var]]
  person_means <- tapply(d$.__panel_value, ids, mean, na.rm = TRUE)
  centered <- d$.__panel_value - unname(person_means[as.character(ids)])
  sd_between <- stats::sd(person_means)
  sd_within <- stats::sd(centered)
  # Intraclass correlation: share of variance that is between-person. High ICC
  # means observations within a subject are strongly correlated (clustering
  # that a naive pooled summary would ignore).
  icc <- if (is.finite(sd_between) && is.finite(sd_within) &&
             (sd_between^2 + sd_within^2) > 0)
    sd_between^2 / (sd_between^2 + sd_within^2) else NA_real_
  data.frame(
    Measure = c("Overall mean", "Between-person SD", "Within-person SD",
                "Intraclass correlation (ICC)", "Subjects", "Observations"),
    Value = c(mean(d$.__panel_value), sd_between, sd_within, icc,
              length(person_means), nrow(d)),
    stringsAsFactors = FALSE
  )
}

panel_missingness_table <- function(df, time_var, var) {
  if (any(!c(time_var, var) %in% names(df)))
    return(data.frame(Note = "Select a valid time and outcome variable."))
  time_values <- df[[time_var]]
  groups <- split(seq_len(nrow(df)), time_values, drop = TRUE)
  rows <- lapply(names(groups), function(label) {
    ix <- groups[[label]]
    missing <- is.na(df[[var]][ix])
    data.frame(Time = label, Observations = length(ix), Missing = sum(missing),
               `Missing %` = 100 * mean(missing), stringsAsFactors = FALSE,
               check.names = FALSE)
  })
  if (!length(rows)) return(data.frame(Note = "No time points are available."))
  do.call(rbind, rows)
}

panel_delta_scores <- function(df, id_var, time_var, var) {
  d <- panel_numeric_frame(df, id_var, time_var, var)
  if (is.null(d))
    return(data.frame(Note = "Baseline-to-follow-up change requires a numeric outcome."))
  split_d <- split(d, d[[id_var]])
  rows <- lapply(names(split_d), function(id) {
    one <- split_d[[id]]
    one <- one[order(one[[time_var]]), , drop = FALSE]
    if (nrow(one) < 2) return(NULL)
    data.frame(ID = id, Baseline = one$.__panel_value[1],
               Follow_up = one$.__panel_value[nrow(one)],
               Change = one$.__panel_value[nrow(one)] - one$.__panel_value[1],
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(Note = "No subjects have both baseline and follow-up values."))
  changes <- do.call(rbind, rows)
  summary <- data.frame(
    Measure = c("Subjects with baseline/follow-up", "Mean change", "SD of change",
                "Median change", "Q1 change", "Q3 change"),
    Value = c(nrow(changes), mean(changes$Change), stats::sd(changes$Change),
              stats::median(changes$Change),
              stats::quantile(changes$Change, 0.25, names = FALSE),
              stats::quantile(changes$Change, 0.75, names = FALSE)),
    stringsAsFactors = FALSE
  )
  attr(summary, "individual_changes") <- changes
  summary
}

# One value per subject for a (time-invariant) variable: the first non-missing
# value per ID. Class is preserved (factors keep their levels), so the ordinary
# type-appropriate descriptive can be applied at the subject level - avoiding
# the double-counting a pooled across-rows summary would cause.
subject_level_values <- function(df, id_var, var) {
  ids <- df[[id_var]]; x <- df[[var]]
  keep <- !is.na(ids); ids <- ids[keep]; x <- x[keep]
  if (!length(ids)) return(x[0])
  first_idx <- tapply(seq_along(ids), ids, function(i) {
    nn <- i[!is.na(x[i])]
    if (length(nn)) nn[1] else i[1]
  })
  x[unlist(first_idx, use.names = FALSE)]
}

# Panel shape: how many subjects, how many observations, and how balanced.
panel_obs_structure <- function(df, id_var, time_var) {
  d <- df[!is.na(df[[id_var]]), , drop = FALSE]
  if (!nrow(d)) return(data.frame(Measure = "Note", Value = "No subjects available."))
  per <- table(d[[id_var]])
  n_time <- length(unique(d[[time_var]][!is.na(d[[time_var]])]))
  data.frame(
    Measure = c("Subjects", "Total observations", "Observations per subject (min)",
                "Observations per subject (median)", "Observations per subject (max)",
                "Balanced design (equal observations)?", "Distinct time points"),
    Value = c(as.character(length(per)), as.character(sum(per)),
              as.character(min(per)), as.character(stats::median(per)),
              as.character(max(per)),
              if (length(unique(as.integer(per))) == 1) "yes" else "no",
              as.character(n_time)),
    stringsAsFactors = FALSE
  )
}

# Time-series summary of a numeric variable: level, spread, range, and the
# net change from first to last time point. The pooled iid-style table is not
# appropriate here because the observations are time-ordered, not independent.
describe_time_series <- function(x, time) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  ok <- !is.na(time) & is.finite(v)
  v <- v[ok]; tt <- time[ok]
  if (!length(v)) return(data.frame(statistic = "n time points", value = "0",
                                    stringsAsFactors = FALSE))
  ord <- order(tt); v <- v[ord]; tt <- tt[ord]; n <- length(v)
  qs <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
  stat <- c("n time points", "first time", "last time", "mean level", "SD",
            "median", "IQR", "min", "max", "first value", "last value",
            "net change (last - first)")
  val <- c(as.character(n), as.character(min(tt)), as.character(max(tt)),
           as.character(round(mean(v), 4)), as.character(round(stats::sd(v), 4)),
           as.character(round(qs[2], 4)), as.character(round(qs[3] - qs[1], 4)),
           as.character(round(min(v), 4)), as.character(round(max(v), 4)),
           as.character(round(v[1], 4)), as.character(round(v[n], 4)),
           as.character(round(v[n] - v[1], 4)))

  # Time-series diagnostics (guarded so short/degenerate series still return the
  # basic summary above). Uses the same engine as the Advanced models tab.
  reg <- tryCatch(ts_regularity(tt), error = function(e) NULL)
  if (!is.null(reg) && !is.na(reg$summary)) {
    stat <- c(stat, "sampling"); val <- c(val, reg$summary)
  }
  freq <- tryCatch(ts_infer_frequency(tt), error = function(e) NA_integer_)
  if (is.finite(freq)) { stat <- c(stat, "inferred seasonal period"); val <- c(val, as.character(freq)) }
  acf1 <- tryCatch(as.numeric(stats::acf(v, lag.max = 1, plot = FALSE)$acf[2]),
                   error = function(e) NA_real_)
  if (is.finite(acf1)) { stat <- c(stat, "lag-1 autocorrelation"); val <- c(val, as.character(round(acf1, 4))) }
  if (n >= 3) {
    mk <- tryCatch(ts_mann_kendall(v), error = function(e) NULL)
    if (!is.null(mk)) {
      dir <- if (mk$p.value < 0.05) (if (mk$S > 0) "increasing" else "decreasing") else "no significant trend"
      stat <- c(stat, "trend (Mann-Kendall)", "Mann-Kendall tau", "Mann-Kendall p-value")
      val <- c(val, dir, as.character(round(mk$tau, 4)), as.character(signif(mk$p.value, 4)))
    }
  }
  if (n >= 8) {
    adf <- tryCatch(ts_adf(v), error = function(e) NULL)
    kpss <- tryCatch(ts_kpss(v, null = "Level"), error = function(e) NULL)
    if (!is.null(adf) && !is.null(kpss)) {
      nonstat <- adf$p.value >= 0.05 || kpss$p.value < 0.05
      stat <- c(stat, "ADF p-value", "KPSS p-value", "stationarity")
      val <- c(val, as.character(signif(adf$p.value, 4)), as.character(signif(kpss$p.value, 4)),
               if (nonstat) "non-stationary (consider differencing)" else "stationary")
    }
  }
  data.frame(statistic = stat, value = val, stringsAsFactors = FALSE)
}

#' The descriptive tables appropriate to the declared dataset KIND.
#'
#' Cross-sectional -> the ordinary type-appropriate table (+ optional grouping).
#' Longitudinal    -> per-subject summary for a time-invariant variable, or the
#'                    panel decomposition (structure, by-time, within/between,
#'                    change, missingness) for a time-varying one. No pooled
#'                    across-rows table (it double-counts repeated subjects).
#' Time series     -> a time-series summary (numeric) or by-time frequencies,
#'                    plus missingness by time. No pooled iid-style table.
#'
#' @return list(kind, status, headline, tables = named list of data frames).
descriptive_tables <- function(df, var, type, panel = NULL, group = NULL) {
  ps <- panel
  kind <- if (!is.null(ps) && !is.null(ps$kind)) ps$kind
          else if (panel_has_subject_time(ps)) "repeated"
          else if (panel_active(ps)) "time_series"
          else "cross_sectional"
  # If a longitudinal / time-series layout was chosen but its role columns are
  # not (yet) selected, we cannot form the panel summaries - fall back to the
  # plain cross-sectional table so the tab still works while roles are set.
  if (identical(kind, "repeated") && !panel_has_subject_time(ps))
    kind <- "cross_sectional"
  if (identical(kind, "time_series") &&
      (is.null(ps$time) || !nzchar(ps$time) || !ps$time %in% names(df)))
    kind <- "cross_sectional"
  x <- df[[var]]
  out <- list()
  has_group <- !is.null(group) && nzchar(group) && !identical(group, "(none)") &&
    group %in% names(df)

  if (identical(kind, "cross_sectional")) {
    out[[sprintf("Descriptives - %s", var)]] <- describe_variable(x, type)
    if (has_group)
      out[[sprintf("%s by %s", var, group)]] <- describe_by_group(x, df[[group]], type)
    return(list(kind = kind, status = NULL, tables = out,
                headline = "Cross-sectional: one row per subject, summarised directly."))
  }

  if (identical(kind, "time_series")) {
    tvar <- ps$time
    if (is_numeric_type(type))
      out[["Time-series summary"]] <- describe_time_series(x, df[[tvar]])
    else
      out[["Summary by time point"]] <- describe_by_group(x, df[[tvar]], type)
    out[["Missingness by time"]] <- panel_missingness_table(df, tvar, var)
    return(list(kind = kind, status = NULL, tables = out,
                headline = "Time series: summarised over time. A pooled table treating rows as independent is not shown."))
  }

  # ---- repeated / longitudinal -------------------------------------------
  id <- ps$id; tvar <- ps$time
  status <- tryCatch(panel_variable_status(df, id, tvar, var)$status,
                     error = function(e) "not assessable")
  if (identical(status, "time-invariant")) {
    sub <- subject_level_values(df, id, var)
    out[[sprintf("Per-subject summary - %s", var)]] <- describe_variable(sub, type)
    return(list(kind = kind, status = status, tables = out,
                headline = sprintf("Longitudinal, time-invariant: '%s' does not change within a subject, so it is summarised once per subject (%d subjects) rather than pooled across rows.",
                                   var, length(sub))))
  }
  # time-varying (or not-enough-repeats): panel decomposition, no pooled table
  out[["Observation structure"]] <- panel_obs_structure(df, id, tvar)
  out[["Summary by time point"]] <- describe_by_group(x, df[[tvar]], type)
  if (is_numeric_type(type)) {
    out[["Within- and between-person variation"]] <-
      panel_within_between_summary(df, id, tvar, var)
    out[["Baseline-to-follow-up change"]] <- panel_delta_scores(df, id, tvar, var)
  }
  out[["Missingness / attrition by time"]] <- panel_missingness_table(df, tvar, var)
  list(kind = kind, status = status, tables = out,
       headline = sprintf("Longitudinal, time-varying: '%s' is summarised by time point and decomposed into within- and between-person variation. A pooled across-rows table is not shown because repeated observations are not independent.",
                          var))
}

panel_chart_data <- function(df, panel, y_var, colour = NULL) {
  if (!panel_has_subject_time(panel) || any(!c(panel$id, panel$time, y_var) %in% names(df)))
    return(NULL)
  d <- df[, unique(c(panel$id, panel$time, y_var, colour)), drop = FALSE]
  d <- d[!is.na(d[[panel$id]]) & !is.na(d[[panel$time]]) &
           is.finite(suppressWarnings(as.numeric(as.character(d[[y_var]])))), , drop = FALSE]
  d[[".__panel_y"]] <- suppressWarnings(as.numeric(as.character(d[[y_var]])))
  d
}

build_panel_chart <- function(df, spec, panel, trajectories = TRUE,
                              aggregate = TRUE) {
  y_var <- spec$y %||% spec$x
  if (is.null(y_var) || !panel_has_subject_time(panel))
    stop("Choose a numeric outcome with a subject ID and time variable for a panel chart.",
         call. = FALSE)
  colour <- if (!is.null(spec$group) && nzchar(spec$group)) spec$group else NULL
  d <- panel_chart_data(df, panel, y_var, colour)
  if (is.null(d) || !nrow(d)) stop("No complete observations are available for the panel chart.", call. = FALSE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[panel$time]],
                                       y = .data[[".__panel_y"]]))
  if (trajectories) {
    if (is.null(colour)) {
      p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[panel$id]]),
                                  alpha = 0.3, colour = "#018786")
    } else {
      p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[panel$id]],
                                               colour = .data[[colour]]), alpha = 0.3)
    }
    p <- p + ggplot2::geom_point(alpha = 0.35, size = 1.3)
  }
  if (aggregate) {
    if (is.null(colour)) {
      p <- p + ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean,
                                     geom = "line", linewidth = 1.2,
                                     colour = "#D55E00") +
        ggplot2::stat_summary(ggplot2::aes(group = 1), fun.data = mean_se,
                              geom = "errorbar", width = 0.1,
                              colour = "#D55E00")
    } else {
      p <- p + ggplot2::stat_summary(ggplot2::aes(group = .data[[colour]],
                                                 colour = .data[[colour]]),
                                     fun = mean, geom = "line", linewidth = 1.2) +
        ggplot2::stat_summary(ggplot2::aes(group = .data[[colour]],
                                           colour = .data[[colour]]),
                              fun.data = mean_se, geom = "errorbar", width = 0.1)
    }
  }
  if (!is.null(spec$facet) && nzchar(spec$facet) && spec$facet %in% names(d))
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[spec$facet]]))
  p + ggplot2::labs(title = if (nzchar(spec$title %||% "")) spec$title else "Longitudinal trajectory",
                    x = panel$time, y = y_var,
                    subtitle = spec$subtitle %||% NULL,
                    caption = spec$caption %||% NULL) +
    ggplot2::theme_minimal(base_size = spec$base_size %||% 13)
}

build_time_series_chart <- function(df, spec, panel, aggregate = TRUE) {
  y_var <- spec$y %||% spec$x
  if (is.null(y_var) || is.null(panel$time) || !panel$time %in% names(df))
    stop("Choose a numeric outcome and time variable for a time-series chart.", call. = FALSE)
  d <- df[, unique(c(panel$time, y_var, spec$group, spec$facet)), drop = FALSE]
  d$.__panel_y <- suppressWarnings(as.numeric(as.character(d[[y_var]])))
  d <- d[!is.na(d[[panel$time]]) & is.finite(d$.__panel_y), , drop = FALSE]
  if (!nrow(d)) stop("No complete observations are available for the time-series chart.", call. = FALSE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[panel$time]], y = .data$.__panel_y)) +
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#018786", alpha = 0.65) +
    ggplot2::geom_point(colour = "#018786", alpha = 0.65)
  if (aggregate)
    p <- p + ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean,
                                   geom = "line", linewidth = 1.2,
                                   colour = "#D55E00") +
      ggplot2::stat_summary(ggplot2::aes(group = 1), fun.data = mean_se,
                            geom = "errorbar", width = 0.1,
                            colour = "#D55E00")
  if (!is.null(spec$group) && spec$group %in% names(d))
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[spec$group]]))
  p + ggplot2::labs(title = if (nzchar(spec$title %||% "")) spec$title else "Time-series trend",
                    x = panel$time, y = y_var,
                    subtitle = spec$subtitle %||% NULL,
                    caption = spec$caption %||% NULL) +
    ggplot2::theme_minimal(base_size = spec$base_size %||% 13)
}
