# fct_timeseries.R -----------------------------------------------------------
# Time-series analysis engine for the Advanced models tab. Handles the
# "time_series" data layout the user declares on the Data tab (one ordered
# series over a time/date column, no subject clustering). Every runner returns
# the same result contract used by the model/survival tabs and the Word report
# (label, method, coefficients, fit_stats, assumptions, diagnostic_plot,
# interpretation, rationale, note, n_total, n_used, effect_name, recommendation).
#
# Design choice: like the rest of the app, the engine uses base {stats}
# (arima, stl, decompose, HoltWinters, spec.pgram, acf/pacf, PP.test, Box.test)
# and {lmtest} (dwtest, bgtest) - both already hard dependencies - rather than
# adding {forecast}/{tseries}/{urca}. Three classical tests that base R does not
# provide (the Augmented Dickey-Fuller and KPSS stationarity tests, and the
# Mann-Kendall trend test with Sen's slope) are reimplemented here and validated
# against independent recomputation / hand-worked examples in the test suite
# (Tier-2 in the validation plan). The unit-root critical-value tables embedded
# below are the standard published response-surface constants (Dickey & Fuller;
# Kwiatkowski, Phillips, Schmidt & Shin 1992) - statistical facts, not code.

TS_METHOD_LABELS <- c(
  ts_decompose    = "Seasonal decomposition (STL / classical)",
  ts_stationarity = "Stationarity and autocorrelation diagnostics",
  ts_trend        = "Trend detection (Mann-Kendall and Sen's slope)",
  ts_holt_winters = "Exponential smoothing forecast (Holt-Winters)",
  ts_arima        = "ARIMA / SARIMA model and forecast",
  ts_regression   = "Time-series regression (trend and seasonality)",
  ts_garch        = "Volatility model (GARCH)"
)

# Optional packages that upgrade the engine when installed (used-when-present,
# like ARTool / rcompanion elsewhere in the app):
#   * urca    -> exact MacKinnon unit-root critical values (ur.df / ur.kpss) and
#                the Zivot-Andrews structural-break test, replacing the built-in
#                approximate ADF/KPSS in the stationarity diagnostics.
#   * rugarch -> the GARCH volatility model (ts_garch); when absent the method is
#                hidden from the menu and its runner returns a clear message.
ts_urca_available <- function() requireNamespace("urca", quietly = TRUE)
ts_rugarch_available <- function() requireNamespace("rugarch", quietly = TRUE)

# The subset of methods that require a seasonal period (>= 2 observations per
# cycle and at least two full cycles) to run.
TS_SEASONAL_METHODS <- c("ts_decompose")

# --- data preparation -------------------------------------------------------

# Infer a plausible seasonal period from the spacing of a time/date column.
# Returns 1 (no seasonality) for numeric indices or yearly-or-coarser dates.
ts_infer_frequency <- function(time) {
  if (inherits(time, c("Date", "POSIXct", "POSIXt"))) {
    # Deduplicate first, so stacked (multi-series) data - where each date repeats
    # once per series - does not collapse the median spacing to zero.
    tn <- sort(unique(as.numeric(as.Date(time))))
    d <- suppressWarnings(as.numeric(stats::median(diff(tn), na.rm = TRUE)))
    if (!is.finite(d) || d <= 0) return(1L)
    if (d <= 1.5)  return(7L)    # daily      -> weekly cycle
    if (d <= 10)   return(52L)   # weekly     -> yearly cycle
    if (d <= 45)   return(12L)   # monthly    -> yearly cycle
    if (d <= 100)  return(4L)    # quarterly  -> yearly cycle
    return(1L)                   # yearly or coarser
  }
  1L
}

# Summarise how evenly spaced the time points are, and count gaps (spacings
# more than 1.5x the typical step). Used for descriptives and to warn the user.
ts_regularity <- function(time) {
  n <- length(time)
  if (n < 3) return(list(regular = NA, n_gaps = 0L, step = NA_real_,
                         summary = "too few points to assess spacing"))
  tnum <- if (inherits(time, c("Date", "POSIXct", "POSIXt")))
    as.numeric(as.Date(time)) else suppressWarnings(as.numeric(time))
  d <- diff(sort(tnum))
  d <- d[is.finite(d) & d > 0]
  if (!length(d)) return(list(regular = NA, n_gaps = 0L, step = NA_real_,
                              summary = "spacing not assessable"))
  step <- stats::median(d)
  n_gaps <- sum(d > 1.5 * step)
  regular <- n_gaps == 0 && stats::sd(d) <= 1e-6 * max(1, step)
  unit <- if (inherits(time, c("Date", "POSIXct", "POSIXt"))) " day(s)" else ""
  list(regular = regular, n_gaps = as.integer(n_gaps), step = step,
       summary = sprintf("typical step %s%s%s", round(step, 3), unit,
                         if (n_gaps > 0) sprintf("; %d gap(s) detected", n_gaps) else ""))
}

# Build an analysable series from a data frame: order by time, coerce the value
# to numeric, drop unusable rows, infer/accept a seasonal period, and wrap it in
# a stats::ts object. `min_n` guards against series too short to analyse.
ts_prepare <- function(df, time_col, value_col, frequency = NULL, min_n = 8L) {
  if (is.null(time_col) || is.null(value_col) ||
      !all(c(time_col, value_col) %in% names(df)))
    stop("The time column and the value column must both be present in the data.",
         call. = FALSE)
  if (identical(time_col, value_col))
    stop("The time column and the value column must be different variables.",
         call. = FALSE)
  tv <- df[[time_col]]
  yv <- suppressWarnings(as.numeric(as.character(df[[value_col]])))
  keep <- !is.na(tv) & is.finite(yv)
  tv <- tv[keep]; yv <- yv[keep]
  if (length(yv) < min_n)
    stop(sprintf("Only %d usable time point(s) are available; at least %d are needed for time-series analysis.",
                 length(yv), min_n), call. = FALSE)
  ord <- order(tv)
  tv <- tv[ord]; yv <- yv[ord]
  # A single ordered series cannot have the same time point twice. Repeated
  # times almost always mean several series are stacked in one file (one per
  # site/group), which - pooled - would be silently mis-analysed. Refuse, and
  # point the user at the series/group selector.
  dup <- sum(duplicated(tv))
  if (dup > 0)
    stop(sprintf(paste0(
      "The time column '%s' has %d repeated time point(s), so these rows are ",
      "not one ordered series. If the file stacks several series together (e.g. ",
      "one per site or group), choose the series/group column so each is ",
      "analysed on its own. Otherwise aggregate or remove the duplicate times."),
      time_col, dup), call. = FALSE)
  reg <- ts_regularity(tv)
  freq <- frequency %||% ts_infer_frequency(tv)
  freq <- suppressWarnings(as.integer(round(as.numeric(freq))))
  if (!is.finite(freq) || freq < 1L) freq <- 1L
  list(y = stats::ts(yv, frequency = freq), values = yv, time = tv,
       n = length(yv), frequency = freq, regular = reg$regular,
       spacing = reg$summary, n_gaps = reg$n_gaps, step = reg$step,
       value_name = value_col, time_name = time_col, n_total = nrow(df))
}

# Extend a time/date vector by `h` steps using the typical spacing, so forecasts
# can be plotted on the original time axis.
ts_future_time <- function(time, h) {
  n <- length(time)
  if (inherits(time, c("Date", "POSIXct", "POSIXt"))) {
    tnum <- as.numeric(as.Date(time)); step <- stats::median(diff(tnum))
    as.Date(tnum[n] + step * seq_len(h), origin = "1970-01-01")
  } else {
    tnum <- suppressWarnings(as.numeric(time)); step <- stats::median(diff(tnum))
    tnum[n] + step * seq_len(h)
  }
}

# --- reimplemented classical tests (Tier-2, validated in tests) -------------

# Mann-Kendall non-parametric trend test with tie correction and continuity
# correction. Returns S, its variance, the z-statistic, two-sided p and Kendall
# tau-b. Validated against a hand-worked example and independent recomputation.
ts_mann_kendall <- function(y) {
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 3) stop("The Mann-Kendall test needs at least 3 observations.", call. = FALSE)
  S <- 0
  for (k in seq_len(n - 1L)) S <- S + sum(sign(y[(k + 1L):n] - y[k]))
  ties <- as.numeric(table(y))
  tie_term <- sum(ties * (ties - 1) * (2 * ties + 5))
  varS <- (n * (n - 1) * (2 * n + 5) - tie_term) / 18
  z <- if (S > 0) (S - 1) / sqrt(varS) else if (S < 0) (S + 1) / sqrt(varS) else 0
  p <- 2 * stats::pnorm(-abs(z))
  tau <- S / (0.5 * n * (n - 1))
  list(S = S, varS = varS, z = z, p.value = min(1, p), tau = tau, n = n)
}

# Theil-Sen slope: the median of all pairwise slopes. Robust to outliers and the
# natural companion to the Mann-Kendall test.
ts_sen_slope <- function(y, time = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  t <- if (is.null(time)) seq_len(n) else {
    if (inherits(time, c("Date", "POSIXct", "POSIXt"))) as.numeric(as.Date(time))
    else suppressWarnings(as.numeric(time))
  }
  ok <- is.finite(y) & is.finite(t)
  y <- y[ok]; t <- t[ok]; n <- length(y)
  if (n < 2) return(NA_real_)
  idx <- utils::combn(n, 2)
  dt <- t[idx[2, ]] - t[idx[1, ]]
  dy <- y[idx[2, ]] - y[idx[1, ]]
  keep <- dt != 0
  if (!any(keep)) return(NA_real_)
  stats::median(dy[keep] / dt[keep])
}

# Augmented Dickey-Fuller unit-root test (trend case), reimplemented to match
# the standard regression-based procedure. The null is a unit root (non-
# stationary); a large negative statistic rejects it. The p-value is interpolated
# from the published Dickey-Fuller response-surface table. `k` is the number of
# lagged differences (default trunc((n-1)^(1/3))).
ts_adf <- function(x, k = NULL) {
  x <- as.numeric(x)
  n_orig <- length(x)
  if (n_orig < 6) stop("The ADF test needs at least 6 observations.", call. = FALSE)
  if (is.null(k)) k <- trunc((n_orig - 1)^(1 / 3))
  k <- k + 1L
  y <- diff(x)
  n <- length(y)
  z <- stats::embed(y, k)
  yt <- z[, 1]
  xt1 <- x[k:n]
  tt <- k:n
  if (k > 1L) {
    yt1 <- z[, 2:k, drop = FALSE]
    res <- stats::lm(yt ~ xt1 + 1 + tt + yt1)
  } else {
    res <- stats::lm(yt ~ xt1 + 1 + tt)
  }
  co <- summary(res)$coefficients
  stat <- co["xt1", 1] / co["xt1", 2]
  # Published Dickey-Fuller critical values (trend case) by sample size and
  # probability; interpolate to this series' length and statistic.
  tbl <- -matrix(c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96,
                   3.95, 3.80, 3.73, 3.69, 3.68, 3.66,
                   3.60, 3.50, 3.45, 3.43, 3.42, 3.41,
                   3.24, 3.18, 3.15, 3.13, 3.13, 3.12,
                   1.14, 1.19, 1.22, 1.23, 1.24, 1.25,
                   0.80, 0.87, 0.90, 0.92, 0.93, 0.94,
                   0.50, 0.58, 0.62, 0.64, 0.65, 0.66,
                   0.15, 0.24, 0.28, 0.31, 0.32, 0.33),
                 nrow = 8, byrow = TRUE)
  tableT <- c(25, 50, 100, 250, 500, 1e5)
  tablep <- c(0.01, 0.025, 0.05, 0.10, 0.90, 0.95, 0.975, 0.99)
  crit_at_n <- vapply(seq_len(nrow(tbl)),
                      function(i) stats::approx(tableT, tbl[i, ], n_orig, rule = 2)$y,
                      numeric(1))
  p <- stats::approx(crit_at_n, tablep, stat, rule = 2)$y
  list(statistic = stat, p.value = p, lag = k - 1L, n = n_orig,
       at_boundary = stat < min(crit_at_n) || stat > max(crit_at_n))
}

# KPSS stationarity test. Unlike ADF the NULL is stationarity, so a large
# statistic (small p) is evidence AGAINST stationarity. `null` selects a level-
# or trend-stationary null. Long-run variance uses a Bartlett kernel with the
# standard automatic bandwidth. Validated by independent recomputation in tests.
ts_kpss <- function(x, null = c("Level", "Trend")) {
  null <- match.arg(null)
  x <- as.numeric(x)
  n <- length(x)
  if (n < 6) stop("The KPSS test needs at least 6 observations.", call. = FALSE)
  if (identical(null, "Trend")) {
    tt <- seq_len(n)
    e <- stats::residuals(stats::lm(x ~ tt))
    crit <- c(0.119, 0.146, 0.176, 0.216)
  } else {
    e <- stats::residuals(stats::lm(x ~ 1))
    crit <- c(0.347, 0.463, 0.574, 0.739)
  }
  tablep <- c(0.10, 0.05, 0.025, 0.01)
  S <- cumsum(e)
  eta <- sum(S^2) / (n^2)
  l <- trunc(4 * (n / 100)^0.25)
  s2 <- sum(e^2) / n
  if (l > 0) for (i in seq_len(l)) {
    w <- 1 - i / (l + 1)
    s2 <- s2 + (2 / n) * w * sum(e[1:(n - i)] * e[(i + 1):n])
  }
  stat <- eta / s2
  p <- stats::approx(crit, tablep, stat, rule = 2)$y
  list(statistic = stat, p.value = p, lag = l, null = null,
       at_boundary = stat < min(crit) || stat > max(crit))
}

# --- chart builders (used by the Charts tab and as diagnostic plots) --------

.ts_theme <- function(p, title, x, y) {
  p + ggplot2::labs(title = title, x = x, y = y) +
    ggplot2::theme_minimal(base_size = 13)
}

# Autocorrelation (ACF) or partial-autocorrelation (PACF) bar plot with the
# +/- 1.96/sqrt(n) white-noise bounds.
ts_correlogram_plot <- function(prepared, partial = FALSE, lag_max = NULL) {
  y <- prepared$values
  n <- prepared$n
  lag_max <- lag_max %||% max(10L, min(as.integer(2 * prepared$frequency), n %/% 2L))
  if (partial) {
    a <- stats::pacf(y, lag.max = lag_max, plot = FALSE)
    d <- data.frame(lag = as.numeric(a$lag), acf = as.numeric(a$acf))
    ttl <- "Partial autocorrelation (PACF)"
  } else {
    a <- stats::acf(y, lag.max = lag_max, plot = FALSE)
    d <- data.frame(lag = as.numeric(a$lag), acf = as.numeric(a$acf))
    d <- d[d$lag > 0, , drop = FALSE]
    ttl <- "Autocorrelation (ACF)"
  }
  bound <- stats::qnorm(0.975) / sqrt(n)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed",
                        colour = "#D55E00") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), colour = "#018786",
                          linewidth = 0.9)
  .ts_theme(p, sprintf("%s - %s", ttl, prepared$value_name),
            "Lag", "Correlation")
}

# STL (preferred) or classical seasonal decomposition, drawn as stacked panels.
ts_decomposition_plot <- function(prepared) {
  parts <- ts_decompose_components(prepared)
  d <- parts$long
  d$component <- factor(d$component,
                        levels = c("observed", "trend", "seasonal", "remainder"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#018786") +
    ggplot2::facet_wrap(~ component, ncol = 1, scales = "free_y")
  .ts_theme(p, sprintf("Seasonal decomposition (%s) - %s", parts$method,
                       prepared$value_name),
            prepared$time_name, prepared$value_name)
}

# Seasonal (subseries) plot: the value against position-in-cycle, one line per
# cycle, so seasonal shape and drift are visible.
ts_seasonal_plot <- function(prepared) {
  if (prepared$frequency < 2)
    stop("A seasonal plot needs a seasonal period of at least 2 (set the period).",
         call. = FALSE)
  f <- prepared$frequency
  n <- prepared$n
  season <- ((seq_len(n) - 1L) %% f) + 1L
  cycle <- ((seq_len(n) - 1L) %/% f) + 1L
  d <- data.frame(season = season, cycle = factor(cycle), value = prepared$values)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = season, y = value,
                                       colour = cycle, group = cycle)) +
    ggplot2::geom_line(alpha = 0.8) + ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_continuous(breaks = seq_len(f))
  .ts_theme(p, sprintf("Seasonal plot (period %d) - %s", f, prepared$value_name),
            "Position in cycle", prepared$value_name) +
    ggplot2::labs(colour = "Cycle")
}

# Spectral density (raw periodogram): power against frequency, to reveal
# dominant cyclical periods.
ts_spectrum_plot <- function(prepared) {
  sp <- stats::spec.pgram(prepared$y, plot = FALSE, detrend = TRUE, taper = 0)
  d <- data.frame(frequency = sp$freq, spectrum = sp$spec)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = frequency, y = spectrum)) +
    ggplot2::geom_line(colour = "#018786")
  .ts_theme(p, sprintf("Spectral density (periodogram) - %s", prepared$value_name),
            "Frequency (cycles per observation)", "Spectral power")
}

# Dispatch a time-series chart from a chart `spec` (used by build_chart on the
# Charts tab). The value column is spec$y (or spec$x); the time column comes
# from spec$ts_time (the Data-tab time variable). Failures degrade to a labelled
# empty plot, matching the other complex chart builders.
.ts_chart <- function(df, spec, which) {
  value_col <- spec$y %||% spec$x
  time_col <- spec$ts_time %||% spec$x
  fail <- function(msg) ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1, label = msg)
  prepared <- tryCatch(ts_prepare(df, time_col, value_col, frequency = spec$ts_frequency),
                       error = function(e) e)
  if (inherits(prepared, "error"))
    return(fail(paste("Time-series chart unavailable:", conditionMessage(prepared))))
  tryCatch(switch(which,
    acf           = ts_correlogram_plot(prepared, partial = FALSE),
    pacf          = ts_correlogram_plot(prepared, partial = TRUE),
    decomposition = ts_decomposition_plot(prepared),
    seasonal      = ts_seasonal_plot(prepared),
    spectrum      = ts_spectrum_plot(prepared),
    fail(paste("Unknown time-series chart:", which))),
    error = function(e) fail(paste("Could not draw chart:", conditionMessage(e))))
}

# --- decomposition helper (shared by the runner and the plot) ---------------
ts_decompose_components <- function(prepared) {
  if (prepared$frequency < 2)
    stop("Seasonal decomposition needs a seasonal period of at least 2. Set the seasonal period on the sidebar (e.g. 12 for monthly data with a yearly cycle).",
         call. = FALSE)
  if (prepared$n < 2 * prepared$frequency)
    stop(sprintf("Seasonal decomposition needs at least two full cycles (>= %d observations for a period of %d); only %d are available.",
                 2 * prepared$frequency, prepared$frequency, prepared$n), call. = FALSE)
  stl_fit <- tryCatch(stats::stl(prepared$y, s.window = "periodic", robust = TRUE),
                      error = function(e) NULL)
  if (!is.null(stl_fit)) {
    cmp <- as.data.frame(stl_fit$time.series)
    trend <- cmp$trend; seasonal <- cmp$seasonal; remainder <- cmp$remainder
    method <- "STL"
  } else {
    dec <- stats::decompose(prepared$y)
    trend <- as.numeric(dec$trend); seasonal <- as.numeric(dec$seasonal)
    remainder <- as.numeric(dec$random); method <- "classical"
  }
  observed <- as.numeric(prepared$y)
  long <- rbind(
    data.frame(time = prepared$time, component = "observed", value = observed),
    data.frame(time = prepared$time, component = "trend", value = trend),
    data.frame(time = prepared$time, component = "seasonal", value = seasonal),
    data.frame(time = prepared$time, component = "remainder", value = remainder)
  )
  # Trend / seasonal strength (Wang, Smith & Hyndman): 1 - Var(remainder)/Var(.)
  var_r <- stats::var(remainder, na.rm = TRUE)
  strength <- function(part) {
    v <- stats::var(part + remainder, na.rm = TRUE)
    if (!is.finite(v) || v <= 0) return(NA_real_)
    max(0, 1 - var_r / v)
  }
  list(method = method, trend = trend, seasonal = seasonal, remainder = remainder,
       long = long, trend_strength = strength(trend),
       seasonal_strength = strength(seasonal))
}

# --- runners ----------------------------------------------------------------

ts_note_common <- function(prepared) {
  reg <- if (isTRUE(prepared$regular)) "evenly spaced"
         else if (isFALSE(prepared$regular)) "not evenly spaced (treated as ordered)"
         else "of unassessed spacing"
  sprintf("%d ordered time points (%s; %s). Analyses assume the observations are in time order.",
          prepared$n, prepared$spacing, reg)
}

run_ts_decompose <- function(prepared) {
  parts <- ts_decompose_components(prepared)
  coef_tab <- data.frame(
    Component = c("Trend", "Seasonal", "Remainder"),
    `Std. dev.` = round(c(stats::sd(parts$trend, na.rm = TRUE),
                          stats::sd(parts$seasonal, na.rm = TRUE),
                          stats::sd(parts$remainder, na.rm = TRUE)), 4),
    check.names = FALSE, stringsAsFactors = FALSE)
  fit_tab <- named_metric_table(c(
    "n used" = prepared$n,
    "Seasonal period" = prepared$frequency,
    "Trend strength (0-1)" = round(parts$trend_strength, 4),
    "Seasonal strength (0-1)" = round(parts$seasonal_strength, 4)))
  interp <- sprintf(
    "The series was separated into trend, a repeating seasonal pattern (period %d) and an irregular remainder using %s decomposition. Trend strength was %s and seasonal strength %s (0 = negligible, 1 = dominant). Decomposition is descriptive; it does not test significance.",
    prepared$frequency, parts$method,
    round(parts$trend_strength, 2), round(parts$seasonal_strength, 2))
  list(
    label = "Seasonal decomposition", method = "ts_decompose",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = sprintf("%s decomposition with a seasonal period of %d; at least two full cycles were present.",
                          parts$method, prepared$frequency))),
    diagnostic_plot = ts_decomposition_plot(prepared),
    interpretation = interp,
    rationale = "Seasonal decomposition splits a series into trend, season and remainder so the underlying movement can be read separately from the repeating pattern.",
    note = "Trend and seasonal strengths near 1 indicate a strong, clearly separable component.",
    n_total = prepared$n_total, n_used = prepared$n,
    effect_name = NULL, recommendation = NULL)
}

# One row of the stationarity results table. `crit5` is the 5% critical value
# (used for the exact urca tests); `p` is a p-value (built-in / base tests).
# Either may be NA depending on which engine produced the row.
ts_stat_row <- function(test, stat, conclusion, crit5 = NA_real_, p = NA_real_) {
  data.frame(Test = test, Statistic = round(stat, 4),
             `Crit. (5%)` = if (is.na(crit5)) NA_real_ else round(crit5, 4),
             `p-value` = if (is.na(p)) NA_real_ else round(p, 4),
             Conclusion = conclusion,
             check.names = FALSE, stringsAsFactors = FALSE)
}

run_ts_stationarity <- function(prepared) {
  y <- prepared$values; n <- prepared$n
  lag_bx <- max(1L, min(as.integer(prepared$frequency), n %/% 5L, 20L))
  use_urca <- ts_urca_available()
  rows <- list()
  adf_nonstat <- NA; kpss_nonstat <- NA

  if (use_urca) {
    engine <- "urca (exact critical values)"
    # ADF (trend case, AIC lag selection): reject unit root if the statistic is
    # below (more negative than) the 5% critical value.
    urdf <- tryCatch(urca::ur.df(y, type = "trend", selectlags = "AIC"),
                     error = function(e) NULL)
    if (!is.null(urdf)) {
      stat <- as.numeric(urdf@teststat)[1]; crit5 <- as.numeric(urdf@cval["tau3", "5pct"])
      adf_nonstat <- !(stat < crit5)
      rows[[length(rows) + 1]] <- ts_stat_row(
        "Augmented Dickey-Fuller (H0: unit root)", stat,
        if (stat < crit5) "stationary (statistic below 5% critical value)"
        else "non-stationary (cannot reject unit root)", crit5 = crit5)
    }
    # KPSS (level null): reject stationarity if the statistic exceeds the 5% value.
    urk <- tryCatch(urca::ur.kpss(y, type = "mu"), error = function(e) NULL)
    if (!is.null(urk)) {
      stat <- as.numeric(urk@teststat); crit5 <- as.numeric(urk@cval[2])
      kpss_nonstat <- stat > crit5
      rows[[length(rows) + 1]] <- ts_stat_row(
        "KPSS (H0: level-stationary)", stat,
        if (stat > crit5) "non-stationary (statistic above 5% critical value)"
        else "stationary (cannot reject stationarity)", crit5 = crit5)
    }
    # Zivot-Andrews: unit root allowing ONE endogenous structural break.
    if (n >= 25) {
      urz <- tryCatch(urca::ur.za(y, model = "both",
                                  lag = max(1L, trunc((n - 1)^(1 / 3)))),
                      error = function(e) NULL)
      if (!is.null(urz)) {
        stat <- as.numeric(urz@teststat); crit5 <- as.numeric(urz@cval[2])
        rows[[length(rows) + 1]] <- ts_stat_row(
          "Zivot-Andrews (H0: unit root with 1 break)", stat,
          if (stat < crit5) sprintf("stationary around a break near observation %d", urz@bpoint)
          else "unit root even allowing one structural break", crit5 = crit5)
      }
    }
  } else {
    engine <- "built-in (approximate p-values)"
    adf <- tryCatch(ts_adf(y), error = function(e) NULL)
    kpss <- tryCatch(ts_kpss(y, null = "Level"), error = function(e) NULL)
    if (!is.null(adf)) {
      adf_nonstat <- adf$p.value >= 0.05
      rows[[length(rows) + 1]] <- ts_stat_row(
        "Augmented Dickey-Fuller (H0: unit root)", adf$statistic,
        if (adf$p.value < 0.05) "stationary (reject unit root)"
        else "non-stationary (cannot reject unit root)", p = adf$p.value)
    }
    if (!is.null(kpss)) {
      kpss_nonstat <- kpss$p.value < 0.05
      rows[[length(rows) + 1]] <- ts_stat_row(
        "KPSS (H0: level-stationary)", kpss$statistic,
        if (kpss$p.value < 0.05) "non-stationary (reject stationarity)"
        else "stationary (cannot reject stationarity)", p = kpss$p.value)
    }
  }

  # Phillips-Perron and Ljung-Box come from base stats in both paths.
  pp <- tryCatch(stats::PP.test(y), error = function(e) NULL)
  if (!is.null(pp)) rows[[length(rows) + 1]] <- ts_stat_row(
    "Phillips-Perron (H0: unit root)", as.numeric(pp$statistic),
    if (as.numeric(pp$p.value) < 0.05) "stationary (reject unit root)"
    else "non-stationary (cannot reject unit root)", p = as.numeric(pp$p.value))
  lb <- tryCatch(stats::Box.test(y, lag = lag_bx, type = "Ljung-Box"),
                 error = function(e) NULL)
  if (!is.null(lb)) rows[[length(rows) + 1]] <- ts_stat_row(
    sprintf("Ljung-Box autocorrelation (lag %d)", lag_bx), as.numeric(lb$statistic),
    if (as.numeric(lb$p.value) < 0.05) "autocorrelated (not white noise)"
    else "no autocorrelation detected", p = as.numeric(lb$p.value))

  acf1 <- tryCatch(as.numeric(stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]),
                   error = function(e) NA_real_)
  coef_tab <- do.call(rbind, rows)
  needs_diff <- isTRUE(adf_nonstat) || isTRUE(kpss_nonstat)
  fit_tab <- named_metric_table(c(
    "n used" = n,
    "Lag-1 autocorrelation" = round(acf1, 4),
    "Suggested differencing d" = if (needs_diff) 1 else 0))
  fit_tab <- rbind(fit_tab, data.frame(Metric = "Unit-root test engine",
                                       Value = engine, stringsAsFactors = FALSE))
  interp <- sprintf(
    "Stationarity was assessed with the ADF and Phillips-Perron unit-root tests (H0: non-stationary) and the KPSS test (H0: stationary)%s; the Ljung-Box test checked for autocorrelation. Critical values came from %s. The evidence suggests the series is %s. %s",
    if (use_urca) ", plus the Zivot-Andrews test that allows one structural break" else "",
    engine,
    if (needs_diff) "non-stationary" else "stationary",
    if (needs_diff) "First differencing (d = 1) is a reasonable next step before fitting an ARIMA model."
    else "No differencing appears necessary.")
  list(
    label = "Stationarity and autocorrelation diagnostics", method = "ts_stationarity",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = sprintf("ADF/KPSS/PP test non-stationarity; Ljung-Box tests residual autocorrelation. ADF and KPSS have opposite nulls, so agreement between them is the strongest signal. Critical values: %s.", engine))),
    diagnostic_plot = ts_correlogram_plot(prepared, partial = FALSE),
    interpretation = interp,
    rationale = "Confirming stationarity (and the differencing needed to achieve it) is the essential first step before ARIMA modelling or forecasting.",
    note = "A stationary series has a constant mean and variance over time; most forecasting models assume it after differencing.",
    n_total = prepared$n_total, n_used = n, effect_name = NULL, recommendation = NULL)
}

run_ts_trend <- function(prepared) {
  y <- prepared$values; t <- prepared$time
  mk <- ts_mann_kendall(y)
  sen <- ts_sen_slope(y, t)
  lm_fit <- stats::lm(y ~ seq_along(y))
  dw <- tryCatch(lmtest::dwtest(lm_fit), error = function(e) NULL)
  direction <- if (mk$p.value < 0.05) {
    if (mk$S > 0) "an increasing" else "a decreasing"
  } else "no significant"
  coef_tab <- data.frame(
    Quantity = c("Mann-Kendall S", "Kendall tau", "Sen's slope (per step)",
                 "Linear (OLS) slope (per step)"),
    Value = round(c(mk$S, mk$tau, sen, stats::coef(lm_fit)[[2]]), 5),
    check.names = FALSE, stringsAsFactors = FALSE)
  metrics <- c("n used" = mk$n, "Mann-Kendall z" = round(mk$z, 4),
               "Mann-Kendall p-value" = round(mk$p.value, 5))
  if (!is.null(dw)) metrics <- c(metrics,
    "Durbin-Watson statistic" = round(as.numeric(dw$statistic), 4),
    "Durbin-Watson p-value" = round(as.numeric(dw$p.value), 4))
  fit_tab <- named_metric_table(metrics)
  d <- data.frame(time = t, value = y)
  med_t <- stats::median(as.numeric(if (inherits(t, c("Date", "POSIXct", "POSIXt")))
    as.Date(t) else t))
  med_y <- stats::median(y)
  d$sen <- med_y + sen * (as.numeric(if (inherits(t, c("Date", "POSIXct", "POSIXt")))
    as.Date(t) else t) - med_t)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#018786", alpha = 0.8) +
    ggplot2::geom_point(colour = "#018786", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = sen), colour = "#D55E00", linewidth = 1.1)
  plt <- .ts_theme(p, sprintf("Mann-Kendall / Sen's slope - %s", prepared$value_name),
                   prepared$time_name, prepared$value_name)
  dw_note <- if (!is.null(dw) && as.numeric(dw$p.value) < 0.05)
    " Residuals are autocorrelated (Durbin-Watson), so the Mann-Kendall test - which does not assume independence in the same way - is the more reliable trend signal." else ""
  interp <- sprintf(
    "The Mann-Kendall test found %s monotonic trend (tau = %s, p = %s). Sen's slope estimates the typical change as %s per time step.%s A trend is an association over time, not proof of a cause.",
    direction, round(mk$tau, 3), p_fmt(mk$p.value), round(sen, 4), dw_note)
  list(
    label = "Trend detection (Mann-Kendall and Sen's slope)", method = "ts_trend",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = "Mann-Kendall is a rank-based test for a monotonic trend; Sen's slope is its robust slope estimate. Both tolerate non-normal data and outliers.")),
    diagnostic_plot = plt, interpretation = interp,
    rationale = "The Mann-Kendall test with Sen's slope is the standard robust method for detecting and quantifying a monotonic trend in environmental and surveillance series.",
    note = "Sen's slope is the median of all pairwise slopes and is reported in outcome units per time step.",
    n_total = prepared$n_total, n_used = mk$n, effect_name = NULL, recommendation = NULL)
}

run_ts_holt_winters <- function(prepared, horizon = 12L) {
  y <- prepared$y; f <- prepared$frequency
  seasonal_ok <- f >= 2 && prepared$n >= 2 * f
  fit <- tryCatch({
    if (seasonal_ok) stats::HoltWinters(y)
    else stats::HoltWinters(y, gamma = FALSE)
  }, error = function(e) tryCatch(stats::HoltWinters(y, gamma = FALSE, beta = FALSE),
                                  error = function(e2)
                                    stop("Holt-Winters smoothing could not be fitted to this series: ",
                                         conditionMessage(e2), call. = FALSE)))
  fc <- stats::predict(fit, n.ahead = horizon, prediction.interval = TRUE, level = 0.95)
  coef_tab <- data.frame(
    Parameter = c("alpha (level)", "beta (trend)", "gamma (season)"),
    Value = round(c(fit$alpha,
                    if (is.logical(fit$beta) && !fit$beta) NA_real_ else as.numeric(fit$beta),
                    if (is.logical(fit$gamma) && !fit$gamma) NA_real_ else as.numeric(fit$gamma)), 4),
    check.names = FALSE, stringsAsFactors = FALSE)
  fit_tab <- named_metric_table(c(
    "n used" = prepared$n, "Seasonal period" = f,
    "SSE (one-step errors)" = round(as.numeric(fit$SSE), 4),
    "Forecast horizon" = horizon,
    "First forecast" = round(as.numeric(fc[1, "fit"]), 4),
    "Last forecast" = round(as.numeric(fc[horizon, "fit"]), 4)))
  ft <- ts_future_time(prepared$time, horizon)
  hist_df <- data.frame(time = prepared$time, value = prepared$values,
                        kind = "observed")
  fc_df <- data.frame(time = ft, value = as.numeric(fc[, "fit"]),
                      lower = as.numeric(fc[, "lwr"]), upper = as.numeric(fc[, "upr"]),
                      kind = "forecast")
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = fc_df, ggplot2::aes(x = time, ymin = lower, ymax = upper),
                         fill = "#D55E00", alpha = 0.2) +
    ggplot2::geom_line(data = hist_df, ggplot2::aes(x = time, y = value),
                       colour = "#018786") +
    ggplot2::geom_line(data = fc_df, ggplot2::aes(x = time, y = value),
                       colour = "#D55E00", linewidth = 1.05)
  plt <- .ts_theme(p, sprintf("Holt-Winters forecast (+%d) - %s", horizon,
                              prepared$value_name),
                   prepared$time_name, prepared$value_name)
  interp <- sprintf(
    "Exponential smoothing (%s) was used to forecast %d step(s) ahead with 95%% prediction intervals. The level adapts with alpha = %s%s%s. Prediction intervals widen with the horizon and assume the fitted error structure holds.",
    if (seasonal_ok) "Holt-Winters with trend and season" else "Holt with trend",
    horizon, round(fit$alpha, 3),
    if (!(is.logical(fit$beta) && !fit$beta)) sprintf(", trend beta = %s", round(as.numeric(fit$beta), 3)) else "",
    if (seasonal_ok && !(is.logical(fit$gamma) && !fit$gamma)) sprintf(", season gamma = %s", round(as.numeric(fit$gamma), 3)) else "")
  list(
    label = "Holt-Winters exponential smoothing forecast", method = "ts_holt_winters",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = sprintf("%s exponential smoothing; %s.",
                          if (seasonal_ok) "Seasonal (multiplicative-free additive) Holt-Winters" else "Non-seasonal Holt",
                          if (seasonal_ok) sprintf("seasonal period %d with at least two full cycles", f)
                          else "no seasonal component was estimated"))),
    diagnostic_plot = plt, interpretation = interp,
    rationale = "Holt-Winters exponential smoothing is a robust, interpretable forecasting method for series with trend and (optionally) a stable seasonal pattern.",
    note = "Smoothing parameters were chosen to minimise one-step-ahead squared error; forecasts extrapolate the fitted level, trend and season.",
    n_total = prepared$n_total, n_used = prepared$n, effect_name = NULL,
    recommendation = NULL)
}

# Lightweight automatic ARIMA order selection by AIC over a bounded grid. Base
# stats::arima is deterministic, so the search is reproducible run-to-run.
ts_auto_arima <- function(y, frequency, max_p = 2L, max_q = 2L, max_d = 1L,
                          seasonal = TRUE) {
  seas_grid <- if (seasonal && frequency > 1)
    expand.grid(P = 0:1, D = 0:1, Q = 0:1) else data.frame(P = 0, D = 0, Q = 0)
  best <- NULL; best_aic <- Inf; best_order <- NULL; best_seas <- NULL
  for (d in 0:max_d) for (p in 0:max_p) for (q in 0:max_q) {
    for (si in seq_len(nrow(seas_grid))) {
      so <- as.integer(seas_grid[si, ])
      if (p == 0 && q == 0 && sum(so[c(1, 3)]) == 0 && d == 0 && so[2] == 0) next
      fit <- tryCatch(stats::arima(y, order = c(p, d, q),
                                   seasonal = list(order = so, period = frequency),
                                   method = "ML", optim.control = list(maxit = 200)),
                      error = function(e) NULL, warning = function(w) NULL)
      if (!is.null(fit) && is.finite(fit$aic) && fit$aic < best_aic) {
        best_aic <- fit$aic; best <- fit
        best_order <- c(p, d, q); best_seas <- so
      }
    }
  }
  if (is.null(best)) stop("No ARIMA model could be fitted to this series.", call. = FALSE)
  list(fit = best, order = best_order, seasonal = best_seas)
}

run_ts_arima <- function(prepared, horizon = 12L, order = NULL, seasonal_order = NULL) {
  y <- prepared$y; f <- prepared$frequency
  if (!is.null(order)) {
    so <- seasonal_order %||% c(0, 0, 0)
    fit <- stats::arima(y, order = order,
                        seasonal = list(order = so, period = f), method = "ML")
    chosen_order <- order; chosen_seas <- so
  } else {
    auto <- ts_auto_arima(y, f, seasonal = f > 1)
    fit <- auto$fit; chosen_order <- auto$order; chosen_seas <- auto$seasonal
  }
  co <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  if (length(co)) {
    z <- co / se
    coef_tab <- coefficient_table(
      term = names(co), estimate = co, std_error = se, statistic = z,
      p_value = 2 * stats::pnorm(-abs(z)),
      ci_lower = co - stats::qnorm(0.975) * se,
      ci_upper = co + stats::qnorm(0.975) * se)
  } else {
    coef_tab <- coefficient_table(term = "(no terms)", estimate = NA, std_error = NA,
                                  statistic = NA, p_value = NA, ci_lower = NA, ci_upper = NA)
  }
  resid_lb <- tryCatch(stats::Box.test(stats::residuals(fit),
                                       lag = max(1L, min(as.integer(f), prepared$n %/% 5L, 20L)),
                                       type = "Ljung-Box", fitdf = length(co)),
                       error = function(e) NULL)
  bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  fit_tab <- named_metric_table(c(
    "n used" = prepared$n,
    "AIC" = round(fit$aic, 3), "BIC" = round(bic, 3),
    "Log-likelihood" = round(as.numeric(fit$loglik), 3),
    "sigma^2" = round(as.numeric(fit$sigma2), 4),
    "Residual Ljung-Box p" = if (!is.null(resid_lb)) round(as.numeric(resid_lb$p.value), 4) else NA_real_,
    "Forecast horizon" = horizon))
  fc <- stats::predict(fit, n.ahead = horizon)
  ft <- ts_future_time(prepared$time, horizon)
  crit <- stats::qnorm(0.975)
  hist_df <- data.frame(time = prepared$time, value = prepared$values)
  fc_df <- data.frame(time = ft, value = as.numeric(fc$pred),
                      lower = as.numeric(fc$pred) - crit * as.numeric(fc$se),
                      upper = as.numeric(fc$pred) + crit * as.numeric(fc$se))
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = fc_df, ggplot2::aes(x = time, ymin = lower, ymax = upper),
                         fill = "#D55E00", alpha = 0.2) +
    ggplot2::geom_line(data = hist_df, ggplot2::aes(x = time, y = value), colour = "#018786") +
    ggplot2::geom_line(data = fc_df, ggplot2::aes(x = time, y = value),
                       colour = "#D55E00", linewidth = 1.05)
  order_txt <- sprintf("(%d,%d,%d)", chosen_order[1], chosen_order[2], chosen_order[3])
  seas_txt <- if (f > 1 && sum(chosen_seas) > 0)
    sprintf("(%d,%d,%d)[%d]", chosen_seas[1], chosen_seas[2], chosen_seas[3], f) else ""
  plt <- .ts_theme(p, sprintf("ARIMA%s%s forecast (+%d) - %s", order_txt, seas_txt,
                              horizon, prepared$value_name),
                   prepared$time_name, prepared$value_name)
  lb_note <- if (!is.null(resid_lb)) {
    if (as.numeric(resid_lb$p.value) < 0.05)
      " The residual Ljung-Box test is significant, so some autocorrelation remains - consider a richer model."
    else " Residuals show no leftover autocorrelation (Ljung-Box not significant), consistent with an adequate fit."
  } else ""
  interp <- sprintf(
    "An ARIMA%s%s model was %s and used to forecast %d step(s) ahead with 95%% intervals (AIC = %s).%s Forecasts and intervals assume the fitted structure continues.",
    order_txt, seas_txt, if (is.null(order)) "selected by AIC" else "fitted as specified",
    horizon, round(fit$aic, 1), lb_note)
  list(
    label = sprintf("ARIMA%s%s model and forecast", order_txt, seas_txt),
    method = "ts_arima", coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = sprintf("Non-seasonal order %s%s, fitted by maximum likelihood. Model adequacy is judged by the residual Ljung-Box test.",
                          order_txt, if (nzchar(seas_txt)) paste(" and seasonal order", seas_txt) else ""))),
    diagnostic_plot = plt, interpretation = interp,
    rationale = "ARIMA/SARIMA models the autocorrelation structure of a stationary (differenced) series and produces principled forecasts with prediction intervals.",
    note = "AR and MA coefficients describe the series' own dynamics; they are not covariate effects.",
    n_total = prepared$n_total, n_used = prepared$n, effect_name = NULL,
    recommendation = NULL)
}

# Time-series regression: outcome on a linear (optionally quadratic) time trend
# plus Fourier seasonal harmonics and any external numeric predictors. Reports
# autocorrelation-robust diagnostics (Durbin-Watson, Breusch-Godfrey).
run_ts_regression <- function(prepared, df, predictors = character(0),
                              trend_degree = 1L, harmonics = NULL) {
  n <- prepared$n; f <- prepared$frequency
  d <- data.frame(.y = prepared$values, .t = seq_len(n))
  terms <- character(0)
  for (deg in seq_len(max(1L, trend_degree))) {
    nm <- if (deg == 1) ".t" else sprintf("I(.t^%d)", deg)
    terms <- c(terms, nm)
  }
  K <- harmonics %||% (if (f >= 4) min(3L, f %/% 2L) else 0L)
  if (K > 0) for (k in seq_len(K)) {
    d[[sprintf("sin%d", k)]] <- sin(2 * pi * k * d$.t / f)
    d[[sprintf("cos%d", k)]] <- cos(2 * pi * k * d$.t / f)
    terms <- c(terms, sprintf("sin%d", k), sprintf("cos%d", k))
  }
  # External predictors: align to the same rows that ts_prepare kept.
  preds_used <- character(0)
  if (length(predictors)) {
    keep <- !is.na(df[[prepared$time_name]]) &
      is.finite(suppressWarnings(as.numeric(as.character(df[[prepared$value_name]]))))
    sub <- df[keep, , drop = FALSE]
    sub <- sub[order(sub[[prepared$time_name]]), , drop = FALSE]
    for (pv in predictors) {
      if (!pv %in% names(sub)) next
      xv <- suppressWarnings(as.numeric(as.character(sub[[pv]])))
      if (all(is.finite(xv)) && length(xv) == n) {
        safe <- make.names(pv)
        d[[safe]] <- xv; terms <- c(terms, safe); preds_used <- c(preds_used, pv)
      }
    }
  }
  form <- stats::as.formula(paste(".y ~", paste(terms, collapse = " + ")))
  fit <- stats::lm(form, data = d)
  sm <- summary(fit)
  co <- sm$coefficients
  ci <- stats::confint(fit)
  coef_tab <- coefficient_table(
    term = rownames(co), estimate = co[, 1], std_error = co[, 2],
    statistic = co[, 3], p_value = co[, 4],
    ci_lower = ci[, 1], ci_upper = ci[, 2])
  dw <- tryCatch(lmtest::dwtest(fit), error = function(e) NULL)
  bg <- tryCatch(lmtest::bgtest(fit, order = min(as.integer(f), n %/% 5L, 4L)),
                 error = function(e) NULL)
  metrics <- c("n used" = n, "R-squared" = round(sm$r.squared, 4),
               "Adjusted R-squared" = round(sm$adj.r.squared, 4),
               "AIC" = round(safe_aic(fit), 3))
  if (!is.null(dw)) metrics <- c(metrics,
    "Durbin-Watson statistic" = round(as.numeric(dw$statistic), 4),
    "Durbin-Watson p-value" = round(as.numeric(dw$p.value), 4))
  if (!is.null(bg)) metrics <- c(metrics,
    "Breusch-Godfrey p-value" = round(as.numeric(bg$p.value), 4))
  fit_tab <- named_metric_table(metrics)
  d$.fitted <- stats::fitted(fit)
  d$time <- prepared$time
  p <- ggplot2::ggplot(d, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = .y), colour = "#018786", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = .fitted), colour = "#D55E00", linewidth = 1.05)
  plt <- .ts_theme(p, sprintf("Time-series regression fit - %s", prepared$value_name),
                   prepared$time_name, prepared$value_name)
  autocorr <- !is.null(dw) && as.numeric(dw$p.value) < 0.05
  interp <- sprintf(
    "A regression on a %s time trend%s%s explained %s of the variance (adjusted R-squared %s). %s Time-series regression describes association, not causation.",
    if (trend_degree >= 2) "polynomial" else "linear",
    if (K > 0) sprintf(" with %d seasonal harmonic(s)", K) else "",
    if (length(preds_used)) sprintf(" and predictor(s) %s", paste(preds_used, collapse = ", ")) else "",
    paste0(round(100 * sm$r.squared, 1), "%"), round(sm$adj.r.squared, 3),
    if (autocorr) "Residuals are autocorrelated (Durbin-Watson significant), so ordinary standard errors are optimistic - prefer ARIMA errors or Newey-West adjustment."
    else "Residuals show no strong autocorrelation, so the ordinary standard errors are reasonable.")
  list(
    label = "Time-series regression (trend and seasonality)", method = "ts_regression",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = sprintf("Linear model with a degree-%d time trend%s%s. The Durbin-Watson and Breusch-Godfrey tests check for residual autocorrelation, which ordinary regression assumes away.",
                          max(1L, trend_degree),
                          if (K > 0) sprintf(", %d Fourier harmonic(s)", K) else "",
                          if (length(preds_used)) sprintf(", predictors %s", paste(preds_used, collapse = ", ")) else ""))),
    diagnostic_plot = plt, interpretation = interp,
    rationale = "Regressing the series on time and seasonal terms isolates a deterministic trend and season, with explicit checks for the autocorrelation that invalidates naive standard errors.",
    note = "Coefficients are on the outcome scale; check the autocorrelation diagnostics before trusting the p-values.",
    n_total = prepared$n_total, n_used = n, effect_name = NULL, recommendation = NULL)
}

# GARCH volatility model (optional; requires rugarch). Fits a GARCH(1,1) with an
# AR(1) mean and forecasts the conditional volatility (standard deviation). This
# is the classical tool for series whose variance clusters over time.
run_ts_garch <- function(prepared, horizon = 10L) {
  if (!ts_rugarch_available())
    stop("The GARCH volatility model needs the optional 'rugarch' package, which is not installed. Ask your administrator to install it, or choose another time-series method.",
         call. = FALSE)
  y <- prepared$values; n <- prepared$n
  if (n < 50)
    stop(sprintf("GARCH volatility models need a longer series (at least 50 observations); only %d are available.",
                 n), call. = FALSE)
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "norm")
  fit <- tryCatch(rugarch::ugarchfit(spec, data = y, solver = "hybrid"),
                  error = function(e)
                    stop("The GARCH(1,1) model could not be fitted to this series: ",
                         conditionMessage(e), call. = FALSE))
  mc <- fit@fit$matcoef
  coef_tab <- coefficient_table(
    term = rownames(mc), estimate = mc[, 1], std_error = mc[, 2],
    statistic = mc[, 3], p_value = mc[, 4],
    ci_lower = mc[, 1] - stats::qnorm(0.975) * mc[, 2],
    ci_upper = mc[, 1] + stats::qnorm(0.975) * mc[, 2])
  pers <- as.numeric(rugarch::persistence(fit))
  ic <- rugarch::infocriteria(fit)
  fit_tab <- named_metric_table(c(
    "n used" = n,
    "Volatility persistence (alpha+beta)" = round(pers, 4),
    "Unconditional SD" = round(as.numeric(rugarch::uncvariance(fit))^0.5, 4),
    "AIC" = round(as.numeric(ic[1]), 4),
    "BIC" = round(as.numeric(ic[2]), 4),
    "Log-likelihood" = round(as.numeric(rugarch::likelihood(fit)), 3),
    "Forecast horizon" = horizon))
  fc <- rugarch::ugarchforecast(fit, n.ahead = horizon)
  sig_hist <- as.numeric(rugarch::sigma(fit))
  sig_fc <- as.numeric(rugarch::sigma(fc))
  ft <- ts_future_time(prepared$time, horizon)
  hist_df <- data.frame(time = prepared$time, sigma = sig_hist)
  fc_df <- data.frame(time = ft, sigma = sig_fc)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = hist_df, ggplot2::aes(x = time, y = sigma), colour = "#018786") +
    ggplot2::geom_line(data = fc_df, ggplot2::aes(x = time, y = sigma),
                       colour = "#D55E00", linewidth = 1.05)
  plt <- .ts_theme(p, sprintf("GARCH(1,1) conditional volatility (+%d) - %s", horizon,
                              prepared$value_name),
                   prepared$time_name, "Conditional standard deviation")
  interp <- sprintf(
    "A GARCH(1,1) model with an AR(1) mean was fitted. Volatility persistence (alpha + beta) was %s: values close to 1 mean shocks to volatility fade slowly, so calm and turbulent periods cluster. The forecast projects the conditional standard deviation %d step(s) ahead.",
    round(pers, 3), horizon)
  list(
    label = "GARCH(1,1) volatility model", method = "ts_garch",
    coefficients = coef_tab, fit_stats = fit_tab,
    assumptions = list(
      list(note = ts_note_common(prepared)),
      list(note = "GARCH assumes the series (or its mean residuals) shows volatility clustering. The alpha + beta persistence must stay below 1 for a stationary variance.")),
    diagnostic_plot = plt, interpretation = interp,
    rationale = "GARCH models a changing (heteroskedastic) variance, capturing periods of high and low volatility that constant-variance models miss.",
    note = "omega is the baseline variance; alpha weights recent shocks and beta weights past volatility.",
    n_total = prepared$n_total, n_used = n, effect_name = NULL, recommendation = NULL)
}

# --- dispatcher -------------------------------------------------------------

#' Fit a selected time-series analysis.
#'
#' spec fields: method (ts_*), value, time, frequency (optional), horizon
#' (forecast steps), predictors (ts_regression), trend_degree (ts_regression).
#' `time` normally comes from the Data-tab declaration (panel$time).
# Run one method on a single, already-subset series.
run_timeseries_single <- function(df, spec) {
  method <- spec$method
  if (is.null(method) || !method %in% names(TS_METHOD_LABELS))
    stop("Choose a time-series analysis method.", call. = FALSE)
  if (is.null(spec$time) || !nzchar(spec$time))
    stop("A time / date column is required. Declare it on the Data tab as a time series.",
         call. = FALSE)
  prepared <- ts_prepare(df, spec$time, spec$value, frequency = spec$frequency)
  horizon <- suppressWarnings(as.integer(spec$horizon %||% 12L))
  if (!is.finite(horizon) || horizon < 1L) horizon <- 12L
  if (horizon > 240L) horizon <- 240L
  switch(method,
    ts_decompose    = run_ts_decompose(prepared),
    ts_stationarity = run_ts_stationarity(prepared),
    ts_trend        = run_ts_trend(prepared),
    ts_holt_winters = run_ts_holt_winters(prepared, horizon = horizon),
    ts_arima        = run_ts_arima(prepared, horizon = horizon,
                                   order = spec$order, seasonal_order = spec$seasonal_order),
    ts_regression   = run_ts_regression(prepared, df,
                                        predictors = spec$predictors %||% character(0),
                                        trend_degree = spec$trend_degree %||% 1L),
    ts_garch        = run_ts_garch(prepared, horizon = horizon),
    stop("Unknown time-series method.", call. = FALSE))
}

# Distinct, non-missing series labels (levels for a factor, else sorted unique).
.ts_group_levels <- function(g) {
  if (is.factor(g)) return(intersect(levels(g), as.character(unique(g[!is.na(g)]))))
  sort(unique(as.character(g[!is.na(g)])))
}

# Analyse every stacked series in one pass: run the chosen method per group,
# combine the tables with a leading "Series" column, and draw a faceted
# overview. Series that are too short or otherwise fail are skipped and named,
# never silently dropped.
run_timeseries_all_series <- function(df, spec, grp, levs) {
  gc <- as.character(df[[grp]])
  per <- lapply(levs, function(lv) {
    sub <- df[!is.na(gc) & gc == lv, , drop = FALSE]
    out <- tryCatch(run_timeseries_single(sub, spec),
                    error = function(e) list(error = conditionMessage(e)))
    list(series = lv, out = out)
  })
  ok <- Filter(function(x) is.null(x$out$error), per)
  bad <- Filter(function(x) !is.null(x$out$error), per)
  if (!length(ok))
    stop(sprintf("No series could be analysed. First issue (%s = %s): %s",
                 grp, bad[[1]]$series, bad[[1]]$out$error), call. = FALSE)

  stack_tab <- function(field) {
    parts <- lapply(ok, function(x) {
      tb <- x$out[[field]]
      if (is.null(tb) || !nrow(tb)) return(NULL)
      data.frame(Series = x$series, tb, check.names = FALSE,
                 stringsAsFactors = FALSE)
    })
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) return(NULL)
    do.call(rbind, parts)
  }

  # Faceted overview of the raw series (always meaningful, unlike merging N
  # method-specific plots). Per-series diagnostics/forecasts stay available by
  # selecting a single series.
  ov <- data.frame(
    .t = df[[spec$time]],
    .y = suppressWarnings(as.numeric(as.character(df[[spec$value]]))),
    .g = gc, stringsAsFactors = FALSE)
  ov <- ov[!is.na(ov$.t) & is.finite(ov$.y) & ov$.g %in% vapply(ok, `[[`, "", "series"), ]
  ov$.g <- factor(ov$.g, levels = vapply(ok, `[[`, "", "series"))
  plt <- .ts_theme(
    ggplot2::ggplot(ov, ggplot2::aes(x = .t, y = .y)) +
      ggplot2::geom_line(colour = "#018786", alpha = 0.85) +
      ggplot2::facet_wrap(~ .g, scales = "free_y"),
    sprintf("%s by %s - %s", TS_METHOD_LABELS[[spec$method]], grp, spec$value),
    spec$time, spec$value)

  first <- ok[[1]]$out
  skipped <- if (length(bad))
    sprintf(" %d series were skipped (too short or unfittable): %s.",
            length(bad), paste(sprintf("%s (%s)",
              vapply(bad, `[[`, "", "series"),
              vapply(bad, function(x) x$out$error, "")), collapse = "; ")) else ""
  list(
    label = sprintf("%s - per series across %s (%d series)",
                    first$label, grp, length(ok)),
    method = spec$method,
    coefficients = stack_tab("coefficients"),
    fit_stats = stack_tab("fit_stats"),
    assumptions = Filter(Negate(is.null), list(
      list(note = sprintf("Each series (%s) was analysed independently on its own time points; results are stacked with a 'Series' column.", grp)),
      if (nzchar(skipped)) list(note = trimws(skipped)),
      first$assumptions[[length(first$assumptions)]])),
    diagnostic_plot = plt,
    interpretation = sprintf(
      "%d series were analysed separately by %s.%s Select a single series above to see its full diagnostics and any forecast plot. Compare the per-series rows in the results table rather than pooling them.",
      length(ok), TS_METHOD_LABELS[[spec$method]], skipped),
    rationale = "Stacked (multi-series) data is analysed one series at a time, because a single time-series model assumes one ordered sequence of observations.",
    effect_name = first$effect_name)
}

run_guided_timeseries <- function(df, spec) {
  grp <- spec$group
  # No series column -> single series (ts_prepare's guard catches stacked data).
  if (is.null(grp) || !nzchar(grp) || !grp %in% names(df))
    return(run_timeseries_single(df, spec))

  levs <- .ts_group_levels(df[[grp]])
  if (length(levs) < 1L)
    stop(sprintf("The series column '%s' has no usable values.", grp), call. = FALSE)
  if (length(levs) > 60L)
    stop(sprintf("The series column '%s' has %d distinct values - too many to analyse as separate series. Filter to fewer series on the Data tab, or pick a coarser grouping.",
                 grp, length(levs)), call. = FALSE)

  gv <- spec$group_value %||% "__all__"
  if (!identical(gv, "__all__")) {
    if (!gv %in% levs)
      stop(sprintf("Series '%s' is not a value of '%s'.", gv, grp), call. = FALSE)
    gc <- as.character(df[[grp]])
    res <- run_timeseries_single(df[!is.na(gc) & gc == gv, , drop = FALSE], spec)
    res$label <- sprintf("%s (%s = %s)", res$label, grp, gv)
    return(res)
  }
  run_timeseries_all_series(df, spec, grp, levs)
}
