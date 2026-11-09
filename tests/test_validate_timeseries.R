# tests/test_validate_timeseries.R ------------------------------------------
# Golden validation for the time-series engine (fct_timeseries.R).
#
# Tier 1 (wrappers): runners that delegate to base {stats} / {lmtest} are
# checked against the same reference computed directly here (near-exact).
# Tier 2 (reimplementations): the Augmented Dickey-Fuller and KPSS statistics,
# the Mann-Kendall trend test and Sen's slope are validated against independent
# recomputation and hand-worked examples.
# Also covers the planner catalogue<->runner sync, the AI/structure gating, and
# determinism of the automatic ARIMA search.

source(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "helper.R"))

# --- fixtures ---------------------------------------------------------------
# A monthly series with strong trend + seasonality (AirPassengers, freq 12).
air_df <- data.frame(
  month = seq(as.Date("1949-01-01"), by = "month", length.out = length(AirPassengers)),
  passengers = as.numeric(AirPassengers),
  stringsAsFactors = FALSE)

# A short, controlled series for the reimplemented tests.
set.seed(42)
ar_series <- as.numeric(stats::arima.sim(list(ar = 0.6), n = 80)) + 10

# ============================ Tier-2 reimplementations =====================

ok("ts_mann_kendall matches the closed form for a strictly increasing series", {
  mk <- ts_mann_kendall(1:12)
  expect_close(mk$S, 12 * 11 / 2, TOL_STAT, "S")      # 66
  expect_close(mk$tau, 1, TOL_STAT, "tau")
})

ok("ts_mann_kendall S matches an independent brute-force count", {
  y <- ar_series
  n <- length(y)
  brute <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) brute <- brute + sign(y[j] - y[i])
  mk <- ts_mann_kendall(y)
  expect_close(mk$S, brute, TOL_STAT, "S vs brute force")
})

ok("ts_sen_slope recovers a known linear slope exactly", {
  expect_close(ts_sen_slope(3 + 2.5 * (1:20)), 2.5, TOL_STAT, "Sen slope")
})

ok("ts_adf statistic equals the lagged-level t-ratio in the ADF regression", {
  x <- ar_series
  k0 <- trunc((length(x) - 1)^(1 / 3)); k <- k0 + 1
  dy <- diff(x); n <- length(dy); z <- embed(dy, k)
  yt <- z[, 1]; xt1 <- x[k:n]; tt <- k:n; yt1 <- z[, 2:k, drop = FALSE]
  m <- stats::lm(yt ~ xt1 + 1 + tt + yt1)
  co <- summary(m)$coefficients
  ref <- co["xt1", 1] / co["xt1", 2]
  adf <- ts_adf(x)
  expect_close(adf$statistic, ref, TOL_STAT, "ADF statistic")
  stopifnot(adf$p.value >= 0.01, adf$p.value <= 0.99)
})

ok("ts_kpss statistic equals an independent long-run-variance recomputation", {
  x <- ar_series; n <- length(x)
  e <- stats::residuals(stats::lm(x ~ 1))
  S <- cumsum(e); eta <- sum(S^2) / n^2
  l <- trunc(4 * (n / 100)^0.25); s2 <- sum(e^2) / n
  for (i in seq_len(l)) {
    w <- 1 - i / (l + 1)
    s2 <- s2 + (2 / n) * w * sum(e[1:(n - i)] * e[(i + 1):n])
  }
  ref <- eta / s2
  kp <- ts_kpss(x, null = "Level")
  expect_close(kp$statistic, ref, TOL_STAT, "KPSS statistic")
})

ok("ADF and KPSS agree that a random walk is non-stationary", {
  set.seed(7); rw <- cumsum(rnorm(120))
  adf <- ts_adf(rw); kp <- ts_kpss(rw, null = "Level")
  stopifnot(adf$p.value > 0.05)   # cannot reject unit root
  stopifnot(kp$p.value < 0.05)    # reject stationarity
})

# ============================ Tier-1 wrappers ==============================

ok("run_ts_arima coefficients and AIC match stats::arima for a fixed order", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  res <- run_ts_arima(prepared, horizon = 6, order = c(2, 0, 0),
                      seasonal_order = c(0, 0, 0))
  ref <- stats::arima(prepared$y, order = c(2, 0, 0),
                      seasonal = list(order = c(0, 0, 0), period = 12), method = "ML")
  ar1 <- res$coefficients$Estimate[res$coefficients$Term == "ar1"]
  expect_close(ar1, unname(stats::coef(ref)["ar1"]), TOL_EFFECT, "ar1 estimate")
  expect_close(field_value(res$fit_stats, "AIC"), round(ref$aic, 3), 1e-2, "AIC")
})

ok("run_ts_holt_winters alpha and SSE match stats::HoltWinters", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  res <- run_ts_holt_winters(prepared, horizon = 12)
  ref <- stats::HoltWinters(prepared$y)
  expect_close(field_value(res$coefficients, "alpha (level)"),
               round(as.numeric(ref$alpha), 4), 1e-3, "alpha")
  expect_close(field_value(res$fit_stats, "SSE (one-step errors)"),
               round(as.numeric(ref$SSE), 4), 1e-2, "SSE")
})

ok("run_ts_decompose components reconstruct the series (STL additivity)", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  parts <- ts_decompose_components(prepared)
  recon <- parts$trend + parts$seasonal + parts$remainder
  stopifnot(max(abs(as.numeric(prepared$y) - recon)) < 1e-6)
  stopifnot(is.finite(parts$seasonal_strength), parts$seasonal_strength > 0.5)
})

ok("run_ts_stationarity reports the base PP and Ljung-Box p-values", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  res <- run_ts_stationarity(prepared)
  n <- prepared$n; lag_bx <- max(1L, min(as.integer(12), n %/% 5L, 20L))
  pp_ref <- stats::PP.test(prepared$values)$p.value
  lb_ref <- stats::Box.test(prepared$values, lag = lag_bx, type = "Ljung-Box")$p.value
  pp_cell <- as.numeric(res$coefficients$`p-value`[
    grepl("Phillips", res$coefficients$Test)])
  lb_cell <- as.numeric(res$coefficients$`p-value`[
    grepl("Ljung", res$coefficients$Test)])
  expect_close(pp_cell, round(pp_ref, 4), 1e-3, "PP p-value")
  expect_close(lb_cell, round(lb_ref, 4), 1e-3, "Ljung-Box p-value")
})

ok("run_ts_trend Durbin-Watson matches lmtest::dwtest on the same trend model", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  res <- run_ts_trend(prepared)
  y <- prepared$values
  dw_ref <- lmtest::dwtest(stats::lm(y ~ seq_along(y)))
  expect_close(field_value(res$fit_stats, "Durbin-Watson p-value"),
               round(as.numeric(dw_ref$p.value), 4), 1e-3, "DW p-value")
})

ok("run_ts_regression R-squared matches a direct lm on trend + harmonics", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  res <- run_ts_regression(prepared, air_df, trend_degree = 1)
  n <- prepared$n; t <- seq_len(n)
  d <- data.frame(.y = prepared$values, .t = t)
  for (k in 1:3) { d[[paste0("s", k)]] <- sin(2*pi*k*t/12); d[[paste0("c", k)]] <- cos(2*pi*k*t/12) }
  ref <- summary(stats::lm(.y ~ .t + s1 + c1 + s2 + c2 + s3 + c3, data = d))$r.squared
  expect_close(field_value(res$fit_stats, "R-squared"), round(ref, 4), 1e-3, "R^2")
})

# ============================ dispatcher & prep ============================

ok("ts_prepare infers a yearly cycle for monthly dates and orders the series", {
  shuffled <- air_df[sample(nrow(air_df)), ]
  prepared <- ts_prepare(shuffled, "month", "passengers")
  stopifnot(prepared$frequency == 12)
  stopifnot(!is.unsorted(prepared$time))
  stopifnot(prepared$n == nrow(air_df))
})

ok("run_guided_timeseries dispatches every method to the result contract", {
  need <- c("label", "coefficients", "fit_stats", "assumptions",
            "diagnostic_plot", "interpretation", "n_used")
  for (m in names(TS_METHOD_LABELS)) {
    res <- run_guided_timeseries(air_df, list(method = m, value = "passengers",
                                              time = "month", horizon = 6))
    stopifnot(all(need %in% names(res)))
    stopifnot(is.data.frame(res$coefficients), is.data.frame(res$fit_stats))
    stopifnot(inherits(res$diagnostic_plot, "ggplot"))
  }
})

ok("time-series analyses refuse a series that is too short", {
  tiny <- air_df[1:5, ]
  msg <- friendly_error(run_guided_timeseries(tiny,
    list(method = "ts_trend", value = "passengers", time = "month")))
  stopifnot(!is.na(msg), grepl("time point", msg))
})

# ============================ determinism ==================================

ok("automatic ARIMA order selection is reproducible run-to-run", {
  prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
  a1 <- ts_auto_arima(prepared$y, 12, seasonal = TRUE)$order
  a2 <- ts_auto_arima(prepared$y, 12, seasonal = TRUE)$order
  stopifnot(identical(a1, a2))
})

# ============================ charts =======================================

ok("every time-series chart type renders a ggplot", {
  for (ct in c("ts_acf", "ts_pacf", "ts_decomposition", "ts_seasonal", "ts_spectrum")) {
    s <- default_chart_spec(); s$type <- ct; s$x <- "passengers"
    s$ts_time <- "month"; s$ts_frequency <- 12
    p <- build_chart(air_df, s)
    stopifnot(inherits(p, "ggplot"))
  }
})

# ============================ gating =======================================

ok("time-series method menu is gated by the AI plan but never emptied", {
  all_m <- names(TS_METHOD_LABELS)
  plan <- list(advanced_regression = "ts_arima")
  stopifnot(identical(plan_advanced_family(plan), "time_series"))
  stopifnot(identical(plan_ts_method(plan), "ts_arima"))
  # active + recommended -> narrows to the recommendation
  stopifnot(setequal(gate_choices(all_m, plan_ts_method(plan), TRUE), "ts_arima"))
  # inactive (override) -> full menu
  stopifnot(setequal(gate_choices(all_m, plan_ts_method(plan), FALSE), all_m))
  # no plan -> full menu (structure gate still governs)
  stopifnot(setequal(gate_choices(all_m, plan_ts_method(NULL), TRUE), all_m))
  # recommendation not in the (feasible) menu -> keep the menu, do not empty it
  stopifnot(setequal(gate_choices(all_m, "cox_ph", TRUE), all_m))
})

ok("the structure gate marks a declared time series as available", {
  df <- air_df
  types <- c(month = "datetime", passengers = "continuous")
  panel <- list(kind = "time_series", time = "month")
  s <- advanced_data_structure(df, types, panel)
  stopifnot(isTRUE(s$available), isTRUE(s$time_series),
            isFALSE(s$clustered), isFALSE(s$survival))
})

# ============================ catalogue <-> runner sync ====================

ok("every planner catalogue id maps to an implemented runner (E.P1)", {
  reg_ids <- setdiff(vapply(planner_regression_catalogue(), function(t) t$id, ""), "none")
  adv_ids <- setdiff(vapply(planner_advanced_catalogue(),  function(t) t$id, ""), "none")
  # standard regression ids must be real model methods
  stopifnot(all(reg_ids %in% names(MODEL_METHOD_LABELS)))
  # each advanced id must resolve to a clustered method, a survival method, or a
  # time-series method - nothing dangling.
  resolves <- function(id) {
    id %in% names(MODEL_METHOD_LABELS) ||          # clustered GEE/LMM/GLMM
      length(plan_survival_method(list(advanced_regression = id))) > 0 ||
      id %in% names(TS_METHOD_LABELS)
  }
  unresolved <- adv_ids[!vapply(adv_ids, resolves, logical(1))]
  stopifnot(length(unresolved) == 0)
})

# ============================ optional: urca (exact unit root) =============

if (ts_urca_available()) {
  ok("run_ts_stationarity uses urca's exact ADF critical value and adds Zivot-Andrews", {
    prepared <- ts_prepare(air_df, "month", "passengers", frequency = 12)
    res <- run_ts_stationarity(prepared)
    engine <- res$fit_stats$Value[res$fit_stats$Metric == "Unit-root test engine"]
    stopifnot(grepl("urca", engine))
    ref <- urca::ur.df(prepared$values, type = "trend", selectlags = "AIC")
    adf_cell <- as.numeric(res$coefficients$Statistic[
      grepl("Dickey", res$coefficients$Test)])
    expect_close(adf_cell, round(as.numeric(ref@teststat)[1], 4), 1e-3, "ADF stat vs ur.df")
    stopifnot(any(grepl("Zivot", res$coefficients$Test)))
  })
} else {
  cat("  SKIP  urca not installed; the exact unit-root path is exercised in CI\n")
}

# ============================ optional: rugarch (GARCH) ====================

if (ts_rugarch_available()) {
  ok("ts_garch is in the method vocabulary and maps to the time-series family", {
    stopifnot("ts_garch" %in% names(TS_METHOD_LABELS))
    stopifnot(identical(plan_advanced_family(list(advanced_regression = "ts_garch")),
                        "time_series"))
  })
  ok("run_ts_garch persistence and beta match a direct rugarch fit", {
    set.seed(11); n <- 220; e <- rnorm(n); h <- numeric(n); h[1] <- 1; y <- numeric(n)
    for (t in 2:n) { h[t] <- 0.05 + 0.1 * y[t - 1]^2 + 0.85 * h[t - 1]; y[t] <- sqrt(h[t]) * e[t] }
    gdf <- data.frame(t = seq_len(n), v = y)
    res <- run_guided_timeseries(gdf, list(method = "ts_garch", value = "v",
                                           time = "t", horizon = 4))
    spec <- rugarch::ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
      distribution.model = "norm")
    ref <- rugarch::ugarchfit(spec, data = y, solver = "hybrid")
    expect_close(field_value(res$fit_stats, "Volatility persistence (alpha+beta)"),
                 round(as.numeric(rugarch::persistence(ref)), 4), 1e-2, "persistence")
    beta_cell <- res$coefficients$Estimate[res$coefficients$Term == "beta1"]
    expect_close(beta_cell, unname(ref@fit$matcoef["beta1", 1]), 1e-2, "beta1")
  })
  ok("run_ts_garch refuses a series that is too short", {
    short <- data.frame(t = 1:30, v = rnorm(30))
    msg <- friendly_error(run_guided_timeseries(short,
      list(method = "ts_garch", value = "v", time = "t")))
    stopifnot(!is.na(msg), grepl("50 observations", msg))
  })
} else {
  cat("  SKIP  rugarch not installed; GARCH is exercised in CI\n")
}

# --- stacked (multi-series) data -------------------------------------------
cat("\n== Stacked / multi-series time series ==\n")

make_stacked <- function() {
  dates <- rep(seq(as.Date("2022-01-31"), by = "month", length.out = 24), times = 3)
  data.frame(
    facility = rep(c("A", "B", "C"), each = 24),
    month = dates,
    cases = c(round(50 + (1:24) * 1.5), round(80 - (1:24) * 0.8),
              round(rep(60, 24))),
    stringsAsFactors = FALSE)
}

ok("pooled stacked data is refused with a series-aware message", {
  df <- make_stacked()
  msg <- friendly_error(run_guided_timeseries(df,
    list(method = "ts_trend", value = "cases", time = "month", frequency = 12)))
  stopifnot(!is.na(msg), grepl("repeated time point", msg),
            grepl("series", msg))
})

ok("per-series (all) stacks results with a Series column and skips nothing", {
  df <- make_stacked()
  r <- run_guided_timeseries(df, list(method = "ts_trend", value = "cases",
        time = "month", frequency = 12, group = "facility", group_value = "__all__"))
  stopifnot("Series" %in% names(r$coefficients))
  stopifnot(setequal(unique(r$coefficients$Series), c("A", "B", "C")))
  # Series A rises, B falls: Mann-Kendall S has opposite signs.
  s_of <- function(ser) r$coefficients$Value[r$coefficients$Series == ser &
                          r$coefficients$Quantity == "Mann-Kendall S"]
  stopifnot(s_of("A") > 0, s_of("B") < 0)
  stopifnot(inherits(r$diagnostic_plot, "ggplot"))
})

ok("selecting one series analyses just that series", {
  df <- make_stacked()
  r <- run_guided_timeseries(df, list(method = "ts_trend", value = "cases",
        time = "month", frequency = 12, group = "facility", group_value = "B"))
  stopifnot(grepl("facility = B", r$label))
  stopifnot(!("Series" %in% names(r$coefficients)))   # a single-series table
})

ok("too-short series are skipped and named, not silently dropped", {
  df <- make_stacked()
  df <- rbind(df, data.frame(facility = "D", month = df$month[1:3],
                             cases = c(1, 2, 3)))       # only 3 points
  r <- run_guided_timeseries(df, list(method = "ts_trend", value = "cases",
        time = "month", frequency = 12, group = "facility", group_value = "__all__"))
  stopifnot(!("D" %in% r$coefficients$Series))
  notes <- paste(vapply(r$assumptions, function(a) a$note %||% "", ""), collapse = " ")
  stopifnot(grepl("\\bD\\b", notes), grepl("skipped", notes))
})

ok("too many distinct series is refused", {
  many <- data.frame(
    g = rep(sprintf("s%02d", 1:70), each = 10),
    t = rep(1:10, times = 70), v = rnorm(700))
  msg <- friendly_error(run_guided_timeseries(many,
    list(method = "ts_trend", value = "v", time = "t", group = "g",
         group_value = "__all__")))
  stopifnot(!is.na(msg), grepl("too many", msg))
})

report_and_exit()
