# Changelog

## 0.2.0 — 2026-07-24

- Rebuilt Tab 0 as an end-to-end 22-point data-readiness workflow with raw
  import checks, declarative data contracts, action-oriented Issue/Impact/
  Recommendation findings, missingness patterns and subgroup diagnostics,
  ranges, dates, identifiers, near-zero variance, correlation, imbalance,
  skewness, logistic separation, and model-suitability checks.
- Added a scorecard, readiness status, CSV/text exports, per-session review
  decisions with required justifications, and a metadata-only audit download;
  original uploads remain unchanged and raw category values are not printed in
  default findings.
- Added `DATA_READINESS.md` and golden end-to-end coverage in
  `tests/test_data_readiness.R`.
- Added an end-to-end time-series analysis family on the Advanced models tab:
  seasonal decomposition (STL/classical), stationarity and autocorrelation
  diagnostics (ADF, KPSS, Phillips–Perron, Ljung–Box), Mann–Kendall trend with
  Sen's slope, Holt–Winters exponential-smoothing forecasts, ARIMA/SARIMA with
  automatic AIC order selection and forecasts, and trend/seasonal regression.
- Enriched the time-series descriptives (sampling regularity, inferred seasonal
  period, lag-1 autocorrelation, Mann–Kendall trend and stationarity verdict)
  and added time-series charts (ACF, PACF, decomposition, seasonal subseries,
  spectral density).
- The ADF, KPSS and Mann–Kendall/Sen's-slope routines are reimplemented (no
  extra dependency) and validated against independent recomputation in
  `tests/test_validate_timeseries.R`.
- Replaced the dangling planner `time_series` catalogue id with the specific
  time-series method ids, wired the AI-plan and structure gating for the family,
  and added a catalogue↔runner sync test so no advertised method can lack an
  implementation.
- Added optional integrations that upgrade the time-series tools when installed:
  `urca` supplies exact MacKinnon unit-root critical values and a Zivot–Andrews
  structural-break test in the stationarity diagnostics (falling back to the
  built-in approximate tests otherwise), and `rugarch` adds a GARCH volatility
  model (hidden from the menu when the package is absent). Both are validated in
  the test suite when present and skipped cleanly when not.
- Added a publication-ready descriptive **summary table (Table 1)** to the
  Descriptives tab (cross-sectional only): rows = characteristics, optional split
  by an outcome plus an Overall column, n (column %) for categorical and
  M (SD) / Median [IQR] for continuous variables. A p-value column and a
  tests-used footnote are added only when a split is chosen, using the existing
  assumption-aware engines (Welch t / Mann-Whitney, ANOVA / Kruskal-Wallis,
  chi-square / Fisher) and reporting the recommended test. The downloaded Word
  report now renders tables and figures to APA style (`flextable::theme_apa`)
  with automatic Table 1/2 and Figure 1/2 numbering. New engine
  `R/fct_tableone.R` with golden tests in `tests/test_validate_tableone.R`.
- Added a way to serve the app over a network with a login: an optional
  `shinymanager` sign-in layer (off by default, so development and tests are
  unaffected), a `run_server.R` launcher that refuses to bind to a non-local
  address without a login configured and sanitises errors, a `setup_auth.R`
  credential-store creator (scrypt-hashed passwords, encrypted store), and
  `DEPLOYMENT.md` covering LAN testing and an HTTPS-tunnel path for internet
  access to a patient-data tool. Guarded by `tests/test_validate_auth.R`.

## 0.1.0 — 2026-07-24

- Added adversarial golden validation suites for all statistical families,
  imports, assumptions, data layouts, AI governance and tab rendering.
- Added CI execution of the complete `tests/run_all.R` gate.
- Preserved labelled variable/value metadata through SPSS, Stata and SAS
  XPORT imports for privacy-safe planner metadata.
- Added explicit import-size/shape/text guards, multicollinearity diagnostics,
  small-sample messages and rank-deficient model handling.
