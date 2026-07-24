# Sabali user guide

## Review data readiness before analysis

Open **0 · Data check** after loading and confirming your variable types. The
first panel shows the 22-point scorecard and prioritised findings. Start with
blockers, then review high-priority warnings. Each finding explains the issue,
its likely analytical impact, and a recommended next action.

Use **Data contract** to declare required fields, expected ranges, allowed
levels, unique identifiers, and subgroup variables for missingness checks. Use
**Analysis readiness** to explicitly select an outcome, predictors, and model
family; this enables correlation, class-imbalance, separation, and modelling-
suitability checks without guessing the study outcome.

Use **Review & audit** to record a data-owner decision. An accepted finding
requires a written justification and remains visible in the exported report.
The original upload is never changed, and the audit export contains metadata
only. See [`DATA_READINESS.md`](DATA_READINESS.md) for the full scorecard and
interpretation rules.

## Compare two independent groups

1. Load a table on **1 · Data**, confirm the outcome as continuous/count/binary
   and the grouping variable as binary or nominal.
2. Declare the data as **one row per subject**, select the outcome, then open
   **4 · Tests**.
3. Choose **Compare groups**, select **two independent groups**, choose the
   outcome and group, and run the analysis.

Sabali reports Welch's t-test and the Mann–Whitney alternative together. Check
the normality and variance notes, the Hedges g effect size, and the complete
case count before interpreting the p-value.

## Repeated measurements

On the Data tab, answer **repeated measures** and select the subject ID and
visit/time columns. Use paired/repeated designs on the Tests tab, or GEE/LMM/
GLMM on **6 · Advanced models**. Do not pool repeated rows into an independent
t-test or regression.

## Time-to-event data

Declare a survival analysis and select either a follow-up duration or start/end
dates plus the event indicator. Use Kaplan–Meier for curves and log-rank
comparisons, or Cox regression for adjusted hazard ratios. Censored follow-up,
event and subject-ID columns are reserved from ordinary analyses.

## Time-series data (a measure tracked over time)

Declare the data as a **time series** on the Data tab and select the time/date
column. The Descriptives tab then reports the sampling interval, the inferred
seasonal period, the lag-1 autocorrelation, a Mann-Kendall trend verdict and a
stationarity verdict. The Charts tab adds seasonal decomposition, ACF/PACF,
seasonal-subseries and spectral plots.

On **6 · Advanced models**, choose the **Time series** family and one of:

- **Seasonal decomposition** — split the series into trend, season and remainder.
- **Stationarity and autocorrelation diagnostics** — ADF, KPSS, Phillips-Perron
  and Ljung-Box tests, with a suggested amount of differencing.
- **Trend detection** — the robust Mann-Kendall test with Sen's slope.
- **Holt-Winters forecast** — exponential smoothing with prediction intervals.
- **ARIMA / SARIMA** — an ARIMA model (order chosen automatically by AIC) with a
  forecast and residual checks.
- **Time-series regression** — a trend-plus-seasonality regression with
  autocorrelation diagnostics.
- **GARCH volatility model** — models a changing variance (volatility
  clustering). Available only when the optional `rugarch` package is installed
  and needs a longer series (at least 50 observations).

If the optional `urca` package is installed, the stationarity diagnostics use
exact critical values and add a Zivot–Andrews structural-break test; otherwise
the built-in approximate tests are used.

Set the **seasonal period** to the number of observations in one cycle (12 for
monthly data with a yearly season, 4 for quarterly, 7 for daily-with-weekly).
Confirm the series is stationary before trusting a forecast, and remember a
trend over time is an association, not a cause.

## Descriptive summary table (Table 1)

On **2 · Descriptives** with a **cross-sectional** dataset, switch **Summary** to
**Summary table (Table 1)**. Pick several variables as the rows, and optionally
**Split by** an outcome to get one column per outcome group plus an Overall
column. Categorical variables show n (column %); continuous variables show
Mean (SD) or Median [IQR] (Auto matches the statistic to the chosen test).

A **p-value column and a tests-used footnote appear only when you split by an
outcome** — the test is chosen automatically from the assumption checks (e.g.
Welch's t-test vs Mann-Whitney, chi-square vs Fisher's exact). With no split it
is a plain one-dimensional summary.

The preview on the tab is plain; the **APA-formatted, numbered** version (Table 1,
Table 2, …) is produced when you **download the report** from **7 · Report** —
build the table, click **Add to report**, keep working, and download when the
analysis is complete. Figures added to the report are numbered and captioned to
APA style the same way.

## Regression and reporting

Select candidate outcomes on the Data tab, choose a method on **5 · Regression**
or **6 · Advanced models**, review diagnostics, and add confirmed tables/charts
to **7 · Report**. Reported p-values are not a substitute for checking design,
missingness, assumptions and clinical relevance.

## AI features

The planner and interpretation panels are disabled until an approved
OpenAI-compatible endpoint is configured. They receive aggregate metadata or
result tables only, never raw patient rows. Big Pickle is not zero-retention;
use it only with synthetic or de-identified data unless governance approval
explicitly covers the data class.
