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

### Form metadata (REDCap or ODK)

If your data came from REDCap or from ODK / Kobo, upload the definition it was
collected with next to the data: a REDCap **Data Dictionary (codebook)** CSV, or
an **XLSForm** workbook. The app detects which it is and takes each field's type
and value labels from it instead of guessing, so coded categories are read as
categories and display their real names throughout - descriptives, Table 1,
charts, the planner and the report. Columns the definition does not describe
(REDCap's own `redcap_event_name`, ODK's `meta/instanceID`) are left to the
detector, and a code found in the data but missing from the definition is kept
and reported rather than silently discarded.

One ODK caveat worth knowing: if your export leaves select-multiple questions as
a single column of space-separated answers, the app will not type it, because
that column is a set rather than a category. Re-export with select multiples
split into one column per choice, or derive the indicators you need on the
Data-check tab.

### Wide files

Files with hundreds or thousands of columns (REDCap and Stata exports often
have) show the type list 25 columns at a time, with a search box and a filter.
On a file with more than 100 columns the list opens on **Needs a look** - the
columns that usually need a decision, namely small sets of whole numbers that may
really be coded categories, and columns detected as IDs. **Empty or constant**
lists the columns nothing can be done with, and **All columns** shows everything.
Corrections are remembered as you page, and a column you never open keeps its
detected type.

The filter is a prompt, not a verdict. It cannot spot every coded category: an
antibiotic code list with 25 values looks exactly like a duration in days, so
check the fields your analysis depends on directly. If your export came with a
REDCap data dictionary, that is the authority on which fields are categorical.

## Regression and reporting

Select candidate outcomes on the Data tab, choose a method on **5 · Regression**
or **6 · Advanced models**, review diagnostics, and add confirmed tables/charts
to **7 · Report**. Reported p-values are not a substitute for checking design,
missingness, assumptions and clinical relevance.

Once two or more predictors are chosen, **Interaction terms** offers every pair
of them. An interaction asks whether the effect of one predictor depends on the
level of another - for example whether a vaccine's effect on prescribing differs
by sex. Both main effects stay in the model, and the interpretation panel reads
the interaction as effect modification rather than as a plain association: when
an interaction is in the model, the main effects of the variables it involves
are conditional, not overall effects.

### Complex sample designs

If your data came from a weighted, stratified or clustered sample - a DHS or
MICS extract, a national health survey, a cluster-randomised trial, a
point-prevalence survey sampling patients within hospitals - declare the design
on the Data tab and the Regression tab will offer design-based estimation.
One- and two-stage designs are supported, and so are replicate-weight designs
(BRR, Fay, JK1, JKn, bootstrap, successive-difference) - NHANES and several DHS
recodes ship a block of replicate weights instead of PSU and stratum
identifiers. Give the replicate columns as a name pattern such as
`^BRR[0-9]+$`.

### Path analysis and mediation

**6 · Advanced models** offers a path model when you want to know not just
whether an exposure is associated with an outcome, but whether it acts *through*
something. Choose the outcome, the mediators in causal order (earliest first),
and the exposures. The app reports each direct path plus an indirect effect for
every route and a total effect per exposure.

Two things are worth reading carefully. The mediator ordering is your causal
claim - the data cannot tell you it is right. And if every variable is connected
to every other, the model is *saturated*: its fit indices are perfect by
arithmetic rather than by fitting well, and the app says so rather than letting
them look like evidence.

### What else is on the Tests tab

Beyond the classical comparisons, the Tests tab covers the measures public-health
work turns on:

- **Effect measures** — risk difference, risk ratio, odds ratio and number needed
  to treat for a binary outcome across two groups. Worth reading together: an
  odds ratio is not a risk ratio once the outcome is common, and the app says so
  when they diverge.
- **Trend** — Cochran-Armitage, for an ordered exposure such as a dose, a wealth
  quintile or an age band. It asks whether risk moves consistently with the
  level, which a chi-square test cannot.
- **Diagnostic accuracy** — sensitivity, specificity, predictive values and
  likelihood ratios against a reference standard. The predictive values are
  recomputed across a range of prevalences, because PPV and NPV depend on how
  common the condition is where the test is used and do not transfer between
  settings.
- **Agreement** — Cohen's kappa for two raters, Fleiss' kappa for a panel of
  three or more, with a per-category breakdown showing which category is being
  applied inconsistently.

**6 · Advanced models** additionally offers incidence rates over person-time,
rate ratios, and direct or indirect (SMR) standardisation.

### Missing data

The Regression tab offers multiple imputation when complete-case analysis would
actually cost you rows. It imputes each missing value several times, fits the
model in each, and pools by Rubin's rules, so the uncertainty about the missing
values reaches the standard errors. The single mean/median fills on the Data
check tab do not do this — they are convenient for tidying a column, but they
shrink standard errors and should not be used before a model you intend to
report.

## AI features

The planner and interpretation panels are disabled until an approved
OpenAI-compatible endpoint is configured. They receive aggregate metadata or
result tables only, never raw patient rows. Big Pickle is not zero-retention;
use it only with synthetic or de-identified data unless governance approval
explicitly covers the data class.
