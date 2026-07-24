# Sabali

**Sabali** is a browser-based guided statistics tool (in the spirit of
numiqo/SPSS) that guides a statistically non-expert team to the correct
analysis. Built in **R Shiny** so all computation runs in reference-quality R
libraries and every chart is drawn with **ggplot2**. Designed to run on an
internal server, so data never leaves the organisation.

This repository contains the **Phase 4 prototype** plus the full design of the
guided decision logic in `Guided_Statistics_Workflow_Design.md`.

Release controls are documented in [`DATA_GOVERNANCE.md`](DATA_GOVERNANCE.md),
[`REVIEW_CHECKLIST.md`](REVIEW_CHECKLIST.md),
[`KNOWN_LIMITATIONS.md`](KNOWN_LIMITATIONS.md), and [`SECURITY.md`](SECURITY.md).
The dependency baseline is recorded in [`renv.lock`](renv.lock); install `renv`
and restore it before a controlled deployment.

## What Phase 1 does

0. **Data check (Tab 0).** Runs the full battery of code-checkable data-quality
   tests against the active dataset — structural issues, missingness, disguised
   missing codes, numbers-stored-as-text, outliers, impossible counts,
   implausible dates, inconsistent categories, and duplicated subject/time
   records — and offers the result as a downloadable `.txt` report. Also builds
   a downloadable CSV subset from a chosen set of columns.
1. **Load data — any format.** CSV, RDA, RDS, Excel, SPSS (`.sav`), Stata
   (`.dta`) and more, read into a data frame via `rio` (which uses `foreign`
   and `haven` underneath). CSV and RDA are the common cases.
2. **Confirm variable types.** Every column's measurement type is auto-detected
   (continuous, count, binary, nominal, ordinal, date, identifier, text) and is
   fully overridable — a mandatory step, because a mis-typed variable is the
   commonest cause of a wrong analysis.
3. **Select analysis roles.** Select one or more candidate outcomes and,
   optionally, a subject ID, time variable, and event indicator. Outcomes are
   converted temporarily to the type required by the chosen analysis; the
   source data are never overwritten.
4. **Guided descriptives.** The summaries match both the variable's measurement
   type **and the dataset kind** declared on the Data tab. Cross-sectional data
   get the plain type-appropriate table (with an optional grouping). Longitudinal
   data are never pooled across rows: a time-invariant variable is summarised
   once per subject, while a time-varying one gets the panel decomposition
   (observation structure, summary by time, within/between variation with ICC,
   baseline-to-follow-up change, missingness by time). Time series get a
   time-series summary (level, spread, trend) rather than an iid-style table.
5. **Chart studio.** Pick 1–3 variables; the app recommends a chart type and
   explains why, then gives you a full aesthetic-customisation pipeline
   (palette, theme, text size, legend, opacity, titles, faceting, and more)
   feeding `ggplot2`. Longitudinal selections can add individual trajectories
   and aggregate mean/error-bar trends. Download any chart as PNG.
6. **One-click Word report.** Add tables and charts as you go, then export them
   all to a `.docx` document.

The **Regression** tab is available for independent outcomes. The dataset's
*kind* is set by an explicit question on the Data tab — **"How is your data
organised?"** (one row per subject / repeated measures / a time series) — not by
auto-detection, so a non-statistician confirms it deliberately. The **Advanced
models** tab appears only when that answer is "repeated measures" (with a chosen
subject-ID and time column) or "time series" (with a chosen time column); it is
hidden for cross-sectional data.

## Run it locally

```r
# 1. install packages (once)
Rscript install_dependencies.R

# 2. launch
R -e "shiny::runApp('.', launch.browser = TRUE)"
```

## Deploy on the internal server

Put this folder on the Shiny Server / Posit Connect host (e.g.
`/srv/shiny-server/sabali`) with the packages installed for the server's R.
Each browser session gets its own isolated data and report store, so the server
can host as many concurrent users as it is provisioned for. No data is written
to disk by the app.

## Project layout

```
app.R                              # UI + server wiring; Shiny auto-sources R/
R/
  fct_import.R                     # read any file format into a data frame
  fct_types.R                      # measurement-type detection + coercion
  fct_descriptives.R               # descriptive statistics by type
  fct_recommend.R                  # chart recommendation decision rules
  fct_charts.R                     # the ggplot2 aesthetic pipeline (core asset)
  fct_report.R                     # Word (.docx) export
  fct_assumptions.R                # Phase 2: normality / variance / expected counts
  fct_tests.R                      # Phase 2: hypothesis-test engine + effect sizes
  fct_recommend_hypothesis.R       # Phase 2: routes roles -> the right test
  fct_models.R                     # Phase 3/4: shared regression/model engine
  fct_outcome.R                    # selected-outcome conversion engine
  fct_panel.R                      # longitudinal summaries and trend charts
  fct_structure.R                  # longitudinal/time-series structure gate
  fct_survival.R                   # Phase 4: Kaplan-Meier and Cox engine
  fct_dataqc.R                     # Tab 0: data-quality check battery + text report
  mod_datacheck.R                  # Tab 0: data-quality report + column subset builder
  mod_data.R                       # import + type-confirmation module
  mod_descriptives.R               # guided descriptives module
  mod_charts.R                     # chart studio module
  mod_tests.R                      # Phase 2: guided hypothesis-testing module
  mod_models.R                     # Phase 3: guided independent regression module
  mod_phase4.R                     # Phase 4: clustered models + survival module
  mod_report.R                     # report builder module
install_dependencies.R
tests/                             # headless test suite
Guided_Statistics_Workflow_Design.md   # full decision-logic design (all phases)
```

## Roadmap

- **Phase 1 (built):** data + descriptives + chart studio + Word export.
- **Phase 2 (built):** guided hypothesis testing — location comparisons
  (one-sample, two independent/paired, >2 independent/repeated, factorial
  two-/three-way ANOVA, between×within mixed-model ANOVA, and ANCOVA) with their
  nonparametric alternatives, posthoc comparisons for multi-group and repeated
  designs, categorical association (chi-square / Fisher / McNemar), the
  one-proportion binomial test, and correlation (Pearson / Spearman / Kendall /
  point-biserial) — each wired to the automatic assumption-check layer, with
  effect sizes, CIs and plain-language interpretation. Both the primary and
  alternative global tests are always shown.
- **Phase 3 (built):** independent regression family — OLS, robust/rank-based,
  quantile, LASSO and ridge (L1/L2-penalised via cross-validated `glmnet`),
  partial least squares (`pls`), logistic, Poisson, and negative-binomial GLMs.
- **Phase 4 (built):** advanced models — GEE, LMM, GLMM, Kaplan–Meier survival
  analysis, and Cox proportional-hazards regression.

Each statistical method must be validated against R reference output and
published test cases before it is exposed — non-negotiable for health data.

## AI interpretation panel

The Descriptives, Tests, Regression and Advanced-models tabs each carry an
**AI interpretation** panel (`R/mod_interpret.R`, `R/fct_interpret.R`), and the
**Data** tab carries an **AI analysis planner** (`R/fct_planner.R`). Both read
only *summary/result* tables or aggregate variable **metadata** (names, types,
factor levels, counts) — never raw patient rows.

These features are **AI-only**: there is no offline fallback. When no AI
endpoint is configured, `ai_available()` is `FALSE` and every AI control across
the app is rendered **inactive** (a disabled button with an explanatory note).
Configure an endpoint to switch them on. The simplest route is OpenCode Zen's
free "Big Pickle" model — set one variable in a project-level `.Renviron`
(`app.R` loads this file explicitly on startup, so the key is picked up
regardless of where R was launched or whether a `~/.Renviron` exists; keep it
out of version control):

```sh
OPENCODE_API_KEY=sk-…        # from https://opencode.ai/auth
```

For any other OpenAI-compatible endpoint (an approved on-prem model, Azure, …)
use the generic variables instead, which take precedence:

```sh
AI_INTERPRET_ENDPOINT=https://…/v1/chat/completions
AI_INTERPRET_KEY=…           # bearer token, if the endpoint needs one
AI_INTERPRET_MODEL=…         # model id
```

The LLM path uses `httr2` (`install_dependencies.R` installs it). If the
endpoint is unreachable the panel shows the error — it does **not** fall back to
any offline output. Restart the app after changing `.Renviron` (it is read once
at launch). The panel's **Source** line names the model that answered (e.g.
"…via OpenCode Zen").

> **Governance:** only point this at an endpoint approved for the data involved.
> OpenCode Zen's "Big Pickle" is **not** zero-retention during its free period
> (its docs state collected data may be used to improve the model), so use it
> only with synthetic / de-identified data — never real patient records. The
> panel sends aggregate result tables only, never raw rows.

## Notes & known limits (prototype)

See [`KNOWN_LIMITATIONS.md`](KNOWN_LIMITATIONS.md) for the release-maintained
list and [`DATA_GOVERNANCE.md`](DATA_GOVERNANCE.md) for the required human
sign-offs. Those approvals are not implied by passing automated tests.

- Raincloud plots use `ggdist` when installed and fall back to violin + jitter
  otherwise; the scatterplot matrix needs `GGally`.
- `mosaic` is rendered as a proportional stacked bar (a true mosaic with
  variable widths is a Phase 2 refinement).
- Type auto-detection uses sensible thresholds (see `R/fct_types.R`); always
  confirm on the Data tab.
- The statistical engine and Shiny module wiring are covered by the headless
  test suite in `tests/`; live browser QA on the deployment server is still
  recommended before team use.
```
