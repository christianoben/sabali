# Sabali — Guided Statistics Tool — Workflow Logic Design

**Sabali — internal guided-statistics tool**
Prepared for: Christian Ochieng
Date: 23 July 2026 (updated with confirmed decisions)
Status: Decisions confirmed — Phase 1 prototype in build

---

## 1. What this document is

This specifies the **decision logic** — the rules that let a statistically non-expert user select variables and be guided to the correct descriptives, charts, tests, and models. It is deliberately engine- and UI-agnostic: the same rules apply whether the R backend runs a t-test or a mixed model.

### Confirmed architecture and product decisions

- **Deployment:** internal server; browser front end, no data leaving the organisation.
- **Compute engine:** R in the background (reference-quality libraries), results (tables, charts, model objects) rendered on the web.
- **Delivery technology:** **R Shiny** app served from a Shiny Server / Posit Connect instance. This keeps everything in one R codebase (matching the engine choice), renders `ggplot2` charts natively, and scales to as many concurrent users as the server is provisioned for.
- **Input formats:** accept *any* file the user supplies. Behind the scenes each file is read into a data frame via `rio` (which dispatches to `foreign`, `haven`, `readxl`, etc.), with `.rda`/`.rds` and `.csv` handled directly. CSV and RDA are expected to be the most common.
- **Reporting:** one-click export to a Word (`.docx`) report of the selected tables and charts.
- **Concurrency:** designed to be stateless per session so the server can host as many concurrent users as are present.
- **Priority focus:** descriptives and a rich, well-designed **chart studio** are the headline asset — a full aesthetic-customisation pipeline feeding `ggplot2`.
- **Mixed models:** confirmed as **LMM and GLMM** (generalised linear mixed model).

### Guiding principles

1. **Recommend, then let the user see.** The tool proposes the primary analysis, runs the assumption checks, and — if an assumption fails — surfaces the recommended alternative *with a plain-language reason*. It never silently swaps methods. A guided tool that hides its reasoning trains bad habits.
2. **Always pair a p-value with an effect size and a confidence interval.** For a team that is not statistically ready, the biggest risk is p-value worship. Effect sizes and CIs are shown by default, not on request.
3. **Warn loudly on sample size, missing data, and multiple comparisons.** These are the three silent killers of naive analysis.
4. **User can always override.** Auto-detection is a starting point, not a verdict.
5. **Plain-language interpretation accompanies every result**, written for a non-statistician, with the caveats stated.

---

## 2. Variable taxonomy

Every column is assigned a **measurement type** (what kind of value it holds) and, during an analysis, a **role** (what job it does in that analysis).

### 2.1 Measurement types

| Type | Definition | Examples |
|---|---|---|
| Continuous | Numeric, interval/ratio, many distinct values | CD4 count, age, weight (kg), viral load |
| Count | Non-negative integers, often right-skewed | number of visits, number of partners |
| Binary | Exactly two categories | HIV status (pos/neg), sex, outcome (yes/no) |
| Nominal | Unordered categories, >2 | county, facility, regimen type |
| Ordinal | Ordered categories | WHO clinical stage, Likert agreement, education level |
| Date/Time | Calendar or clock values | enrolment date, visit date |
| Identifier | Effectively unique per row; not analysed as data | patient ID, record ID |
| Text (free) | Unstructured; excluded from statistical analysis | comments |

### 2.2 Roles (assigned per analysis)

- **Outcome (dependent):** what is being explained or compared.
- **Predictor / exposure (independent):** explanatory variable(s).
- **Grouping factor:** defines the groups being compared.
- **Cluster / subject ID:** identifies repeated measurements on the same unit (drives mixed models and GEE).
- **Time variable:** follow-up time (survival) or measurement occasion (repeated measures).
- **Event / censoring indicator:** for time-to-event outcomes.

At the Data step, users may select multiple candidate outcomes and optional
subject ID, time, and event roles. A selected outcome is converted on a
temporary analysis copy to the type required by the chosen test or model; the
imported data and confirmed metadata remain unchanged. Numeric-looking text
can be converted safely, while unsafe nominal-to-numeric conversions are
rejected with a plain-language message.

For repeated observations, the Descriptives and Charts modules also use the
selected ID/time roles to provide within-person versus between-person
variation, missingness by time point, baseline-to-follow-up changes, individual
trajectories, and aggregate trend lines with error bars.
- **Covariate / adjustment / weight.**

### 2.3 Auto-detection heuristics (all overridable)

Applied on import, then shown to the user for confirmation:

- Numeric, non-integer OR distinct values > ~15–20 → **Continuous**.
- Numeric, integer, non-negative, moderate range → **Count** (offer Continuous as alternative).
- Exactly 2 distinct non-missing values → **Binary**.
- Character / factor, unordered, ≥3 levels → **Nominal**.
- Ordered factor, or user flags an order, or recognised ordinal keywords → **Ordinal**.
- Parses as date → **Date/Time**.
- Distinct count ≈ number of rows, or column name matches id/uid/mrn → **Identifier**.

**Critical UX rule:** the confirm-types screen is mandatory. Mis-typing (e.g. an ordinal Likert read as continuous, or a coded categorical `1/2/3` read as numeric) is the single most common source of wrong analysis. The tool shows detected type, distinct values, and % missing per column and asks the user to confirm before anything else.

---

## 3. Descriptives — decision rules

Driven purely by the **measurement type** of each selected variable.

| Type | Reported by default | Notes |
|---|---|---|
| Continuous | n, missing (n and %), mean, SD, SE, 95% CI of mean, median, IQR (Q1, Q3), min, max, range, skewness, kurtosis, + normality check | If skew is high or normality fails, the UI **emphasises median/IQR** and flags "distribution is skewed — prefer median". |
| Count | Same as continuous, plus mean-to-variance ratio (dispersion flag) | High dispersion flag feeds later model choice (Poisson vs negative binomial). |
| Binary | Counts, proportion of each level, 95% CI for the proportion (Wilson), valid n, missing | Wilson interval, not normal-approx, so small samples behave. |
| Nominal | Frequency table (count, %, valid %, cumulative), mode | |
| Ordinal | Frequency table + median, IQR, min, max | Mean shown only with a caveat. |
| Date/Time | n, min, max, span, missing | |

**Grouped descriptives:** if a grouping factor is also selected, all of the above are produced per group side by side.

---

## 4. Charts — decision rules

Driven by **how many** variables are selected and **their types**. The tool proposes a default (marked ★) and offers the rest as one-click alternatives.

### 4.1 One variable

| Variable type | Charts offered |
|---|---|
| Continuous / Count | ★ Histogram, density plot, boxplot, violin plot, raincloud plot, Q-Q plot (normality), ECDF |
| Binary / Nominal | ★ Bar chart, Pareto chart, frequency table; pie chart offered but flagged "harder to read" |
| Ordinal | ★ Ordered bar chart, frequency table |
| Date/Time | ★ Timeline / count-over-time |

### 4.2 Two variables

| Combination | Charts offered |
|---|---|
| Continuous × Continuous | ★ Scatter plot (+ optional loess / regression line), 2D density, Bland–Altman (if method-comparison flagged) |
| Continuous × Categorical | ★ Grouped boxplot, grouped violin, raincloud by group, bar of means ± 95% CI, jitter/strip plot |
| Categorical × Categorical | ★ Grouped (clustered) bar, stacked bar (100% option), mosaic plot, contingency heatmap, Sankey |
| Continuous × Time | ★ Line chart, run chart |

### 4.3 Three or more variables

- Correlation matrix heatmap and scatterplot matrix (for sets of continuous variables).
- Any bivariate chart above, **faceted or colour-coded** by a third categorical variable.

**Auto-decision summary shown to the user:** e.g. *"You selected one continuous variable (CD4 count) and one categorical grouping (sex). Recommended: grouped boxplot + descriptives per group. Because CD4 count is right-skewed, the boxplot is preferred over a bar-of-means."*

---

## 5. Hypothesis testing — decision tree

The engine first asks three questions from the roles and types already assigned:

1. **What is the outcome type?** (continuous/ordinal location, vs categorical proportion/association, vs a relationship between two continuous variables)
2. **How many groups, and are they independent or paired/repeated?**
3. **Do the assumptions hold?** (checked automatically — see Section 7)

### 5.1 Comparing means / locations (continuous or ordinal outcome)

| Design | Assumptions OK → primary | Assumptions fail → alternative |
|---|---|---|
| 1 sample vs a reference value | One-sample t-test | Wilcoxon signed-rank (or sign test) |
| 2 independent groups | Welch's t-test *(default)*; pooled t-test if variances equal | Mann–Whitney U; **exact** version if small/ties |
| 2 paired groups | Paired t-test | Wilcoxon signed-rank |
| >2 independent groups | One-way ANOVA + Tukey post-hoc; **Welch ANOVA** + Games–Howell if variances unequal | Kruskal–Wallis + Dunn post-hoc |
| >2 repeated measures | Repeated-measures ANOVA (check sphericity → Greenhouse–Geisser correction) + Holm-adjusted paired comparisons | Friedman test + Holm-adjusted paired Wilcoxon comparisons |
| 2+ factors (factorial) | Two-/three-way ANOVA (main effects + interactions) | Scheirer–Ray–Hare (2-way) or Aligned Rank Transform (ART) ANOVA |
| ANCOVA | Covariate-adjusted group comparison | Quade rank ANCOVA |
| Mixed / split-plot (between × within) | Linear mixed-model ANOVA with a subject random intercept | Rank-transformed mixed-model sensitivity analysis |

**Default choice note:** Welch's t-test is the recommended default for two independent groups because it does not assume equal variances and loses almost nothing when they are equal. This avoids a fragile two-step (test variances, then choose) that trips up non-experts.

**Effect sizes shown:** Cohen's d / Hedges' g (t-tests), eta-squared / omega-squared (ANOVA), rank-biserial r (Mann–Whitney), epsilon-squared (Kruskal–Wallis).

### 5.2 Categorical outcomes — proportions and associations

| Design | Primary | Small-sample / exact alternative |
|---|---|---|
| 1 proportion vs reference | z-test for a proportion | **Exact binomial test** |
| 2 independent proportions (2×2) | Chi-square test of independence | **Fisher's exact test** (when expected cell counts are low) |
| r × c table | Chi-square test | **Fisher–Freeman–Halton** exact; ordinal → Cochran–Armitage / linear-by-linear trend |
| Paired proportions (2×2) | McNemar's test | McNemar exact (small samples) |
| Repeated binary outcome (3+ matched conditions/time points) | Cochran's Q test | Pairwise McNemar tests with Holm adjustment after a significant global Q |
| Paired r × c | Stuart–Maxwell / Bowker | — |

**Switch rule (auto):** if any expected cell count < 5 in more than 20% of cells (or any expected count < 1), the tool recommends the exact test and says so.

**Reported alongside:** risk difference, risk ratio, odds ratio with 95% CIs (2×2), and association strength (Cramér's V / phi).

### 5.3 Correlations

| Data | Primary | When |
|---|---|---|
| Continuous × continuous, linear, roughly normal | Pearson r | default for bivariate normal, linear relationships |
| Non-normal / ordinal / monotonic-but-nonlinear | Spearman's rho | robust to outliers and monotone nonlinearity |
| Ordinal × ordinal, small n or many ties | Kendall's tau | better small-sample behaviour |
| Binary × continuous | Point-biserial | |
| Two numeric measures repeated within subjects | Repeated-measures correlation (rmcorr) | Ordinary Pearson/Spearman correlation as a descriptive sensitivity analysis |

All reported with CI and significance; a scatter plot is shown automatically so the user can eyeball linearity before trusting Pearson.

---

## 6. Regression and survival — model selection rules

The engine detects the **data structure** first, then the **outcome type**.

**Data structure:**

- *Independent observations* — one row per unit, no cluster/subject ID.
- *Clustered / repeated* — a cluster or subject ID is present (multiple rows per unit).
- *Longitudinal / time-to-event* — a time variable (and, for survival, an event indicator) is present.

The interface exposes independent **Regression** methods generally. **Advanced
models** is shown only when the confirmed dataset supports repeated
within-subject observations or contains a date/time series; cross-sectional
datasets do not expose that tab.

### 6.1 Independent observations

| Outcome type | Primary model | Alternatives / when |
|---|---|---|
| Continuous | OLS linear regression (simple or multiple) | If residuals are heteroscedastic, heavy-tailed, or influenced by outliers → **robust (M-estimator) regression** or **rank-based regression**. If interest is in conditional quantiles or the outcome is very skewed → **quantile regression**. |
| Count | GLM — Poisson | Overdispersion → **negative binomial** (or quasi-Poisson); excess zeros → zero-inflated (Phase 2). Use an offset for rates. |
| Binary | GLM — logistic | Rare events / separation → Firth's penalised logistic. |
| Ordinal | Proportional-odds (ordinal) logistic | Check proportional-odds assumption. |
| Nominal (>2) | Multinomial logistic | |

### 6.2 Clustered / repeated / longitudinal

| Question | Model |
|---|---|
| Population-average effect, correlated observations | **GEE** (choose working correlation: exchangeable, AR(1), unstructured) |
| Subject-specific effect, continuous outcome | **Linear mixed model (LMM)** — random intercepts/slopes |
| Subject-specific effect, non-normal outcome (binary/count) | **Generalised linear mixed model (GLMM)** |

> Confirmed: the two mixed models in scope are **LMM** (continuous outcome) and **GLMM** (generalised linear mixed model, for binary/count outcomes on clustered data).

### 6.3 Time-to-event (survival)

| Goal | Method |
|---|---|
| Describe/compare survival between groups | **Kaplan–Meier** curves + **log-rank test** |
| Model effect of covariates on hazard | **Cox proportional-hazards regression** (check PH assumption via Schoenfeld residuals) |
| PH assumption violated | Stratified Cox, time-varying covariates, or parametric AFT models |
| Competing events present | Fine–Gray / cumulative incidence (Phase 2 extension) |

**Reported for models:** coefficients with 95% CIs, exponentiated effects where meaningful (odds ratios, rate ratios, hazard ratios), model fit (R²/pseudo-R², AIC), and the assumption diagnostics from Section 7.

---

## 7. Assumption and distribution test layer

This is the connective tissue that reroutes recommendations. Each check runs automatically and reports a plain-language verdict; a failed check surfaces the recommended alternative but does not force it.

| Assumption / property | Test(s) | Feeds decision |
|---|---|---|
| Normality | Shapiro–Wilk (n ≤ ~5000), Anderson–Darling, Lilliefors KS; plus Q–Q plot, skew, kurtosis | t-tests / ANOVA / Pearson → nonparametric alternatives |
| Equal variances | **Levene** (default, robust), Brown–Forsythe; Bartlett only if normal | pooled vs Welch t-test / ANOVA |
| Sphericity | Mauchly's test | repeated-measures ANOVA → Greenhouse–Geisser / Huynh–Feldt correction |
| Expected cell counts | Rule-based (<5 in >20% of cells) | chi-square → exact test |
| Linearity | Residual and component+residual plots | linear regression → transform / GAM / quantile |
| Homoscedasticity (regression) | Breusch–Pagan, residual-vs-fitted plot | OLS → robust regression |
| Multicollinearity | Variance inflation factor (VIF) | warn; suggest dropping/combining predictors |
| Influential points | Cook's distance, leverage | flag points; suggest robust regression |
| Overdispersion (counts) | Dispersion statistic | Poisson → negative binomial |
| Proportional hazards | Schoenfeld residuals test | Cox → stratified / time-varying |
| Independence | Study-design prompt; Durbin–Watson if ordered | flags need for mixed model / GEE |

**Important calibration:** normality tests are over-sensitive at large n and under-powered at small n. The tool does **not** mechanically switch to nonparametric on a failed Shapiro–Wilk alone. For n greater than ~30–40 per group it notes that means-based tests are robust to mild non-normality (central limit theorem) and lets the user proceed, while still offering the nonparametric option. This prevents the classic over-correction that a naive rule would cause.

---

## 8. End-to-end user workflow

1. **Import data** — any format, auto-read into a data frame via `rio`/`foreign`/`haven` (CSV and RDA most common).
2. **Confirm variable types** (mandatory screen; auto-detected, user-overridable).
3. **Choose a goal** — either pick a task directly ("compare two groups", "look at one variable") or answer *"What do you want to find out?"* and let the tool route.
4. **Assign roles** — outcome, groups, predictors, ID, time, event — with only valid roles offered for each variable type.
5. **Review the proposed analysis plan** — the tool states, in plain language, which descriptives, charts, test/model, and assumption checks it will run, and why.
6. **Run** in the R backend.
7. **Read the results** — tables, charts, effect sizes with CIs, assumption verdicts, and a plain-language interpretation with caveats. Export to report.

**Cross-cutting guards, always on:** missing-data report (with a warning before listwise deletion), minimum-sample-size warnings, and multiplicity control whenever several tests are run together. The Tests tab uses Tukey/Games–Howell adjustments for their corresponding ANOVA follow-ups and Holm adjustment for Dunn and paired follow-ups.

---

## 9. Build phases (as implemented)

The prototype in this folder is **Phase 4**. The phase boundaries are kept
visible in the UI so independent regressions are distinct from clustered and
time-to-event analyses.

- **Phase 1 — Data + Descriptives + Chart Studio + Word export (BUILT).** Import any format → confirm variable types and roles → guided descriptives → a full `ggplot2` chart studio with an aesthetic-customisation pipeline → collect items into a one-click Word report. This is the headline asset and is useful on day one.
- **Phase 2 — Hypothesis testing + assumption layer (BUILT).** Means/locations + nonparametrics (one-sample, two independent/paired, >2 independent/repeated, factorial, and mixed/split-plot), multiplicity-adjusted posthoc comparisons for multi-group and repeated designs, categorical + exact tests (one-proportion binomial, chi-square/Fisher/McNemar, and Cochran's Q for repeated binary outcomes), correlations (Pearson/Spearman/Kendall, plus repeated-measures correlation for longitudinal numeric pairs), each wired to the Section 7 assumption checks. Every result shows both the recommended primary global test and its alternative, with effect sizes, CIs, term tables for factorial/mixed designs, and a plain-language interpretation.
- **Phase 3 — Independent regression family (BUILT).** OLS,
  robust/rank-based, quantile, and the GLM family with diagnostics.
- **Phase 4 — Advanced models (BUILT).** GEE, LMM, and GLMM for repeated or
  clustered observations, plus survival analysis (Kaplan–Meier and Cox).

Each phase is independently useful, and each added method must be **validated against R reference output and published test cases** before it is exposed — non-negotiable for health data.

---

## 10. Decisions resolved / remaining questions

**Resolved (this round):** mixed models are LMM + GLMM; input = any format via `rio`/`foreign` (CSV/RDA common); one-click Word export; server hosts as many concurrent users as present; descriptives + chart studio are the priority.

**Still worth confirming as we go:**

1. **Authentication/audit** — does the internal Shiny Server need per-user logins and an audit trail, or is network-restricted access enough?
2. **Chart branding** — do you want an official organisation colour palette and fonts baked into the chart studio as a default theme? (A placeholder brand palette is included now.)
3. **Priority datasets** — a couple of representative team datasets would let us tune type-detection defaults and Phase 2 test coverage to real usage.
