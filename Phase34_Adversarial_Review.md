# Adversarial Code Review — Phase 3 (Regression) & Phase 4 (Advanced Models + Survival)

**Project:** Sabali (R Shiny)
**Reviewer role:** adversarial QA
**Date:** 23 July 2026
**Scope:** `R/fct_models.R`, `R/fct_survival.R`, `R/fct_structure.R`, `R/mod_models.R`, `R/mod_phase4.R`

New end-to-end suite: **`tests/test_phase34_adversarial.R`** (wired into `run_all.R`).
It pins **every column** of each coefficient table against the reference R fit
(not just the point estimate, which is where the existing suites stop), plus
the input-guard and structure-gating paths. Result today: **34 pass, 4 fail** —
the 4 failures are the two findings below.

---

## Verdict

The Phase 3/4 engines are, on the whole, **statistically sound**. Point
estimates, p-values, effect ratios (OR/RR/HR) and their confidence intervals
all match `lm` / `glm` / `MASS::rlm` / `quantreg::rq` / `geepack::geeglm` /
`lme4` / `survival` to 1e-6, missing-data handling and `n used` are correct, and
the input guards (bad tau, single cluster, AR-1 without time, time == event,
zero events, negative follow-up, outcome-as-predictor) all fire.

**One real correctness/labelling bug** was found, in the Cox coefficient table.

---

## Finding 1 — Cox coefficient table mislabels two columns  ·  **Severity: Medium-High**

**Where:** `R/fct_survival.R:275` (`run_cox_model`) → `matrix_to_coefficient_table()`
in `R/fct_models.R:146`.

**What:** For Cox regression the reported **"Std. Error" column actually holds
`exp(coef)`**, and the **"Statistic" column actually holds `se(coef)`**. The
Estimate, Hazard ratio, p-value and HR confidence interval are all **correct**.

**Root cause:** `matrix_to_coefficient_table()` defaults to `statistic_col = 3`
and `se = col 2`, which is the column order for `lm`/`glm`
(`Estimate, Std.Error, statistic, p`). But `summary(coxph)$coefficients` inserts
an extra column, so its order is:

```
coef | exp(coef) | se(coef) | z | Pr(>|z|)
  1        2          3        4      5
```

So the generic mapper reads column 2 (`exp(coef)`) as the standard error and
column 3 (`se(coef)`) as the statistic. The p-value survives because it is
pulled by name (`p_col = "Pr(>|z|)"`), and the CI survives because it is computed
separately via `confint.default()`.

**Evidence (reproduced):**

| Column shown | App value (predictor `x`) | Should be |
|---|---|---|
| Std. Error | 1.2379 | 0.1023 (`se(coef)`) |
| Statistic  | 0.1023 | 2.0862 (`z`) |

A clinician reading "coefficient 0.213, Std. Error 1.238" would infer the term
is far from significant — yet p = 0.037. The row is internally contradictory.

**Fix (targeted, keeps the generic helper intact):** in `run_cox_model`, map the
columns by name rather than by position, e.g.

```r
coef_tab <- coefficient_table(
  term      = rownames(co),
  estimate  = co[, "coef"],
  std_error = co[, "se(coef)"],
  statistic = co[, "z"],
  p_value   = co[, "Pr(>|z|)"],
  ci_lower  = ci[, 1], ci_upper = ci[, 2],
  effect_name = "Hazard ratio")
```

**Tests that lock it (fail today):**
`[BUG] Cox 'Std. Error' column equals reference se(coef)`,
`[BUG] Cox 'Statistic' column equals reference z`,
`[BUG] Cox 'Std. Error' is NOT the hazard ratio`.

> Note: the same generic mapper is correct for OLS/robust/quantile/GLM/GEE
> because those summaries follow the standard 4-column order — verified green in
> sections 1-4 of the suite. Cox is the only victim of the extra `exp(coef)`
> column.

---

## Finding 2 — Cryptic error for a single-level factor predictor  ·  **Severity: Low**

**Where:** `R/fct_models.R` (`model_frame` / the `lm`/`glm` fit path).

**What:** Selecting a predictor that collapses to a single level (after
complete-case filtering) surfaces the raw R message
*"contrasts can be applied only to factors with 2 or more levels"* rather than a
plain-language explanation. The Shiny tab does catch it (no crash) and shows it
in the red alert box, but the wording is not the guided, non-expert-friendly
style the design doc calls for.

**Fix:** in `model_frame`, after subsetting to complete rows, check each factor
predictor's `nlevels(droplevels(.))` and `stop()` with a clear message naming
the offending variable (mirrors the existing friendly guards in
`run_guided_model`).

**Test that locks it (fails today):**
`[BUG] single-level factor predictor gives a plain-language error`.

---

## Lower-priority observations (no test, no action required unless you want it)

1. **GEE fit-stats coercion** (`R/fct_models.R:320`) — `metrics` mixes the
   character `corstr` into a numeric `c(...)`, coercing `n used`/`clusters` to
   strings. Cosmetic only (display table is character anyway), but if you ever
   compute on those values they are text.
2. **Structure-detection "reason" text** (`R/fct_structure.R`) — on the real
   cohort it lists `missed_visits` among the "time" variables (it matches the
   `visit` name-hint and varies within subject). Gating is still correct
   (Advanced tab available = longitudinal); only the human-readable reason names
   a column that is really an outcome. Consider excluding count/continuous
   outcomes from the time-hint list, or softening the wording.

---

## What was checked and is correct (regression guards, all green)

- **OLS** — Estimate, Std. Error, t, p, 95% CI vs `lm`/`confint`.
- **Logistic** — SE, z, p, odds ratios and OR CIs vs `glm`.
- **Poisson** — SE, rate ratios, Pearson dispersion vs `glm`.
- **GEE (Gaussian)** — Estimate and robust Std. Error vs `geepack::geeglm`;
  AR-1 with a waves/time variable fits.
- **LMM/GLMM** — fixed effects vs `lme4`; random-slope formula builds and fits.
- **Kaplan-Meier** — per-group median survival and the log-rank p vs
  `survfit`/`survdiff`; events + censored == n used.
- **Cox** — coef, HR, p, HR CI vs `coxph` (only the SE/Statistic labels are wrong).
- **Guards** — outcome-as-predictor, no predictors, tau∉(0,1), single cluster,
  AR-1 without time, cluster == outcome, time == event, zero events, negative
  time — all refuse with a handled error.
- **Structure gating** — repeated ID without within-subject time variation and
  cross-sectional unique IDs are correctly hidden; the real cohort is detected
  as longitudinal.

## Run it

```bash
Rscript tests/test_phase34_adversarial.R   # 34 pass, 4 fail (the two findings)
Rscript tests/run_all.R                    # 15 of 16 suites pass
```

"Done" = Finding 1 fixed (3 `[BUG]` checks go green) and, optionally, Finding 2
(1 `[BUG]` check). No `[GUARD]` check should regress.

---

## Resolution (applied)

Both findings fixed; the two cosmetic observations are left as noted.

- **Finding 1 (Cox columns)** — `run_cox_model()` in `R/fct_survival.R` now maps
  `summary(coxph)$coefficients` by column name (`coef`, `se(coef)`, `z`,
  `Pr(>|z|)`) via `coefficient_table()`, instead of the positional
  `matrix_to_coefficient_table()`. Estimate, Hazard ratio, p-value and HR CI are
  unchanged; "Std. Error" now shows `se(coef)` and "Statistic" shows `z`. The
  generic helper is untouched, so OLS/robust/quantile/GLM/GEE are unaffected.
- **Finding 2 (single-level factor)** — `model_frame()` in `R/fct_models.R` now
  checks each categorical predictor's level count after complete-case filtering
  and `stop()`s with a plain-language message naming the variable, pre-empting
  R's raw "contrasts can be applied…" error.

Expected: the 4 `[BUG]` checks in `tests/test_phase34_adversarial.R` go green
with no `[GUARD]` regression. (Verified by code trace against the suite; run
`Rscript tests/test_phase34_adversarial.R` to confirm — R was unavailable in the
authoring sandbox.)
