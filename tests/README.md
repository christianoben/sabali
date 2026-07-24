# Tests

Headless tests for the app pipeline. They run all the app's `R/` logic
without a browser, against the bundled sample data
(`data/lvct_cohort_repeated.csv`) and small crafted fixtures.

## Run

From the project root:

```bash
Rscript tests/run_all.R            # run every suite, one summary line each
```

Or run a single suite:

```bash
Rscript tests/test_pipeline.R      # full end-to-end (all 24 chart types)
Rscript tests/test_types.R         # detect_type() correctness
Rscript tests/test_import.R        # .rds / .rda / haven import paths
Rscript tests/test_descriptives.R  # statistical correctness + missing counts
Rscript tests/test_charts.R        # assign_chart_roles() + palette branches
Rscript tests/test_heatmaps.R      # continuous-fill chart regression
Rscript tests/test_assumptions.R   # Phase 2 assumption / distribution checks
Rscript tests/test_datacheck.R     # Tab 0 data-quality engine + subset builder
Rscript tests/test_data_readiness.R # 22-point contract, actions, review, and UI flow
Rscript tests/test_design_coverage.R  # checklist of classical tests vs what is wired
Rscript tests/test_lasso.R         # Phase 3 LASSO regression (glmnet) engine + wiring
Rscript tests/test_ridge_pls.R     # Phase 3 ridge (glmnet) + PLS (pls) regression
Rscript tests/test_ancova_pointbiserial.R  # Tests-tab ANCOVA + point-biserial correlation
Rscript tests/test_hypothesis.R    # Phase 2 hypothesis-test engine + routing
Rscript tests/test_descriptives_grouping.R  # Issue: grouped descriptives, all types
Rscript tests/test_method_filtering.R       # Issue: type-aware method/role filtering
Rscript tests/test_models.R                  # Phase 3 independent regression
Rscript tests/test_advanced_models.R        # Phase 4 GEE, LMM, and GLMM
Rscript tests/test_survival.R                # Phase 4 Kaplan-Meier and Cox
Rscript tests/test_structure.R               # Advanced-model data-structure gate/UI
Rscript tests/test_validate_location.R       # Tier-1/Tier-2 location golden tests
Rscript tests/test_validate_categorical.R    # chi-square/Fisher/Q/proportion goldens
Rscript tests/test_validate_correlation.R    # Pearson/Spearman/rmcorr goldens
Rscript tests/test_validate_regression.R     # GLM/OLS/penalised adversarial goldens
Rscript tests/test_validate_advanced.R       # GEE/LMM/GLMM/survival goldens
Rscript tests/test_validate_assumptions.R    # engineered assumption violations
Rscript tests/test_validate_types.R          # import/type/label/edge-case checks
Rscript tests/test_validate_governance_ui.R  # AI-off, catalogue, all-tab smoke checks
Rscript tests/test_validate_security.R       # cleanup, audit, and governance wiring
Rscript tests/test_validate_empty_state.R    # pre-upload Tests-tab rendering
Rscript tests/test_validate_report.R         # Word report golden structure
Rscript tests/test_validate_fuzz.R           # deterministic detect_type properties
Rscript tests/test_validate_performance.R    # scale and progress guards
Rscript tests/test_validate_static.R          # package, namespace, lock, lint checks
Rscript tests/test_shinytest2_smoke.R        # browser tab-render smoke test
Rscript tests/test_coverage.R                # covr target (80% line coverage)
```

> **Note:** `test_descriptives_grouping.R` and `test_method_filtering.R` began
> as specification suites for the defects in
> `Defect_Report_Grouping_and_Filtering.md`. Their `[GUARD]` and `[BUG]` checks
> now pass and are retained as regression coverage.

Each script prints `PASS`/`FAIL` per check and exits non-zero if anything
fails, so they can be wired into CI or a pre-deploy gate. `run_all.R` runs each
suite in its own R process and exits non-zero if any suite fails.

## Files

- `helper.R` — shared setup: finds the project root, sources `R/`, provides the
  `ok()` assertion helper and `report_and_exit()`. Sourced by the suites; not
  run directly.
- `run_all.R` — runs all `test_*.R` files and prints an overall summary.
- `test_pipeline.R` — import → type detection → coercion → descriptives (all
  types + grouped) → chart recommendation → build & render every chart type →
  Word `.docx` export. The "does it run / does it render" gate.
- `test_types.R` — `detect_type()` on crafted vectors (binary, count,
  continuous, identifier, ordinal, datetime, nominal, all-NA text).
- `test_import.R` — `.rds`/`.rda` round-trips, `.read_rda` first-data-frame
  selection and clean error when none is present, and haven-label stripping to
  factor / numeric.
- `test_descriptives.R` — numeric summaries vs base R (`mean`/`sd`/`quantile`),
  skewness / excess kurtosis / `wilson_ci` vs hand-computed constants and their
  edge cases, and missing-data accounting (incl. `cd4_count` = 37 NA).
- `test_charts.R` — `assign_chart_roles()` variable→x/y/group mapping, and all
  three branches of `.apply_palette` (manual, Viridis, and the empty-aesthetics
  path used by continuous-fill charts).
- `test_heatmaps.R` — confirms the contingency and correlation heatmaps render
  and emit neither the "discrete scale" error nor the "Scale for fill is already
  present" warning (the bug fixed in `R/fct_charts.R`).
- `test_assumptions.R` — Phase 2 assumption layer (`fct_assumptions.R`):
  `p_fmt`, and the normality / variance-homogeneity / expected-cell-count checks
  whose verdicts drive test routing, pinned on data with a known answer.
- `test_hypothesis.R` — Phase 2 engine (`fct_tests.R`), routing
  (`fct_recommend_hypothesis.R`) and result formatting (`mod_tests.R`): effect
  sizes vs hand-computed constants; each runner's p-value vs base R's own test
  on identical data; Tukey, Games-Howell, Dunn/Holm, and repeated-measures
  posthoc follow-ups; routing verdicts (primary vs alternative); the "both
  global tests always reported" invariant; dispatch and preview text.
- `test_descriptives_grouping.R` — drives `mod_descriptives_server` with
  `shiny::testServer`. Confirms grouped numeric stats are correct (guards) and
  specifies that grouped descriptives must be produced for binary / nominal /
  ordinal main variables too, added to the report, with missing-group rows
  accounted for (currently failing — the fix).
- `test_method_filtering.R` — drives `mod_charts_server` and `mod_tests_server`.
  Specifies that the chart-type dropdown is restricted to the type-appropriate
  set, and that the tests tab gates families by available types and roles by
  condition (2-level vs ≥3-level groups, `x ≠ y`, subject ID ≠ outcome), with
  clear errors instead of raw assertions, and verifies that a multi-group
  posthoc table is rendered and added to the report.
- `test_models.R` — Phase 3 model-engine reference checks for OLS, robust,
  quantile, logistic, Poisson, and negative-binomial regression, plus a Shiny
  wiring check for the independent Models tab.
- `test_advanced_models.R` — Phase 4 reference checks for GEE, LMM, and GLMM,
  plus Advanced Models tab wiring.
- `test_survival.R` — Phase 4 Kaplan-Meier and Cox reference checks against
  `{survival}`, validation guards, and survival-tab wiring.
- `test_structure.R` — confirms Advanced models are gated to longitudinal or
  time-series data and verifies the revised model-tab UI omits formula sections.

- `test_datacheck.R` — Tab 0 data-quality engine (`fct_dataqc.R`) and module
  (`mod_datacheck.R`). Every check is asserted both ways — it must fire on a
  crafted trigger and stay silent on clean data — across structural, missingness,
  value-content, numeric/date, and panel/ID checks; plus the `.txt` report text
  and the column-subset builder wiring via `shiny::testServer`.

- `test_design_coverage.R` — audits the classical test/design checklist (t-test,
  paired t, Mann-Whitney, chi-square, one-way/repeated/factorial ANOVA,
  mixed-model ANOVA, Kruskal-Wallis, Friedman, Wilcoxon and binomial test).
  Generates a dataset per design, routes it through `run_guided_test` as the
  Tests tab does, and matches reference base-R.

- `test_lasso.R` — LASSO regression on the Regression tab: coefficients matched
  against a direct `glmnet::cv.glmnet` fit, the deliberate absence of standard
  errors / p-values / CIs, variable selection (`Selected` column), odds/rate
  ratios for binary/count outcomes, the lambda rule (min vs 1-SE), reproducible
  CV folds with global RNG state restored, the two-predictor guard, and the
  Models-tab wiring via `shiny::testServer`.

- `test_ridge_pls.R` — ridge (glmnet `alpha = 0`) and PLS (`pls::plsr`)
  regression: coefficients matched to the direct package fit, ridge keeping all
  predictors (no `Selected` column) vs LASSO, PLS component selection by CV
  RMSEP, the absence of SE/p-values, reproducible CV with RNG restored, guards,
  and Models-tab wiring.
- `test_ancova_pointbiserial.R` — ANCOVA (group F/p vs `anova(reduced, full)`,
  partial eta-squared, homogeneity-of-slopes check, multi-covariate and 2-group
  cases, guards) and point-biserial correlation (r/p vs `cor.test` on 0/1 coding
  and the pooled two-sample t, rank-biserial alternative, opt-in gating, guards),
  each routed through `run_guided_test` and the Tests tab via `testServer`.

- `test_interpret_backend.R` — adversarial coverage for the embedded
  interpreter's newest behaviour: the redundancy fix (offline read-out does not
  echo the app's own interpretation line), the distinct "In short:" headline
  computed from the numbers, deterministic offline output, the pluggable-backend
  config (OpenCode "Big Pickle" turnkey path + generic `AI_INTERPRET_*`
  precedence), and graceful reasoned fallback when a backend is configured but
  unreachable (exercised on loopback only — no data leaves the machine).

- `test_descriptives_kind.R` — the descriptives shown must match the declared
  dataset kind (`descriptive_tables()`): cross-sectional gets the plain type
  table (no within/between); longitudinal gives a per-subject summary for a
  time-invariant variable and the panel decomposition (structure, by-time,
  within/between + ICC, change, missingness) for a time-varying one — never a
  pooled across-rows table; time series get a time-series summary. Includes the
  ICC vs manual variance-component check and module wiring via `testServer`.

- `test_method_gating.R` — the Charts / Tests / Regression tabs must offer only
  methods and variables appropriate to the declared dataset kind:
  `reserved_analysis_vars()` (survival follow-up/event/start/end + subject ID)
  and `analysis_kind_note()` guidance, plus module checks that the reserved
  columns are dropped from every variable pool and the kind-warning banner shows
  for survival / repeated / time-series data (and not for cross-sectional).

## Requirements

The hard dependencies from `install_dependencies.R`. The optional packages
`ggdist` and `GGally` are not required — the app's documented fallbacks are
exercised when they are absent. `test_import.R` skips its haven checks if
`haven` is not installed.
