# Test Coverage Audit — Classical Tests & Research Designs

**Project:** Sabali (R Shiny)
**Date:** 23 July 2026
**Question answered:** Are all the requested tests included, and correctly wired
for every research design (verified on generated datasets)?

Audited against the guided engine (`R/fct_tests.R`), its router
(`R/fct_recommend_hypothesis.R:run_guided_test`) and the Tests-tab wiring
(`R/mod_tests.R`). Each design is checked by **generating a dataset with a known
structure and routing it exactly as the app does**, then comparing to reference
base-R. Suite: **`tests/test_design_coverage.R`** — all coverage checks pass.

---

## Verdict

**14 of the 14 original checklist items remain present and correctly wired; Cochran's Q is now an additional repeated-binary design.**

| # | Requested test | Status | Where / route |
|---|---|---|---|
| 1 | t-Test | ✅ present & wired | `location` → `one_sample` **and** `two_independent` (Welch) |
| 2 | Paired t-Test | ✅ present & wired | `location` → `paired_repeated` → `two_paired` |
| 3 | Mann-Whitney U | ✅ present & wired | alternative of `two_independent` |
| 4 | Chi-Square | ✅ present & wired | `categorical` (Fisher / McNemar alternatives) |
| 5 | ANOVA (generic) | ✅ present & wired | maps to one-way (`multi_independent`) |
| 6 | One-Way ANOVA | ✅ present & wired | `location` → `multi_independent` (Welch if variances differ) |
| 7 | **Two-Way ANOVA** | ✅ present & wired | `location` → `factorial` (main effects + interaction) |
| 8 | **Three-Way ANOVA** | ✅ present & wired | `location` → `factorial` (main effects + interactions) |
| 9 | Repeated-Measures ANOVA | ✅ present & wired | `location` → `paired_repeated` → `repeated` |
| 10 | **Mixed-model ANOVA** | ✅ present & wired | `location` → `mixed` (between × within, subject random intercept) |
| 11 | Kruskal-Wallis | ✅ present & wired | alternative of one-way ANOVA |
| 12 | Friedman | ✅ present & wired | alternative of repeated measures |
| 13 | **Binomial test** | ✅ present & wired | `proportion` family (exact binomial + normal-approximation alternative) |
| 14 | Wilcoxon | ✅ present & wired | signed-rank (one-sample & paired) + rank-sum (= Mann-Whitney) |
| — | **Cochran's Q** | ✅ present & wired | `cochran_q` family when subject ID + time/condition are active; complete matched binary blocks |

Every "present & wired" item was verified end-to-end: the router produces the
right primary **and** alternative test, and the p-value/statistic matches
`t.test` / `wilcox.test` / `aov` / `kruskal.test` / `friedman.test` /
`chisq.test` / `binom.test` on generated data. Factorial results retain a term
table for main effects and interactions; mixed-model results retain the
fixed-effect term table.

---

## The four wired additions (checklist items 7, 8, 10, 13)

The guided router (`run_guided_test`) now has explicit `factorial`, `mixed`, and
`proportion` branches in addition to the existing location, categorical, and
correlation routes. The Tests tab exposes the corresponding role controls only
when the selected data can support them.

### 7 & 8 — Two-Way / Three-Way ANOVA (factorial designs)
- **Need:** an outcome crossed with 2–3 categorical factors; report main effects
  **and** interactions.
- **Reference to match:** `summary(aov(y ~ A * B))` (and `A * B * C`).
- **Implemented:** `run_factorial_anova(df, outcome, factors)` returns a terms
  table (each factor + each interaction: df, F, p, partial η²), routes through
  `design = "factorial"`, and reports Scheirer–Ray–Hare plus ART alternatives
  for two-way designs (ART remains the alternative for three-way designs). The
  Tests tab exposes a two-/three-factor picker and validates factor roles.

### ANCOVA — Quade rank ANCOVA
- **Implemented:** ANCOVA now reports the parametric covariate-adjusted group
  F-test and a Quade rank ANCOVA alternative. Quade ranks the response and
  covariates, removes the ranked covariate effects, and tests the group effect
  on the residuals.

### 10 — Mixed-model / split-plot ANOVA (between × within)
- **Need:** one between-subjects factor + one within-subjects (repeated) factor
  on the same subjects.
- **Reference to match:** `aov(y ~ between*within + Error(subject/within))`, or an
  `lmer` equivalent (Phase 4 already fits LMMs, so the engine exists — it is just
  not offered as an ANOVA design in the Tests tab).
- **Implemented:** `design = "mixed"` takes `group` (between) + `within` +
  `subject`, fits a Phase 4-compatible `lme4` random-intercept LMM, reports
  fixed-effect terms and the between-by-within interaction, and includes a
  rank-transformed sensitivity analysis.

### 13 — Binomial test (one proportion vs a reference)
- **Need:** a single binary outcome tested against a reference proportion.
- **Reference to match:** `binom.test(successes, n, p)`; normal-approx
  `prop.test` as the large-sample alternative.
- **Implemented:** `family = "proportion"` takes a binary `outcome`, success
  level, and `ref_prop`; it reports `binom.test` as primary and `prop.test`
  without continuity correction as the large-sample alternative. The family is
  offered when at least one binary candidate outcome exists.

---

## Tests locking the work (`tests/test_design_coverage.R`)

- **Existing guards** — the original location, categorical, and correlation
  designs continue to route correctly and match reference R.
- **New wired checks** — Two-Way ANOVA, Three-Way ANOVA, Mixed-model ANOVA and
  the Binomial test each generate the correct design and assert they route
  through `run_guided_test` and match reference R.

```bash
Rscript tests/test_design_coverage.R   # all coverage checks pass
Rscript tests/run_all.R                # full suite, including this coverage audit
```

"Done" = the four new design checks pass with no regression in the existing
guards. The observable contract remains the right test plus a
reference-matching p-value.

---

## Note on terminology

- **"ANOVA" vs "One-Way ANOVA"** — treated as the same capability (one-way);
  both map to the `multi_independent` route.
- **"Wilcoxon"** — covered in both senses: the signed-rank test (one-sample and
  paired designs) and the rank-sum test (surfaced as Mann-Whitney U for two
  independent groups).
- **"Mixed-model ANOVA"** — distinct from the Phase 4 LMM/GLMM tab. The Tests
  tab now provides the guided between×within design using the same `lme4`
  random-intercept machinery.
