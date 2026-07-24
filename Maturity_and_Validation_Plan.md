# Sabali — Maturity Checklist & Statistical Validation Test Plan

_Prepared for Lvcthealth. A working document to move the guided-statistics tool
from "advanced prototype" to "production-ready". Two parts: (1) a maturity
checklist across engineering and statistical dimensions, and (2) a concrete
statistical-validation test plan that checks the app's numbers against trusted
references._

---

## Part 1 — Maturity checklist

Tick items as they're met. Priority: **P0** = blocks production use, **P1** =
needed for a confident release, **P2** = hardening / nice-to-have.

### A. Statistical correctness (the core — see Part 2)

- [ ] **P0** Every analysis runner is validated against a trusted reference (base R, an authoritative package, or a hand-worked example) — see Part 2.
- [ ] **P0** Reported p-values, test statistics, degrees of freedom, effect sizes and confidence intervals all match the reference to a defined tolerance.
- [ ] **P0** Reimplemented methods (Cochran's Q, Scheirer–Ray–Hare, Quade ANCOVA, aligned-rank transform, any bespoke maths) are checked against an independent implementation, not just "it runs".
- [ ] **P1** Reference-level (base level) selection provably changes regression contrasts as intended.
- [ ] **P1** Assumption checks (normality, dispersion, homogeneity of slopes, multicollinearity) trigger correctly on datasets engineered to violate them.
- [ ] **P1** Missing-data handling is explicit and documented per analysis (complete-case counts shown and correct).

### B. Test coverage & continuous integration

- [ ] **P0** Automated tests exercise **every tab** and **every data layout** (cross-sectional, repeated, time-series, survival). _The `ns` bug on the Regression tab proved UI paths aren't all exercised._
- [ ] **P0** A CI pipeline runs the full `tests/` suite on every push and blocks merges on failure.
- [ ] **P1** UI smoke tests that actually render each module's `renderUI` outputs (e.g. `shinytest2`), so undefined-symbol errors like `ns` are caught automatically.
- [ ] **P1** Coverage measured (`covr`) with a target (e.g. ≥ 80% of `R/`), and gaps reviewed.
- [ ] **P2** Golden-file tests for the Word report output (structure, not byte-identical).

### C. Robustness & error handling

- [ ] **P0** Every import format in `read_any_format` is tested with a real sample file (CSV, RDA, RDS, Excel, SPSS, Stata, SAS) including a labelled file.
- [ ] **P1** Graceful, human-readable failure for: empty file, single-column file, all-missing column, single-level factor, constant column, non-UTF-8 text, huge file.
- [ ] **P1** Every `stop()`/`validate()` message is analyst-friendly (no raw R errors surfaced to the user).
- [ ] **P2** Fuzz/property testing on the type detector (`detect_type`) with randomised columns.

### D. Code structure & packaging

- [ ] **P1** Convert from sourced scripts to a proper R package (or `golem`/`rhino` Shiny structure) with `DESCRIPTION`, namespaces and `roxygen2` docs.
- [ ] **P1** Dependencies pinned (`renv`) so builds are reproducible.
- [ ] **P2** Remove the latent `ns` class of bug repo-wide: standardise on `ns <- session$ns` at the top of every `moduleServer` (audit all modules).
- [ ] **P2** Lint/style gate (`lintr`, `styler`) in CI.

### E. AI features & governance

- [ ] **P0** Written data-governance sign-off: confirm only aggregate metadata / result tables ever leave the machine (planner and interpreter), documented and reviewed.
- [ ] **P0** Approved-endpoint policy: production only points at an endpoint cleared for the data class (Big Pickle is **not** zero-retention — synthetic/de-identified only).
- [ ] **P1** AI-off path fully tested: with no endpoint, every AI control is inactive (planner + interpreter on all tabs) and nothing errors. _(Tests added; run them.)_
- [ ] **P1** Planner catalogue kept in sync with the real runners (a test asserts every catalogue id maps to an implemented analysis).
- [ ] **P1** Malformed / adversarial AI replies handled (invalid ids → clear message, no crash, no silent wrong gate). _(Partially covered by `validate_plan`.)_
- [ ] **P2** Log/audit of what metadata was sent, for governance review.

### F. Security & privacy

- [ ] **P0** No patient data written to disk in temp/log files; uploads cleared on session end.
- [ ] **P1** `.Renviron` / API keys never logged or echoed; secrets management documented.
- [ ] **P1** If deployed multi-user (Shiny Server / Posit Connect), sessions are isolated (they appear to be — verify) and auth is enforced.
- [ ] **P2** Dependency vulnerability scan.

### G. UX & guidance quality

- [ ] **P1** A statistician reviews the guided recommendations end-to-end for pedagogical correctness (does the tool steer non-experts to defensible choices?).
- [ ] **P1** Every recommendation/plan is clearly framed as "suggestion — confirm before running" (largely done).
- [ ] **P2** Onboarding / worked example dataset shipped so new users can try the full workflow.

### H. Documentation & release

- [ ] **P1** User guide (task-oriented: "how do I compare two groups?") separate from the technical README.
- [ ] **P1** Methods appendix: for each analysis, what is computed, assumptions, and references — important for defensibility in public-health reporting.
- [ ] **P1** Semantic versioning + changelog; a tagged release.
- [ ] **P2** Known-limitations page kept current.

### I. Performance & scale

- [ ] **P2** Benchmark on a realistically large dataset (rows × columns typical of your cohorts); confirm the 200 MB upload cap and reactivity hold up.
- [ ] **P2** Long-running fits (GLMM, GEE) show progress and can't freeze the UI.

---

## Part 2 — Statistical validation test plan

### Objective

Prove that the numbers Sabali reports are correct, not merely that the code runs
without error. This is the single most important gate for a statistics tool used
in public-health decision-making.

### Strategy

Use **reference-based ("golden") tests**: run each analysis on a fixed public
dataset, and assert the app's reported quantities equal an independent
reference to a stated tolerance.

Two tiers, because the runners fall into two groups:

- **Tier 1 — wrappers.** Runners that call a base-R or well-established package
  function under the hood (t-tests, ANOVA, `chisq.test`, `cor.test`, `glm`,
  `MASS::glm.nb`, `survival::coxph`, `geepack::geeglm`, `lme4::glmer`,
  `nlme`/`lme4` LMM). Assert **near-exact** agreement with that same reference
  computed directly in the test (tolerance ~1e-6 on statistics/p-values), because
  any difference means a wiring/labelling error in the app.

- **Tier 2 — reimplementations.** Bespoke maths not delegated to a standard
  function: the "dependency-free" **Cochran's Q**, **Scheirer–Ray–Hare**,
  **Quade rank ANCOVA**, **aligned-rank-transform ANOVA**, effect-size
  formulae, and any manual CI construction. Validate against an **independent**
  authoritative source — a dedicated package (e.g. `rstatix`, `coin`,
  `PMCMRplus`, `ARTool`, `effectsize`, `DescTools::CochranQTest`) **or** a
  hand-computed worked example with the expected value written into the test.
  Tolerance looser (~1e-3) to allow legitimate algorithmic differences, but
  document why any difference exists.

### Tolerances (define once, reuse)

```
TOL_STAT   <- 1e-6   # Tier 1: statistic, df — must match the reference call
TOL_P      <- 1e-6   # Tier 1 p-values
TOL_EFFECT <- 1e-4   # effect sizes
TOL_CI     <- 1e-4   # confidence-interval bounds
TOL_T2     <- 1e-3   # Tier 2 reimplementations vs independent source
```

Helper to pull a value out of the app's result tables (which are `Field/Value`
or `Term/Estimate/…/p-value` shaped) and compare:

```r
# extract a Field/Value cell, coercing to numeric
fv <- function(tab, field) suppressWarnings(as.numeric(
        as.character(tab[[2]][match(field, as.character(tab[[1]]))])))
expect_close <- function(actual, expected, tol, label) {
  ok(label, stopifnot(is.finite(actual), abs(actual - expected) <= tol))
}
```

### Reference datasets (bundled with R — deterministic, citable)

| Dataset | Use for |
| --- | --- |
| `sleep` | paired & two-sample t-tests, Wilcoxon |
| `ToothGrowth` | two/one-way ANOVA, Kruskal–Wallis, factorial (supp × dose) |
| `mtcars` | correlation (Pearson/Spearman), OLS, quantile/robust regression, point-biserial (`am` vs `mpg`) |
| `iris` | multi-group ANOVA, MANOVA-style, correlation matrix |
| `infert` | logistic regression (`case ~ …`), chi-square, Fisher |
| `warpbreaks` | Poisson / negative-binomial (counts), factorial |
| `esoph` | ordered factors, proportions |
| `survival::lung`, `survival::veteran` | Cox PH, Kaplan–Meier, log-rank |
| `lme4::sleepstudy` | LMM, repeated measures |
| `geepack::dietox` or `respiratory` | GEE (clustered) |
| Small hand-built 2×k paired binary frame | McNemar, Cochran's Q |

Prefer built-in datasets so the tests are self-contained and reviewers can
reproduce them.

### Coverage matrix (app runner → reference → what to assert)

**Location / group comparison** (`fct_tests.R`)

- `run_two_independent` → `stats::t.test(var.equal = FALSE)` on `sleep`/`ToothGrowth`: assert `t`, `df`, `p-value`, and the reported effect size (Hedges' g) vs `effectsize::hedges_g`.
- `run_two_paired` → `t.test(paired = TRUE)` and `wilcox.test(paired = TRUE)`: assert statistic + p for both primary and alternative.
- `run_one_sample` → `t.test(mu = …)` and `wilcox.test(mu = …)`.
- `run_anova_oneway` → `oneway.test` (Welch) and `aov` + `summary`; alternative `kruskal.test`. Assert F, df1/df2, p; Kruskal χ², df, p.
- `run_repeated` → compare to an `ez`/`afex` repeated-measures ANOVA and `friedman.test`.
- `run_factorial_anova` → `aov(y ~ a*b)`: assert each main effect and interaction F + p. Tier-2 alt (`run_scheirer_ray_hare`) → `rcompanion::scheirerRayHare` or hand-worked H values.
- `run_mixed_anova` → `afex::aov_ez` or `lme4` LMM: assert fixed-effect F/p within tolerance (note method-dependent df).
- `run_ancova` → `car::Anova(lm(y ~ cov + group), type = "…")`; check homogeneity-of-slopes test. Tier-2 `run_quade_ancova` → `PMCMRplus`/hand example.

**Categorical** (`run_categorical`, `run_proportion_test`, `run_cochran_q`, `run_repeated_binary`)

- `run_categorical` (unpaired) → `chisq.test` and `fisher.test` on `infert`/`esoph` cross-tabs: assert χ², df, p and, for Fisher, the exact p and odds ratio.
- `run_categorical` (paired 2×2) → `mcnemar.test`: assert statistic + p; validate the reported success-proportion difference by hand.
- `run_proportion_test` → `prop.test`/`binom.test`: assert p and CI.
- `run_cochran_q` (**Tier 2, reimplemented**) → `DescTools::CochranQTest` **and** a hand-worked 3-condition example: assert Q, df, p. This one deserves extra scrutiny because it's dependency-free.
- `run_repeated_binary` / McNemar post-hoc → `rstatix`/hand example, including Holm adjustment of pairwise p-values.

**Correlation** (`run_correlation`, `run_repeated_correlation`, `run_point_biserial`)

- `run_correlation` → `cor.test(method = "pearson"/"spearman")` on `mtcars`: assert r/ρ, statistic, df, p, and CI.
- `run_point_biserial` → `cor.test` of binary-as-0/1 vs numeric, or `ltm::biserial.cor`: assert r and p.
- `run_repeated_correlation` (**Tier 2**) → `rmcorr::rmcorr`: assert r and p on `sleepstudy`-like data.

**Regression — independent** (`fct_models.R`)

- `ols` → `lm`: assert every coefficient estimate, SE, t, p; R², adjusted R², AIC, n used.
- `logistic` → `glm(family = binomial)` on `infert`: assert coefficients, SEs, p, **odds ratios** (= `exp(coef)`) and their CIs; pseudo-R²; n.
- `poisson` → `glm(family = poisson)` on `warpbreaks`; check reported Pearson dispersion by hand.
- `negative_binomial` → `MASS::glm.nb`: coefficients, θ, AIC.
- `robust` → `MASS::rlm`; `quantile` → `quantreg::rq(tau)`: coefficients at the chosen τ.
- `lasso`/`ridge` → `glmnet` with a **fixed seed** and stated λ rule ("min"/"1se"): assert which terms are retained and that results are reproducible run-to-run (determinism test).
- `pls` → `pls::plsr`: number of components and fitted metrics.

**Regression — advanced** (`fct_models.R`, `mod_phase4`)

- `gee_*` → `geepack::geeglm` with matching `corstr`: coefficients, robust SEs, working correlation.
- `lmm` → `lme4::lmer`/`nlme::lme`: fixed effects, variance components.
- `glmm_*` → `lme4::glmer`: fixed effects (note optimiser/convergence).
- Cox (`survival::coxph`) → assert hazard ratios (= `exp(coef)`), SEs, p, concordance; **Kaplan–Meier** survival estimates at fixed time points and the **log-rank** χ² vs `survival::survdiff`.

**Type detection & data handling** (`fct_types.R`)

- `detect_type` / `detect_all_types` on engineered columns: binary, count, continuous, nominal, ordinal, datetime (incl. ISO-8601 strings), identifier (by name and by values), all-missing. Assert exact type labels.
- `coerce_to_type` round-trips and `relevel` sets the intended base level.
- Labelled import: a fixture `.sav`/`.dta` (or a `haven`-attributed frame) flows through so variable/value labels reach the planner metadata. _(Unit test for the metadata builder already added; add an end-to-end import test.)_

### Assumption-check validation

For each assumption reported, build a dataset that clearly satisfies it and one
that clearly violates it, and assert the check flips:

- Normality of residuals → Shapiro/QQ on a normal vs a heavily skewed outcome.
- Poisson dispersion → equidispersed vs over-dispersed counts (dispersion > ~1.5 flagged).
- Homogeneity of slopes (ANCOVA) → parallel vs crossing group slopes.
- Multicollinearity → independent predictors vs two near-collinear predictors (high VIF flagged).

### Edge cases (must not crash; must warn sensibly)

Separation in logistic regression; singular/`rank-deficient` model matrix; a
group with n = 1; empty contingency cells; ordered-factor predictors; a factor
with a single observed level; near-zero-variance predictor; > 50% missing in the
outcome.

### Determinism

Any method with randomness (cross-validated `glmnet`, bootstrap CIs, GLMM
optimisers) must set a seed and produce identical output across two runs — add
an explicit "same input → identical output" test for each.

### How to implement in your harness

Add files under `tests/` following the existing pattern (they auto-source `R/`
via `tests/helper.R` and use `ok(...)` / `report_and_exit()`):

```
tests/test_validate_location.R      # t-tests, ANOVA, Kruskal, paired/repeated
tests/test_validate_categorical.R   # chi-square, Fisher, McNemar, Cochran's Q, proportions
tests/test_validate_correlation.R   # Pearson, Spearman, point-biserial, rmcorr
tests/test_validate_regression.R    # OLS, GLMs, negbin, robust, quantile, penalised, PLS
tests/test_validate_advanced.R      # GEE, LMM, GLMM, Cox, KM, log-rank
tests/test_validate_assumptions.R   # violation-detection flips
tests/test_validate_types.R         # detect_type / coerce / labels
```

Each test: fix a dataset → run the app's runner → compute the reference inline →
`expect_close(fv(result, "p-value"), ref$p.value, TOL_P, "…")`. Any reference
package used only in tests goes in `Suggests`, and the test skips cleanly if the
package is absent (so CI without it still runs Tier 1).

### Suggested sequence

1. Tier 1 wrappers first (fast wins, catches wiring/labelling bugs).
2. Tier 2 reimplementations next (highest risk — Cochran's Q, Scheirer–Ray–Hare, Quade, ART, bespoke effect sizes/CIs).
3. Assumption-check flips.
4. Edge cases + determinism.
5. Wire all of it into CI (Part 1, section B) so it can never silently regress.

---

_Definition of done for statistical maturity: every runner has at least one
Tier-1 or Tier-2 validation test passing in CI, every reimplemented method is
checked against an independent source, and the assumption checks demonstrably
flip on violation. At that point the tool's numbers can be trusted for
reporting._
