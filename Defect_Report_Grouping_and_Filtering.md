# Defect Report — Grouped Descriptives & Type-Aware Method Filtering

**Project:** Sabali (R Shiny)
**Prepared for:** the developer AI implementing the fixes
**Date:** 23 July 2026
**Author:** QA pass (Christian Ochieng)

> **Current status:** The two defects documented below are resolved in the
> current codebase. The associated `[BUG]` checks are now regression tests and
> pass in the full suite; this document is retained as historical context.

---

## 0. TL;DR

Two issues were reported and both were **confirmed and reproduced**:

1. **Grouped descriptives only work for numeric variables.** For any binary /
   nominal / ordinal (and date) main variable the "By group" table is silently
   empty. The design doc requires grouped summaries for *all* analysable types.
2. **Method / variable filtering by type and condition is not wired into every
   tab.** The Descriptives tab filters correctly, but:
   - the **Chart Studio** offers *all 25 chart types* regardless of the
     selected variables' types (a single categorical variable can be sent to a
     scatter/Q-Q/correlation-heatmap);
   - the **Hypothesis-testing tab** always shows all three test families and
     does not gate role choices by condition — a >2-level factor can be picked
     for a two-group test (raw assertion error), and a variable can be
     correlated / cross-tabulated with itself.

Two test suites encode the complete target behaviour. They are wired into
`tests/run_all.R` and now pass on the resolved scenarios:

```bash
Rscript tests/test_descriptives_grouping.R
Rscript tests/test_method_filtering.R
```

`[GUARD]`-labelled tests already passed and must stay green. `[BUG]`-labelled
tests defined the required fixes and now pass as regression checks.

---

## 1. Issue 1 — Grouped descriptives

### 1.1 Symptom
Selecting a "Group by" variable produces a per-group table only when the main
variable is numeric. For a categorical/ordinal main variable, nothing appears
(the panel shows *"Select a group and a numeric variable."*).

### 1.2 Root cause
[R/mod_descriptives.R](R/mod_descriptives.R) — the `grouped()` reactive is
hard-limited to numeric variables and calls a numeric-only helper:

```r
# mod_descriptives.R:66-72
grouped <- shiny::reactive({
  shiny::req(input$var, input$group)
  if (input$group == "(none)") return(NULL)
  df <- data()
  if (!is_numeric_type(types()[[input$var]])) return(NULL)   # <-- silent dead-end
  describe_numeric_by_group(df[[input$var]], df[[input$group]])
})
```

[R/fct_descriptives.R](R/fct_descriptives.R:104) provides only
`describe_numeric_by_group()`. There is no grouped path for categorical,
ordinal or date variables. Design doc Section 3 states: *"if a grouping factor
is also selected, all of the above are produced per group side by side."*

Two secondary defects on the same feature:
- **Report export gap** — [mod_descriptives.R:80-90](R/mod_descriptives.R:80)
  only adds a grouped table to the Word report when `grouped()` is non-NULL, so
  categorical grouped tables can never reach the report.
- **Missing-group rows dropped silently** — `split(x, g)`
  ([fct_descriptives.R:105](R/fct_descriptives.R:105)) drops rows whose grouping
  value is `NA` with no count or note, violating the "warn loudly on missing
  data" principle (design doc §Guiding principles 3).

### 1.3 Fix surface (suggested)
- Add a type-aware dispatcher, e.g. `describe_by_group(x, g, type)`:
  - numeric/count → existing `describe_numeric_by_group()`;
  - binary/nominal/ordinal → per-group frequency/percentage breakdown
    (a cross-tab: group × level counts, or a tidy long table);
  - datetime → per-group n / earliest / latest / span.
- In `mod_descriptives.R`, drop the `is_numeric_type` guard and call the
  dispatcher for **any analysable** main variable.
- Account for `NA` grouping values (add a `(missing)` row **or** a note stating
  how many rows were dropped).
- Extend the `observeEvent(input$add)` path so a grouped categorical table is
  also added to the report.

### 1.4 Tests → `tests/test_descriptives_grouping.R`
| Scenario | Label | Status today |
|---|---|---|
| numeric grouped stats correct vs base R (n, mean, missing excluded) | GUARD | ✅ pass |
| single-obs group → sd NA, no crash | GUARD | ✅ pass |
| numeric var + factor group → table; group=(none) → NULL | GUARD | ✅ pass |
| **binary / nominal / ordinal main var + group → non-empty table** | BUG | ❌ fail |
| **categorical var + "Add to report" → grouped table added** | BUG | ❌ fail |
| **NA grouping category reported (row or note), not dropped silently** | BUG | ❌ fail |

---

## 2. Issue 2 — Type-/condition-aware method filtering

### 2.1 Chart Studio: the chart-type list is not filtered

**Root cause** — [R/mod_charts.R:99-107](R/mod_charts.R:99). The recommendation
engine returns the correct allowed set, but the dropdown re-appends *every other
chart type*:

```r
# mod_charts.R:100-106
shiny::observeEvent(reco(), {
  opts <- reco()$options
  all_types <- names(CHART_LABELS)
  ordered <- c(opts, setdiff(all_types, opts))   # <-- re-adds all 25 types
  shiny::updateSelectInput(session, "type",
    choices = stats::setNames(ordered, CHART_LABELS[ordered]),
    selected = reco()$default)
})
```

Reproduced: selecting the single categorical variable `county` still offers all
25 chart types (histogram, scatter, Q-Q, correlation heatmap, …). The
*default* selection is correct; only the *filtering* is missing.

**Fix surface** — offer only `reco()$options` (i.e. `ordered <- opts`). If you
want to honour "user can always override" (design principle 4), show the
remaining types in a clearly-separated, collapsed "Advanced / not recommended"
group rather than inline — but the default dropdown must contain only the
type-appropriate charts. `recommend_charts()` is already the single source of
truth; keep it that way.

### 2.2 Hypothesis-testing tab: families and roles not gated

**Root causes** — [R/mod_tests.R](R/mod_tests.R):
- **Families are static** ([mod_tests.R:31-35](R/mod_tests.R:31)) — all three
  are always shown even when the data cannot support them (e.g. correlation with
  fewer than two numeric variables).
- **Grouping variable not level-gated** ([mod_tests.R:87](R/mod_tests.R:87)) —
  both `two_independent` and `multi_independent` draw from the same `cat_vars()`
  with no level check. Picking a 5-level factor (`county`) for a two-group test
  reaches the engine and dies on a raw assertion:
  `run_two_independent()` → [fct_tests.R:95](R/fct_tests.R:95)
  `stopifnot(nlevels(d$g) == 2)` → surfaced to the user as
  *"nlevels(d$g) == 2 is not TRUE"*.
- **Subject-ID list includes everything** ([mod_tests.R:69](R/mod_tests.R:69),
  `id_vars <- reactive(names(types()))`) — the numeric outcome itself can be
  chosen as the subject ID.
- **No `x ≠ y` guard** — correlation ([mod_tests.R:100-101](R/mod_tests.R:100))
  and categorical association ([mod_tests.R:95-96](R/mod_tests.R:95)) let the
  same variable fill both roles, yielding a nonsensical r = 1 / perfect
  association with no warning.

**Fix surface (suggested helpers, matched by the tests):**
- `available_test_families(types)` → subset of
  `c("location","categorical","correlation")`:
  location needs ≥1 numeric; correlation needs ≥2 numeric; categorical needs ≥2
  categorical. Drive the `family` control from this.
- `valid_grouping_vars(types, df, design)`:
  `two_independent` → categorical vars with **exactly 2** non-missing levels;
  `multi_independent` → categorical vars with **≥3** levels. Drive the group
  dropdown from this.
- `valid_subject_vars(types, outcome)` → candidate subject IDs excluding the
  chosen outcome (and, ideally, preferring identifier-like columns).
- Make the second role's choices exclude the first (`y ≠ x`), **and** add a
  defensive `validate_spec(df, spec)` in `run_guided_test()`
  ([fct_recommend_hypothesis.R:35](R/fct_recommend_hypothesis.R:35)) that raises
  **human-readable** errors (replace the raw `stopifnot`s in
  [fct_tests.R:95](R/fct_tests.R:95) and [fct_tests.R:134](R/fct_tests.R:134)).

### 2.3 Tests → `tests/test_method_filtering.R`
| Scenario | Label | Status today |
|---|---|---|
| `recommend_charts()` option sets per type combo | GUARD | ✅ pass |
| test-tab outcome source = numeric only; group source = categorical only | GUARD | ✅ pass |
| valid specs (2-group sex, multi county, corr, categorical) run | GUARD | ✅ pass |
| **chart dropdown == recommended set for 1-cat / 1-num / 2-cat / num+cat** | BUG | ❌ fail |
| **no numeric-only charts for a categorical var (and vice-versa)** | BUG | ❌ fail |
| **`available_test_families()` gates families by available types** | BUG | ❌ fail |
| **`valid_grouping_vars()` gates group choices by level count** | BUG | ❌ fail |
| **>2-level group for a two-group test refused with a clear message** | BUG | ❌ fail |
| **correlation / association refuse `x == y`** | BUG | ❌ fail |
| **subject-ID choices exclude the chosen outcome** | BUG | ❌ fail |

---

## 3. How to run

```bash
Rscript tests/run_all.R                       # full suite (10 files)
Rscript tests/test_descriptives_grouping.R    # Issue 1
Rscript tests/test_method_filtering.R         # Issue 2
```

The 8 pre-existing suites still pass unchanged. "Done" = both new suites go
fully green **without** editing the `[BUG]` assertions (adjusting a helper name
is fine if you keep the same contract, but update the test in the same commit
and say so).

---

## 4. Open design decisions — RESOLVED

1. **Categorical grouped layout** → **tidy long table** (one row per
   group × level, with within-group count and column %). Implemented in
   `describe_categorical_by_group()`.
2. **"Advanced" charts** → **kept in a collapsed group** so power users can
   override. The Chart Studio dropdown offers only the recommended, type-
   appropriate charts by default; an *Advanced → "Show all chart types"*
   checkbox (collapsed `<details>`) re-adds every type on demand.
3. **`patient_id` mis-typed as continuous** → **superseded**: now auto-detected
   as an identifier. `detect_type()` gained name-aware ID detection — an
   id-like column name (`id`, `patient_id`, `patientID`, `MRN`, …) plus
   plausible id values (whole numbers or text, >2 distinct, mostly non-missing)
   is classified as `identifier`, even when the ID repeats across rows in a
   longitudinal dataset. The result stays fully overridable on the confirm-types
   screen, and calling `detect_type()` without a name is unchanged.

## 5. Resolution — what was changed

**Issue 1 — grouped descriptives (all types)**
- `fct_descriptives.R`: added `describe_by_group(x, g, type)` dispatcher plus
  `describe_categorical_by_group()` (tidy long, explicit `(missing)` group) and
  `describe_datetime_by_group()`; `describe_numeric_by_group()` now carries a
  missing-group `note` attribute instead of dropping NA-group rows silently.
- `mod_descriptives.R`: `grouped()` now runs for **any analysable** main
  variable (numeric, binary, nominal, ordinal, datetime); the missing-group note
  is surfaced in the info banner and the report caption; the categorical grouped
  table now reaches the Word report via the existing add path.

**Issue 2a — Chart Studio type filtering**
- `mod_charts.R`: the type dropdown is populated from `recommend_charts()` only;
  an *Advanced → Show all chart types* checkbox restores the full list.

**Issue 2b — hypothesis-test family / role gating**
- `fct_recommend_hypothesis.R`: added `available_test_families()`,
  `valid_grouping_vars()`, `valid_subject_vars()` and `validate_spec()`
  (human-readable errors), called at the top of `run_guided_test()`.
- `fct_tests.R`: the raw `stopifnot()`s in `run_two_independent()` and
  `run_two_paired()` are replaced with clear `stop()` messages.
- `mod_tests.R`: families are gated by `available_test_families()`; the grouping
  dropdown by `valid_grouping_vars()` (2-level vs ≥3-level per design); the
  subject-ID list by `valid_subject_vars()` (excludes the chosen outcome); and
  the second role (y) excludes the first (x) in the two-variable families.

**Supporting fix (root cause of a role-pollution symptom)**
- `fct_types.R`: `detect_type()` now recognises ISO-8601 date strings imported
  as text (e.g. `visit_date`) as `datetime`, so a date column is no longer
  mistaken for a 431-level categorical in grouping / role choices. This is what
  makes `visit_date` drop out of `valid_grouping_vars()`.

**Verification note.** Both new suites were verified by close tracing and by
checking every data-dependent expectation against `data/lvct_cohort_repeated.csv`
(750 rows; 2-level factors = `sex`, `on_art`; ≥3-level = `visit`, `county`,
`regimen`; `visit_date` all-ISO → datetime). The R runtime was not available in
the authoring sandbox, so please run `Rscript tests/run_all.R` to confirm all
10 suites go green.
