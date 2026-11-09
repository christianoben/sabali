# Changelog

## Unreleased

- **Ten methods a general public-health analyst needs, none of which existed.**
  An audit against routine epidemiological practice - not against any one study -
  found ten missing methods. All are now built, each validated against an
  independent implementation or against hand-computed arithmetic.

  *Diagnostic and screening accuracy* (`R/fct_diagnostic.R`): sensitivity,
  specificity, predictive values, likelihood ratios, Youden's J and ROC/AUC. AUC
  matches `pROC` and the Wilson intervals match `DescTools` to six decimals.
  Wilson rather than Wald throughout, because diagnostic work sits at the
  boundaries where a Wald interval on 20/20 is [1, 1] - zero width, claiming
  certainty from twenty observations. Predictive values are recomputed across a
  range of prevalences, because PPV and NPV are not properties of a test: at
  sensitivity 0.80 and specificity 0.90 the PPV is 0.727 in a 25%-prevalence
  validation sample and 0.008 in a 0.1%-prevalence screening programme.

  *Effect measures and trend* (`R/fct_epi.R`): risk difference (Newcombe
  interval), risk ratio (Katz), odds ratio, number needed to treat, and the
  Cochran-Armitage trend test for an ordered exposure. The app reports odds
  ratios from every logistic model, so it now also says when an odds ratio must
  not be read as a risk ratio - at 50% baseline risk an OR of 2.33 is a risk
  ratio of 1.40, and the note says so in those words. NNT is withheld when the
  risk-difference interval spans zero, because the NNT interval is then two
  disjoint ranges and a single range would be wrong.

  *Fleiss' kappa* for three or more raters, with a per-category breakdown showing
  which category a case definition is being applied inconsistently on.

  *Rates and standardisation* (`R/fct_rates.R`): incidence rates over
  person-time, rate ratios, direct standardisation and the SMR, all with exact
  Poisson intervals (checked against `poisson.test`) rather than normal
  approximations that put a lower limit below zero on small numerators.

  *Multiple imputation* (`R/fct_impute.R`): mice with Rubin's-rules pooling,
  offered on the Regression tab only when complete-case analysis would actually
  cost something, and refusing variables more than 60% missing rather than
  inventing most of their values.

  *Replicate-weight survey designs*: BRR, Fay, JK1, JKn, bootstrap and
  successive-difference, resolved by name pattern. NHANES and several DHS
  recodes ship these instead of PSU/stratum identifiers.

- **Fleiss' kappa intervals were miscalibrated in every implementation checked.**
  The classic Fleiss (1971) standard error is derived under kappa = 0, so it is
  right for the significance test and wrong for a confidence interval. Simulated
  against the actual sampling distribution (40 subjects, 4 raters, 3 categories),
  it comes out at 0.70 times the empirical SD of kappa-hat, and the variant
  `DescTools` uses at 0.67 - intervals from either are about a third too narrow.
  The interval is now bootstrapped over subjects (1.01 times the empirical SD,
  93.5% coverage of a nominal 95% interval) and the analytic SE is used only for
  the p-value. The point estimate still matches `DescTools` exactly.

- **Fixed a silent data leak in the imputation pooling.** `with(imps,
  lm(formula))` looks like the mice idiom but passes no `data =`, so a
  dynamically built formula resolves its variables through the formula's
  environment - the caller's frame, or the global environment - and fits whatever
  columns of that name it finds there. On a test fixture it silently fitted the
  COMPLETE data instead of the imputed data, returning an estimate that matched
  the full-data fit to four decimal places with a fraction of missing information
  of 0.002 on 27% missingness. Each imputation is now fitted explicitly with
  `data =`, and the formula's environment is neutralised.

- **Fixed a partial-matching bug, and removed the class of it.** R's `$` does
  PARTIAL matching on lists: when the exact name is absent it silently returns a
  field whose name merely starts with it. `spec$per` returned the value of
  `spec$person_time` - the string "pt" - so an incidence rate was computed as
  "pt" * events. `spec$reference` would have picked up `reference_rates` the same
  way, and `spec$m` in the imputation module would have picked up `method`. All
  spec and design-object access in the new modules now uses exact `[[`
  (136 call sites). An audit of the rest of `R/` lists further candidates; they
  are left alone because in each the exact field does exist, so nothing is
  currently wrong, and churning working code to satisfy a lint is not a fix.

- **Guarded against redefining an existing function.** `R/` is sourced
  alphabetically into one environment, so a second definition of an existing name
  silently replaces the first everywhere. `fct_diagnostic.R` redefined
  `wilson_ci()` with a different return shape and quietly broke every caller in
  `fct_descriptives.R`; it was caught only because that module has tests.
  `tests/test_validate_static.R` now scans all top-level definitions across `R/`
  (457 functions in 41 files) and fails on any duplicate.

- **Generalisation audit, and two-stage sampling designs.** Sabali is for any
  public-health analyst, so the features built while working against one
  institution's studies were audited for skew. Executable code, `app.R` and the
  shipped documentation (`USER_GUIDE.md`, `METHODS_APPENDIX.md`) contain no study,
  site, institution or study-variable names, and no absolute paths; every test
  that touches real study data is opt-in behind an environment variable and skips
  otherwise. The module headers that explained *why* a feature exists have been
  rewritten to lead with the general epidemiological problem - two clinicians
  reading the same slides, a rapid test against a reference assay, an
  intervention acting through adherence, DHS/MICS and national surveys - with the
  measured evidence kept but the study names removed.

  One genuine skew was found and fixed: the complex-sample design accepted only a
  SINGLE sampling stage, because the first design it was built against was
  single-stage. Two-stage designs - villages then households, households then
  people - are the norm in DHS, MICS and most national household surveys, and the
  Data tab now accepts a second-stage unit and its own finite population
  correction (Stata's `svyset psu, ... || ssu, ...`). Verified against
  `survey::svydesign` called directly with both stages.

  The audit also named two limits it did not close at the time - replicate-weight
  survey designs, and Fleiss' kappa for three or more raters. Both were built
  immediately afterwards; see the entry above.

- **Path analysis and mediation for observed variables.** A regression cannot
  answer "does X act THROUGH M" - does an intervention work via adherence, does
  deprivation act via access to care, does a water source act via hygiene
  behaviour. Adjusting for a mediator in one regression does not decompose that
  effect; it removes it. Analysts reach for Stata's `gsem ... nocapslatent` or
  lavaan for this, and the app had no equivalent. The Advanced models
  tab now offers **Path analysis / mediation**: pick the outcome, the mediators in
  causal order, the exposures, and optional covariates, and the app builds the
  lavaan model, fits it, and reports every direct path plus an indirect effect per
  route and a total effect per exposure, each with a confidence interval. A
  mediator chain is supported, so exposure to healthcare-seeking to
  antibiotic-days to colonisation is expressible - the ordering is taken from the
  order the analyst lists them, because it is a causal claim rather than something
  estimable from the data.

  Scope is deliberately bounded: observed variables only, no latent factors, and
  no free arrow-drawing - a path model an analyst can draw arbitrarily is one they
  can silently mis-specify. Estimation is lavaan's; what is ours is the syntax
  construction, the effect decomposition, the design mapping and the guards.
  `lavaan` is added to `install_dependencies.R` as an optional package.

  Two honesty features matter more than the arithmetic. First, a **saturated**
  model (every arrow drawn, 0 degrees of freedom) has CFI 1, TLI 1 and RMSEA 0 by
  construction; those are now called out as carrying no information rather than
  reported as though the model had been tested. Second, lavaan cannot combine its
  threshold (probit) estimator with sampling weights or cluster-robust standard
  errors. Rather than silently dropping one, the probit model is kept - it is the
  right estimand - and the note says exactly what could not be applied and what
  ticking "treat binary variables as continuous" would buy and cost (the estimates
  become risk differences).

  Verified on the real extract: the community model fits in 0.4 s over 1,704
  individuals, giving 18 indirect routes of which 7 reach p < 0.05, with
  colonisation on antibiotic-days at -0.078 and on healthcare-seeking at +0.075
  (p = 0.0002). Covered by `tests/test_path_analysis.R`, which also pins that the
  module's confounding caveat is real: dropping a mediator-outcome confounder from
  the same data inflates the indirect effect and manufactures a direct one.

- **Fixed a pre-existing flaky test.** `tests/test_validate_assumptions.R` drew
  its skewed sample with `rexp(40, 1)` two lines BEFORE its `set.seed()`, so the
  sample came from whatever RNG state the process started in. Shapiro-Wilk fails
  to flag an n = 40 exponential about 2.7% of the time (54 of 2,000 draws), so the
  suite failed at random roughly once in forty runs - which is what it did during
  this work. Seeded before the draw.

- **ODK / XLSForm form definitions are now read too, alongside REDCap
  dictionaries.** Same problem, different platform: an ODK export codes its
  select_one answers as short names, which no shape-based rule tells from a
  count, and its dates arrive as text. The Data tab's metadata upload now accepts
  either a REDCap "Data Dictionary (codebook)" CSV or an ODK / Kobo XLSForm
  workbook, detected from the file rather than declared by the analyst. XLSForm
  question types map to measurement types (`integer` to count, `decimal` and
  `range` to continuous, `date`/`datetime`/`start`/`end`/`today` to datetime with
  ODK's ISO order, `select_one` to nominal or binary by choice count,
  `acknowledge` to binary, `deviceid`/`username`/`simserial` to identifier), while
  `text`, `calculate`, `time`, geo and media types deliberately make no claim.
  Group and repeat nesting is tracked, so a bare field name matches the
  path-prefixed column an export actually writes (`household/person/ward`) -
  and only when exactly one field claims it, because two questions of the same
  name in different groups are genuinely ambiguous. Translated label columns
  (`label::English`) are used when there is no plain `label`.

  Both select_multiple export conventions are handled. Split into one 0/1 column
  per choice, each becomes a Checked/Unchecked binary. Left as ODK Central's
  default single column of space-separated answers, it is REFUSED rather than
  factorised: that column holds a set, not a category, and turning it into a
  factor would invent a level per observed combination. No type claim is made and
  the summary says so, with what to do about it.

  The metadata layer was generalised rather than duplicated:
  `apply_redcap_dictionary()` / `redcap_match_columns()` became
  `apply_form_metadata()` / `form_match_columns()`, driven by an `expand_sep`
  attribute ("___" for REDCap, "/" for ODK) and an explicit `multi` flag instead
  of a REDCap-specific field-type test. The REDCap path is unchanged - still 61
  mistyped fields to 0 on the PPS export.

  **Verification is weaker here than elsewhere in this suite, deliberately
  stated:** there is no ODK form in the WSU study folders. All 1,653 workbooks
  were scanned for a survey+choices sheet pair and none has one; there is no ODK
  XML and no ODK-shaped CSV export. That work is REDCap (with CDISC ODM XML
  backups) plus one Epicollect5 file. So this reader is written to the XLSForm
  specification and tested against fixtures written from it, not against a real
  form. `tests/test_odk_form.R` says so at the top and makes its fixtures awkward
  on purpose - translated labels, nested groups, both begin/end spellings, an
  external choice list, a duplicate choice code.

- **Design-based estimation for complex samples.** Most population-based
  public-health data is sampled with weights, strata or clusters - DHS and MICS,
  national health surveys, cluster-randomised trials, point-prevalence surveys
  sampling patients within hospitals - and an analysis plan declares that design,
  e.g. `svyset psu [pweight = w], fpc(fpc_psu) strata(region)`. The app could
  honour none of it, so both the estimates and their standard errors were wrong
  for such data: unequal selection probabilities bias an unweighted estimate, and
  clustering makes an unweighted standard error too small. The Data tab now has an optional
  **complex sample design** declaration (sampling weight, stratum, PSU, finite
  population correction), and when one is present the Regression tab offers
  survey-weighted linear, logistic and Poisson regression, selected by default
  because every other method on that tab assumes a simple random sample. New
  `survey_proportion()` / `survey_mean()` give design-based proportions and means
  with design-based intervals and the design effect - the `svy: proportion` and
  `svy: mean` the WASH analysis reports.

  Variance estimation is the `survey` package's, not a reimplementation: Taylor
  linearisation with strata, PSUs and finite-population corrections is not
  something to hand-roll. `survey` is added to `install_dependencies.R` as an
  optional package, and the feature reports its absence rather than failing.
  What is ours is the mapping from the Stata-style declaration, the guards
  (missing design columns are dropped and counted rather than silently changing
  the population; zero, negative and non-numeric weights are refused by name; a
  stratum with a single PSU is adjusted rather than aborting, with
  `survey.lonely.psu` set locally and restored), and the honest omissions -
  AIC/BIC are not reported for a weighted fit, because the likelihood of a
  weighted complex sample is not the likelihood of those rows.

  Verified on the real data: on the PPS Phase II patient export with hospitals
  as PSUs, residual degrees of freedom fall from 560 to 12 (17 PSUs minus 5
  parameters), which is the entire point of design-based inference, and the
  design-based standard errors run 0.86-1.42 times the naive ones. Covered by
  `tests/test_survey_design.R`.

- **Cohen's kappa and weighted kappa (agreement).** Concordance questions are
  everywhere in epidemiology - two clinicians reading the same slides, a rapid
  test against a reference assay, two anatomical sites in the same person, paired
  household members, test-retest reliability - and the app had no way to measure
  agreement at all. McNemar tests whether the two margins differ and
  chi-square whether the two are associated; neither asks whether they agree case
  by case, and a pair of measurements can pass either while disagreeing on every
  individual. The Tests tab's categorical family now offers an **agreement**
  design giving unweighted kappa, linear- and quadratic-weighted kappa for
  ordered categories, a confidence interval, and - for a 2x2 table - the
  prevalence and bias indices with PABAK, because kappa is depressed by a
  dominant category and a colonisation study is often exactly that case. McNemar
  is reported alongside kappa rather than as an alternative to it, since the two
  answer different questions. The write-up points the reader at the interval, not
  at the p-value, which only tests agreement against chance.

  The arithmetic uses the Fleiss-Cohen-Everitt (1969) asymptotic variance in the
  agreement-weight convention throughout - the two conventions give the same
  kappa but different variances, so they are never mixed - and is validated to
  six decimal places against `DescTools::CohenKappa` and `psych::cohen.kappa`,
  point estimate, SE and interval, unweighted and weighted. On the real ARCH 2.0
  household extract, mother-infant colonisation concordance comes out at
  kappa 0.230 with 77.5% raw agreement over 2,008 pairs. Covered by
  `tests/test_agreement.R`.

- **REDCap data dictionaries are now read.** Type detection works from a
  column's shape, and on data from an electronic capture system shape is not
  enough: a radio or dropdown
  field exported as 1/2/3 is indistinguishable from a count, so it was detected as
  a count and would enter a model as a linear number instead of as categories.
  Measured against one 237-column export's own dictionary, 61 of its
  radio/dropdown fields were typed as numeric that way, and 14 of those cannot be
  separated from genuine counts by any shape-based rule - `ant_one` (25 antibiotic
  codes) and `dur6` (19 durations in days) have identical form. The Data tab now
  accepts the "Data Dictionary (codebook)" CSV that ships with the export and
  takes each field's type and value labels from it. On that export it corrects all
  61, and the confirm-types screen's "needs a look" count falls from 44 columns to
  3.

  Coded fields become properly labelled factors, so every tab downstream shows
  "SURGERY(GENERAL)" rather than `4`; date-validated text is parsed with the
  day/month order the dictionary states rather than a guessed one; numeric
  validations become numeric; the field label travels with the column for the
  planner and the report. Checkbox fields are matched through their expanded
  `x___k` columns. A column the dictionary does not describe - REDCap's own
  `redcap_event_name`, `<form>_complete` - is left entirely to the detector, and a
  code present in the data but absent from the dictionary is KEPT as its own level
  and reported, never silently turned into NA. Malformed choice entries are
  skipped rather than failing the field, which real dictionaries need: three ARCH
  1.0 hospital fields list an option with no comma at all. All 735 coded fields
  across the nine dictionaries in the WSU studies parse. Covered by
  `tests/test_redcap_dictionary.R`.

- **The AI planner can propose an interaction.** The plan schema gained an
  `interactions` field with a prompt rule for effect-modification questions, and
  a planned interaction arrives pre-ticked on the Regression and Advanced-models
  tabs, so vaccines SA2 is now plannable end to end rather than only fittable by
  hand. Interactions naming a variable that is not also a predictor are narrowed
  away rather than left to fail at fit time. Also hardened
  `build_planner_metadata()`, which looked a type up with `types[[nm]] %||%
  "text"` - `[[` on a named atomic vector throws for a missing name instead of
  returning NULL, the same trap the function already guarded against for
  `base_levels` two lines below.

- **The confirm-types screen now handles wide files.** It rendered one row -
  two dropdowns plus an observer - for every column in the upload. On the ARCH
  2.0 household extracts (1,743 columns) that is 3,486 select widgets, 3.6 MB of
  HTML and 1,743 observers built before the analyst can click anything, plus a
  date-parse preview output registered for every column when only the 60
  datetime ones could ever show it. Import took a second; this screen took
  seven, and left the browser holding all of it. It is now paged (25 rows),
  searchable by column name, and filterable. First page on that file:
  0.04 MB and 0.07 s, 50 widgets, 25 observers. Date previews are registered
  only for columns actually typed as datetime.

  Row inputs stay keyed by the column's position in the detected-types table, so
  ids survive paging and searching and `types()` / `base_levels()` /
  `date_formats()` read them unchanged; an unvisited row has no input and those
  readers already fall back to the detected type. A row is re-rendered from its
  retained value rather than from the detected type - otherwise re-rendering
  would push the default back to the browser and silently discard a correction
  made before paging away.

  Two filters narrow the list. "Needs a look" shows columns that need a
  *decision*: small sets of whole numbers that may really be categories, and
  columns detected as identifiers. "Empty or constant" shows the ones nothing can
  be done with. Keeping those apart is what makes the filter useful - counting
  them together flagged 1,058 of 1,743 columns on that file, 623 of them
  empty; separated, it flags 172. On a file with more than 100 columns the list
  opens on "Needs a look", with the count shown and "All columns" one click away.

  Measured against the PPS Phase II patient export's own REDCap dictionary, of
  the 61 radio/dropdown fields the detector types as numeric: 36 appear under
  "Needs a look", 11 more are constant and appear under "Empty or constant", and
  14 are code lists with more than 10 distinct values. Those 14 cannot be
  separated from genuine counts by shape - `ant_one` (25 antibiotic codes) and
  `dur6` (19 durations in days) are identical in form. A "dense run of codes"
  rule was tried and measured: it matched 1 of the 14, so it was not adopted.
  Closing that gap needs the data dictionary itself, not a better heuristic.
  Covered by `tests/test_type_editor_paging.R`.

- **Regression models can now include interaction terms.** `model_formula()`
  built main effects only, and `run_guided_model()` rejected `a:b` as a
  predictor "not present in the data", so a question about effect modification
  could not be asked at all - the vaccines-and-AMR plan's SA2 (does Rotarix's
  effect on prescribing depend on prior PCV-10 vaccination?) had no route
  through the app. Both model tabs now offer an **Interaction terms** selector
  listing every pair of the chosen predictors, and the spec carries an
  `interactions` field through to the formula. Both main effects are required,
  because an interaction fitted without them does not mean what it appears to;
  `a:b` and `b:a` are treated as the same term; and a still-valid selection
  survives a change to the predictor list. Interactions work across OLS, robust,
  quantile, penalised and PLS regression, the GLMs, and GEE/LMM/GLMM. The
  interpretation panel no longer describes an interaction as an "adjusted
  association" - it reports it as effect modification and flags that the main
  effects of the variables involved are then conditional. Also fixed in passing:
  `model_formula()` used `reformulate()`, which fails outright on a
  non-syntactic column name (the TTC survey workbook has `1_1_Household_ID`);
  names that R cannot parse bare are now backtick-quoted, leaving the formula
  for ordinary names byte-identical. Covered by `tests/test_interactions.R`.

- **Fixed GEE reporting fabricated significance on string cluster ids.**
  `run_gee_model()` handed the cluster column to `geepack::geeglm()` untouched.
  geepack derives its cluster boundaries from `diff(as.numeric(id))`, so a
  non-numeric id - household codes like `01/003/03`, or a REDCap
  `participant_id` - coerced to NA, the cluster structure collapsed, and the fit
  returned standard errors of ~1e-07 with every p-value at exactly 0. No error,
  no warning: a fabricated significant result presented as a confident one. On a
  133-household repeated-measures extract the antibiotic-use term came back at
  p = 0 where the correct fit gives p = 0.23 (and the GLMM on the same data
  gives p = 0.21). Fit time was also 277 s instead of 0.4 s. A second, related
  defect: even with numeric ids the working correlation was estimated from the
  wrong pairs when the rows were not already grouped by cluster, so the p-value
  moved with the sort order of the uploaded file (alpha +0.081 sorted versus
  -0.073 shuffled). The id is now coded to its integer level and the rows sorted
  cluster-major - and by wave within cluster for AR-1 - once, after every
  derived column is attached. Numeric, already-sorted data is unaffected.
  Covered by `tests/test_gee_cluster_id.R`, which pins usable standard errors,
  agreement with a correctly specified `geeglm()` reference, and invariance to
  row shuffling; set `SABALI_ARCH2_DTA` to also run the checks against a real
  household extract.

- The AI backend now works with providers that pin the sampling temperature.
  Moonshot's Kimi models allow only their own default and reject an explicit
  `temperature` with HTTP 400, which failed every interpretation and write-up
  outright. `.llm_chat()` now retries such a rejection once with the field
  omitted (detected from the response body, so an unrelated 400 still errors),
  and the request body moved to a testable `.llm_body()`. A new optional
  `AI_INTERPRET_THINKING` variable ("disabled" / "enabled") controls reasoning
  mode on models that bill thinking tokens as output - on kimi-k2.6 an
  interpretation cost 1,965 completion tokens and ~53 s with thinking on versus
  278 tokens and ~9 s with it off. The field is sent only when the variable is
  set, so endpoints that do not know it are unaffected, and it is now carried
  into the async worker alongside the other AI variables. Covered by
  `tests/test_interpret_backend.R`.

- Report AI write-ups now survive free-tier rate limits. The endpoint (e.g.
  Gemini's free tier) returns HTTP 429 on bursts, which was failing the
  write-ups outright. Two fixes: (1) all LLM calls now retry transient failures
  (429 and 5xx) with exponential backoff honouring Retry-After, via a shared
  `.llm_chat()` helper in `R/fct_interpret.R`; (2) the report write-up is now a
  SINGLE AI call per item (it writes prose directly from the summary tables,
  reusing the tab's interpretation as context when present) instead of two, so
  a report of several items stays well under the per-minute quota. The
  provenance label is now endpoint-agnostic ("<model> via <host>") rather than
  hard-coded to "OpenCode Zen".

- Fixed stale analysis results after a data change. A fitted model/test is now
  held in a reactiveVal and INVALIDATED whenever the underlying data changes -
  so a coefficient for a level later removed via the tab-0 Data contract (e.g.
  `adhealthcareUnknown`) can no longer linger in the Tests, Regression or
  Advanced-models output; the analyst re-runs and the fit reflects the current
  data. Belt-and-braces, `prepare_outcome_data()` now drops unused factor levels
  on the model variables, so an empty (0-row) level can never surface as a
  phantom model term. Covered by `tests/test_stale_refit.R`.

- Report polish: post-hoc pairwise comparison tables (e.g. pairwise McNemar) are
  no longer pasted into the report as their own table - a full grid of pairwise
  rows is rarely reported in a paper. They still travel in the AI payload, so the
  prose write-up can call out the comparisons that matter. The report list's
  AI-write-up status now reads plainly as "writing" / "done" / "failed". The
  background write-up worker was extracted to a testable `report_enricher()` in
  `R/mod_report.R` (injectable AI runner); `tests/test_report_enrich.R` proves an
  item goes pending -> done, that a multi-item queue drains, and that a worker
  failure is marked "failed" (never stuck on "writing").

- A data file is no longer refused because some columns are constant or empty.
  Previously a single constant column (only one observed value) or an all-missing
  column made the whole import fail with an error - a real problem for wide
  clinical/REDCap exports, which routinely contain such columns (and more so now
  that blank cells are treated as missing). These are now advisory: the data
  loads and a non-blocking notification lists which columns cannot be used in a
  comparison or model. Only genuinely fatal problems (no rows/columns, invalid
  non-UTF-8 text) still stop the load. Split into `.assert_loadable()` (fatal)
  and `.import_warnings()` (advisory) in `R/fct_import.R`; covered by
  `tests/test_import.R` and `tests/test_validate_types.R`.

- Fixed a lasting cause of "my outcome/variable is missing on the Tests /
  Regression / Advanced tabs". The analysis tabs decide what a variable can be
  used for from its ACTUAL values, so a field that is conceptually binary but
  still holds a stray extra value (a blank, a typo, an out-of-range code) was
  read as multi-level categorical and hidden from binary/continuous/count
  analyses. Two changes address this:
  (1) blank / whitespace-only cells are now treated as missing on import
  (REDCap-style exports store missing as ""), so a two-level field with blanks
  is correctly detected as binary;
  (2) the tab-0 Data contract is now ENFORCED, not just checked. A value outside
  a variable's declared allowed levels (categorical) or numeric min/max is
  coerced to missing (NA) in the data every analysis tab uses - descriptives,
  charts, tests, models and the report. Set the rule once on the Data-check tab
  and it carries through the whole analysis; leaving a field at its observed
  default changes nothing, and the tab-0 checks/audit still describe the original
  file. Enforcement is derived from the contract (`dq_contract_to_constraints`)
  and applied centrally by the value-cleaning layer (both in `R/fct_clean.R`),
  wired so the contract is built from the pre-enforcement data (no reactive
  cycle). Covered by `tests/test_value_constraints.R` and `tests/test_import.R`.

- Made the Word report publication-ready. Tables added to a report are now
  trimmed to what a paper actually reports: the baked-in "Interpretation" row is
  dropped from hypothesis-test tables, raw "Std. Error"/"Statistic" columns are
  removed from coefficient tables, and when an exponentiated effect (OR/RR/IRR/HR)
  carries its own confidence interval the redundant log-scale estimate and its CI
  are dropped. Numbers are formatted for display (p-values via `p_fmt`, counts as
  integers, everything else to a tidy fixed precision). Every report table is now
  single-spaced with tight padding and auto-fits so it stays compact. Model and
  advanced-model diagnostics/assumption plots are no longer added to the report by
  default (an "Include diagnostics in the report" checkbox opts them back in).
  New `publication_table_df()` in `R/fct_report.R`; covered by
  `tests/test_report_publication.R`.

- The interpretation now lives in prose beneath the table/figure, not inside the
  table. Clicking "Add to report" runs a two-pass AI flow in the background: if
  the tab was already interpreted that text is reused, otherwise an interpretation
  is produced, and then a second pass turns the tables + interpretation into a
  short Results-section paragraph placed under the item in the report. Driven by a
  per-session enrichment queue in `app.R` (a shared `ExtendedTask`), a new
  `ai_report_writeup()` second pass and a `"writeup"` async job kind; the report
  list shows a "writing…" status and export finishes any stragglers synchronously.

- Charts can now show data labels (values on bars, points and pie slices; the mean
  overlay on longitudinal/time-series charts), controlled by a "Data labels"
  toggle (on by default). Fixed a pie-chart aesthetic bug uncovered by the labels
  (the percentage column is now computed before the plot captures its data).

- Charts gained AI interpretation. The Charts tab now carries the same
  interpretation panel as the analysis tabs, reading an aggregate summary of what
  the chart depicts (counts/percentages, group-wise summaries, or an association)
  via new `chart_summary_tables()` - never raw patient rows - and that same
  summary feeds the report write-up for a chart.

## 0.2.0 — 2026-07-24

- Made the two AI calls (the analysis planner on the Data tab and the results
  interpreter on the analysis tabs) run asynchronously in a background worker
  process, so the app no longer freezes while the AI is thinking - you can
  switch to and work in other tabs and the suggestion/interpretation appears
  when it is ready, with a live "working..." indicator. Built on Shiny's
  ExtendedTask + a small `future` multisession pool (new `R/fct_async.R`); it
  degrades gracefully to the previous in-process behaviour if `future` is
  unavailable. Covered by `tests/test_async.R`.

- Added support for stacked (multi-series) time-series data on the Advanced
  models tab. Previously a file holding several series in one column (e.g. one
  row per site per month) was silently pooled into a single, meaningless series.
  Now `ts_prepare` refuses a series with repeated time points (pointing the user
  at the series column), and the time-series family offers an optional
  "series / group column": pick one series to analyse it in full (with its
  forecast/diagnostic plots), or "All series" to run the chosen method on every
  series and get one results table with a leading Series column plus a faceted
  overview. Series too short to fit are skipped and named, never dropped
  silently; more than 60 distinct series is refused. Covered by new stacked-data
  tests in `tests/test_validate_timeseries.R`.

- Fixed the repeated-measures summary tables to honour an ordered factor's
  declared level order. When the time/visit variable was ordinal (e.g.
  "Baseline" < "6 months" < "12 months") and the rows were not already stored
  in visit order, the "Across time points" columns and the "Baseline (first
  time point)" table used the row-appearance order instead - so columns could
  come out in the wrong sequence and the baseline row could be taken from the
  wrong visit. `.t1_time_order` now follows `levels()` for ordered factors and
  `tableone_baseline` picks the lowest declared level per subject. Numeric,
  date and plain-labelled time variables are unchanged.

- Made date/time handling versatile. The app now recognises and parses text
  dates in many shapes - "apr-24", "Apr-2024", "9 Apr 2024", "Apr 9, 2024",
  "04/2024", "2024-04", "13/02/2024", ISO date-times and more - into real
  Date/POSIXct values so time-series, survival and Table 1 recognise them.
  Month-only values are placed on the last day of that month (so "apr-24" =
  30 April 2024). A "Confirm date formats" editor appears for any datetime
  column with a format dropdown (Automatic + explicit shapes) and a live
  raw→parsed preview; numeric day/month order is read day-first by default and
  flagged as ambiguous so the analyst can override. New `R/fct_dates.R`,
  covered by `tests/test_dates.R`.

- Made the optional subset filters responsive on large datasets. Filter changes
  are now debounced, so dragging a range slider or ticking categories no longer
  re-filters the whole dataset and re-runs every downstream tab on each
  keystroke; the filter and downstream recompute run once the selection settles.
  Category pickers now start empty (an empty pick means "keep all") instead of
  pre-selecting - and so rendering - one removable tag per level, and a column
  with more than 500 distinct values shows a note rather than building an
  unusably large picker. When no filter is active the coerced data is passed
  through unchanged, with no row-mask or copy.

- Added three optional subset filters on the "1. Data" tab. The analyst picks a
  variable and, based on its confirmed type, either keeps a range (continuous,
  count or parsed date/time columns) or ticks the levels to keep (binary,
  nominal, ordinal). A row is retained only if it passes every active filter;
  an untouched filter (no variable, the full range, or all levels) is a no-op
  and does not drop rows missing that variable. The resulting subset flows to
  every downstream tab as the analysis data set, with a live rows-kept status.
  Covered by `tests/test_subset_filters.R`.

- Mixed models (LMM/GLMM on the Advanced tab and the mixed-model ANOVA on the
  Tests tab) now report the random-effect variance components and convert them
  to an intraclass correlation (ICC) alongside the fixed effects. The ICC is
  exact for a Gaussian LMM and latent-scale (residual variance π²/3) for a
  binomial GLMM; variance components are still shown, without a headline ICC,
  when the residual scale is not fixed (e.g. Poisson) or a random slope is
  present. The variance-components table is included in the Word report.

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
