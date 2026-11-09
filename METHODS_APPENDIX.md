# Sabali methods appendix

This appendix describes the computations exposed by the guided workflow. The
automated validation suite compares the numerical output with the reference
implementation named in the final column.

| Family | Methods | Main assumptions / notes | Reference |
| --- | --- | --- | --- |
| Descriptive summary table (Table 1) | multi-variable summary; rows = characteristics, optional split by an outcome (columns) + Overall; n (column %) for categorical, M (SD) / Median [IQR] for continuous; p-value only when split, using the recommended assumption-aware test with a tests-used footnote | cross-sectional layout only; complete-case per column; p-values unadjusted | own engine (`build_table_one`); p-values via `run_two_independent` / `run_anova_oneway` / `run_categorical`; APA rendering via `flextable::theme_apa` |
| Location | one/two-sample t, paired t, Welch ANOVA, one-way ANOVA | finite numeric outcome; inspect normality and variance notes | `stats::t.test`, `stats::aov`, `stats::oneway.test` |
| Rank sensitivity | Wilcoxon, Mann–Whitney, Kruskal–Wallis, Friedman | ordinal/rank interpretation; ties and incomplete repeated blocks are explicit | base `stats` |
| Factorial / ANCOVA | factorial ANOVA, ART, Scheirer–Ray–Hare, ANCOVA, Quade | inspect interaction, slope homogeneity and residual diagnostics | `stats`, `rcompanion` in validation |
| Categorical | chi-square, Fisher exact, McNemar, Cochran's Q, exact proportion | expected counts and complete matched blocks are checked | `stats`, `DescTools` in validation |
| Correlation | Pearson, Spearman, Kendall, point-biserial, repeated-measures correlation | variation, normality and within-subject pairing are checked | `stats`, fixed-effects reference for rmcorr |
| Independent regression | OLS, robust, quantile, LASSO, ridge, PLS, logistic, Poisson, negative binomial | complete cases are reported; VIF, dispersion and model diagnostics are shown; two-way interactions may be added, and both main effects are then required so the product term is interpretable | `stats`, `MASS`, `quantreg`, `glmnet`, `pls` |
| Clustered regression | GEE, LMM, GLMM | subject/cluster ID is required (coded to an integer level and sorted cluster-major before fitting, since GEE derives its clusters from the id order); two-way interactions are supported; working correlation/random effects need review; the fixed effects and the random-effect variance components (converted to an ICC) are both reported | `geepack`, `lme4` |
| Form metadata | REDCap data dictionary (CSV), ODK / Kobo XLSForm (workbook) | field types and value labels are taken from the definition instead of inferred from column shape; unmatched columns fall back to detection; unlisted codes are kept and reported; an unsplit ODK select_multiple is refused rather than factorised. The ODK reader is verified against the XLSForm specification, not against a real study form | `readxl` |
| Diagnostic accuracy | sensitivity, specificity, PPV/NPV, likelihood ratios, Youden's J, ROC/AUC | directional - one variable is the reference standard; Wilson intervals; predictive values are recomputed across a range of prevalences because they do not transfer between settings | reimplemented; `pROC`, `DescTools` in validation |
| Effect measures | risk difference (Newcombe), risk ratio (Katz), odds ratio, NNT | binary outcome across two groups; NNT is withheld when the risk-difference interval spans zero; the OR/RR divergence is stated when the outcome is common | reimplemented; `DescTools` in validation |
| Trend | Cochran-Armitage test for trend | ordered exposure with 3+ levels; the scores set what "linear" means and are reported | `stats::prop.trend.test` |
| Rates | incidence rate, rate ratio, direct standardisation, SMR | person-time denominators; exact Poisson/gamma intervals; direct standardisation needs stable stratum rates, otherwise use the SMR | reimplemented; `stats::poisson.test`, `binom.test` in validation |
| Missing data | multiple imputation with Rubin's-rules pooling | offered only when complete-case analysis would lose 1%+ of rows; refuses variables >60% missing; assumes missing at random given the imputation model | `mice` |
| Agreement | Cohen's kappa (2 raters), Fleiss' kappa (3+ raters), weighted kappa, prevalence and bias indices, PABAK, per-category kappa | Fleiss intervals are bootstrapped over subjects because the classic analytic SE is a null-hypothesis SE and gives intervals about a third too narrow; two categorical measurements of the SAME cases; Fleiss-Cohen-Everitt asymptotic variance in the agreement-weight convention, validated against `DescTools` and `psych`; McNemar reported alongside as the marginal question | reimplemented; `DescTools`, `psych` in validation |
| Path analysis | observed-variable path models with mediation; direct, indirect (per route) and total effects | exposures, mediators (in analyst-declared causal order) and outcome must be numeric or two-level; a saturated model's fit indices are flagged as uninformative; lavaan cannot combine its threshold estimator with sampling weights or clustering, and the note states what was not applied | `lavaan` (`sem`) |
| Complex samples | design-based proportions and means, survey-weighted linear/logistic/Poisson regression; one- and two-stage designs | a sampling weight, stratum or PSU must be declared on the Data tab; one- and two-stage Taylor-linearisation designs and replicate-weight designs (BRR, Fay, JK1, JKn, bootstrap, successive-difference); standard errors use Taylor linearisation and residual degrees of freedom follow the PSU count, not the row count; likelihood-based fit statistics are not reported for a weighted fit | `survey` (`svydesign`, `svymean`, `svyby`, `svyglm`) |
| Survival | Kaplan–Meier, log-rank, Cox, start–stop Cox | non-negative follow-up, observed events and proportional hazards need review | `survival` |
| Time series | seasonal decomposition (STL/classical), stationarity & autocorrelation diagnostics (ADF, KPSS, Phillips–Perron, Ljung–Box, and Zivot–Andrews when `urca` is present), Mann–Kendall trend & Sen's slope, Holt–Winters exponential smoothing forecast, ARIMA/SARIMA forecast, trend/seasonal regression, GARCH volatility model | rows must be in time order; a seasonal period is required for seasonal methods; stationarity should be confirmed before forecasting; forecasts assume the fitted structure persists; GARCH needs a longer series with volatility clustering | `stats` (`stl`, `arima`, `HoltWinters`, `spec.pgram`, `PP.test`, `Box.test`), `lmtest` (`dwtest`, `bgtest`); ADF, KPSS and Mann–Kendall/Sen are reimplemented and validated against independent recomputation; `urca` (`ur.df`, `ur.kpss`, `ur.za`) and `rugarch` (`ugarchfit`) upgrade the diagnostics/volatility when installed |

The ADF, KPSS and Mann–Kendall/Sen's-slope routines are reimplemented so the
time-series diagnostics run with no extra package. Their statistics are checked
against independent recomputation and hand-worked examples in the validation
suite. When the optional `urca` package is installed the stationarity tab uses
its exact MacKinnon critical values (and adds the Zivot–Andrews structural-break
test) in place of the built-in approximate p-values; without it the built-in
tables are used and are approximate near the tabulated boundaries. The GARCH
volatility model requires the optional `rugarch` package and is hidden from the
menu when it is not installed.

For linear and generalised linear mixed models (LMM/GLMM) and the mixed-model
ANOVA, the output reports the fixed-effect coefficients alongside the
random-effect variance components and their conversion to an intraclass
correlation (ICC = between-cluster variance ÷ total variance). The ICC is exact
for a Gaussian LMM; for a binomial GLMM it is a latent-scale ICC computed with a
fixed residual variance of π²/3 ≈ 3.29. When the residual variance is not on a
fixed scale (e.g. a Poisson GLMM) or a random slope is present, the variance
components are reported without a single headline ICC.

P-values and confidence intervals describe statistical evidence under the
selected model; they do not establish causation. The analyst must confirm the
design and substantive interpretation before reporting results.
