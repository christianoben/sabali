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
| Independent regression | OLS, robust, quantile, LASSO, ridge, PLS, logistic, Poisson, negative binomial | complete cases are reported; VIF, dispersion and model diagnostics are shown | `stats`, `MASS`, `quantreg`, `glmnet`, `pls` |
| Clustered regression | GEE, LMM, GLMM | subject/cluster ID is required; working correlation/random effects need review | `geepack`, `lme4` |
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

P-values and confidence intervals describe statistical evidence under the
selected model; they do not establish causation. The analyst must confirm the
design and substantive interpretation before reporting results.
