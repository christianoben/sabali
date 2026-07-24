# Sabali methods appendix

This appendix describes the computations exposed by the guided workflow. The
automated validation suite compares the numerical output with the reference
implementation named in the final column.

| Family | Methods | Main assumptions / notes | Reference |
| --- | --- | --- | --- |
| Location | one/two-sample t, paired t, Welch ANOVA, one-way ANOVA | finite numeric outcome; inspect normality and variance notes | `stats::t.test`, `stats::aov`, `stats::oneway.test` |
| Rank sensitivity | Wilcoxon, Mann–Whitney, Kruskal–Wallis, Friedman | ordinal/rank interpretation; ties and incomplete repeated blocks are explicit | base `stats` |
| Factorial / ANCOVA | factorial ANOVA, ART, Scheirer–Ray–Hare, ANCOVA, Quade | inspect interaction, slope homogeneity and residual diagnostics | `stats`, `rcompanion` in validation |
| Categorical | chi-square, Fisher exact, McNemar, Cochran's Q, exact proportion | expected counts and complete matched blocks are checked | `stats`, `DescTools` in validation |
| Correlation | Pearson, Spearman, Kendall, point-biserial, repeated-measures correlation | variation, normality and within-subject pairing are checked | `stats`, fixed-effects reference for rmcorr |
| Independent regression | OLS, robust, quantile, LASSO, ridge, PLS, logistic, Poisson, negative binomial | complete cases are reported; VIF, dispersion and model diagnostics are shown | `stats`, `MASS`, `quantreg`, `glmnet`, `pls` |
| Clustered regression | GEE, LMM, GLMM | subject/cluster ID is required; working correlation/random effects need review | `geepack`, `lme4` |
| Survival | Kaplan–Meier, log-rank, Cox, start–stop Cox | non-negative follow-up, observed events and proportional hazards need review | `survival` |

P-values and confidence intervals describe statistical evidence under the
selected model; they do not establish causation. The analyst must confirm the
design and substantive interpretation before reporting results.
