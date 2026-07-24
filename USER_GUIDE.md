# Sabali user guide

## Compare two independent groups

1. Load a table on **1 · Data**, confirm the outcome as continuous/count/binary
   and the grouping variable as binary or nominal.
2. Declare the data as **one row per subject**, select the outcome, then open
   **4 · Tests**.
3. Choose **Compare groups**, select **two independent groups**, choose the
   outcome and group, and run the analysis.

Sabali reports Welch's t-test and the Mann–Whitney alternative together. Check
the normality and variance notes, the Hedges g effect size, and the complete
case count before interpreting the p-value.

## Repeated measurements

On the Data tab, answer **repeated measures** and select the subject ID and
visit/time columns. Use paired/repeated designs on the Tests tab, or GEE/LMM/
GLMM on **6 · Advanced models**. Do not pool repeated rows into an independent
t-test or regression.

## Time-to-event data

Declare a survival analysis and select either a follow-up duration or start/end
dates plus the event indicator. Use Kaplan–Meier for curves and log-rank
comparisons, or Cox regression for adjusted hazard ratios. Censored follow-up,
event and subject-ID columns are reserved from ordinary analyses.

## Regression and reporting

Select candidate outcomes on the Data tab, choose a method on **5 · Regression**
or **6 · Advanced models**, review diagnostics, and add confirmed tables/charts
to **7 · Report**. Reported p-values are not a substitute for checking design,
missingness, assumptions and clinical relevance.

## AI features

The planner and interpretation panels are disabled until an approved
OpenAI-compatible endpoint is configured. They receive aggregate metadata or
result tables only, never raw patient rows. Big Pickle is not zero-retention;
use it only with synthetic or de-identified data unless governance approval
explicitly covers the data class.
