# Data readiness workflow

Tab 0 is a data-management workflow, not only an error list. It runs three
layers of checks:

1. Raw-import checks identify values lost during coercion, unexpected storage
   types, and invalid text encoding.
2. Data-contract checks compare the typed data with declared required fields,
   allowed levels, ranges, identifiers, keys, dates, and subgroups.
3. Analysis-readiness checks run only after an analyst explicitly selects an
   outcome, predictors, and analysis family.

The original upload is never modified. Any cleaning or recoding must be done in
a separate, documented dataset.

## The 22-point scorecard

| Point | Domain | Check | Context |
|---:|---|---|---|
| 1 | Structure | Structural completeness | Always |
| 2 | Schema | Schema integrity | Always/contract |
| 3 | Missingness | Missingness levels | Always |
| 4 | Missingness | Missingness patterns | Always |
| 5 | Missingness | Missingness by subgroup | Contract |
| 6 | Duplicates | Exact duplicate rows | Always |
| 7 | Duplicates | Duplicate keys | Contract/layout |
| 8 | Categories | Category consistency | Always/contract |
| 9 | Categories | Empty or sparse categories | Always |
| 10 | Values | Numeric validity | Always |
| 11 | Values | Expected ranges | Contract |
| 12 | Values | Outliers | Always |
| 13 | Variation | Constant variables | Always |
| 14 | Variation | Near-zero variance | Always |
| 15 | Dates | Date integrity | Always/contract |
| 16 | Identifiers | Identifier integrity | Contract/layout |
| 17 | Schema | Unexpected data types | Raw/contract |
| 18 | Model readiness | Highly correlated predictors | Analysis |
| 19 | Model readiness | Class imbalance | Analysis |
| 20 | Distributions | Skewed distributions | Always |
| 21 | Model readiness | Logistic separation | Logistic context |
| 22 | Model readiness | Modeling suitability | Analysis |

A scorecard item can be "passed", "issues found", or "not evaluated". A
context-dependent item is not silently treated as passed when its required
contract or analysis context has not been supplied.

## Using the tab

Start on **Data contract** after confirming variable types. Declare:

- required fields;
- expected minimum and maximum values;
- allowed category levels;
- unique identifier fields; and
- subgroup variables for missingness comparisons.

Then open **Analysis readiness** and explicitly choose an outcome, predictors,
and model family when model-specific checks are needed.

The report uses four levels:

- **Blocker** — resolve before analysis or document a formal exception.
- **High** — likely to affect validity or interpretation.
- **Medium** — investigate and document before relying on the result.
- **Low/info** — useful cleanup or review information.

Every non-pass finding contains:

- Issue;
- Impact;
- Recommended action;
- Affected count and percentage where meaningful;
- Scope and evidence; and
- Session review status.

## Review workflow

Under **Review & audit**, the data owner or analyst can mark a finding:

- Open;
- Accepted with justification;
- Not applicable; or
- Fixed — re-run required.

Accepted findings remain visible in the report. They are not deleted, and an
acceptance without a written reason is rejected by the UI.

The metadata audit contains event names, field names, dimensions, and timestamps.
It does not contain row values, category values, API keys, or uploaded file
contents.

## Interpretation principles

Automated findings are signals, not automatic cleaning instructions.

- Do not delete outliers without checking the source and domain limits.
- Do not map category labels or sentinel values in place; create an audited
  cleaned dataset.
- Do not treat missingness as harmless because the overall percentage is small;
  inspect patterns and subgroups.
- Do not run logistic regression with separation without changing the method or
  clearly documenting the limitation.
- Do not infer a study outcome from a variable name; select it explicitly.

The CSV findings and scorecard downloads are intended for the data-management
record and can be attached to the study's review documentation.

