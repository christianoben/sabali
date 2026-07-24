# Sample data — LVCT HIV cohort (repeated measures)

`lvct_cohort_repeated.csv` — 750 rows, 250 patients each measured at 3 visits
(Baseline, 6 months, 12 months). Synthetic; for testing only.

Regenerate with `python3 make_sample_data.py`; make a clean-typed `.rda`
version with `Rscript make_sample_data.R`.

## Columns

| Column | Meaning | Type to confirm as |
|---|---|---|
| patient_id | Patient identifier (repeats across visits) | Identifier |
| visit | Baseline / 6 months / 12 months | Ordinal |
| visit_date | Date of visit | Date/time |
| county | County | Nominal |
| sex | Female / Male | Binary |
| age_years | Age in years | Continuous |
| who_stage | WHO clinical stage 1–4 | Ordinal |
| on_art | On ART: Yes / No | Binary |
| cd4_count | CD4 count (cells/µL); rises on ART | Continuous |
| viral_load | Viral load (copies/mL); strongly right-skewed | Continuous |
| adherence_pct | Adherence (%) | Continuous |
| missed_visits | Number of missed visits | Count |
| regimen | ART regimen | Nominal |

## Suggested smoke test (5 minutes)

1. **Data tab** → load `lvct_cohort_repeated.csv`. Check the auto-detected
   types. Note the tool guesses `patient_id`, `visit` and `who_stage` from the
   raw values — set `patient_id` to *Identifier*, `visit` and `who_stage` to
   *Ordinal*, and `visit_date` to *Date/time*. (Loading the `.rda` instead
   already has these set — a good way to test both paths.)
2. **Descriptives tab** → pick `cd4_count`, then group by `visit` to see it
   rise over time; pick `on_art` to see proportions with a Wilson CI.
3. **Charts tab**
   - `viral_load` alone → the tool should flag it as skewed and prefer a
     histogram/boxplot; try the raincloud plot.
   - `cd4_count` + `visit` → boxplot by group (change palette/theme/labels).
   - `cd4_count` + `viral_load` → scatter with a trend line.
   - `visit` + `on_art` → grouped or stacked bar; toggle "show as proportions".
   - Add a few charts and descriptives to the report as you go.
4. **Report tab** → export to Word and open the `.docx`.
