"""Generate a synthetic repeated-measures (longitudinal) cohort dataset for
smoke-testing Sabali (the guided statistics tool).

Structure: patients measured over three visits (baseline, 6 months, 12 months).
Deliberately mixes measurement types and includes within-patient correlation,
a right-skewed variable, an ordinal variable, and scattered missing values so
that type detection, descriptives, grouping and every chart type can be tried.

Run:  python3 make_sample_data.py   ->   writes lvct_cohort_repeated.csv
"""
import numpy as np
import pandas as pd

rng = np.random.default_rng(2026)

N_PATIENTS = 250
VISITS = ["Baseline", "6 months", "12 months"]
COUNTIES = ["Nairobi", "Kisumu", "Mombasa", "Nakuru", "Kiambu"]
REGIMENS = ["TDF+3TC+DTG", "AZT+3TC+NVP", "ABC+3TC+EFV"]

# --- patient-level (time-invariant) attributes -----------------------------
pid = np.arange(1, N_PATIENTS + 1)
sex = rng.choice(["Female", "Male"], N_PATIENTS, p=[0.56, 0.44])
county = rng.choice(COUNTIES, N_PATIENTS, p=[0.34, 0.22, 0.18, 0.14, 0.12])
regimen = rng.choice(REGIMENS, N_PATIENTS, p=[0.6, 0.2, 0.2])
base_age = rng.integers(18, 66, N_PATIENTS)
enrol = pd.to_datetime("2024-01-01") + pd.to_timedelta(
    rng.integers(0, 300, N_PATIENTS), unit="D")

# per-patient random intercepts -> within-patient correlation over visits
u_cd4 = rng.normal(0, 90, N_PATIENTS)
u_vl = rng.normal(0, 0.5, N_PATIENTS)

rows = []
for i in range(N_PATIENTS):
    on_art_prev = rng.random() < 0.60          # ART status at baseline
    for v, visit in enumerate(VISITS):
        # ART uptake climbs with time
        on_art = on_art_prev or (rng.random() < 0.5 + 0.2 * v)
        on_art_prev = on_art

        # CD4 rises on ART; correlated within patient
        cd4 = 330 + u_cd4[i] + (70 * v if on_art else 5 * v) + rng.normal(0, 40)
        cd4 = max(20, round(cd4))

        # viral load: right-skewed (lognormal), falls on ART
        log_vl = 4.6 + u_vl[i] - (1.3 * v if on_art else 0.1 * v) + rng.normal(0, 0.4)
        vl = int(10 ** max(1.3, log_vl))
        if on_art and rng.random() < 0.3 + 0.2 * v:
            vl = int(rng.integers(20, 200))    # virally suppressed

        who = int(np.clip(round(rng.normal(2.2 - 0.3 * v, 0.8)), 1, 4))
        adherence = float(np.clip(
            rng.normal(85 if on_art else 60, 12), 0, 100).round(1))
        missed = int(rng.poisson(0.6 if on_art else 1.6))

        rows.append({
            "patient_id": pid[i],
            "visit": visit,
            "visit_date": (enrol[i] + pd.DateOffset(months=6 * v)).date(),
            "county": county[i],
            "sex": sex[i],
            "age_years": int(base_age[i] + v // 2),
            "who_stage": who,
            "on_art": "Yes" if on_art else "No",
            "cd4_count": cd4,
            "viral_load": vl,
            "adherence_pct": adherence,
            "missed_visits": missed,
            "regimen": regimen[i],
        })

df = pd.DataFrame(rows)

# scatter some realistic missingness
for col, frac in [("cd4_count", 0.05), ("adherence_pct", 0.04), ("who_stage", 0.03)]:
    idx = rng.choice(df.index, int(frac * len(df)), replace=False)
    df.loc[idx, col] = np.nan

df.to_csv("lvct_cohort_repeated.csv", index=False)
print(f"Wrote lvct_cohort_repeated.csv: {len(df)} rows, {df.shape[1]} columns, "
      f"{df['patient_id'].nunique()} patients x {len(VISITS)} visits.")
