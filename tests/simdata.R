# tests/simdata.R -----------------------------------------------------------
# Stressor generators: synthetic data shaped like the files in data/, with the
# pathologies dialled up on purpose.
#
# This is DEVELOPER-SIDE ONLY. It is not in R/, so it never loads into the app
# and adds nothing to what a user runs. It is sourced by the property tests
# (tests/test_properties.R) and by the calibration study
# (tools/calibration_study.R).
#
# Two jobs:
#
#  1. CONFORMING generators produce data that genuinely satisfies an assumption,
#     so we can measure how often a check cries wolf.
#  2. VIOLATING generators produce data that genuinely breaks it, by a dialled
#     amount, so we can measure how often a check notices.
#
# The distinction matters: a check is only calibrated if we know BOTH rates.
# Everything is a pure function of an explicit seed, so a calibration table is
# reproducible and a failure can be re-run exactly.

# --- helpers ----------------------------------------------------------------

# Deterministic per-draw seeding: every generator takes `seed` and uses it.
.sim_seed <- function(seed) { set.seed(seed); invisible(TRUE) }

# --- 1. Distributional shape ------------------------------------------------
# Each returns a numeric vector. `severity` is the dial: 0 = conforming.

#' Normal data - the conforming reference for every shape check.
sim_normal <- function(n = 200, seed = 1, mean = 0, sd = 1) {
  .sim_seed(seed); stats::rnorm(n, mean, sd)
}

#' Right-skewed, log-normal. severity is the log-scale sigma: 0.1 is nearly
#' normal, 1.5 is viral-load territory (five orders of magnitude, extreme tail).
sim_skewed <- function(n = 200, seed = 1, severity = 1) {
  .sim_seed(seed); exp(stats::rnorm(n, 0, max(1e-6, severity)))
}

#' Heavy symmetric tails via a t distribution. severity 0 -> normal,
#' 1 -> df = 3. Skewness stays near zero, so this isolates kurtosis.
sim_heavy_tailed <- function(n = 200, seed = 1, severity = 1) {
  .sim_seed(seed)
  df <- if (severity <= 0) 200 else max(2.5, 30 - 27 * min(1, severity))
  stats::rt(n, df)
}

#' Bimodal mixture. THE blind spot for moment-based normality: a symmetric
#' two-mode mixture has skewness ~0 and negative excess kurtosis, so a rule
#' built on moments reports it as normal. Real: viral load is genuinely bimodal
#' (virally suppressed versus failing). `separation` is in pooled SDs.
sim_bimodal <- function(n = 200, seed = 1, separation = 4, p = 0.5) {
  .sim_seed(seed)
  z <- stats::rbinom(n, 1, p)
  stats::rnorm(n, ifelse(z == 1, separation / 2, -separation / 2), 1)
}

#' Ceiling pile-up, like adherence_pct: a bounded percentage with a lump at 100.
#' `at_ceiling` is the share sitting exactly on the bound.
sim_ceiling <- function(n = 200, seed = 1, at_ceiling = 0.35, upper = 100) {
  .sim_seed(seed)
  x <- stats::rbeta(n, 6, 1.4) * upper
  hit <- sample(seq_len(n), floor(at_ceiling * n))
  x[hit] <- upper
  x
}

#' Counts with excess zeros, like missed_visits. `extra_zero` is the share of
#' structural zeros added on top of the Poisson zeros.
sim_zero_inflated <- function(n = 200, seed = 1, lambda = 3, extra_zero = 0.3) {
  .sim_seed(seed)
  y <- stats::rpois(n, lambda)
  if (extra_zero > 0) y[sample(seq_len(n), floor(extra_zero * n))] <- 0L
  y
}

#' Heaped / rounded values, like self-reported adherence given to the nearest 5.
#' Creates heavy ties, which is what breaks rank and quantile methods.
sim_heaped <- function(n = 200, seed = 1, to = 5, mean = 80, sd = 12) {
  .sim_seed(seed); round(stats::rnorm(n, mean, sd) / to) * to
}

#' Left-censored assay values: everything below a detection limit is reported
#' AT the limit ("<50" becomes 50). Ubiquitous in viral-load data and a point
#' mass no transformation can remove. `lod_share` is the share below the limit.
sim_detection_limit <- function(n = 200, seed = 1, lod = 50, lod_share = 0.4) {
  .sim_seed(seed)
  x <- exp(stats::rnorm(n, log(lod) + 2, 2.2))
  cut <- stats::quantile(x, lod_share, names = FALSE)
  x[x < cut] <- lod
  pmax(x, lod)
}

#' Clean data with a single extreme value bolted on - the 835,344 case.
sim_one_outlier <- function(n = 200, seed = 1, multiple = 500) {
  .sim_seed(seed)
  x <- stats::rnorm(n, 100, 15)
  x[1] <- mean(x) * multiple
  x
}

# --- 2. Degenerate and adversarial -----------------------------------------

#' Every degenerate column an entry point must survive without throwing.
#' Returned as a named list so a test can loop over all of them.
sim_degenerate <- function(n = 30) {
  list(
    all_identical   = rep(7, n),
    all_zero        = rep(0, n),
    two_values      = rep(c(0, 1), length.out = n),
    all_missing     = rep(NA_real_, n),
    mostly_missing  = c(1, 2, rep(NA_real_, n - 2)),
    single_value    = 5,
    empty           = numeric(0),
    with_inf        = c(rnorm(n - 2), Inf, -Inf),
    all_negative    = -abs(stats::rnorm(n)) - 1,
    proportions_01  = rep(c(0, 1), length.out = n),
    huge_range      = c(1e-8, 1e8, stats::rnorm(n - 2)),
    constant_plus_1 = c(rep(3, n - 1), 4))
}

#' Sentinel codes standing in for missing (99, -999, 9999). These silently
#' destroy every magnitude statistic, and the assumption layer cannot tell
#' whether the value-constraints step was ever applied.
sim_sentinel <- function(n = 200, seed = 1, sentinel = -999, share = 0.1) {
  .sim_seed(seed)
  x <- stats::rnorm(n, 50, 8)
  x[sample(seq_len(n), floor(share * n))] <- sentinel
  x
}

#' A numeric column that arrived as text, in the spirit of the messy file:
#' thousands separators, a stray "N/A", and a below-detection string.
sim_text_numeric <- function(n = 200, seed = 1) {
  .sim_seed(seed)
  x <- as.character(round(exp(stats::rnorm(n, 6, 1.5))))
  x[sample(seq_len(n), 5)] <- "N/A"
  x[sample(seq_len(n), 5)] <- "<50"
  x
}

#' Height recorded in a mix of centimetres and metres - the real defect found
#' in messy_cross_sectional_dataset.csv. Produces bimodality and extreme
#' derived values at the same time.
sim_unit_mixup <- function(n = 200, seed = 1, share_metres = 0.05) {
  .sim_seed(seed)
  h <- stats::rnorm(n, 168, 9)
  idx <- sample(seq_len(n), floor(share_metres * n))
  h[idx] <- h[idx] / 100
  h
}

# --- 3. Structural stressors ------------------------------------------------

#' A balanced panel like lvct_cohort_repeated: `subjects` people, `visits` each.
#' `icc` dials the share of variance sitting between subjects, which is exactly
#' what an independence check ought to react to.
sim_panel <- function(subjects = 100, visits = 3, seed = 1, icc = 0.3,
                      unbalanced = FALSE) {
  .sim_seed(seed)
  between <- sqrt(max(0, icc)); within <- sqrt(max(1e-9, 1 - max(0, icc)))
  u <- stats::rnorm(subjects, 0, between)
  d <- data.frame(
    id    = rep(seq_len(subjects), each = visits),
    visit = rep(paste0("v", seq_len(visits)), times = subjects),
    time  = rep(seq_len(visits), times = subjects),
    stringsAsFactors = FALSE)
  d$y <- u[d$id] + stats::rnorm(nrow(d), 0, within)
  if (unbalanced) {
    # Attrition: keep every baseline, then drop later visits at random. This is
    # what real cohorts look like, and what a complete-cases check must cope with.
    keep <- d$time == 1 | stats::runif(nrow(d)) > 0.35
    d <- d[keep, , drop = FALSE]
  }
  d
}

#' A series with a dialled AR(1) coefficient. phi = 0 is independent.
sim_autocorrelated <- function(n = 200, seed = 1, phi = 0.7) {
  .sim_seed(seed)
  if (abs(phi) < 1e-9) return(stats::rnorm(n))
  as.numeric(stats::arima.sim(list(ar = phi), n, sd = 1))
}

#' Repeated measures with sphericity broken by a dialled amount: the variance of
#' condition k is scaled, which inflates the spread of the difference scores.
#' `severity` 0 gives compound symmetry (epsilon 1).
sim_sphericity <- function(subjects = 100, conditions = 3, seed = 1, severity = 1) {
  .sim_seed(seed)
  u <- stats::rnorm(subjects, 0, 1.5)
  scale <- 1 + severity * (seq_len(conditions) - 1)^2
  d <- data.frame(
    id = rep(seq_len(subjects), times = conditions),
    condition = rep(paste0("t", seq_len(conditions)), each = subjects),
    stringsAsFactors = FALSE)
  d$y <- rep(u, times = conditions) +
    stats::rnorm(nrow(d), 0, rep(scale, each = subjects))
  d
}

#' Two predictors correlated to a dialled degree, plus an outcome.
sim_collinear <- function(n = 200, seed = 1, r = 0.9) {
  .sim_seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- r * x1 + sqrt(max(0, 1 - r^2)) * stats::rnorm(n)
  data.frame(x1 = x1, x2 = x2, y = 1 + x1 + stats::rnorm(n))
}

#' A predictor-outcome pair whose relationship bends by a dialled amount.
#' `severity` 0 is a straight line.
sim_curved <- function(n = 200, seed = 1, severity = 1, noise = 1) {
  .sim_seed(seed)
  x <- stats::runif(n, -3, 3)
  data.frame(x = x, y = 2 + 1.5 * x + severity * x^2 + stats::rnorm(n, 0, noise))
}

#' Groups whose variances differ by a dialled ratio.
sim_unequal_variance <- function(n = 200, seed = 1, ratio = 4, groups = 2) {
  .sim_seed(seed)
  sds <- sqrt(seq(1, ratio, length.out = groups))
  g <- rep(paste0("g", seq_len(groups)), length.out = n)
  data.frame(g = g, x = stats::rnorm(n, 0, sds[as.integer(factor(g))]),
             stringsAsFactors = FALSE)
}

#' Survival data with a dialled censoring rate, like amr_timetoevent.
sim_survival <- function(n = 300, seed = 1, censor_rate = 0.5, horizon = 30) {
  .sim_seed(seed)
  x <- stats::rnorm(n)
  t_event <- stats::rexp(n, rate = 0.05 * exp(0.4 * x))
  t_cens <- stats::runif(n, 0, horizon / max(1e-6, censor_rate))
  data.frame(x = x,
             time = pmin(t_event, t_cens, horizon),
             event = as.integer(t_event <= pmin(t_cens, horizon)))
}

# --- 4. Whole datasets in the spirit of data/ -------------------------------

#' A cohort shaped like lvct_cohort_repeated.csv, with every pathology it has:
#' log-normal viral load spanning orders of magnitude, adherence piled at 100,
#' zero-heavy missed visits, an ordinal WHO stage stored as a number, and a
#' balanced (or, on request, attrition-thinned) panel.
sim_cohort <- function(subjects = 150, visits = 3, seed = 1,
                       detection_limit = FALSE, unbalanced = FALSE,
                       sentinel = FALSE) {
  .sim_seed(seed)
  n <- subjects * visits
  d <- data.frame(
    patient_id = rep(seq_len(subjects), each = visits),
    visit = rep(c("Baseline", "6 months", "12 months")[seq_len(visits)],
                times = subjects),
    visit_date = rep(as.Date("2024-01-15"), n) +
      rep(c(0, 180, 365)[seq_len(visits)], times = subjects),
    sex = rep(sample(c("Female", "Male"), subjects, TRUE), each = visits),
    age_years = rep(round(stats::rnorm(subjects, 38, 11)), each = visits),
    stringsAsFactors = FALSE)
  u <- stats::rnorm(subjects, 0, 1.2)                    # subject random effect
  d$viral_load <- round(exp(stats::rnorm(n, 5.2, 2.4) + u[d$patient_id]))
  d$cd4_count <- round(pmax(10, stats::rnorm(n, 480, 190)))
  d$adherence_pct <- pmin(100, round(stats::rbeta(n, 7, 1.5) * 100, 1))
  d$missed_visits <- stats::rpois(n, 0.7)
  d$who_stage <- sample(1:4, n, TRUE, prob = c(.4, .3, .2, .1))
  if (detection_limit) d$viral_load[d$viral_load < 50] <- 50
  if (sentinel) d$cd4_count[sample(n, floor(0.06 * n))] <- -999
  if (unbalanced) {
    keep <- d$visit == "Baseline" | stats::runif(n) > 0.3
    d <- d[keep, , drop = FALSE]
  }
  d
}

# --- 5. Registries the studies iterate over ---------------------------------

#' Shape generators paired with whether the data genuinely IS normal, so a
#' calibration run can compute false-alarm and detection rates without the
#' answer key being derived from the rule under test.
SIM_SHAPES <- list(
  list(id = "normal",          truly_normal = TRUE,
       gen = function(n, seed, sev) sim_normal(n, seed)),
  # Deliberately borderline: log-normal with sigma 0.25 has a true skewness of
  # about 0.77. It is NOT normal, so it must not sit in the false-alarm
  # denominator - counting it there inflated the measured false-alarm rate from
  # 7% to 17% at n = 30 and made the check look far worse calibrated than it is.
  list(id = "mild_skew",       truly_normal = FALSE,
       gen = function(n, seed, sev) sim_skewed(n, seed, 0.25)),
  list(id = "skewed",          truly_normal = FALSE,
       gen = function(n, seed, sev) sim_skewed(n, seed, sev)),
  list(id = "heavy_tailed",    truly_normal = FALSE,
       gen = function(n, seed, sev) sim_heavy_tailed(n, seed, sev)),
  list(id = "bimodal",         truly_normal = FALSE,
       gen = function(n, seed, sev) sim_bimodal(n, seed, 2 + 3 * sev)),
  list(id = "ceiling",         truly_normal = FALSE,
       gen = function(n, seed, sev) sim_ceiling(n, seed, 0.15 + 0.3 * sev)),
  list(id = "heaped",          truly_normal = FALSE,
       gen = function(n, seed, sev) sim_heaped(n, seed)),
  list(id = "detection_limit", truly_normal = FALSE,
       gen = function(n, seed, sev) sim_detection_limit(n, seed, 50, 0.2 + 0.3 * sev)),
  list(id = "one_outlier",     truly_normal = FALSE,
       gen = function(n, seed, sev) sim_one_outlier(n, seed, 50 * sev + 1)))

#' Every generator that yields a plain numeric column, for the property tests
#' that must hold whatever the input looks like.
sim_all_numeric <- function(n = 120, seed = 1) {
  list(
    normal          = sim_normal(n, seed),
    skewed          = sim_skewed(n, seed, 1.2),
    heavy_tailed    = sim_heavy_tailed(n, seed, 1),
    bimodal         = sim_bimodal(n, seed),
    ceiling         = sim_ceiling(n, seed),
    zero_inflated   = as.numeric(sim_zero_inflated(n, seed)),
    heaped          = sim_heaped(n, seed),
    detection_limit = sim_detection_limit(n, seed),
    one_outlier     = sim_one_outlier(n, seed),
    sentinel        = sim_sentinel(n, seed),
    unit_mixup      = sim_unit_mixup(n, seed),
    autocorrelated  = sim_autocorrelated(n, seed, 0.8))
}
