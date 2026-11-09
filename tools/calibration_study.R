#!/usr/bin/env Rscript
# tools/calibration_study.R --------------------------------------------------
# Calibration study for the assumption checks and the transformation engine.
#
#   Rscript tools/calibration_study.R [reps]
#
# DEVELOPER TOOL. Not part of the app and not part of the test suite - it is too
# slow to run on every change. It writes reference tables to
# tools/calibration/, which are committed so that drift is visible later
# without paying the cost each time.
#
# WHAT IT MEASURES, and why it is not circular:
#
#   * FALSE-ALARM RATE - how often a check flags data that genuinely satisfies
#     the assumption. The generator decides the truth, not the rule under test.
#     A check that cries wolf on good data trains people to ignore it.
#   * DETECTION RATE - how often a check catches data that genuinely violates
#     it, by a dialled amount. Reported per severity so we can see where a
#     threshold starts to bite.
#   * TRANSFORM EFFECTIVENESS - whether each transformation actually achieves
#     what its catalogue entry claims, across the whole stressor set, rather
#     than on one tidy fixture.
#
# The answer key comes from how the data was generated, never from the statistic
# the check itself computes.

suppressPackageStartupMessages({ library(shiny); library(ggplot2) })

root <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(root)
for (f in list.files("R", pattern = "^[^_].*[.][Rr]$", full.names = TRUE))
  source(f, local = FALSE)
source("tests/simdata.R")

args <- commandArgs(TRUE)
REPS <- if (length(args) && !is.na(suppressWarnings(as.integer(args[1]))))
  as.integer(args[1]) else 200
OUT <- "tools/calibration"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Calibration study: %d replicates per cell\n", REPS))
cat(strrep("=", 78), "\n")

pct <- function(x) sprintf("%.1f%%", 100 * x)
rate <- function(f, reps, offset) mean(vapply(seq_len(reps),
  function(s) isTRUE(f(offset + s)), logical(1)))

# ============================================================================
# 1. Normality: false-alarm and detection across shapes and sample sizes
# ============================================================================
cat("\n[1/5] Normality across shapes and sample sizes\n")
sizes <- c(15, 30, 60, 150, 400, 1000)
rows <- list()
for (shape in SIM_SHAPES) {
  for (n in sizes) {
    for (sev in c(0.5, 1)) {
      if (shape$truly_normal && sev != 1) next
      flags <- vapply(seq_len(REPS), function(s) {
        x <- shape$gen(n, 10000 + s, sev)
        check_normality(x)$severity
      }, character(1))
      rows[[length(rows) + 1L]] <- data.frame(
        shape = shape$id, truly_normal = shape$truly_normal, n = n,
        severity_dial = sev,
        flagged = mean(flags != "ok"),
        violated = mean(flags == "violated"),
        untested = mean(flags == "untested"),
        stringsAsFactors = FALSE)
    }
  }
  cat("  ", shape$id, "\n")
}
normality <- do.call(rbind, rows)
utils::write.csv(normality, file.path(OUT, "normality.csv"), row.names = FALSE)

conforming <- normality[normality$truly_normal, ]
cat("\n  FALSE ALARMS on genuinely normal data:\n")
for (n in sizes) {
  sub <- conforming[conforming$n == n, ]
  cat(sprintf("    n = %-5d flagged %s\n", n, pct(mean(sub$flagged))))
}
cat("\n  DETECTION on genuinely non-normal data (severity 1):\n")
viol <- normality[!normality$truly_normal & normality$severity_dial == 1, ]
for (sh in unique(viol$shape)) {
  sub <- viol[viol$shape == sh, ]
  cat(sprintf("    %-16s caught %s  (at n=150: %s)\n", sh, pct(mean(sub$flagged)),
              pct(sub$flagged[sub$n == 150])))
}

# ============================================================================
# 2. Structural checks against their dials
# ============================================================================
cat("\n[2/5] Structural checks against their dials\n")

auto_rows <- lapply(c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9), function(phi)
  data.frame(phi = phi,
    flagged = rate(function(s)
      check_autocorrelation(sim_autocorrelated(200, 20000 + s, phi))$severity != "ok",
      REPS, 0),
    violated = rate(function(s)
      check_autocorrelation(sim_autocorrelated(200, 20000 + s, phi))$severity == "violated",
      REPS, 0)))
autocorrelation <- do.call(rbind, auto_rows)
utils::write.csv(autocorrelation, file.path(OUT, "autocorrelation.csv"), row.names = FALSE)
cat("  autocorrelation (AR1 phi -> flagged):\n")
for (i in seq_len(nrow(autocorrelation)))
  cat(sprintf("    phi = %.1f  flagged %s  violated %s\n", autocorrelation$phi[i],
              pct(autocorrelation$flagged[i]), pct(autocorrelation$violated[i])))

sph_reps <- max(20, REPS %/% 5)
sph_rows <- lapply(c(0, 0.25, 0.5, 1, 2), function(sev) {
  eps <- vapply(seq_len(sph_reps), function(s) {
    d <- sim_sphericity(100, 3, seed = 30000 + s, severity = sev)
    check_sphericity(d$y, d$id, d$condition)$statistic }, numeric(1))
  fl <- vapply(seq_len(sph_reps), function(s) {
    d <- sim_sphericity(100, 3, seed = 30000 + s, severity = sev)
    check_sphericity(d$y, d$id, d$condition)$severity != "ok" }, logical(1))
  data.frame(severity = sev, mean_epsilon = mean(eps), min_epsilon = min(eps),
             flagged = mean(fl))
})
sphericity <- do.call(rbind, sph_rows)
utils::write.csv(sphericity, file.path(OUT, "sphericity.csv"), row.names = FALSE)
cat("  sphericity (severity -> epsilon, flagged):\n")
for (i in seq_len(nrow(sphericity)))
  cat(sprintf("    severity %.2f  mean epsilon %.3f  min %.3f  flagged %s\n",
              sphericity$severity[i], sphericity$mean_epsilon[i],
              sphericity$min_epsilon[i], pct(sphericity$flagged[i])))

# Sphericity on UNBALANCED panels: how much of the sample survives the
# complete-cases requirement, and does the check admit what it dropped?
unb <- lapply(c(0, 0.2, 0.4, 0.6), function(drop) {
  kept <- vapply(seq_len(max(10, REPS %/% 10)), function(s) {
    set.seed(40000 + s)
    d <- sim_sphericity(100, 3, seed = 40000 + s, severity = 0)
    if (drop > 0) d <- d[d$condition == "t1" | stats::runif(nrow(d)) > drop, ]
    r <- check_sphericity(d$y, d$id, d$condition)
    if (is.null(r$detail$subjects)) 0 else r$detail$subjects / 100
  }, numeric(1))
  data.frame(attrition = drop, share_of_subjects_used = mean(kept))
})
unbalanced <- do.call(rbind, unb)
utils::write.csv(unbalanced, file.path(OUT, "sphericity_unbalanced.csv"), row.names = FALSE)
cat("  sphericity on unbalanced panels (attrition -> subjects actually used):\n")
for (i in seq_len(nrow(unbalanced)))
  cat(sprintf("    attrition %.0f%%  used %s of subjects\n",
              100 * unbalanced$attrition[i], pct(unbalanced$share_of_subjects_used[i])))

zi_rows <- lapply(c(0, 0.1, 0.2, 0.4, 0.6), function(z)
  data.frame(extra_zero = z,
    flagged = rate(function(s) {
      y <- sim_zero_inflated(300, 50000 + s, 3, z)
      check_zero_inflation(y, rep(3, length(y)))$severity != "ok" }, REPS, 0)))
zero_inflation <- do.call(rbind, zi_rows)
utils::write.csv(zero_inflation, file.path(OUT, "zero_inflation.csv"), row.names = FALSE)
cat("  zero-inflation (extra zeros -> flagged):\n")
for (i in seq_len(nrow(zero_inflation)))
  cat(sprintf("    extra zeros %.0f%%  flagged %s\n", 100 * zero_inflation$extra_zero[i],
              pct(zero_inflation$flagged[i])))

lin_rows <- lapply(c(0, 0.1, 0.25, 0.5, 1, 2), function(k)
  data.frame(curvature = k,
    flagged = rate(function(s) {
      d <- sim_curved(200, 60000 + s, severity = k)
      check_linearity(d$x, d$y)$severity != "ok" }, REPS, 0)))
linearity <- do.call(rbind, lin_rows)
utils::write.csv(linearity, file.path(OUT, "linearity.csv"), row.names = FALSE)
cat("  linearity (curvature -> flagged):\n")
for (i in seq_len(nrow(linearity)))
  cat(sprintf("    curvature %.2f  flagged %s\n", linearity$curvature[i],
              pct(linearity$flagged[i])))

var_rows <- lapply(c(1, 1.5, 2, 3, 5, 10), function(rr)
  data.frame(variance_ratio = rr,
    flagged = rate(function(s) {
      d <- sim_unequal_variance(240, 70000 + s, ratio = rr)
      check_variance_homogeneity(d$x, d$g)$severity != "ok" }, REPS, 0)))
variance <- do.call(rbind, var_rows)
utils::write.csv(variance, file.path(OUT, "variance.csv"), row.names = FALSE)
cat("  equal variance (ratio -> flagged):\n")
for (i in seq_len(nrow(variance)))
  cat(sprintf("    ratio %5.1f  flagged %s\n", variance$variance_ratio[i],
              pct(variance$flagged[i])))

# ============================================================================
# 3. Consequence check: does a flag correspond to inference actually breaking?
# ============================================================================
# The deepest question. An assumption matters only insofar as ignoring it
# damages the inference it guards. Here the guarded inference is a one-sample
# t-test; we measure its real type I error under each shape and compare that
# with how often the normality check complains. A check is well calibrated when
# it is quiet where the error rate holds at 5% and loud where it does not.
cat("\n[3/5] Consequence: type I error of a t-test versus what the check says\n")
cons_reps <- max(200, REPS)
# The null must be tested against the POPULATION mean. Centring each sample on
# its own mean would make the null exactly true in every draw, forcing the
# t-statistic to zero and the measured error rate to zero - a meaningless table.
# So take a large reference draw per shape to pin the true mean, then test
# H0: mu = that value.
true_mean <- vapply(SIM_SHAPES, function(shape)
  mean(shape$gen(200000, 111, 1), na.rm = TRUE), numeric(1))
names(true_mean) <- vapply(SIM_SHAPES, function(s) s$id, character(1))

cons <- lapply(SIM_SHAPES, function(shape) {
  mu0 <- true_mean[[shape$id]]
  do.call(rbind, lapply(c(20, 60, 200), function(n) {
    rej <- 0; flagged <- 0
    for (s in seq_len(cons_reps)) {
      x <- shape$gen(n, 80000 + s, 1)
      x <- x[is.finite(x)]
      if (length(x) < 3 || stats::sd(x) == 0) next
      p <- tryCatch(stats::t.test(x, mu = mu0)$p.value, error = function(e) NA_real_)
      if (!is.na(p) && p < 0.05) rej <- rej + 1
      if (check_normality(x)$severity != "ok") flagged <- flagged + 1
    }
    data.frame(shape = shape$id, n = n, type1_error = rej / cons_reps,
               flagged = flagged / cons_reps, stringsAsFactors = FALSE)
  }))
})
consequence <- do.call(rbind, cons)
utils::write.csv(consequence, file.path(OUT, "consequence.csv"), row.names = FALSE)
cat("  shape             n    true type I error   check flags\n")
for (i in seq_len(nrow(consequence)))
  cat(sprintf("  %-16s %4d   %-17s %s\n", consequence$shape[i], consequence$n[i],
              pct(consequence$type1_error[i]), pct(consequence$flagged[i])))
cat("\n  Reading it: a row where the error rate is near 5% but the check flags\n")
cat("  often is over-cautious; a row where the error rate is far from 5% but\n")
cat("  the check stays quiet is a blind spot.\n")

# ============================================================================
# 4. Do the transformations achieve what they claim?
# ============================================================================
cat("\n[4/5] Transformation effectiveness across the stressor set\n")
shapes_for_tf <- c("skewed", "heavy_tailed", "bimodal", "ceiling",
                   "detection_limit", "one_outlier", "heaped")
methods_for_tf <- c("log10", "log1p", "sqrt", "cbrt", "boxcox", "yeojohnson",
                    "inverse_normal", "winsorise", "zscore")
tf_reps <- max(20, REPS %/% 5)
tf_rows <- list()
for (sh in shapes_for_tf) {
  gen <- Filter(function(z) z$id == sh, SIM_SHAPES)[[1]]
  for (m in methods_for_tf) {
    improved <- 0; applied <- 0; fixed <- 0
    for (s in seq_len(tf_reps)) {
      x <- gen$gen(200, 90000 + s, 1)
      d <- data.frame(x = x)
      r <- apply_transform(d, transform_spec(m, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      applied <- applied + 1
      before <- abs(.as_skewness(x))
      after <- abs(.as_skewness(r$df[[r$added[1]]]))
      if (is.finite(after) && after < before) improved <- improved + 1
      if (check_normality(r$df[[r$added[1]]])$severity == "ok") fixed <- fixed + 1
    }
    tf_rows[[length(tf_rows) + 1L]] <- data.frame(
      shape = sh, method = m,
      applicable = applied / tf_reps,
      reduced_skew = if (applied) improved / applied else NA_real_,
      now_passes_normality = if (applied) fixed / applied else NA_real_,
      stringsAsFactors = FALSE)
  }
  cat("  ", sh, "\n")
}
transforms <- do.call(rbind, tf_rows)
utils::write.csv(transforms, file.path(OUT, "transforms.csv"), row.names = FALSE)
cat("\n  Share of draws where the transform left the data passing normality:\n")
cat(sprintf("  %-16s %s\n", "shape",
            paste(sprintf("%-10s", methods_for_tf), collapse = "")))
for (sh in shapes_for_tf) {
  sub <- transforms[transforms$shape == sh, ]
  vals <- vapply(methods_for_tf, function(m) {
    v <- sub$now_passes_normality[sub$method == m]
    if (!length(v) || is.na(v)) "  -  " else sprintf("%5s", pct(v)) }, character(1))
  cat(sprintf("  %-16s %s\n", sh, paste(sprintf("%-10s", vals), collapse = "")))
}

# ============================================================================
# 5. Robustness sweep: nothing throws, nothing lies
# ============================================================================
cat("\n[5/5] Robustness sweep over degenerate and messy columns\n")
problems <- character(0)
messy <- c(sim_all_numeric(120, 1), sim_degenerate(40))
for (nm in names(messy)) {
  x <- messy[[nm]]
  if (!length(x)) next
  d <- data.frame(x = x)
  for (m in transform_method_ids()) {
    r <- tryCatch(apply_transform(d, transform_spec(m, variable = "x"),
                                  c(x = "continuous")),
                  error = function(e) structure(list(e = conditionMessage(e)),
                                                class = "boom"))
    if (inherits(r, "boom")) { problems <- c(problems, sprintf("THROW %s/%s: %s", m, nm, r$e)); next }
    if (isTRUE(r$ok)) {
      v <- r$df[[r$added[1]]]
      if (is.numeric(v) && any(is.nan(v)))
        problems <- c(problems, sprintf("NaN   %s/%s", m, nm))
      if (all(is.na(v)))
        problems <- c(problems, sprintf("EMPTY %s/%s", m, nm))
    }
  }
  for (f in list(check_normality, function(v) check_autocorrelation(v),
                 function(v) check_zero_inflation(abs(v)))) {
    r <- tryCatch(f(x), error = function(e) structure(list(e = conditionMessage(e)),
                                                      class = "boom"))
    if (inherits(r, "boom")) problems <- c(problems, sprintf("THROW check/%s: %s", nm, r$e))
  }
}
writeLines(if (length(problems)) problems else "none",
           file.path(OUT, "robustness_problems.txt"))
if (length(problems)) {
  cat(sprintf("  %d problem(s):\n", length(problems)))
  for (p in utils::head(problems, 25)) cat("    ", p, "\n")
} else cat("  no throws, no NaNs, no silently empty outputs\n")

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Tables written to ", OUT, "/\n", sep = "")
