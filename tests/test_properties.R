# tests/test_properties.R ----------------------------------------------------
# Property tests: invariants that must hold for EVERY generated dataset, not
# just for a hand-picked fixture.
#
# The distinction from the other suites matters. Those pin specific numbers on
# specific inputs. These assert things that must be true whatever the data looks
# like - that a transform never destroys the original column, that an invertible
# transform always round-trips, that no guard ever throws on a degenerate
# column, that a verdict is always one of the four. Those are the properties a
# user actually relies on, and a single fixture cannot establish them.
#
# Everything is seeded, so a failure is reproducible. Rates are asserted over
# many draws rather than single verdicts, so nothing here is flaky by design.
#
# Run:  Rscript tests/test_properties.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
  source(file.path(here, "simdata.R"))
})

cols <- sim_all_numeric(150, seed = 101)

# ============================================================================
# Assumption checks: total functions
# ============================================================================
ok("no shape generator makes a check throw, and every verdict is valid", {
  for (nm in names(cols)) {
    r <- check_normality(cols[[nm]])
    stopifnot(r$severity %in% ASSUMPTION_SEVERITY,
              is.character(r$note), nzchar(r$note),
              r$check == "normality")
  }
})
ok("no degenerate column makes any check throw", {
  for (nm in names(sim_degenerate(40))) {
    x <- sim_degenerate(40)[[nm]]
    for (f in list(function(v) check_normality(v),
                   function(v) check_zero_inflation(abs(v)),
                   function(v) check_autocorrelation(v),
                   function(v) check_linearity(v, rev(v)),
                   function(v) check_variance_homogeneity(v, rep(c("a","b"), length.out = length(v))))) {
      r <- tryCatch(f(x), error = function(e) structure(list(err = conditionMessage(e)),
                                                        class = "simfail"))
      if (inherits(r, "simfail")) stop(sprintf("%s threw on '%s': %s", "a check", nm, r$err))
      stopifnot(r$severity %in% ASSUMPTION_SEVERITY)
    }
  }
})
ok("a check never claims a verdict it cannot support", {
  # An all-missing or single-value column must be 'untested', never a pass.
  deg <- sim_degenerate(40)
  stopifnot(check_normality(deg$all_missing)$severity == "untested",
            check_normality(deg$single_value)$severity == "untested",
            check_normality(deg$empty)$severity == "untested")
})
ok("every result that is not 'ok' offers something to do about it", {
  for (nm in names(cols)) {
    r <- check_normality(cols[[nm]])
    if (r$severity %in% c("review", "violated")) stopifnot(length(r$remedy) > 0)
    if (r$severity == "ok") stopifnot(length(r$remedy) == 0)
  }
})

# ============================================================================
# Transformations: the four contracts, over every generated shape
# ============================================================================
ok("the additive contract holds for every method on every shape", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in transform_method_ids(c("distribution"))) {
      r <- apply_transform(d, transform_spec(m, variable = "x"),
                           c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      stopifnot(identical(r$df$x, d$x),            # original untouched
                ncol(r$df) == 2, nrow(r$df) == nrow(d))
    }
  }
})
ok("no transformation throws on any generated shape - it refuses in words", {
  bad <- character(0)
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in transform_method_ids()) {
      r <- tryCatch(apply_transform(d, transform_spec(m, variable = "x"),
                                    c(x = "continuous")),
                    error = function(e) structure(list(e = conditionMessage(e)),
                                                  class = "simfail"))
      if (inherits(r, "simfail")) bad <- c(bad, sprintf("%s/%s: %s", m, nm, r$e))
      else stopifnot(is.logical(r$ok), nzchar(r$reason) || isTRUE(r$ok))
    }
  }
  if (length(bad)) stop(paste(utils::head(bad, 5), collapse = " | "))
})
ok("no transformation throws on a degenerate column either", {
  bad <- character(0)
  for (nm in names(sim_degenerate(30))) {
    x <- sim_degenerate(30)[[nm]]
    if (!length(x)) next
    d <- data.frame(x = x)
    for (m in transform_method_ids()) {
      r <- tryCatch(apply_transform(d, transform_spec(m, variable = "x"),
                                    c(x = "continuous")),
                    error = function(e) structure(list(e = conditionMessage(e)),
                                                  class = "simfail"))
      if (inherits(r, "simfail")) bad <- c(bad, sprintf("%s/%s: %s", m, nm, r$e))
    }
  }
  if (length(bad)) stop(paste(utils::head(bad, 5), collapse = " | "))
})
ok("every invertible transform round-trips on every shape it accepts", {
  worst <- 0; worst_at <- ""
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in Filter(function(z) isTRUE(z$invertible), transform_catalogue())) {
      r <- apply_transform(d, transform_spec(m$id, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      back <- transform_inverse(r$record, r$df[[r$added]])
      if (is.null(back)) next                      # correctly refuses (sign lost)
      keep <- is.finite(back) & is.finite(d$x)
      if (sum(keep) < 3) next
      err <- max(abs(back[keep] - d$x[keep])) / max(1, max(abs(d$x[keep])))
      if (err > worst) { worst <- err; worst_at <- sprintf("%s/%s", m$id, nm) }
    }
  }
  if (worst > 1e-6) stop(sprintf("worst relative round-trip error %.3g at %s", worst, worst_at))
})
ok("a transform that reports success never returns an all-missing column", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in transform_method_ids()) {
      r <- apply_transform(d, transform_spec(m, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      v <- r$df[[r$added[1]]]
      if (all(is.na(v)))
        stop(sprintf("%s on '%s' reported success but produced no values", m, nm))
    }
  }
})
ok("a transform's record always accounts for values it lost", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in transform_method_ids()) {
      r <- apply_transform(d, transform_spec(m, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      stopifnot(r$record$n_lost >= 0,
                r$record$n_lost == max(0, r$record$n_in - r$record$n_out))
    }
  }
})

# ============================================================================
# Do the transforms achieve what they claim?
# ============================================================================
ok("log10 reduces right-skew across many independently drawn datasets", {
  worse <- 0
  for (s in 1:60) {
    x <- sim_skewed(200, seed = 900 + s, severity = runif(1, 0.5, 2))
    a <- .as_skewness(x); b <- .as_skewness(log10(x))
    if (!(abs(b) < abs(a))) worse <- worse + 1
  }
  stopifnot(worse == 0)
})
ok("Box-Cox and Yeo-Johnson leave the data no more skewed than they found it", {
  fails <- character(0)
  for (s in 1:25) {
    d <- data.frame(x = sim_skewed(200, seed = 700 + s, severity = 1.4))
    for (m in c("boxcox", "yeojohnson")) {
      r <- apply_transform(d, transform_spec(m, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      a <- abs(.as_skewness(d$x)); b <- abs(.as_skewness(r$df[[r$added]]))
      if (b > a + 1e-8) fails <- c(fails, sprintf("%s seed %d: %.2f -> %.2f", m, s, a, b))
    }
  }
  if (length(fails)) stop(paste(utils::head(fails, 3), collapse = " | "))
})
ok("a z-score always centres and scales, whatever shape it is given", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    r <- apply_transform(d, transform_spec("zscore", variable = "x"), c(x = "continuous"))
    if (!isTRUE(r$ok)) next
    v <- r$df[[r$added]][is.finite(r$df[[r$added]])]
    stopifnot(abs(mean(v)) < 1e-8, abs(stats::sd(v) - 1) < 1e-8)
  }
})
ok("rank-based scores are always monotone in the input", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    for (m in c("rank", "percentile", "inverse_normal")) {
      r <- apply_transform(d, transform_spec(m, variable = "x"), c(x = "continuous"))
      if (!isTRUE(r$ok)) next
      v <- r$df[[r$added]]
      keep <- is.finite(v) & is.finite(d$x)
      if (sum(keep) < 5) next
      stopifnot(stats::cor(v[keep], d$x[keep], method = "spearman") > 0.999)
    }
  }
})
ok("winsorising never changes the sample size; trimming always may", {
  for (nm in names(cols)) {
    d <- data.frame(x = cols[[nm]])
    w <- apply_transform(d, transform_spec("winsorise", variable = "x"), c(x = "continuous"))
    if (isTRUE(w$ok)) stopifnot(w$record$n_lost == 0)
  }
})

# ============================================================================
# Structural checks react to the dial
# ============================================================================
ok("the autocorrelation check tracks the AR coefficient it was given", {
  sev <- vapply(c(0, 0.3, 0.6, 0.9), function(phi) {
    v <- vapply(1:15, function(s)
      .as_rank(check_autocorrelation(sim_autocorrelated(200, 500 + s, phi))$severity),
      numeric(1))
    mean(v)
  }, numeric(1))
  # Monotone in phi, quiet at phi = 0, loud at phi = 0.9.
  stopifnot(all(diff(sev) >= 0), sev[1] < 1.2, sev[4] > 2.5)
})
ok("the sphericity check tracks the severity it was given", {
  eps <- vapply(c(0, 0.5, 2), function(sev) {
    mean(vapply(1:8, function(s) {
      d <- sim_sphericity(120, 3, seed = 300 + s, severity = sev)
      check_sphericity(d$y, d$id, d$condition)$statistic }, numeric(1)))
  }, numeric(1))
  stopifnot(all(diff(eps) < 0), eps[1] > 0.9, eps[3] < 0.75)
})
ok("the zero-inflation check tracks the share of structural zeros", {
  st <- vapply(c(0, 0.2, 0.5), function(z) {
    mean(vapply(1:15, function(s) {
      y <- sim_zero_inflated(300, 400 + s, lambda = 3, extra_zero = z)
      check_zero_inflation(y, rep(3, length(y)))$statistic }, numeric(1)))
  }, numeric(1))
  stopifnot(all(diff(st) > 0), st[1] < 1.25, st[3] > 2)
})
ok("the linearity check tracks how bent the relationship is", {
  sev <- vapply(c(0, 0.3, 1.5), function(k) {
    mean(vapply(1:12, function(s) {
      d <- sim_curved(200, 200 + s, severity = k)
      .as_rank(check_linearity(d$x, d$y)$severity) }, numeric(1)))
  }, numeric(1))
  stopifnot(all(diff(sev) >= 0), sev[1] < 1.3, sev[3] > 2.5)
})
ok("the variance check tracks the variance ratio it was given", {
  sev <- vapply(c(1, 3, 10), function(rr) {
    mean(vapply(1:15, function(s) {
      d <- sim_unequal_variance(240, 600 + s, ratio = rr)
      .as_rank(check_variance_homogeneity(d$x, d$g)$severity) }, numeric(1)))
  }, numeric(1))
  stopifnot(all(diff(sev) >= 0), sev[1] < 1.3, sev[3] > 2.5)
})

# ============================================================================
# False-alarm rates on data that genuinely conforms
# ============================================================================
# These are the numbers that decide whether a threshold is usable. A check that
# flags conforming data more than about one time in ten trains users to ignore
# it, which is worse than not checking at all.
ok("normality on genuinely normal data: false-alarm rate under 10%", {
  flag <- mean(vapply(1:200, function(s)
    check_normality(sim_normal(200, 1000 + s))$severity != "ok", logical(1)))
  if (flag > 0.10) stop(sprintf("false-alarm rate %.1f%%", 100 * flag))
})
ok("normality false-alarm rate stays low across sample sizes", {
  for (n in c(30, 100, 750, 2000)) {
    flag <- mean(vapply(1:60, function(s)
      check_normality(sim_normal(n, 2000 + s))$severity != "ok", logical(1)))
    if (flag > 0.15) stop(sprintf("n = %d: false-alarm rate %.1f%%", n, 100 * flag))
  }
})
ok("sphericity on compound-symmetric data: false-alarm rate under 10%", {
  flag <- mean(vapply(1:40, function(s) {
    d <- sim_sphericity(120, 3, seed = 3000 + s, severity = 0)
    check_sphericity(d$y, d$id, d$condition)$severity != "ok" }, logical(1)))
  if (flag > 0.10) stop(sprintf("false-alarm rate %.1f%%", 100 * flag))
})
ok("independence on independent data: false-alarm rate under 10%", {
  flag <- mean(vapply(1:100, function(s)
    check_autocorrelation(sim_normal(200, 4000 + s))$severity != "ok", logical(1)))
  if (flag > 0.10) stop(sprintf("false-alarm rate %.1f%%", 100 * flag))
})
ok("equal variances on equal-variance data: false-alarm rate under 10%", {
  flag <- mean(vapply(1:80, function(s) {
    d <- sim_unequal_variance(240, 5000 + s, ratio = 1)
    check_variance_homogeneity(d$x, d$g)$severity != "ok" }, logical(1)))
  if (flag > 0.10) stop(sprintf("false-alarm rate %.1f%%", 100 * flag))
})
ok("linearity on straight data: false-alarm rate under 10%", {
  flag <- mean(vapply(1:80, function(s) {
    d <- sim_curved(200, 6000 + s, severity = 0)
    check_linearity(d$x, d$y)$severity != "ok" }, logical(1)))
  if (flag > 0.10) stop(sprintf("false-alarm rate %.1f%%", 100 * flag))
})

# ============================================================================
# Detection rates on data that genuinely violates
# ============================================================================
ok("badly skewed data is detected almost every time", {
  hit <- mean(vapply(1:100, function(s)
    check_normality(sim_skewed(200, 7000 + s, severity = 1.5))$severity == "violated",
    logical(1)))
  if (hit < 0.90) stop(sprintf("detection rate %.1f%%", 100 * hit))
})
ok("heavy tails are detected", {
  hit <- mean(vapply(1:100, function(s)
    check_normality(sim_heavy_tailed(200, 8000 + s, 1))$severity != "ok", logical(1)))
  if (hit < 0.70) stop(sprintf("detection rate %.1f%%", 100 * hit))
})
ok("strong autocorrelation is detected", {
  hit <- mean(vapply(1:60, function(s)
    check_autocorrelation(sim_autocorrelated(200, 9000 + s, 0.8))$severity == "violated",
    logical(1)))
  if (hit < 0.90) stop(sprintf("detection rate %.1f%%", 100 * hit))
})

report_and_exit()
