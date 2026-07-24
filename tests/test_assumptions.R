# tests/test_assumptions.R --------------------------------------------------
# Phase 2 assumption / distribution checks (fct_assumptions.R). These drive the
# test routing, so their verdicts are pinned on data with a KNOWN answer, not
# just checked for shape. qnorm(ppoints(n)) gives an ideal, deterministic
# normal sample (no RNG, reproducible across R versions).
#
# Run from the project root:  Rscript tests/test_assumptions.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

near <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol
normal_sample <- function(n) qnorm(ppoints(n))     # deterministic, ~perfectly normal

cat("\n== p_fmt() ==\n")
ok("p_fmt(NA) -> 'NA'",          stopifnot(identical(p_fmt(NA_real_), "NA")))
ok("p_fmt(tiny) -> '< 0.001'",   stopifnot(identical(p_fmt(0.0004), "< 0.001")))
ok("p_fmt(0.5) -> '0.500'",      stopifnot(identical(p_fmt(0.5), "0.500")))
ok("p_fmt(0.0498) -> '0.050'",   stopifnot(identical(p_fmt(0.0498), "0.050")))

cat("\n== check_normality() ==\n")
ok("normal sample -> ok TRUE, Shapiro-Wilk", {
  r <- check_normality(normal_sample(60))
  stopifnot(isTRUE(r$ok), identical(r$test, "Shapiro-Wilk"), r$p >= 0.05,
            is.character(r$note))
})
ok("strongly non-normal -> ok FALSE", {
  r <- check_normality(c(rep(0, 40), rep(1, 40), 1000))
  stopifnot(isFALSE(r$ok), r$p < 0.05)
})
ok("n < 3 -> ok NA, test 'none'", {
  r <- check_normality(c(1, 2))
  stopifnot(is.na(r$ok), identical(r$test, "none"))
})
ok("p matches stats::shapiro.test exactly", {
  x <- normal_sample(50)
  stopifnot(near(check_normality(x)$p, shapiro.test(x)$p.value))
})

cat("\n== check_normality_by_group() ==\n")
ok("all groups normal -> ok TRUE", {
  x <- c(normal_sample(30), normal_sample(30) + 5)
  g <- rep(c("a", "b"), each = 30)
  r <- check_normality_by_group(x, g)
  stopifnot(isTRUE(r$ok), length(r$by_group) == 2)
})
ok("one non-normal group -> ok FALSE (AND across groups)", {
  x <- c(normal_sample(30), rep(0, 29), 500)
  g <- rep(c("a", "b"), each = 30)
  stopifnot(isFALSE(check_normality_by_group(x, g)$ok))
})

cat("\n== check_variance_homogeneity() (Levene, median-centred) ==\n")
ok("equal variances -> ok TRUE", {
  x <- c(normal_sample(30), normal_sample(30) + 5)   # same spread, shifted mean
  g <- rep(c("a", "b"), each = 30)
  r <- check_variance_homogeneity(x, g)
  stopifnot(isTRUE(r$ok), r$p >= 0.05)
})
ok("very unequal variances -> ok FALSE (prefer Welch)", {
  x <- c(normal_sample(30), normal_sample(30) * 6)
  g <- rep(c("a", "b"), each = 30)
  stopifnot(isFALSE(check_variance_homogeneity(x, g)$ok))
})
ok("fewer than two groups -> ok NA", {
  r <- check_variance_homogeneity(normal_sample(20), rep("a", 20))
  stopifnot(is.na(r$ok))
})

cat("\n== check_expected_counts() (chi-square vs exact) ==\n")
ok("large balanced table -> adequate (ok TRUE)", {
  r <- check_expected_counts(matrix(c(50, 50, 50, 50), 2))
  stopifnot(isTRUE(r$ok), near(r$min_expected, 50), r$prop_low == 0)
})
ok("sparse table -> inadequate (ok FALSE)", {
  r <- check_expected_counts(matrix(c(1, 2, 2, 1), 2))  # all expected = 1.5
  stopifnot(isFALSE(r$ok), r$prop_low == 1, near(r$min_expected, 1.5))
})

report_and_exit()
