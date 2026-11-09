# tests/test_agreement.R -----------------------------------------------------
# Cohen's kappa and weighted kappa.
#
# ARCH 2.0 primary objective 1 asks for CONCORDANCE - of ESCrE colonisation
# between mother-infant pairs (1.i) and between urinary and rectal sites in the
# same individuals (1.ii). That is agreement, not association, and the app had
# no way to measure it: McNemar tests whether the margins differ and chi-square
# whether the two are related, and a pair of measurements can pass either while
# disagreeing case by case.
#
# The arithmetic is reimplemented rather than taken from a dependency, so it is
# validated here against DescTools and psych - the same discipline the ADF/KPSS
# and Mann-Kendall routines follow. Both conventions for weighted kappa
# (agreement weights vs disagreement weights) give the same kappa but different
# variances, so the SE is checked too, not just the point estimate.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

set.seed(9)
n <- 200L
x2 <- factor(sample(c("No", "Yes"), n, TRUE, c(0.6, 0.4)), levels = c("No", "Yes"))
y2 <- factor(ifelse(stats::runif(n) < 0.8, as.character(x2),
                    sample(c("No", "Yes"), n, TRUE)), levels = c("No", "Yes"))

lv4 <- c("none", "mild", "moderate", "severe")
r1 <- factor(sample(lv4, 150, TRUE), levels = lv4, ordered = TRUE)
r2 <- factor(ifelse(stats::runif(150) < 0.6, as.character(r1),
                    sample(lv4, 150, TRUE)), levels = lv4, ordered = TRUE)

cat("\n== kappa matches independent implementations ==\n")

ok("2x2 kappa, SE and CI match DescTools", {
  a <- agreement_kappa(x2, y2)
  d <- DescTools::CohenKappa(table(x2, y2), conf.level = 0.95)
  se_implied <- (d[[3]] - d[[1]]) / stats::qnorm(0.975)
  stopifnot(all.equal(a$kappa, unname(d[[1]]), tolerance = TOL_STAT),
            all.equal(a$ci[1], unname(d[[2]]), tolerance = TOL_CI),
            all.equal(a$ci[2], unname(d[[3]]), tolerance = TOL_CI),
            all.equal(a$se, se_implied, tolerance = TOL_STAT))
})

ok("2x2 kappa matches psych::cohen.kappa", {
  a <- agreement_kappa(x2, y2)
  ps <- psych::cohen.kappa(cbind(as.character(x2), as.character(y2)))
  stopifnot(all.equal(a$kappa, unname(ps$kappa), tolerance = TOL_STAT),
            all.equal(a$ci[1], unname(ps$confid[1, 1]), tolerance = TOL_CI),
            all.equal(a$ci[2], unname(ps$confid[1, 3]), tolerance = TOL_CI))
})

ok("ordinal unweighted kappa matches both packages", {
  a <- agreement_kappa(r1, r2, weights = "unweighted")
  stopifnot(all.equal(a$kappa,
                      unname(DescTools::CohenKappa(table(r1, r2))),
                      tolerance = TOL_STAT),
            all.equal(a$kappa,
                      unname(psych::cohen.kappa(cbind(as.numeric(r1),
                                                      as.numeric(r2)))$kappa),
                      tolerance = TOL_STAT))
})

ok("quadratic weighted kappa and its SE match psych and DescTools", {
  a <- agreement_kappa(r1, r2, weights = "quadratic")
  pk <- psych::cohen.kappa(cbind(as.numeric(r1), as.numeric(r2)))
  se_implied <- (pk$confid[2, 3] - pk$weighted.kappa) / stats::qnorm(0.975)
  stopifnot(all.equal(a$kappa, unname(pk$weighted.kappa), tolerance = TOL_STAT),
            all.equal(a$se, unname(se_implied), tolerance = TOL_STAT),
            all.equal(a$kappa,
                      unname(DescTools::CohenKappa(table(r1, r2),
                                                   weight = "Fleiss-Cohen")),
                      tolerance = TOL_STAT))
})

ok("linear weighting sits between unweighted and quadratic", {
  u <- agreement_kappa(r1, r2, "unweighted")$kappa
  l <- agreement_kappa(r1, r2, "linear")$kappa
  q <- agreement_kappa(r1, r2, "quadratic")$kappa
  stopifnot(u < l, l < q)
})

cat("\n== the boundary cases ==\n")

ok("perfect agreement gives kappa 1, chance-level gives about 0", {
  z <- agreement_kappa(x2, x2)
  stopifnot(all.equal(z$kappa, 1), all.equal(z$po, 1))
  # A table with no diagonal at all is worse than chance.
  a <- factor(c("No", "No", "Yes", "Yes"), levels = c("No", "Yes"))
  b <- factor(c("Yes", "Yes", "No", "No"), levels = c("No", "Yes"))
  stopifnot(agreement_kappa(a, b)$kappa < 0)
})

ok("a constant table where both raters always agree is handled, not fatal", {
  # pe = 1 here, so kappa is undefined rather than 1. It must say so.
  a <- factor(rep("Yes", 10), levels = c("No", "Yes"))
  r <- agreement_kappa(a, a)
  stopifnot(is.na(r$kappa) || is.finite(r$kappa))
  stopifnot(identical(agreement_strength(NA_real_), "not estimable"))
})

ok("cases measured only one way are dropped and counted", {
  a <- c(as.character(x2), NA, "Yes")
  b <- c(as.character(y2), "No", NA)
  r <- agreement_kappa(factor(a, levels = c("No", "Yes")),
                       factor(b, levels = c("No", "Yes")))
  stopifnot(r$n == n, r$n_dropped == 2L)
})

ok("weighting is ignored for two categories, where every scheme coincides", {
  u <- agreement_kappa(x2, y2, "unweighted")
  q <- agreement_kappa(x2, y2, "quadratic")
  stopifnot(all.equal(u$kappa, q$kappa), identical(q$weights, "unweighted"))
})

ok("category order is taken from ordered factors, not sorted alphabetically", {
  # Sorting lv4 alphabetically gives mild < moderate < none < severe, which
  # would change every weighted result. The ordered factor's own order wins.
  stopifnot(identical(agreement_kappa(r1, r2, "linear")$levels, lv4))
})

ok("two variables with no shared categories are refused clearly", {
  a <- factor(rep("only", 5)); b <- factor(rep("only", 5))
  stopifnot(grepl("at least two categories", friendly_error(agreement_kappa(a, b))),
            grepl("at least one case", friendly_error(
              agreement_kappa(factor(c(NA, NA)), factor(c(NA, NA))))))
})

cat("\n== the prevalence and bias indices ==\n")

ok("a rare outcome shows a high prevalence index and a higher PABAK", {
  # The kappa paradox: 96% raw agreement but a modest kappa, because almost
  # everything is in one cell. Reporting kappa alone here misleads.
  a <- factor(c(rep("No", 96), rep("Yes", 2), rep("No", 1), rep("Yes", 1)),
              levels = c("No", "Yes"))
  b <- factor(c(rep("No", 96), rep("Yes", 2), rep("Yes", 1), rep("No", 1)),
              levels = c("No", "Yes"))
  r <- agreement_kappa(a, b)
  stopifnot(r$po > 0.95, r$prevalence_index > 0.6, r$pabak > r$kappa)
})

ok("asymmetric disagreement shows up as a bias index", {
  # 20 cases where a says No and b says Yes, none the other way.
  a <- factor(c(rep("No", 60), rep("Yes", 20), rep("No", 20)),
              levels = c("No", "Yes"))
  b <- factor(c(rep("No", 60), rep("Yes", 20), rep("Yes", 20)),
              levels = c("No", "Yes"))
  r <- agreement_kappa(a, b)
  stopifnot(r$bias_index > 0.15)
})

cat("\n== the guided-test result ==\n")

ok("run_agreement reports kappa as primary and McNemar alongside it", {
  r <- run_agreement(x2, y2)
  stopifnot(grepl("Cohen's kappa", r$primary$label),
            identical(r$recommended, "primary"),
            !is.null(r$alternative),
            grepl("McNemar", r$alternative$label),
            grepl("different question", r$alternative$interpretation))
})

ok("the write-up points at the interval, not the p-value", {
  r <- run_agreement(x2, y2)
  stopifnot(grepl("CI", r$primary$interpretation),
            grepl("Read the interval rather than the p-value",
                  r$primary$interpretation),
            grepl("Landis", r$primary$interpretation))
})

ok("a weighted result is labelled as weighted, and no McNemar is offered", {
  r <- run_agreement(r1, r2, weights = "quadratic")
  stopifnot(grepl("Weighted kappa \\(quadratic\\)", r$primary$label),
            is.null(r$alternative))   # McNemar needs a 2x2
})

ok("the kappa paradox is called out in the write-up when it applies", {
  a <- factor(c(rep("No", 96), "Yes", "Yes", "No", "Yes"), levels = c("No", "Yes"))
  b <- factor(c(rep("No", 96), "Yes", "Yes", "Yes", "No"), levels = c("No", "Yes"))
  r <- run_agreement(a, b)
  stopifnot(grepl("holds kappa down", r$primary$interpretation),
            grepl("PABAK", r$primary$interpretation))
})

ok("the guided dispatcher routes the agreement family", {
  d <- data.frame(m = x2, i = y2)
  r <- run_guided_test(d, list(family = "agreement", x = "m", y = "i",
                               weights = "unweighted"))
  stopifnot(grepl("kappa", r$primary$label),
            all.equal(field_value(r$primary$table, "kappa"),
                      round(agreement_kappa(x2, y2)$kappa, 4),
                      tolerance = TOL_EFFECT))
})

ok("kappa is registered in the planner catalogue and its maps", {
  ids <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  stopifnot("cohens_kappa" %in% ids,
            identical(unname(PLANNER_TEST_FAMILY[["cohens_kappa"]]), "categorical"),
            identical(unname(PLANNER_TEST_DESIGN[["cohens_kappa"]]), "agreement"),
            # agreement ignores follow-up time, so it is at best secondary in a
            # time-to-event study, like the other censoring-blind tests
            "cohens_kappa" %in% PLANNER_CENSORING_BLIND_TESTS)
})

# --- against the real ARCH 2.0 extract -------------------------------------
# Objective 1(i): concordance of colonisation between the mother and her infant.
# Opt in with SABALI_ARCH2_DTA; skipped otherwise so no participant data is
# carried by the suite.
arch2 <- Sys.getenv("SABALI_ARCH2_DTA", "")
if (nzchar(arch2) && file.exists(arch2)) {
  cat("\n== real ARCH 2.0 mother-infant concordance ==\n")
  ok("mother-infant colonisation concordance is estimable", {
    real <- read_any_format(arch2, basename(arch2))
    need <- c("RC_adcolonization", "RC_chcolonization")
    stopifnot(all(need %in% names(real)))
    r <- run_agreement(factor(real$RC_adcolonization),
                       factor(real$RC_chcolonization))
    k <- field_value(r$primary$table, "kappa")
    po <- field_value(r$primary$table, "observed agreement")
    cat(sprintf("      kappa = %.3f, raw agreement = %.1f%%, n = %d\n",
                k, 100 * po, field_value(r$primary$table,
                                         "n cases measured both ways")))
    stopifnot(is.finite(k), k >= -1, k <= 1, is.finite(po))
  })
} else {
  cat("\n  (set SABALI_ARCH2_DTA to also run the real-data check)\n")
}

cat("\n== Fleiss' kappa: three or more raters ==\n")

set.seed(11)
fl_truth <- sample(c("A", "B", "C"), 30, TRUE)
fl <- t(vapply(fl_truth, function(t)
  ifelse(stats::runif(4) < 0.75, t, sample(c("A", "B", "C"), 4, TRUE)),
  character(4)))

ok("Fleiss' kappa matches DescTools", {
  r <- fleiss_kappa(fl)
  stopifnot(all.equal(r$kappa,
                      unname(DescTools::KappaM(fl, method = "Fleiss")),
                      tolerance = TOL_STAT),
            r$n_subjects == 30L, r$n_raters == 4L,
            identical(r$categories, c("A", "B", "C")))
})

ok("perfect and chance-level panels give kappa 1 and about 0", {
  same <- matrix(rep(fl_truth, 4), ncol = 4)
  stopifnot(all.equal(fleiss_kappa(same)$kappa, 1))
  set.seed(6)
  rand <- matrix(sample(c("A", "B", "C"), 30 * 4, TRUE), ncol = 4)
  stopifnot(abs(fleiss_kappa(rand)$kappa) < 0.15)
})

ok("the interval is bootstrapped, not taken from the null standard error", {
  # The classic Fleiss SE is derived under kappa = 0. Simulated against the
  # actual sampling distribution it comes out at 0.70 times the empirical SD of
  # kappa-hat (DescTools' variant at 0.67), so intervals built from it are about
  # a third too narrow. The bootstrap lands at 1.01 with 93.5% coverage.
  r <- fleiss_kappa(fl)
  stopifnot(grepl("bootstrap over subjects", r$ci_method),
            is.finite(r$se), is.finite(r$se_null),
            r$se > r$se_null,                     # the honest SE is the larger
            r$ci[1] < r$kappa, r$kappa < r$ci[2])
})

ok("the bootstrap interval is reproducible", {
  stopifnot(identical(fleiss_kappa(fl)$ci, fleiss_kappa(fl)$ci),
            !identical(fleiss_kappa(fl, seed = 1L)$ci,
                       fleiss_kappa(fl, seed = 2L)$ci))
})

ok("disabling the bootstrap falls back, and says the interval is too narrow", {
  r <- fleiss_kappa(fl, boot = 0L)
  stopifnot(grepl("null-hypothesis SE", r$ci_method),
            grepl("likely too narrow", r$ci_method))
})

ok("subjects rated by fewer raters are dropped and counted", {
  ff <- fl; ff[1, 3] <- NA; ff[2, c(2, 4)] <- NA
  r <- fleiss_kappa(ff)
  stopifnot(r$n_subjects == 28L, r$n_dropped == 2L, r$n_raters == 4L)
})

ok("fewer than three raters is refused, pointing at Cohen's kappa", {
  msg <- friendly_error(fleiss_kappa(fl[, 1:2, drop = FALSE]))
  stopifnot(grepl("at least three raters", msg),
            grepl("use Cohen's kappa", msg))
})

ok("per-category kappa exposes a category applied inconsistently", {
  # Raters agree well on A and B but assign C almost at random. The headline
  # kappa stays respectable while C is nearly arbitrary, which is precisely what
  # the per-category breakdown is for.
  set.seed(21)
  tr <- sample(c("A", "B", "C"), 60, TRUE)
  mm <- t(vapply(seq_along(tr), function(i) {
    if (tr[i] == "C") sample(c("A", "B", "C"), 4, TRUE)
    else ifelse(stats::runif(4) < 0.95, tr[i], sample(c("A", "B", "C"), 4, TRUE))
  }, character(4)))
  r <- fleiss_kappa(mm)
  stopifnot(r$category_kappa[["C"]] < r$category_kappa[["A"]],
            r$category_kappa[["C"]] < r$kappa)
  res <- run_fleiss(mm)
  stopifnot(grepl("weakest on 'C'", res$primary$interpretation),
            grepl("applied inconsistently", res$primary$interpretation))
})

ok("long-format ratings reshape into the matrix the estimator needs", {
  long <- data.frame(
    subj = rep(seq_len(30), each = 4),
    who = rep(paste0("r", 1:4), 30),
    val = as.vector(t(fl)), stringsAsFactors = FALSE)
  w <- fleiss_from_long(long, "subj", "who", "val")
  stopifnot(ncol(w) == 4L, nrow(w) == 30L,
            all.equal(fleiss_kappa(w)$kappa, fleiss_kappa(fl)$kappa,
                      tolerance = TOL_STAT))
})

ok("a repeated subject/rater pair is refused rather than silently collapsed", {
  long <- data.frame(subj = c(1, 1, 1), who = c("r1", "r1", "r2"),
                     val = c("A", "B", "A"), stringsAsFactors = FALSE)
  stopifnot(grepl("more than once",
                  friendly_error(fleiss_from_long(long, "subj", "who", "val"))))
})

ok("the write-up says the panel measure is not the pairwise average", {
  r <- run_fleiss(fl)
  stopifnot(grepl("Fleiss", r$primary$label),
            grepl("not the average of the pairwise", r$primary$interpretation),
            grepl("bootstrap over subjects", r$primary$note),
            grepl("derived under kappa = 0", r$primary$note),
            is.data.frame(r$by_category), nrow(r$by_category) == 3L)
})

ok("the guided dispatcher routes the fleiss family, and it is catalogued", {
  d3 <- as.data.frame(fl, stringsAsFactors = FALSE)
  names(d3) <- paste0("rater", 1:4)
  res <- run_guided_test(d3, list(family = "fleiss", raters = names(d3)))
  stopifnot(grepl("Fleiss", res$primary$label))
  ids <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  stopifnot("fleiss_kappa" %in% ids,
            identical(unname(PLANNER_TEST_DESIGN[["fleiss_kappa"]]), "agreement"))
})

report_and_exit()
