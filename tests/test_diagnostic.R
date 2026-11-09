# tests/test_diagnostic.R ----------------------------------------------------
# Diagnostic and screening test accuracy.
#
# This is a DIRECTIONAL question, unlike agreement: one variable is the reference
# standard and the other is judged against it, so swapping them is a different
# analysis, not the same one relabelled. Sensitivity and specificity are
# properties of the test; predictive values are properties of the test AND the
# prevalence where it is used - the commonest misreading in the field, and the
# reason predictive values are recomputed across a range of prevalences here.
#
# The arithmetic is reimplemented rather than taken from a dependency, so it is
# validated against pROC (AUC) and DescTools (Wilson intervals).

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# A test with known operating characteristics, built from an explicit 2x2 so the
# expected values are arithmetic rather than simulation.
#   TP = 80, FN = 20  -> sensitivity 0.80
#   FP = 30, TN = 270 -> specificity 0.90
mk <- function(tp, fp, fn, tn) {
  data.frame(
    test = factor(rep(c("Pos", "Pos", "Neg", "Neg"), c(tp, fp, fn, tn)),
                  levels = c("Neg", "Pos")),
    ref  = factor(rep(c("Pos", "Neg", "Pos", "Neg"), c(tp, fp, fn, tn)),
                  levels = c("Neg", "Pos")),
    stringsAsFactors = FALSE)
}
d <- mk(80, 30, 20, 270)

cat("\n== the 2x2 measures ==\n")

ok("sensitivity, specificity and accuracy are exact", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  stopifnot(a$tp == 80, a$fp == 30, a$fn == 20, a$tn == 270, a$n == 400,
            all.equal(a$sensitivity[["estimate"]], 0.80),
            all.equal(a$specificity[["estimate"]], 0.90),
            all.equal(a$accuracy[["estimate"]], 350 / 400),
            all.equal(a$prevalence[["estimate"]], 100 / 400))
})

ok("predictive values and likelihood ratios are exact", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  stopifnot(all.equal(a$ppv[["estimate"]], 80 / 110),
            all.equal(a$npv[["estimate"]], 270 / 290),
            all.equal(a$lr_positive[["estimate"]], 0.80 / 0.10),
            all.equal(a$lr_negative[["estimate"]], 0.20 / 0.90),
            all.equal(a$youden, 0.70))
})

ok("Wilson intervals match DescTools", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  ref <- DescTools::BinomCI(80, 100, method = "wilson")
  stopifnot(all.equal(a$sensitivity[["estimate"]], unname(ref[1]), tolerance = TOL_STAT),
            all.equal(a$sensitivity[["lower"]], unname(ref[2]), tolerance = TOL_STAT),
            all.equal(a$sensitivity[["upper"]], unname(ref[3]), tolerance = TOL_STAT))
})

ok("a Wilson interval stays inside 0 and 1 where Wald would not", {
  # 20/20 is exactly where a screening test usually sits and exactly where the
  # normal approximation fails: Wald gives a zero-width interval at [1, 1].
  # .prop_est_ci() wraps the shared wilson_ci() rather than redefining it.
  w <- .prop_est_ci(20, 20)
  stopifnot(w[["estimate"]] == 1, w[["lower"]] < 1, w[["lower"]] > 0.8,
            w[["upper"]] == 1)
  z <- .prop_est_ci(0, 20)
  # The analytic lower bound at 0 successes is 0; floating point lands a shade
  # above it, so this asserts "at the boundary", not bitwise equality.
  stopifnot(z[["estimate"]] == 0, z[["upper"]] > 0, z[["upper"]] < 0.2,
            z[["lower"]] < 1e-8)
})

cat("\n== direction matters ==\n")

ok("swapping test and reference is a different analysis, not a relabelling", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  b <- diagnostic_accuracy(d$ref, d$test, positive = "Pos")
  # sensitivity of A-vs-B is the PPV of B-vs-A; they are not the same number
  stopifnot(!isTRUE(all.equal(a$sensitivity[["estimate"]],
                              b$sensitivity[["estimate"]])),
            all.equal(a$sensitivity[["estimate"]], b$ppv[["estimate"]]))
})

ok("choosing the wrong positive value swaps sensitivity and specificity", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  b <- diagnostic_accuracy(d$test, d$ref, positive = "Neg")
  stopifnot(all.equal(a$sensitivity[["estimate"]], b$specificity[["estimate"]]),
            all.equal(a$specificity[["estimate"]], b$sensitivity[["estimate"]]))
})

cat("\n== predictive values depend on prevalence ==\n")

ok("PPV falls sharply at low prevalence, as Bayes requires", {
  # sens 0.80, spec 0.90 gives PPV 0.727 in this 25%-prevalence sample but only
  # 0.0079 in a 0.1%-prevalence screening programme. Quoting the first as though
  # it were the second is the misreading this table exists to prevent.
  pv_here <- predictive_values_at(0.80, 0.90, 0.25)
  pv_screen <- predictive_values_at(0.80, 0.90, 0.001)
  stopifnot(all.equal(unname(pv_here[["ppv"]]), 80 / 110, tolerance = 1e-6),
            pv_screen[["ppv"]] < 0.01, pv_screen[["npv"]] > 0.999)
})

ok("the reported PPV at the observed prevalence equals the sample PPV", {
  a <- diagnostic_accuracy(d$test, d$ref, positive = "Pos")
  pv <- predictive_values_at(a$sensitivity[["estimate"]],
                             a$specificity[["estimate"]],
                             a$prevalence[["estimate"]])
  stopifnot(all.equal(unname(pv[["ppv"]]), a$ppv[["estimate"]], tolerance = 1e-8))
})

ok("an impossible prevalence is refused", {
  stopifnot(grepl("between 0 and 1",
                  friendly_error(predictive_values_at(0.8, 0.9, 1.5))))
})

cat("\n== ROC and AUC ==\n")

set.seed(5)
n <- 500L
roc_ref <- factor(stats::rbinom(n, 1, 0.3), levels = c(0, 1),
                  labels = c("Neg", "Pos"))
roc_score <- stats::rnorm(n, ifelse(roc_ref == "Pos", 1.2, 0))

ok("AUC matches pROC", {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    cat("      (pROC not installed; comparing against the rank identity instead)\n")
    r <- roc_curve(roc_score, roc_ref, positive = "Pos")
    u <- stats::wilcox.test(roc_score[roc_ref == "Pos"],
                            roc_score[roc_ref == "Neg"])$statistic
    stopifnot(all.equal(r$auc, unname(u) / (sum(roc_ref == "Pos") *
                                              sum(roc_ref == "Neg")),
                        tolerance = TOL_STAT))
  } else {
    r <- roc_curve(roc_score, roc_ref, positive = "Pos")
    p <- pROC::roc(roc_ref, roc_score, levels = c("Neg", "Pos"),
                   direction = "<", quiet = TRUE)
    stopifnot(all.equal(r$auc, as.numeric(pROC::auc(p)), tolerance = TOL_STAT))
  }
})

ok("AUC is the probability a positive outscores a negative", {
  r <- roc_curve(roc_score, roc_ref, positive = "Pos")
  pos <- roc_score[roc_ref == "Pos"]; neg <- roc_score[roc_ref == "Neg"]
  manual <- mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "=="))
  stopifnot(all.equal(r$auc, manual, tolerance = TOL_STAT))
})

ok("a useless score gives AUC about 0.5 and a perfect one gives 1", {
  set.seed(3)
  noise <- stats::rnorm(n)
  stopifnot(abs(roc_curve(noise, roc_ref, positive = "Pos")$auc - 0.5) < 0.06)
  perfect <- ifelse(roc_ref == "Pos", 1, 0)
  stopifnot(all.equal(roc_curve(perfect, roc_ref, positive = "Pos")$auc, 1))
})

ok("direction is honoured, and reverses the AUC about 0.5", {
  up <- roc_curve(roc_score, roc_ref, positive = "Pos", direction = ">")
  dn <- roc_curve(roc_score, roc_ref, positive = "Pos", direction = "<")
  stopifnot(all.equal(up$auc, 1 - dn$auc, tolerance = TOL_STAT),
            grepl("direction must be", friendly_error(
              roc_curve(roc_score, roc_ref, positive = "Pos", direction = "up"))))
})

ok("the best threshold maximises Youden's J and is on the original scale", {
  r <- roc_curve(roc_score, roc_ref, positive = "Pos")
  stopifnot(all.equal(r$best_youden, max(r$curve$youden)),
            r$best_threshold >= min(roc_score),
            r$best_threshold <= max(roc_score),
            all.equal(r$best_youden,
                      r$best_sensitivity + r$best_specificity - 1))
})

cat("\n== the guards ==\n")

ok("more than two values across test and reference is refused by name", {
  dd <- d; levels(dd$test) <- c("Neg", "Pos"); dd$test <- as.character(dd$test)
  dd$test[1] <- "Indeterminate"
  msg <- friendly_error(diagnostic_accuracy(dd$test, dd$ref))
  stopifnot(grepl("exactly two values", msg), grepl("Indeterminate", msg))
})

ok("a positive value that does not occur is refused", {
  stopifnot(grepl("not one of the observed values",
                  friendly_error(diagnostic_accuracy(d$test, d$ref,
                                                     positive = "Maybe"))))
})

ok("cases missing either variable are dropped and counted", {
  dd <- rbind(d, data.frame(test = factor(c(NA, "Pos"), levels = c("Neg", "Pos")),
                            ref = factor(c("Pos", NA), levels = c("Neg", "Pos"))))
  a <- diagnostic_accuracy(dd$test, dd$ref, positive = "Pos")
  stopifnot(a$n == 400L, a$n_dropped == 2L)
})

ok("an empty cell is reported rather than hidden", {
  a <- diagnostic_accuracy(mk(50, 0, 10, 100)$test, mk(50, 0, 10, 100)$ref,
                           positive = "Pos")
  stopifnot(a$fp == 0, is.infinite(a$lr_positive[["estimate"]]),
            a$specificity[["estimate"]] == 1,
            a$specificity[["lower"]] < 1)      # not a zero-width interval
  r <- run_diagnostic(mk(50, 0, 10, 100)$test, mk(50, 0, 10, 100)$ref,
                      positive = "Pos")
  stopifnot(grepl("empty", r$primary$interpretation),
            grepl("not evidence that the true rate is 0 or 1",
                  r$primary$interpretation))
})

ok("an ROC needs both a positive and a negative case", {
  # The factor DECLARES two levels but only one occurs, so the check has to look
  # at observed values - a declared-but-empty level is not a case.
  allpos <- factor(rep("Pos", 10), levels = c("Neg", "Pos"))
  msg <- friendly_error(roc_curve(stats::rnorm(10), allpos))
  stopifnot(grepl("exactly two observed values", msg), grepl("Pos", msg))
})

ok("a declared-but-unobserved level does not block the analysis", {
  # e.g. a subset filter left an 'Indeterminate' level with no rows behind it.
  dd <- d
  dd$test <- factor(as.character(dd$test),
                    levels = c("Neg", "Pos", "Indeterminate"))
  a <- diagnostic_accuracy(dd$test, dd$ref, positive = "Pos")
  stopifnot(a$n == 400L, all.equal(a$sensitivity[["estimate"]], 0.80))
})

cat("\n== the guided-test result ==\n")

ok("the write-up separates test properties from population properties", {
  r <- run_diagnostic(d$test, d$ref, positive = "Pos")
  stopifnot(grepl("properties of the test", r$primary$interpretation),
            grepl("do not transfer", r$primary$interpretation),
            grepl("Wilson", r$primary$note))
})

ok("predictive values are tabulated across prevalences", {
  r <- run_diagnostic(d$test, d$ref, positive = "Pos")
  pv <- r$predictive_values
  stopifnot(is.data.frame(pv), nrow(pv) >= 5L,
            all(c("Prevalence", "PPV", "NPV") %in% names(pv)),
            # PPV must rise with prevalence, NPV must fall
            !is.unsorted(pv$PPV), !is.unsorted(rev(pv$NPV)))
})

ok("the guided dispatcher routes the diagnostic family", {
  res <- run_guided_test(d, list(family = "diagnostic", test = "test",
                                 reference = "ref", positive = "Pos"))
  stopifnot(grepl("Diagnostic accuracy", res$primary$label),
            all.equal(field_value(res$primary$table, "sensitivity"), 0.80,
                      tolerance = TOL_EFFECT))
})

ok("it is registered in the planner catalogue", {
  ids <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  stopifnot("diagnostic_accuracy" %in% ids,
            identical(unname(PLANNER_TEST_FAMILY[["diagnostic_accuracy"]]),
                      "diagnostic"))
})

report_and_exit()
