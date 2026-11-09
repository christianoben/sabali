# tests/test_epi_measures.R --------------------------------------------------
# Effect measures for a binary outcome, and the trend test for an ordered
# exposure.
#
# A chi-square test says whether two groups differ; it does not say by how much,
# and in public health "by how much" is the question. The same 2x2 table gives a
# risk difference, a risk ratio, an odds ratio and an NNT - different questions,
# not interchangeable, and they diverge sharply when the outcome is common. The
# app reports odds ratios from every logistic model, so it needs to be able to
# report the risk measures too, and to say when the two part company.
#
# The fixtures are built from explicit 2x2 counts so every expected value is
# arithmetic rather than simulation, and the intervals are cross-checked against
# DescTools and against the textbook formulae computed by hand.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# exposed 40/200 (20%), reference 20/200 (10%)
#   RD = 0.10   RR = 2.00   OR = (40*180)/(160*20) = 2.25   NNT = 10
mk2 <- function(a, b, c_, d_) data.frame(
  out = factor(rep(c("Yes", "No", "Yes", "No"), c(a, b, c_, d_)),
               levels = c("No", "Yes")),
  grp = factor(rep(c("Exposed", "Reference"), c(a + b, c_ + d_)),
               levels = c("Reference", "Exposed")),
  stringsAsFactors = FALSE)
d <- mk2(40, 160, 20, 180)
eff <- function(dd = d, ...) two_group_effects(dd$out, dd$grp, positive = "Yes",
                                               exposed = "Exposed", ...)

cat("\n== the four effect measures ==\n")

ok("risks, risk difference, risk ratio and odds ratio are exact", {
  e <- eff()
  stopifnot(e$a == 40, e$b == 160, e$c == 20, e$d == 180, e$n == 400,
            all.equal(e$risk_exposed[["estimate"]], 0.20),
            all.equal(e$risk_reference[["estimate"]], 0.10),
            all.equal(e$risk_difference[["estimate"]], 0.10),
            all.equal(e$risk_ratio[["estimate"]], 2.00),
            all.equal(e$odds_ratio[["estimate"]], 2.25))
})

ok("the odds ratio interval matches DescTools", {
  e <- eff()
  ref <- DescTools::OddsRatio(matrix(c(40, 160, 20, 180), 2, byrow = TRUE),
                              conf.level = 0.95)
  stopifnot(all.equal(e$odds_ratio[["estimate"]], unname(ref[1]), tolerance = TOL_STAT),
            all.equal(e$odds_ratio[["lower"]], unname(ref[2]), tolerance = TOL_CI),
            all.equal(e$odds_ratio[["upper"]], unname(ref[3]), tolerance = TOL_CI))
})

ok("the risk-ratio interval is the textbook Katz interval", {
  # SE(log RR) = sqrt(1/a - 1/n1 + 1/c - 1/n0), computed here independently.
  e <- eff()
  se <- sqrt(1 / 40 - 1 / 200 + 1 / 20 - 1 / 200)
  z <- stats::qnorm(0.975)
  stopifnot(all.equal(e$risk_ratio[["lower"]], 2 * exp(-z * se), tolerance = TOL_STAT),
            all.equal(e$risk_ratio[["upper"]], 2 * exp(z * se), tolerance = TOL_STAT))
})

ok("the risk-difference interval stays inside -1 and 1", {
  # Newcombe rather than Wald: at an extreme risk difference Wald limits fall
  # outside the range a difference of proportions can take.
  e <- eff(mk2(100, 0, 0, 100))
  stopifnot(all.equal(e$risk_difference[["estimate"]], 1),
            e$risk_difference[["lower"]] >= -1,
            e$risk_difference[["upper"]] <= 1,
            e$risk_difference[["lower"]] < 1)   # not a zero-width interval
})

ok("NNT is the reciprocal of the risk difference, with its direction named", {
  e <- eff()
  stopifnot(all.equal(e$nnt, 10), identical(e$nnt_direction, "harm"),
            !e$nnt_spans_null, all(is.finite(e$nnt_ci)))
  # reversing exposure flips the sign of the risk difference, not its size
  e2 <- two_group_effects(d$out, d$grp, positive = "Yes", exposed = "Reference")
  stopifnot(all.equal(e2$risk_difference[["estimate"]], -0.10),
            all.equal(e2$nnt, 10), identical(e2$nnt_direction, "benefit"))
})

ok("NNT is withheld when the risk difference interval spans zero", {
  # The NNT interval is then two disjoint ranges (benefit ... infinity ... harm),
  # so a single range would be wrong. It is not reported rather than fudged.
  e <- eff(mk2(21, 179, 20, 180))
  stopifnot(e$risk_difference[["lower"]] < 0, e$risk_difference[["upper"]] > 0,
            e$nnt_spans_null, all(is.na(e$nnt_ci)))
  r <- run_two_group_effects(mk2(21, 179, 20, 180)$out, mk2(21, 179, 20, 180)$grp,
                             positive = "Yes", exposed = "Exposed")
  stopifnot(grepl("two disjoint ranges", r$primary$interpretation))
})

cat("\n== the odds ratio is not the risk ratio ==\n")

ok("with a rare outcome the two agree, and the note says so", {
  e <- eff(mk2(4, 196, 2, 198))       # 2% vs 1%
  stopifnot(e$risk_reference[["estimate"]] < 0.10,
            abs(e$odds_ratio[["estimate"]] - e$risk_ratio[["estimate"]]) < 0.06,
            grepl("are close", or_rr_divergence_note(e)))
})

ok("with a common outcome the odds ratio overstates, and the note warns", {
  e <- eff(mk2(140, 60, 100, 100))    # 70% vs 50%
  note <- or_rr_divergence_note(e)
  stopifnot(e$odds_ratio[["estimate"]] > e$risk_ratio[["estimate"]] + 0.5,
            grepl("OVERSTATES", note),
            grepl("do not read it as", note),
            grepl("logistic model reports the odds ratio", note))
})

cat("\n== the guards ==\n")

ok("a non-binary outcome or group is refused by name", {
  dd <- d; dd$out <- as.character(dd$out); dd$out[1] <- "Unknown"
  stopifnot(grepl("outcome must have exactly two observed values",
                  friendly_error(two_group_effects(dd$out, d$grp))),
            grepl("Unknown", friendly_error(two_group_effects(dd$out, d$grp))))
  dd2 <- d; dd2$grp <- as.character(dd2$grp); dd2$grp[1] <- "Third"
  msg <- friendly_error(two_group_effects(d$out, dd2$grp))
  stopifnot(grepl("group must have exactly two", msg),
            grepl("collapse the groups first", msg))
})

ok("a declared-but-unobserved level does not block the analysis", {
  dd <- d
  dd$out <- factor(as.character(dd$out), levels = c("No", "Yes", "Unknown"))
  e <- two_group_effects(dd$out, dd$grp, positive = "Yes", exposed = "Exposed")
  stopifnot(e$n == 400L, all.equal(e$risk_difference[["estimate"]], 0.10))
})

ok("an empty cell gives a point estimate without a fabricated interval", {
  e <- eff(mk2(10, 190, 0, 200))
  stopifnot(is.infinite(e$odds_ratio[["estimate"]]) ||
              is.na(e$odds_ratio[["estimate"]]),
            is.na(e$odds_ratio[["lower"]]))
})

ok("rows missing the outcome or the group are dropped and counted", {
  dd <- rbind(d, data.frame(
    out = factor(c(NA, "Yes"), levels = c("No", "Yes")),
    grp = factor(c("Exposed", NA), levels = c("Reference", "Exposed"))))
  e <- two_group_effects(dd$out, dd$grp, positive = "Yes", exposed = "Exposed")
  stopifnot(e$n == 400L, e$n_dropped == 2L)
})

cat("\n== the trend test ==\n")

set.seed(1)
lv <- c("None", "Low", "Medium", "High")
tr_exp <- factor(rep(lv, each = 100), levels = lv, ordered = TRUE)
tr_out <- factor(stats::rbinom(400, 1, rep(c(.05, .10, .20, .35), each = 100)),
                 levels = c(0, 1), labels = c("No", "Yes"))

ok("a monotonic dose-response is detected", {
  t <- run_trend_test(tr_out, tr_exp, positive = "Yes")
  stopifnot(t$primary$p_value < 0.001,
            grepl("increases", t$primary$interpretation),
            nrow(t$by_level) == 4L,
            !is.unsorted(t$by_level$Proportion))
})

ok("the trend test matches prop.trend.test called directly", {
  t <- run_trend_test(tr_out, tr_exp, positive = "Yes")
  ev <- as.integer(tapply(tr_out == "Yes", tr_exp, sum))
  n_i <- as.integer(table(tr_exp))
  ref <- stats::prop.trend.test(ev, n_i, score = 1:4)
  stopifnot(all.equal(t$primary$p_value, ref$p.value, tolerance = TOL_P),
            all.equal(field_value(t$primary$table, "chi-squared (trend)"),
                      round(unname(ref$statistic), 4), tolerance = TOL_EFFECT))
})

ok("the chi-square test is reported alongside, and labelled as ignoring order", {
  t <- run_trend_test(tr_out, tr_exp, positive = "Yes")
  stopifnot(!is.null(t$alternative),
            grepl("ordering ignored", t$alternative$label),
            grepl("ignores the ordering", t$alternative$interpretation),
            identical(t$recommended, "primary"))
})

ok("a non-monotonic pattern is not called a trend", {
  # Proportions go up then back down: a chi-square test sees a difference, the
  # trend test correctly does not see a consistent direction.
  set.seed(4)
  ex <- factor(rep(lv, each = 200), levels = lv, ordered = TRUE)
  ou <- factor(stats::rbinom(800, 1, rep(c(.05, .35, .35, .05), each = 200)),
               levels = c(0, 1), labels = c("No", "Yes"))
  t <- run_trend_test(ou, ex, positive = "Yes")
  stopifnot(t$primary$p_value > 0.05, t$alternative$p_value < 0.001)
})

ok("custom scores change what linear means, and are reported", {
  # Unequally spaced categories: scoring age bands 1,2,3,4 is not the same as
  # scoring them by their midpoints, and the note has to say which was used.
  t1 <- run_trend_test(tr_out, tr_exp, positive = "Yes")
  t2 <- run_trend_test(tr_out, tr_exp, positive = "Yes", scores = c(0, 1, 5, 20))
  # Compare the STATISTIC, not the p-value: both p-values are around 1e-11, and
  # all.equal() judges near-zero numbers by absolute difference, so it calls
  # 2.8e-11 and 2.9e-12 equal. The chi-square values are 44.29 and 48.77.
  s1 <- field_value(t1$primary$table, "chi-squared (trend)")
  s2 <- field_value(t2$primary$table, "chi-squared (trend)")
  stopifnot(is.finite(s1), is.finite(s2), abs(s1 - s2) > 1,
            grepl("0, 1, 5, 20", t2$primary$note),
            grepl("supply the midpoints", t2$primary$note))
})

ok("fewer than three levels, or a mismatched score vector, is refused", {
  two <- factor(rep(c("Low", "High"), each = 50), levels = c("Low", "High"),
                ordered = TRUE)
  out2 <- factor(stats::rbinom(100, 1, 0.3), levels = c(0, 1),
                 labels = c("No", "Yes"))
  msg <- friendly_error(run_trend_test(out2, two, positive = "Yes"))
  stopifnot(grepl("at least three ordered exposure levels", msg),
            grepl("use the effect measures instead", msg))
  stopifnot(grepl("scores were supplied",
                  friendly_error(run_trend_test(tr_out, tr_exp,
                                                positive = "Yes",
                                                scores = c(1, 2)))))
})

ok("a numeric exposure keeps its own values as the scores", {
  dose <- rep(c(0, 5, 10, 20), each = 100)
  t <- run_trend_test(tr_out, dose, positive = "Yes")
  stopifnot(identical(t$by_level$Score, c(0, 5, 10, 20)),
            grepl("0, 5, 10, 20", t$primary$note))
})

cat("\n== the guided dispatcher and the catalogue ==\n")

ok("both families route through run_guided_test", {
  r1 <- run_guided_test(d, list(family = "effects", outcome = "out",
                                group = "grp", positive = "Yes",
                                exposed = "Exposed"))
  stopifnot(grepl("Effect measures", r1$primary$label),
            all.equal(field_value(r1$primary$table, "risk ratio"), 2,
                      tolerance = TOL_EFFECT))
  td <- data.frame(out = tr_out, ex = tr_exp)
  r2 <- run_guided_test(td, list(family = "trend", outcome = "out",
                                 exposure = "ex", positive = "Yes"))
  stopifnot(grepl("Cochran-Armitage", r2$primary$label))
})

ok("both are registered in the planner catalogue and its maps", {
  ids <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  stopifnot(all(c("effect_measures", "trend_test") %in% ids),
            identical(unname(PLANNER_TEST_FAMILY[["effect_measures"]]), "effects"),
            identical(unname(PLANNER_TEST_FAMILY[["trend_test"]]), "trend"),
            # both assume independent rows, so both must be demotable under
            # clustering like the other independence tests
            all(c("effect_measures", "trend_test") %in%
                  PLANNER_CENSORING_BLIND_TESTS))
})

report_and_exit()
