# tests/test_design_coverage.R ----------------------------------------------
# Coverage audit: for every classical test/design a user expects, generate a
# dataset with a KNOWN structure, route it through the guided engine exactly as
# the Tests tab does (run_guided_test), and check that (a) the correct test is
# produced and (b) its p-value/statistic matches the reference base-R function.
#
# The checklist audited (user-supplied):
#   t-Test, Paired t-Test, Mann-Whitney U, Chi-Square, ANOVA, One-Way ANOVA,
#   Two-Way ANOVA, Three-Way ANOVA, Repeated-Measures ANOVA, Mixed-model ANOVA,
#   Kruskal-Wallis, Friedman, Binomial test, Wilcoxon.
#
# Labels:
#   [GUARD]   - design is implemented AND wired; must stay green.
#   [MISSING] - design is on the checklist but NOT reachable through the guided
#               engine today; fails on purpose and defines the work.
#
# Run:  Rscript tests/test_design_coverage.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))
near <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol
# route through the guided engine; return the result or a one-line error string
route <- function(df, spec) tryCatch(run_guided_test(df, spec),
                                     error = function(e) conditionMessage(e))
ran_ok <- function(r) is.list(r) && !is.null(r$primary)

set.seed(2026)

# ===========================================================================
cat("\n== PRESENT & WIRED (checklist items 1-6, 9, 11-12, 14) [GUARD] ==\n")
# ===========================================================================

# 1 + 14: one-sample t-Test with the Wilcoxon signed-rank alternative --------
d_os <- data.frame(y = rnorm(45, 1.5, 4))
r_os <- route(d_os, list(family = "location", outcome = "y",
                         design = "one_sample", ref_value = 0))
ok("[GUARD] t-Test (one-sample) + Wilcoxon signed-rank routed & correct", {
  stopifnot(ran_ok(r_os),
            grepl("one-sample t-test", r_os$primary$label, ignore.case = TRUE),
            grepl("Wilcoxon signed-rank", r_os$alternative$label, ignore.case = TRUE),
            near(r_os$primary$p_value, t.test(d_os$y, mu = 0)$p.value))
})

# 1 + 3: two-independent t-Test (Welch) + Mann-Whitney U ---------------------
d_ti <- data.frame(y = rnorm(80), g = factor(rep(c("A", "B"), 40)))
d_ti$y <- d_ti$y + ifelse(d_ti$g == "B", 0.8, 0)
r_ti <- route(d_ti, list(family = "location", outcome = "y",
                         design = "two_independent", group = "g"))
ok("[GUARD] t-Test (two independent, Welch) + Mann-Whitney U routed & correct", {
  stopifnot(ran_ok(r_ti),
            grepl("t-test", r_ti$primary$label, ignore.case = TRUE),
            grepl("Mann-Whitney", r_ti$alternative$label, ignore.case = TRUE),
            near(r_ti$primary$p_value, t.test(y ~ g, data = d_ti)$p.value),
            near(r_ti$alternative$p_value,
                 suppressWarnings(wilcox.test(y ~ g, data = d_ti))$p.value))
})

# 2 + 14: Paired t-Test + Wilcoxon signed-rank (long format) -----------------
np <- 32
d_pt <- data.frame(subj = factor(rep(1:np, 2)),
                   cond = factor(rep(c("pre", "post"), each = np)),
                   y = c(rnorm(np, 5, 2), rnorm(np, 6, 2)))
r_pt <- route(d_pt, list(family = "location", outcome = "y", design = "two_paired",
                         within = "cond", subject = "subj"))
ok("[GUARD] Paired t-Test + Wilcoxon signed-rank routed", {
  stopifnot(ran_ok(r_pt),
            grepl("paired t-test", r_pt$primary$label, ignore.case = TRUE),
            grepl("Wilcoxon", r_pt$alternative$label, ignore.case = TRUE))
})

# 4: Chi-Square (with Fisher alternative) ------------------------------------
d_chi <- data.frame(a = factor(sample(c("x", "y"), 120, TRUE)),
                    b = factor(sample(c("p", "q"), 120, TRUE)))
r_chi <- route(d_chi, list(family = "categorical", x = "a", y = "b", paired = FALSE))
ok("[GUARD] Chi-Square test routed & correct", {
  ref <- suppressWarnings(chisq.test(table(d_chi$a, d_chi$b), correct = FALSE))
  stopifnot(ran_ok(r_chi),
            grepl("chi-square", r_chi$primary$label, ignore.case = TRUE),
            near(r_chi$primary$p_value, ref$p.value))
})

# 5 + 6 + 11: (One-Way) ANOVA + Kruskal-Wallis -------------------------------
d_aov <- data.frame(y = rnorm(120), g = factor(rep(c("A", "B", "C"), 40)))
d_aov$y <- d_aov$y + as.numeric(d_aov$g)
r_aov <- route(d_aov, list(family = "location", outcome = "y",
                           design = "multi_independent", group = "g"))
ok("[GUARD] One-Way ANOVA + Kruskal-Wallis routed & correct", {
  stopifnot(ran_ok(r_aov),
            grepl("ANOVA", r_aov$primary$label),
            grepl("Kruskal-Wallis", r_aov$alternative$label),
            near(r_aov$alternative$p_value, kruskal.test(y ~ g, data = d_aov)$p.value))
})

# 9 + 12: Repeated-Measures ANOVA + Friedman ---------------------------------
nr <- 30
d_rm <- data.frame(subj = factor(rep(1:nr, 3)),
                   cond = factor(rep(c("t1", "t2", "t3"), each = nr)),
                   y = rnorm(3 * nr))
r_rm <- route(d_rm, list(family = "location", outcome = "y", design = "repeated",
                         within = "cond", subject = "subj"))
ok("[GUARD] Repeated-Measures ANOVA + Friedman routed", {
  stopifnot(ran_ok(r_rm),
            grepl("repeated-measures anova", r_rm$primary$label, ignore.case = TRUE),
            grepl("Friedman", r_rm$alternative$label, ignore.case = TRUE))
})

# ===========================================================================
cat("\n== NEWLY WIRED DESIGNS (items 7, 8, 10, 13) ==\n")
# These generate the correct design and assert the guided engine routes it
# through the Tests-tab contract and matches the relevant reference analysis.
# ===========================================================================

# 7: Two-Way ANOVA (two crossed factors, main effects + interaction) ---------
d_2w <- expand.grid(A = factor(c("a1", "a2")), B = factor(c("b1", "b2", "b3")),
                    rep = 1:20)
d_2w$y <- 2 * as.numeric(d_2w$A) + 1.3 * as.numeric(d_2w$B) +
  ifelse(d_2w$A == "a2" & d_2w$B == "b3", 2.5, 0) + rnorm(nrow(d_2w))
ref_2w <- summary(aov(y ~ A * B, data = d_2w))[[1]]
srh_ranks <- rank(d_2w$y)
srh_fit <- lm(srh_ranks ~ d_2w$A + d_2w$B + d_2w$A:d_2w$B)
srh_an <- anova(srh_fit)
srh_tie <- 1 - sum(table(srh_ranks)^3 - table(srh_ranks)) /
  (length(srh_ranks)^3 - length(srh_ranks))
srh_ref_p <- pchisq(
  (srh_an$`Sum Sq`[3] / (length(srh_ranks) * (length(srh_ranks) + 1) / 12)) /
    srh_tie,
  df = srh_an$Df[3], lower.tail = FALSE)
ok("[WIRED] Two-Way ANOVA is routable and matches aov(y ~ A*B)", {
  r <- route(d_2w, list(family = "location", outcome = "y", design = "factorial",
                        factors = c("A", "B")))
  stopifnot(ran_ok(r), grepl("two-way|factorial", r$primary$label, ignore.case = TRUE),
            # interaction p-value must match the reference
            near(r$primary$p_value, ref_2w[["Pr(>F)"]][3], tol = 1e-5) ||
            any(vapply(r$terms$p %||% numeric(0),
                       function(p) near(p, ref_2w[["Pr(>F)"]][3], 1e-5), logical(1))))
})
ok("[WIRED] Two-Way ANOVA reports Scheirer-Ray-Hare and ART alternatives", {
  r <- route(d_2w, list(family = "location", outcome = "y", design = "factorial",
                        factors = c("A", "B")))
  stopifnot(grepl("Scheirer", r$alternative$label, ignore.case = TRUE),
            near(r$alternative$p_value, srh_ref_p, tol = 1e-5),
            grepl("Aligned Rank Transform", r$extra$label, ignore.case = TRUE))
})

# 8: Three-Way ANOVA ---------------------------------------------------------
d_3w <- expand.grid(A = factor(c("a1", "a2")), B = factor(c("b1", "b2")),
                    C = factor(c("c1", "c2")), rep = 1:15)
d_3w$y <- as.numeric(d_3w$A) + as.numeric(d_3w$B) + as.numeric(d_3w$C) +
  rnorm(nrow(d_3w))
ok("[WIRED] Three-Way ANOVA is routable (y ~ A*B*C)", {
  r <- route(d_3w, list(family = "location", outcome = "y", design = "factorial",
                        factors = c("A", "B", "C")))
  stopifnot(ran_ok(r), grepl("three-way|factorial", r$primary$label, ignore.case = TRUE))
})

# 10: Mixed-model / split-plot ANOVA (between factor + within factor) --------
nm <- 24
d_mx <- expand.grid(subj = 1:nm, time = factor(c("t1", "t2", "t3")))
d_mx$grp <- factor(ifelse(d_mx$subj <= nm / 2, "ctrl", "trt"))
d_mx$subj <- factor(d_mx$subj)
d_mx$y <- as.numeric(d_mx$time) + ifelse(d_mx$grp == "trt", 1.5, 0) + rnorm(nrow(d_mx))
ok("[WIRED] Mixed-model (split-plot) ANOVA is routable (between x within)", {
  r <- route(d_mx, list(family = "location", outcome = "y", design = "mixed",
                        group = "grp", within = "time", subject = "subj"))
  stopifnot(ran_ok(r), grepl("mixed|split-plot|between", r$primary$label, ignore.case = TRUE))
})

# 13: Binomial test (one proportion vs a reference) --------------------------
d_bin <- data.frame(resp = factor(rep(c("yes", "no"), c(34, 66)),
                                  levels = c("no", "yes")))
ok("[WIRED] Binomial test (one proportion) is routable & matches binom.test", {
  r <- route(d_bin, list(family = "proportion", outcome = "resp", ref_prop = 0.5))
  ref <- binom.test(34, 100, p = 0.5)
  stopifnot(ran_ok(r), grepl("binomial", r$primary$label, ignore.case = TRUE),
            near(r$primary$p_value, ref$p.value, tol = 1e-6))
})

report_and_exit()
