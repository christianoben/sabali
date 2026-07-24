# Statistical validation: location, factorial, repeated and ANCOVA runners.
# Tier-1 values are compared with base R; rank methods are compared with the
# independent packages/hand calculations named beside each assertion.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== one-sample and two-group tests ==\n")
one <- run_one_sample(sleep$extra, mu = 0)
one_ref <- stats::t.test(sleep$extra, mu = 0)
ok("one-sample t statistic, df and p match stats::t.test", {
  expect_close(field_value(one$primary$table, "t"), unname(one_ref$statistic), 5e-4)
  expect_close(field_value(one$primary$table, "df"), unname(one_ref$parameter), 5e-4)
  expect_close(one$primary$p_value, one_ref$p.value, TOL_P)
})
ok("one-sample CI is the reference CI to display precision", {
  expect_close(field_value(one$primary$table, "95% CI lower"), one_ref$conf.int[1], 5e-4)
  expect_close(field_value(one$primary$table, "95% CI upper"), one_ref$conf.int[2], 5e-4)
})

two <- run_two_independent(sleep$extra, sleep$group)
two_ref <- stats::t.test(extra ~ group, data = sleep)
ok("Welch t statistic, df and p match stats::t.test", {
  expect_close(field_value(two$primary$table, "t"), unname(two_ref$statistic), 1e-3)
  expect_close(field_value(two$primary$table, "df"), unname(two_ref$parameter), 5e-2)
  expect_close(two$primary$p_value, two_ref$p.value, TOL_P)
})
ok("Hedges g matches the app's stated effect-size formula", {
  ref_g <- hedges_g(sleep$extra[sleep$group == "1"], sleep$extra[sleep$group == "2"])
  expect_close(as.numeric(two$primary$effect[["Hedges g"]]), round(ref_g, 3), TOL_EFFECT)
})

cat("\n== paired, one-way and repeated designs ==\n")
paired <- run_two_paired(sleep$extra, sleep$group, sleep$ID)
wide_sleep <- reshape_wide(sleep$extra, sleep$group, sleep$ID)
paired_ref <- stats::t.test(wide_sleep[, 1], wide_sleep[, 2], paired = TRUE)
ok("paired t statistic, df and p match stats::t.test(paired=TRUE)", {
  expect_close(field_value(paired$primary$table, "t"), unname(paired_ref$statistic), 5e-4)
  expect_close(field_value(paired$primary$table, "df"), unname(paired_ref$parameter), 5e-4)
  expect_close(paired$primary$p_value, paired_ref$p.value, TOL_P)
})

tg <- transform(ToothGrowth, dose = factor(dose))
oneway <- run_anova_oneway(tg$len, tg$dose)
anova_ref <- stats::aov(len ~ dose, data = tg)
welch_ref <- stats::oneway.test(len ~ dose, data = tg, var.equal = FALSE)
kw_ref <- stats::kruskal.test(len ~ dose, data = tg)
ok("one-way primary agrees with the selected base-R reference", {
  if (identical(oneway$primary$label, "One-way ANOVA")) {
    a <- summary(anova_ref)[[1]]
    expect_close(field_value(oneway$primary$table, "F"), a$`F value`[1], 5e-4)
    expect_close(oneway$primary$p_value, a$`Pr(>F)`[1], TOL_P)
  } else {
    expect_close(field_value(oneway$primary$table, "F"), unname(welch_ref$statistic), 5e-4)
    expect_close(oneway$primary$p_value, welch_ref$p.value, TOL_P)
  }
  expect_close(oneway$alternative$p_value, kw_ref$p.value, TOL_P)
})

set.seed(7)
rm_d <- expand.grid(subject = factor(seq_len(12)), within = factor(c("pre", "post", "late")))
rm_d$y <- with(rm_d, as.numeric(subject) + as.numeric(within) * 0.6 +
                 rep(c(-0.4, 0, 0.4), 12))
repeated <- run_repeated(rm_d$y, rm_d$within, rm_d$subject)
rm_mat <- reshape_wide(rm_d$y, rm_d$within, rm_d$subject)
fried_ref <- stats::friedman.test(rm_mat)
ok("Friedman repeated-measures alternative matches stats::friedman.test", {
  expect_close(repeated$alternative$p_value, fried_ref$p.value, TOL_P)
  expect_close(field_value(repeated$alternative$table, "chi-squared"),
               unname(fried_ref$statistic), 5e-4)
})
ok("repeated runner reports both a primary and an alternative", {
  stopifnot(!is.null(repeated$primary), !is.null(repeated$alternative),
            !is.null(repeated$posthoc), nrow(repeated$posthoc$table) == 3L)
})

cat("\n== factorial, mixed and ANCOVA designs ==\n")
fac <- run_factorial_anova(tg, "len", c("supp", "dose"))
fac_ref <- stats::aov(len ~ supp * dose, data = tg)
fac_tab <- summary(fac_ref)[[1]]
ok("factorial ANOVA term table matches aov() F and p values", {
  terms <- fac$terms
  for (term in c("supp", "dose", "supp:dose")) {
    i <- match(term, terms$term); j <- match(term, trimws(rownames(fac_tab)))
    expect_close(terms$F[i], fac_tab$`F value`[j], TOL_STAT)
    expect_close(terms$p[i], fac_tab$`Pr(>F)`[j], TOL_P)
  }
})
if (requireNamespace("rcompanion", quietly = TRUE)) {
  srh_ref <- rcompanion::scheirerRayHare(len ~ supp + dose, data = tg, verbose = FALSE)
  srh <- run_scheirer_ray_hare(tg, "len", c("supp", "dose"))
  ok("Scheirer-Ray-Hare interaction agrees with rcompanion", {
    expect_close(field_value(srh$primary$table, "H (interaction)"), srh_ref["supp:dose", "H"], TOL_T2)
    expect_close(srh$primary$p_value, srh_ref["supp:dose", "p.value"], TOL_T2)
  })
} else cat("  SKIP  rcompanion not installed\n")
if (requireNamespace("ARTool", quietly = TRUE)) {
  art_ref <- ARTool::anova(ARTool::art(len ~ supp * dose, data = tg))
  art <- run_factorial_anova(tg, "len", c("supp", "dose"))$extra
  ok("aligned-rank-transform interaction agrees with ARTool", {
    a <- art$terms_table
    term <- "supp:dose"
    i <- match(term, a$term); j <- match(term, trimws(rownames(art_ref)))
    expect_close(a$p[i], art_ref$`Pr(>F)`[j], TOL_T2)
  })
} else cat("  SKIP  ARTool not installed\n")

set.seed(12)
mx <- expand.grid(subject = factor(seq_len(24)), within = factor(c("pre", "post")))
mx$group <- factor(ifelse(as.integer(mx$subject) <= 12, "A", "B"))
mx$y <- with(mx, 5 + 0.5 * (group == "B") + 0.8 * (within == "post") +
              0.9 * (group == "B") * (within == "post") + rnorm(nrow(mx), 0, .15))
mixed <- run_mixed_anova(mx, "y", "group", "within", "subject")
mixed_ref <- lme4::lmer(y ~ group * within + (1 | subject), data = mx, REML = FALSE)
ok("mixed ANOVA fixed effects agree with the reference LMM", {
  got <- setNames(mixed$terms$estimate, mixed$terms$term)
  ref <- lme4::fixef(mixed_ref); ref <- ref[names(ref) != "(Intercept)"]
  stopifnot(all.equal(unname(got[names(ref)]), unname(ref), tolerance = TOL_STAT))
})

set.seed(14)
ac <- data.frame(group = factor(rep(c("A", "B"), each = 20)),
                 cov = rep(seq(-2, 2, length.out = 20), 2))
ac$y <- 2 + 1.4 * ac$cov + (ac$group == "B") * 0.8 +
  rep(c(-.2, -.1, 0, .1, .2), 8)
anc <- run_ancova(ac, "y", "group", "cov")
anc_ref <- stats::anova(lm(y ~ cov, data = ac), lm(y ~ cov + group, data = ac))
ok("ANCOVA group test matches the nested lm() reference", {
  expect_close(field_value(anc$primary$table, "F"), anc_ref$F[2], 5e-4)
  expect_close(anc$primary$p_value, anc_ref$`Pr(>F)`[2], TOL_P)
})

report_and_exit()
