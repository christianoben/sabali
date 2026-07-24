# tests/test_hypothesis.R ---------------------------------------------------
# Phase 2 hypothesis-test engine (fct_tests.R), routing
# (fct_recommend_hypothesis.R) and result formatting (mod_tests.R).
#
# Strategy (following the "gaps" good practice, not just smoke tests):
#   * effect-size formulas vs independently hand-computed constants;
#   * each runner's p-value vs base R's own test on the SAME data, so we prove
#     the engine wires up the right test and reads the right numbers (exact
#     match, independent of RNG stability because reference uses the same data);
#   * routing verdicts (primary vs alternative) on data with a known verdict;
#   * structural invariants — BOTH tests reported, common shape, p never hidden;
#   * dispatch and preview text.
#
# Run from the project root:  Rscript tests/test_hypothesis.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

near <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol
normal_sample <- function(n) qnorm(ppoints(n))
sval <- function(res, name) {                        # pull one statistic from a result table
  v <- res$table$value[res$table$statistic == name]
  if (length(v)) v[1] else NA_real_
}

# ===========================================================================
cat("\n== effect sizes vs hand-computed values ==\n")
# c(1:5) vs c(2:6): means 3 & 4, both var 2.5, pooled sd sqrt(2.5)=1.58114
# d = (3-4)/1.58114 = -0.632456
ok("cohens_d_two", stopifnot(near(cohens_d_two(1:5, 2:6), -0.6324555, 1e-5)))
# hedges g = d * (1 - 3/(4*10-9)) = -0.6324555 * 0.9032258 = -0.5712502
ok("hedges_g",     stopifnot(near(hedges_g(1:5, 2:6), -0.5712502, 1e-6)))
ok("cohens_d_two zero SD -> NA", stopifnot(is.na(cohens_d_two(c(5,5,5), c(5,5,5)))))
# rank-biserial: 1 - 2*5/(4*4) = 0.375
ok("rank_biserial_mw", stopifnot(near(rank_biserial_mw(5, 4, 4), 0.375)))
# Cramer's V for 2x2 {10,20;20,10}: chi=6.6667, V=sqrt(6.6667/60)=0.33333
ok("cramers_v", stopifnot(near(cramers_v(matrix(c(10,20,20,10), 2)), 1/3, 1e-4)))
# eta^2 (H) = (H - k + 1)/(n - k) = (6 - 3 + 1)/(30 - 3) = 4/27
ok("eta_sq_kruskal", stopifnot(near(eta_sq_kruskal(6, 3, 30), 4/27, 1e-6)))

# ===========================================================================
cat("\n== reshape_wide() ==\n")
ok("reshape_wide builds complete subject x within matrix", {
  x <- c(1, 2, 3, 4, 5, 6)
  within  <- c("pre","post","pre","post","pre","post")
  subject <- c("s1","s1","s2","s2","s3","s3")
  m <- reshape_wide(x, within, subject)
  stopifnot(nrow(m) == 3, ncol(m) == 2, all(complete.cases(m)))
})
ok("reshape_wide drops incomplete subjects", {
  x <- c(1, 2, 3)                       # s2 has only one measurement
  within  <- c("pre","post","pre")
  subject <- c("s1","s1","s2")
  m <- reshape_wide(x, within, subject)
  stopifnot(nrow(m) == 1)
})

# ===========================================================================
cat("\n== sig_phrase() ==\n")
ok("p < alpha -> significant", stopifnot(grepl("statistically significant", sig_phrase(0.01))))
ok("p >= alpha -> not significant", stopifnot(grepl("No statistically significant", sig_phrase(0.5))))
ok("NA p -> could not be evaluated", stopifnot(grepl("could not be evaluated", sig_phrase(NA_real_))))

# ===========================================================================
cat("\n== run_one_sample() ==\n")
set.seed(1); x <- rnorm(40, mean = 2)
r <- run_one_sample(x, mu = 0)
ok("primary p == stats::t.test", stopifnot(near(r$primary$p_value, t.test(x, mu = 0)$p.value)))
ok("alt p == wilcox.test",
   stopifnot(near(r$alternative$p_value, suppressWarnings(wilcox.test(x, mu = 0))$p.value)))
ok("both tests reported (p never hidden)",
   stopifnot(is.finite(r$primary$p_value), is.finite(r$alternative$p_value)))
ok("normal, n>=30 -> recommends primary (t-test)", stopifnot(r$recommended == "primary"))
ok("non-normal & n<30 -> recommends alternative", {
  r2 <- run_one_sample(c(rep(1, 8), 50, 60), mu = 5)
  stopifnot(r2$recommended == "alternative")
})

cat("\n== run_two_independent() ==\n")
set.seed(2); xi <- c(normal_sample(30), normal_sample(30) + 1); gi <- rep(c("a","b"), each = 30)
ri <- run_two_independent(xi, gi)
ok("primary p == Welch t.test(x~g)",
   stopifnot(near(ri$primary$p_value, t.test(xi ~ gi)$p.value)))
ok("alt p == Mann-Whitney wilcox.test(x~g)",
   stopifnot(near(ri$alternative$p_value, suppressWarnings(wilcox.test(xi ~ gi))$p.value)))
ok("Hedges g in result matches hedges_g()", {
  g_ref <- hedges_g(xi[gi == "a"], xi[gi == "b"])
  stopifnot(near(as.numeric(ri$primary$effect[["Hedges g"]]), round(g_ref, 3)))
})
ok("both groups normal -> recommends primary (Welch)", stopifnot(ri$recommended == "primary"))
ok("errors when grouping has != 2 levels", {
  err <- tryCatch({ run_two_independent(1:9, rep(c("a","b","c"), 3)); NA },
                  error = function(e) "err")
  stopifnot(identical(err, "err"))
})

cat("\n== run_two_paired() ==\n")
set.seed(3); n <- 20
pre  <- rnorm(n, 10); post <- pre + rnorm(n, 1)   # within-subject change
xp   <- as.vector(rbind(pre, post))
within  <- rep(c("pre","post"), times = n)
subject <- rep(paste0("s", seq_len(n)), each = 2)
rp <- run_two_paired(xp, within, subject)
ok("paired primary p == paired t.test (order-independent, two-sided)",
   stopifnot(near(rp$primary$p_value, t.test(pre, post, paired = TRUE)$p.value)))
ok("paired alt p == paired wilcox.test",
   stopifnot(near(rp$alternative$p_value,
                  suppressWarnings(wilcox.test(pre, post, paired = TRUE))$p.value)))

cat("\n== run_anova_oneway() ==\n")
set.seed(4)
xa <- c(normal_sample(30), normal_sample(30) + 1, normal_sample(30) + 2)  # equal variance
ga <- rep(c("a","b","c"), each = 30)
ra <- run_anova_oneway(xa, ga)
ok("equal-variance -> classic one-way ANOVA chosen",
   stopifnot(identical(ra$primary$label, "One-way ANOVA")))
ok("ANOVA primary p == anova(aov(x~g))", {
  d <- data.frame(x = xa, g = factor(ga))
  stopifnot(near(ra$primary$p_value, anova(aov(x ~ g, data = d))$`Pr(>F)`[1]))
})
ok("ANOVA alt p == kruskal.test",
   stopifnot(near(ra$alternative$p_value, kruskal.test(xa, factor(ga))$p.value)))
ok("unequal variances -> Welch ANOVA chosen", {
  xu <- c(normal_sample(30), normal_sample(30) * 6, normal_sample(30) * 12)
  ru <- run_anova_oneway(xu, ga)
  stopifnot(grepl("Welch", ru$primary$label))
})
ok("classic ANOVA -> Tukey HSD posthoc with reference-adjusted p-values", {
  d <- data.frame(x = xa, g = factor(ga))
  ref <- TukeyHSD(aov(x ~ g, data = d), which = "g")$g
  ph <- ra$posthoc
  stopifnot(identical(ph$label, "Tukey HSD"), nrow(ph$table) == 3,
            all.equal(ph$table[["p-adjusted"]], unname(ref[, "p adj"])),
            all.equal(ph$table$Estimate, unname(ref[, "diff"])))
})
ok("Welch ANOVA -> Games-Howell posthoc", {
  xu <- c(normal_sample(30), normal_sample(30) * 6, normal_sample(30) * 12)
  ru <- run_anova_oneway(xu, ga)
  ph <- ru$posthoc
  stopifnot(grepl("Games-Howell", ph$label), nrow(ph$table) == 3,
            all(is.finite(ph$table[["p-adjusted"]])))
  x1 <- xu[ga == "a"]; x2 <- xu[ga == "b"]
  v1 <- var(x1) / length(x1); v2 <- var(x2) / length(x2)
  df_12 <- (v1 + v2)^2 / (v1^2 / (length(x1) - 1) + v2^2 / (length(x2) - 1))
  q_12 <- abs(mean(x1) - mean(x2)) / (sqrt(v1 + v2) / sqrt(2))
  p_12 <- ptukey(q_12 * sqrt(2), nmeans = 3, df = df_12,
                 lower.tail = FALSE)
  stopifnot(near(ph$table[["p-adjusted"]][1], p_12))
})
ok("Dunn posthoc uses pooled ranks and Holm adjustment", {
  dd <- data.frame(x = c(1:8, 2:9, 6:13),
                   g = factor(rep(c("a", "b", "c"), each = 8)))
  ph <- posthoc_dunn(dd, "Kruskal-Wallis test", 0.01)
  raw <- ph$table[["p-value"]]
  stopifnot(identical(ph$label, "Dunn's test"), nrow(ph$table) == 3,
            all.equal(ph$table[["p-adjusted"]], p.adjust(raw, "holm")),
            all(ph$table[["p-adjusted"]] >= raw))
})

cat("\n== run_repeated() ==\n")
set.seed(5); ns <- 10; conds <- c("t1","t2","t3")
subj_eff <- rnorm(ns, 0, 2)
vals <- as.vector(sapply(seq_along(conds), function(j) subj_eff + (j - 1) + rnorm(ns, 0, 0.5)))
xr <- vals
within_r  <- rep(conds, each = ns)
subject_r <- rep(paste0("s", seq_len(ns)), times = length(conds))
rr <- run_repeated(xr, within_r, subject_r)
ok("Friedman alt p == friedman.test", {
  m <- reshape_wide(xr, within_r, subject_r)
  stopifnot(near(rr$alternative$p_value, suppressWarnings(friedman.test(m))$p.value))
})
ok("RM-ANOVA primary fitted (p not NA) on well-formed data",
   stopifnot(is.finite(rr$primary$p_value), rr$recommended == "primary"))
ok("RM-ANOVA -> Holm-adjusted paired t posthoc", {
  ph <- rr$posthoc
  stopifnot(identical(ph$label, "Pairwise paired t-tests"), nrow(ph$table) == 3,
            all.equal(ph$table[["p-adjusted"]],
                      p.adjust(ph$table[["p-value"]], "holm")),
            all(ph$table[["n pairs"]] == ns))
})
ok("Friedman -> Holm-adjusted paired Wilcoxon posthoc", {
  m <- reshape_wide(xr, within_r, subject_r)
  ph <- posthoc_paired_wilcox(m, "Friedman test", rr$alternative$p_value)
  stopifnot(identical(ph$label, "Pairwise Wilcoxon signed-rank tests"),
            nrow(ph$table) == 3,
            all.equal(ph$table[["p-adjusted"]],
                      p.adjust(ph$table[["p-value"]], "holm")),
            all(ph$table[["n pairs"]] == ns))
})

cat("\n== run_categorical() ==\n")
xc <- factor(rep(c("a","b"), each = 60)); yc <- factor(rep(c("p","q"), times = 60))
rc <- run_categorical(xc, yc)
ok("chi p == chisq.test(correct=FALSE)",
   stopifnot(near(rc$primary$p_value,
                  suppressWarnings(chisq.test(table(xc, yc), correct = FALSE))$p.value)))
ok("Fisher alt p == fisher.test",
   stopifnot(near(rc$alternative$p_value, fisher.test(table(xc, yc))$p.value)))
ok("Cramer's V in result matches cramers_v()",
   stopifnot(near(as.numeric(rc$primary$effect[["Cramer's V"]]),
                  round(cramers_v(table(xc, yc)), 3))))
ok("adequate expected counts -> recommends chi-square (primary)",
   stopifnot(rc$recommended == "primary"))
ok("sparse table -> recommends Fisher (alternative)", {
  xs <- factor(c("a","a","b","b","c","c")); ys <- factor(c("p","q","p","q","p","q"))
  rs <- run_categorical(xs, ys)
  stopifnot(rs$recommended == "alternative")
})
ok("paired 2x2 -> McNemar, no alternative, p == mcnemar.test", {
  xm <- factor(c("y","y","n","n","y","n","y","n","n","y"))
  ym <- factor(c("y","n","n","y","y","y","n","n","y","n"))
  rm2 <- run_categorical(xm, ym, paired = TRUE)
  stopifnot(grepl("McNemar", rm2$primary$label), is.null(rm2$alternative),
            near(rm2$primary$p_value, mcnemar.test(table(xm, ym))$p.value))
})

cat("\n== adjusted standardized residuals (chi-square post-hoc) ==\n")
ok("adjusted_std_residuals() == chisq.test()$stdres", {
  tb <- matrix(c(10, 20, 30, 15, 25, 5), nrow = 2, byrow = TRUE)
  ref <- suppressWarnings(chisq.test(tb))$stdres
  stopifnot(max(abs(adjusted_std_residuals(tb) - ref)) < 1e-8)
})
ok("chi-square result carries a residual post-hoc, one row per cell", {
  set.seed(9)
  xa <- factor(sample(c("a","b","c"), 240, replace = TRUE))
  yb <- factor(sample(c("p","q"), 240, replace = TRUE))
  r  <- run_categorical(xa, yb)
  ph <- r$posthoc
  stopifnot(!is.null(ph), is.data.frame(ph$table),
            nrow(ph$table) == nlevels(xa) * nlevels(yb),
            all(c("Cell","Observed","Expected","Adj. residual","p-value","p-adjusted")
                %in% names(ph$table)))
})
ok("post-hoc residual magnitudes match chisq.test()$stdres", {
  set.seed(10)
  xa <- factor(sample(c("a","b","c"), 240, replace = TRUE))
  yb <- factor(sample(c("p","q"), 240, replace = TRUE))
  r  <- run_categorical(xa, yb)
  ref <- suppressWarnings(chisq.test(table(xa, yb)))$stdres
  stopifnot(abs(max(abs(r$posthoc$table[["Adj. residual"]])) - max(abs(ref))) < 1e-2)
})
ok("paired 2x2 McNemar has no residual post-hoc", {
  xm <- factor(c("y","y","n","n","y","n","y","n","n","y"))
  ym <- factor(c("y","n","n","y","y","y","n","n","y","n"))
  stopifnot(is.null(run_categorical(xm, ym, paired = TRUE)$posthoc))
})

cat("\n== run_correlation() ==\n")
set.seed(6); xx <- normal_sample(40); yy <- rev(normal_sample(40)) + normal_sample(40) * 0.01
rk <- run_correlation(xx, yy)
ok("Pearson p == cor.test(pearson)",
   stopifnot(near(rk$primary$p_value, cor.test(xx, yy, method = "pearson")$p.value)))
ok("Spearman rho matches cor.test(spearman)",
   stopifnot(near(sval(rk$alternative, "rho"),
                  round(unname(suppressWarnings(cor.test(xx, yy, method = "spearman"))$estimate), 3))))
ok("Kendall reported as 'extra'",
   stopifnot(!is.null(rk$extra), grepl("Kendall", rk$extra$label)))
ok("both variables normal -> recommends Pearson (primary)",
   stopifnot(rk$recommended == "primary"))
ok("non-normal -> recommends Spearman (alternative)", {
  xn <- (1:30)^2; yn <- (1:30)^2 + (1:30)
  stopifnot(run_correlation(xn, yn)$recommended == "alternative")
})

# ===========================================================================
cat("\n== run_guided_test() dispatch ==\n")
dfd <- data.frame(num1 = normal_sample(40), num2 = rev(normal_sample(40)),
                  grp = rep(c("a","b"), each = 20),
                  cat1 = rep(c("p","q"), times = 20))
ok("dispatch correlation == direct call", {
  a <- run_guided_test(dfd, list(family = "correlation", x = "num1", y = "num2"))
  b <- run_correlation(dfd$num1, dfd$num2)
  stopifnot(near(a$primary$p_value, b$primary$p_value))
})
ok("dispatch location/two_independent == direct call", {
  a <- run_guided_test(dfd, list(family = "location", design = "two_independent",
                                 outcome = "num1", group = "grp"))
  b <- run_two_independent(dfd$num1, dfd$grp)
  stopifnot(near(a$primary$p_value, b$primary$p_value))
})
ok("dispatch categorical == direct call", {
  a <- run_guided_test(dfd, list(family = "categorical", x = "grp", y = "cat1"))
  b <- run_categorical(dfd$grp, dfd$cat1)
  stopifnot(near(a$primary$p_value, b$primary$p_value))
})
ok("unknown family errors", {
  err <- tryCatch({ run_guided_test(dfd, list(family = "bogus")); NA },
                  error = function(e) "err")
  stopifnot(identical(err, "err"))
})

cat("\n== suggest_location_test() & n_group_levels() ==\n")
ok("one_sample preview", stopifnot(grepl("One-sample", suggest_location_test("one_sample")$primary)))
ok("two_independent preview", stopifnot(grepl("Welch", suggest_location_test("two_independent")$primary)))
ok("repeated preview", stopifnot(grepl("Repeated-measures", suggest_location_test("repeated")$primary)))
ok("unknown design -> placeholder", stopifnot(identical(suggest_location_test("???")$primary, "-")))
ok("n_group_levels drops NA and unused levels", {
  g <- factor(c("a","b",NA,"a"), levels = c("a","b","c"))   # 'c' unused, one NA
  stopifnot(n_group_levels(g) == 2)
})

# ===========================================================================
cat("\n== result_to_df() (mod_tests.R formatting) ==\n")
ok("result_to_df has Test / p-value / Interpretation rows + effect", {
  rows <- result_to_df(run_correlation(dfd$num1, dfd$num2)$primary)
  stopifnot(is.data.frame(rows), identical(names(rows), c("Field", "Value")),
            "Test" %in% rows$Field, "p-value" %in% rows$Field,
            "Interpretation" %in% rows$Field, "r" %in% rows$Field)
})
ok("non-multi-group tests do not gain an irrelevant posthoc table", {
  stopifnot(is.null(run_two_independent(dfd$num1, dfd$grp)$posthoc),
            is.null(run_correlation(dfd$num1, dfd$num2)$posthoc))
})

report_and_exit()
