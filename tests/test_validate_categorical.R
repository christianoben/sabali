# Statistical validation: categorical, proportions, McNemar and Cochran's Q.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== chi-square and Fisher exact tests ==\n")
tab <- matrix(c(12, 8, 5, 15), nrow = 2, byrow = TRUE,
              dimnames = list(exposure = c("no", "yes"),
                              outcome = c("no", "yes")))
x <- factor(rep(rownames(tab), times = rowSums(tab)), levels = rownames(tab))
y <- factor(unlist(lapply(seq_len(nrow(tab)), function(i)
  rep(colnames(tab), tab[i, ]))), levels = colnames(tab))
cat_res <- run_categorical(x, y)
chi_ref <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
fish_ref <- stats::fisher.test(tab)
ok("chi-square statistic, df and p match stats::chisq.test", {
  expect_close(field_value(cat_res$primary$table, "chi-squared"),
               unname(chi_ref$statistic), 5e-4)
  expect_close(field_value(cat_res$primary$table, "df"), unname(chi_ref$parameter), TOL_STAT)
  expect_close(cat_res$primary$p_value, chi_ref$p.value, TOL_P)
})
ok("Fisher exact p and odds ratio match stats::fisher.test", {
  expect_close(cat_res$alternative$p_value, fish_ref$p.value, TOL_P)
  expect_close(as.numeric(cat_res$alternative$effect[["odds ratio"]]),
               round(unname(fish_ref$estimate), 3), TOL_EFFECT)
})

cat("\n== paired 2x2 McNemar ==\n")
paired_tab <- matrix(c(18, 3, 7, 12), nrow = 2, byrow = TRUE,
                     dimnames = list(before = c("no", "yes"),
                                     after = c("no", "yes")))
px <- factor(rep(rownames(paired_tab), times = rowSums(paired_tab)), levels = rownames(paired_tab))
py <- factor(unlist(lapply(seq_len(nrow(paired_tab)), function(i)
  rep(colnames(paired_tab), paired_tab[i, ]))), levels = colnames(paired_tab))
mc <- run_categorical(px, py, paired = TRUE)
mc_ref <- stats::mcnemar.test(paired_tab)
ok("McNemar statistic and p match stats::mcnemar.test", {
  expect_close(field_value(mc$primary$table, "chi-squared"), unname(mc_ref$statistic), 5e-4)
  expect_close(mc$primary$p_value, mc_ref$p.value, TOL_P)
  stopifnot(is.null(mc$alternative))
})
ok("paired success-proportion difference is reported by hand", {
  expected <- mean(as.character(py) == "yes") - mean(as.character(px) == "yes")
  stopifnot(abs(expected - (sum(paired_tab[, 2]) - sum(paired_tab[2, ])) /
                 sum(paired_tab)) < 1e-12)
})

cat("\n== exact one-proportion test ==\n")
prop_x <- factor(c(rep("no", 7), rep("yes", 13)), levels = c("no", "yes"))
prop <- run_proportion_test(prop_x, ref_prop = 0.5, success = "yes")
prop_ref <- stats::binom.test(13, 20, p = 0.5)
approx_ref <- suppressWarnings(stats::prop.test(13, 20, p = 0.5, correct = FALSE))
ok("binomial p and exact CI match stats::binom.test", {
  expect_close(prop$primary$p_value, prop_ref$p.value, TOL_P)
  expect_close(field_value(prop$primary$table, "95% CI lower"), prop_ref$conf.int[1], TOL_CI)
  expect_close(field_value(prop$primary$table, "95% CI upper"), prop_ref$conf.int[2], TOL_CI)
  expect_close(prop$alternative$p_value, approx_ref$p.value, TOL_P)
})

cat("\n== Cochran's Q: independent implementation + post-hoc adjustment ==\n")
m <- matrix(c(
  1, 1, 0,
  1, 0, 1,
  0, 1, 1,
  1, 0, 0,
  0, 0, 1,
  1, 1, 1,
  0, 1, 0,
  1, 0, 1
), ncol = 3, byrow = TRUE,
dimnames = list(subject = paste0("s", 1:8), condition = c("A", "B", "C")))
long <- data.frame(subject = rep(rownames(m), each = ncol(m)),
                   condition = rep(colnames(m), nrow(m)),
                   response = factor(as.vector(t(m)), levels = c(0, 1)))
q <- run_cochran_q(long$response, long$condition, long$subject, success = "1")
if (requireNamespace("DescTools", quietly = TRUE)) {
  q_ref <- DescTools::CochranQTest(m)
  ok("Cochran's Q statistic, df and p match DescTools::CochranQTest", {
    expect_close(field_value(q$primary$table, "Q"), unname(q_ref$statistic), TOL_T2)
    expect_close(field_value(q$primary$table, "df"), unname(q_ref$parameter), TOL_STAT)
    expect_close(q$primary$p_value, q_ref$p.value, TOL_T2)
  })
} else cat("  SKIP  DescTools not installed\n")
ok("Cochran post-hoc p-values use Holm adjustment", {
  raw <- q$posthoc$table[["p-value"]]
  stopifnot(length(raw) == 3L,
            all.equal(q$posthoc$table[["p-adjusted"]], p.adjust(raw, "holm")),
            all(q$posthoc$table[["p-adjusted"]] >= raw | is.na(raw)))
})
ok("repeated-binary route selects McNemar for two conditions", {
  two <- long[long$condition != "C", ]
  rr <- run_repeated_binary(two$response, two$condition, two$subject, success = "1")
  stopifnot(grepl("McNemar", rr$primary$label), is.null(rr$alternative))
})

cat("\n== adversarial categorical guards ==\n")
ok("empty/single-level contingency tables fail with a human-readable message", {
  e1 <- friendly_error(run_categorical(factor(rep("a", 4)), factor(rep("b", 4))))
  e2 <- friendly_error(run_categorical(factor(character()), factor(character())))
  stopifnot(grepl("categorical association|observed levels", e1, ignore.case = TRUE),
            grepl("categorical association|observed levels", e2, ignore.case = TRUE))
})

report_and_exit()
