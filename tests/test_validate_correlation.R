# Statistical validation: Pearson, Spearman, point-biserial and repeated
# measures correlation.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== Pearson and Spearman on mtcars ==\n")
corr <- run_correlation(mtcars$mpg, mtcars$wt)
pear_ref <- stats::cor.test(mtcars$mpg, mtcars$wt, method = "pearson")
spear_ref <- stats::cor.test(mtcars$mpg, mtcars$wt, method = "spearman")
ok("Pearson r, p and CI match stats::cor.test", {
  expect_close(field_value(corr$primary$table, "r"), unname(pear_ref$estimate), 5e-4)
  expect_close(corr$primary$p_value, pear_ref$p.value, TOL_P)
  expect_close(field_value(corr$primary$table, "95% CI lower"), pear_ref$conf.int[1], 5e-4)
  expect_close(field_value(corr$primary$table, "95% CI upper"), pear_ref$conf.int[2], 5e-4)
})
ok("Spearman rho and p match stats::cor.test", {
  expect_close(field_value(corr$alternative$table, "rho"), unname(spear_ref$estimate), 5e-4)
  expect_close(corr$alternative$p_value, spear_ref$p.value, TOL_P)
})
ok("correlation runner always retains Kendall as an additional sensitivity result", {
  stopifnot(!is.null(corr$extra), grepl("Kendall", corr$extra$label))
})

cat("\n== point-biserial ==\n")
pb_x <- factor(mtcars$am, levels = c(0, 1), labels = c("automatic", "manual"))
pb <- run_point_biserial(pb_x, mtcars$mpg)
pb_ref <- stats::cor.test(as.integer(pb_x == "manual"), mtcars$mpg)
tt_ref <- stats::t.test(mtcars$mpg ~ pb_x, var.equal = TRUE)
ok("point-biserial r/p match Pearson on 0/1 coding and pooled t-test", {
  expect_close(field_value(pb$primary$table, "r_pb"), unname(pb_ref$estimate), 5e-4)
  expect_close(pb$primary$p_value, pb_ref$p.value, TOL_P)
  expect_close(pb$primary$p_value, tt_ref$p.value, TOL_P)
})
ok("point-biserial alternative uses the rank-biserial path", {
  stopifnot(grepl("Rank-biserial", pb$alternative$label),
            is.finite(as.numeric(pb$alternative$effect[["rank-biserial r"]])))
})

cat("\n== repeated-measures correlation ==\n")
set.seed(23)
nr <- 18
rmc <- data.frame(subject = factor(rep(seq_len(nr), each = 3)),
                  x = rep(c(-1, 0, 1), nr))
rmc$y <- with(rmc, 2 + as.numeric(subject) * .3 + .9 * x +
                rep(c(-.25, .05, .2), nr))
rmc_res <- run_repeated_correlation(rmc$x, rmc$y, rmc$subject)
fe <- stats::lm(y ~ subject + x, data = rmc,
                contrasts = list(subject = "contr.sum"))
drop <- stats::drop1(fe, . ~ ., test = "F")
i <- match("x", rownames(drop)); ss <- drop[["Sum of Sq"]][i]; rss <- drop[["RSS"]][1]
rmc_r <- sign(coef(fe)[["x"]]) * sqrt(ss / (ss + rss))
ok("repeated-measures r and p match the subject-fixed-effects reference", {
  expect_close(field_value(rmc_res$primary$table, "r_rm"), rmc_r, 5e-4)
  expect_close(rmc_res$primary$p_value, drop[["Pr(>F)"]][i], TOL_P)
})
ok("repeated correlation rejects incomplete subject pairs instead of silently pooling", {
  bad <- rmc
  bad$y[bad$subject == levels(bad$subject)[1] & bad$x != 1] <- NA_real_
  e <- friendly_error(run_repeated_correlation(bad$x, bad$y, bad$subject))
  stopifnot(grepl("at least two complete paired observations", e, ignore.case = TRUE))
})

cat("\n== correlation edge cases ==\n")
ok("too few observations and constant predictors fail clearly", {
  e1 <- friendly_error(run_correlation(c(1, 2), c(2, 3)))
  e2 <- friendly_error(run_correlation(1:5, rep(1, 5)))
  stopifnot(grepl("at least three", e1, ignore.case = TRUE),
            grepl("no variation", e2, ignore.case = TRUE))
})

report_and_exit()
