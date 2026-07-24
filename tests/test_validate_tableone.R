# tests/test_validate_tableone.R --------------------------------------------
# Validation for the Table 1 builder (fct_tableone.R): descriptive cells against
# base R, assumption-aware p-values against the existing test engines, and the
# structural rules (p-value column only when a split is given).

source(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "helper.R"))

# infert: a bundled cross-sectional dataset. case = 0/1 outcome; education is a
# 3-level factor; age is continuous; parity is a count.
d <- infert
d$case <- factor(ifelse(d$case == 1, "Case", "Control"))
types <- c(age = "continuous", parity = "count",
           education = "nominal", induced = "count", spontaneous = "count")
vars <- c("age", "parity", "education")

cell <- function(t1, char, col) {
  i <- match(char, t1$table$Characteristic)
  as.character(t1$table[[col]][i])
}

ok("p-value column appears only when a split is given", {
  t_split <- build_table_one(d, vars, strata = "case", types = types)
  t_plain <- build_table_one(d, vars, strata = NULL, types = types)
  stopifnot(isTRUE(t_split$has_p), "p" %in% names(t_split$table))
  stopifnot(isFALSE(t_plain$has_p), !("p" %in% names(t_plain$table)))
  # a plain table lists no tests in the note
  stopifnot(length(t_plain$tests_used) == 0)
})

ok("continuous M (SD) cell matches base R on the stratum subset", {
  t1 <- build_table_one(d, vars, strata = "case", types = types,
                        opts = list(cont_style = "mean_sd"))
  sub <- d$age[d$case == "Case"]
  expected <- sprintf("%.2f (%.2f)", mean(sub), sd(sub))
  stopifnot(identical(cell(t1, "age, M (SD)", "Case"), expected))
})

ok("categorical n (column %) cell matches base R", {
  t1 <- build_table_one(d, vars, strata = "case", types = types)
  cnt <- sum(d$education == levels(factor(d$education))[1] & d$case == "Control")
  den <- sum(d$case == "Control" & !is.na(d$education))
  expected <- sprintf("%d (%.1f%%)", cnt, 100 * cnt / den)
  lev1 <- levels(factor(d$education))[1]
  stopifnot(identical(cell(t1, lev1, "Control"), expected))
})

ok("column Ns in headers match the split counts", {
  t1 <- build_table_one(d, vars, strata = "case", types = types)
  stopifnot(any(grepl(sprintf("(n = %d)", sum(d$case == "Case")), t1$columns, fixed = TRUE)))
  stopifnot(any(grepl(sprintf("(n = %d)", sum(d$case == "Control")), t1$columns, fixed = TRUE)))
})

ok("continuous p-value matches the recommended two-group test", {
  t1 <- build_table_one(d, vars, strata = "case", types = types,
                        opts = list(cont_style = "mean_sd"))
  ref <- run_two_independent(d$age, d$case)
  ref_p <- ref[[ref$recommended]]$p_value
  stopifnot(identical(cell(t1, "age, M (SD)", "p"), p_apa(ref_p)))
})

ok("categorical p-value matches the recommended association test", {
  t1 <- build_table_one(d, vars, strata = "case", types = types)
  ref <- run_categorical(as.factor(d$education), d$case, paired = FALSE)
  ref_p <- ref[[ref$recommended]]$p_value
  # p sits on the categorical header row
  stopifnot(identical(cell(t1, "education, n (%)", "p"), p_apa(ref_p)))
})

ok("row_kind marks continuous, category headers and indented levels", {
  t1 <- build_table_one(d, vars, strata = "case", types = types)
  kinds <- t1$row_kind
  stopifnot("cont" %in% kinds, "cat_header" %in% kinds, "cat_level" %in% kinds)
  # every cat_header is followed by at least one cat_level
  hdr <- which(kinds == "cat_header")
  stopifnot(all(kinds[hdr + 1L] == "cat_level"))
})

ok("p_apa formats APA-style with no leading zero", {
  stopifnot(identical(p_apa(0.0004), "< .001"))
  stopifnot(identical(p_apa(0.023), ".023"))
  stopifnot(identical(p_apa(0.5), ".500"))
  stopifnot(identical(p_apa(NA_real_), "—"))
})

ok("auto continuous style follows a rank-based recommended test", {
  # A heavily skewed outcome in SMALL groups (n < 30, so the CLT-robustness rule
  # does not kick in) triggers a nonparametric test -> the cell uses Median [IQR].
  set.seed(1)
  dd <- data.frame(grp = rep(c("A", "B"), each = 18),
                   y = c(rexp(18, 1), rexp(18, 3)))
  tt <- c(y = "continuous", grp = "binary")
  ref <- run_two_independent(dd$y, factor(dd$grp))
  stopifnot(identical(ref$recommended, "alternative"))   # confirm rank-based is recommended
  t1 <- build_table_one(dd, "y", strata = "grp", types = tt,
                        opts = list(cont_style = "auto"))
  stopifnot(any(grepl("Median \\[IQR\\]", t1$table$Characteristic)))
})

# --- repeated-measures extensions ------------------------------------------
rm_df <- data.frame(
  id = rep(1:4, each = 3),
  visit = rep(c("Baseline", "6 months", "12 months"), 4),
  sex = rep(c("F", "M", "F", "M"), each = 3),
  score = c(10, 20, 30, 12, 22, 32, NA, 24, 34, 14, 26, 36),
  stringsAsFactors = FALSE)
rm_types <- c(score = "continuous", sex = "binary", visit = "nominal")

ok("tableone_baseline picks each subject's first (data-order) row", {
  b <- tableone_baseline(rm_df, "id", "visit")
  stopifnot(nrow(b) == 4L, all(b$visit == "Baseline"),
            identical(b$score, c(10, 12, NA, 14)))
})

ok(".t1_time_order sorts numeric/date but keeps label appearance order", {
  stopifnot(identical(.t1_time_order(rm_df$visit),
                      c("Baseline", "6 months", "12 months")))
  stopifnot(identical(.t1_time_order(c(12, 0, 6, 0)), c("0", "6", "12")))
})

ok("evolution table: chronological columns, no p, retention + missing rows", {
  t <- build_table_one(rm_df, "score", strata = "visit", types = rm_types,
        opts = list(show_p = FALSE, cont_style = "mean_sd", show_overall = FALSE,
                    show_missing = TRUE, retention = TRUE,
                    strata_order = .t1_time_order(rm_df$visit)))
  stopifnot(isFALSE(t$has_p), !("p" %in% names(t$table)))
  stopifnot(identical(t$col_names, c("Baseline", "6 months", "12 months")))
  stopifnot("meta" %in% t$row_kind,
            any(grepl("^Participants", t$table$Characteristic)))
  # baseline score mean over non-missing (10,12,14) = 12.00 (2.00)
  i <- match("score, M (SD)", t$table$Characteristic)
  stopifnot(grepl("^12\\.00 ", t$table$Baseline[i]))
  # continuous Missing row: one missing at baseline
  mi <- which(t$table$Characteristic == "Missing")[1]
  stopifnot(identical(t$table$Baseline[mi], "1"))
})

report_and_exit()
