# tests/test_base_level.R ---------------------------------------------------
# The Data tab lets the user choose the base (reference) level for factor
# variables. These checks confirm the helper behaves, that releveling actually
# changes which level a regression treats as the reference, and that the choice
# flows through the model engine coefficient names.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# --- factor_levels_of -------------------------------------------------------
ok("factor_levels_of returns sorted observed levels", {
  lv <- factor_levels_of(c("b", "a", "c", NA, "a"))
  stopifnot(identical(lv, c("a", "b", "c")))
})

ok("factor_levels_of drops empty strings", {
  lv <- factor_levels_of(c("", "x", "y"))
  stopifnot(identical(lv, c("x", "y")))
})

# --- which types accept a reference level -----------------------------------
ok("only binary and nominal are relevellable", {
  stopifnot(is_relevellable_type("binary"))
  stopifnot(is_relevellable_type("nominal"))
  stopifnot(!is_relevellable_type("ordinal"))
  stopifnot(!is_relevellable_type("continuous"))
})

# --- releveling changes the regression reference ----------------------------
set.seed(1)
df <- data.frame(
  y = rnorm(60),
  grp = factor(rep(c("apple", "banana", "cherry"), length.out = 60))
)

ok("default reference is R's first level (apple)", {
  fit <- lm(y ~ grp, data = df)
  terms <- names(coef(fit))
  # apple is the reference, so it should NOT appear as a grp term
  stopifnot(!("grpapple" %in% terms))
  stopifnot(all(c("grpbanana", "grpcherry") %in% terms))
})

ok("relevel to cherry makes cherry the reference", {
  df2 <- df
  df2$grp <- stats::relevel(df2$grp, ref = "cherry")
  fit <- lm(y ~ grp, data = df2)
  terms <- names(coef(fit))
  stopifnot(!("grpcherry" %in% terms))
  stopifnot(all(c("grpapple", "grpbanana") %in% terms))
})

# --- the chosen base survives the model engine ------------------------------
ok("run_guided_model honours a releveled predictor", {
  df3 <- df
  df3$grp <- stats::relevel(df3$grp, ref = "banana")
  res <- run_guided_model(df3, list(
    outcome = "y", outcome_type = "continuous",
    predictors = "grp", method = "ols"
  ))
  terms <- res$coefficients$Term
  # banana is now the reference, so it is absorbed into the intercept
  stopifnot(!("grpbanana" %in% terms))
  stopifnot(all(c("grpapple", "grpcherry") %in% terms))
})

report_and_exit()
