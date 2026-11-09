# tests/test_gee_cluster_id.R -----------------------------------------------
# Regression tests for the GEE cluster-id contract.
#
# geepack finds its cluster boundaries with diff(as.numeric(id)) and assumes the
# rows already arrive grouped by cluster. Two properties of real study exports
# break that assumption, and both used to produce a WRONG ANSWER rather than an
# error, which is why they are pinned here:
#
#   1. Household / participant ids are strings ("01/003/03" in the ARCH 2.0
#      household extracts, "PPS-014" in REDCap). as.numeric() turns those into
#      NA, the cluster structure collapses, and the fit returns standard errors
#      of ~1e-07 with every p-value at exactly 0 - a fabricated significant
#      result, reported confidently.
#   2. Rows in an arbitrary order make the working correlation be estimated
#      from the wrong pairs, so alpha and the p-values move with the sort order
#      of the uploaded file.
#
# The fixture mirrors the shape of
#   ARCH 2.0 study/2.0 DATA/Dataset/Risk factor analysis/*_All 133 households.dta
# (string household code, unbalanced visits per household, binary colonisation
# outcome, household-level random intercept). Set SABALI_ARCH2_DTA to that file
# to additionally run the checks against the real data - see the end of this
# script. The synthetic fixture is the one CI depends on, so the suite carries
# no participant data.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(shiny)
  library(geepack)
})

# --- fixture ---------------------------------------------------------------
# Household codes carry the slashes that make as.numeric() give up.
set.seed(2026)
n_hh <- 40L
visits <- sample(6:12, n_hh, replace = TRUE)          # deliberately unbalanced
hh <- sprintf("01/%03d/05", seq_len(n_hh))
u <- stats::rnorm(n_hh, 0, 0.8)                       # household random effect

d <- do.call(rbind, lapply(seq_len(n_hh), function(i) {
  k <- visits[i]
  x <- stats::rnorm(k)
  data.frame(
    hhid  = rep(hh[i], k),
    visit = seq_len(k),
    x     = x,
    y     = 1 + 0.6 * x + u[i] + stats::rnorm(k, 0, 0.5),
    col   = factor(stats::rbinom(k, 1, stats::plogis(-0.3 + 0.7 * x + u[i])),
                   levels = c(0, 1), labels = c("No", "Yes")),
    stringsAsFactors = FALSE
  )
}))
d$hhid_code <- as.integer(as.factor(d$hhid))          # what the fix passes on
stopifnot(is.character(d$hhid),
          all(is.na(suppressWarnings(as.numeric(d$hhid)))))

set.seed(7)
d_shuffled <- d[sample(nrow(d)), , drop = FALSE]

gee_spec <- function(method, outcome, type, corstr = "exchangeable", time = NULL) {
  list(outcome = outcome, outcome_type = type, predictors = "x",
       method = method, cluster = "hhid", corstr = corstr, time = time)
}
se_of <- function(res) res$coefficients[["Std. Error"]]
p_of  <- function(res) res$coefficients[["p-value"]]

cat("\n== GEE with a string cluster id must not fabricate significance ==\n")

ok("string cluster id yields usable standard errors", {
  r <- run_guided_clustered_model(d, gee_spec("gee_gaussian", "y", "continuous"))
  # The bug signature was SE ~1e-07 across the board; a real fit on 40
  # households is orders of magnitude above that.
  stopifnot(all(is.finite(se_of(r))), all(se_of(r) > 1e-3))
})

ok("string cluster id does not drive every p-value to zero", {
  r <- run_guided_clustered_model(d, gee_spec("gee_gaussian", "y", "continuous"))
  stopifnot(all(is.finite(p_of(r))), !all(p_of(r) == 0))
})

ok("string cluster id keeps all clusters (not collapsed to one)", {
  r <- run_guided_clustered_model(d, gee_spec("gee_gaussian", "y", "continuous"))
  stopifnot(field_value(r$fit_stats, "clusters") == n_hh)
})

cat("\n== the fit matches a correctly specified geeglm() reference ==\n")

ok("Gaussian GEE matches integer-coded, cluster-sorted geeglm()", {
  r <- run_guided_clustered_model(d, gee_spec("gee_gaussian", "y", "continuous"))
  ref_d <- d[order(d$hhid_code), , drop = FALSE]
  ref <- geepack::geeglm(y ~ x, id = ref_d$hhid_code, data = ref_d,
                         family = stats::gaussian(), corstr = "exchangeable")
  s <- summary(ref)$coefficients
  stopifnot(all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(se_of(r), unname(s[, 2]), tolerance = TOL_STAT),
            all.equal(p_of(r), unname(s[, "Pr(>|W|)"]), tolerance = TOL_P))
})

ok("binary GEE matches integer-coded, cluster-sorted geeglm()", {
  r <- run_guided_clustered_model(d, gee_spec("gee_binomial", "col", "binary"))
  ref_d <- d[order(d$hhid_code), , drop = FALSE]
  ref_d$.y <- as.numeric(ref_d$col == "Yes")
  ref <- geepack::geeglm(.y ~ x, id = ref_d$hhid_code, data = ref_d,
                         family = stats::binomial(), corstr = "exchangeable")
  s <- summary(ref)$coefficients
  stopifnot(all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(se_of(r), unname(s[, 2]), tolerance = TOL_STAT))
})

cat("\n== the answer must not depend on the row order of the upload ==\n")

for (m in list(c("gee_gaussian", "y", "continuous"),
               c("gee_binomial", "col", "binary"))) {
  ok(sprintf("%s is invariant to row shuffling", m[[1]]), {
    a <- run_guided_clustered_model(d,          gee_spec(m[[1]], m[[2]], m[[3]]))
    b <- run_guided_clustered_model(d_shuffled, gee_spec(m[[1]], m[[2]], m[[3]]))
    stopifnot(all.equal(a$coefficients$Estimate, b$coefficients$Estimate,
                        tolerance = TOL_STAT),
              all.equal(se_of(a), se_of(b), tolerance = TOL_STAT),
              all.equal(p_of(a), p_of(b), tolerance = TOL_P))
  })
}

ok("AR-1 GEE orders waves within cluster and is shuffle-invariant", {
  spec <- gee_spec("gee_gaussian", "y", "continuous", corstr = "ar1", time = "visit")
  a <- run_guided_clustered_model(d, spec)
  b <- run_guided_clustered_model(d_shuffled, spec)
  stopifnot(all(is.finite(se_of(a))), all(se_of(a) > 1e-3),
            all.equal(a$coefficients$Estimate, b$coefficients$Estimate,
                      tolerance = TOL_STAT),
            all.equal(se_of(a), se_of(b), tolerance = TOL_STAT))
})

cat("\n== every cluster-id column type, not just the string case ==\n")

# The fix must not be specific to the slash-separated household codes that
# exposed it. Whatever type the analyst's id column happens to be, the fit has
# to be the same one - the id is only a grouping label.
id_types <- list(
  character = function(x) x,
  factor    = function(x) factor(x),
  integer   = function(x) as.integer(factor(x)),
  double    = function(x) as.numeric(factor(x)) + 0.0,
  Date      = function(x) as.Date("2020-01-01") + as.integer(factor(x)),
  # A numeric id whose lexical and numeric orders disagree (1, 2, 10, ...),
  # which is where a naive as.factor() sort could regroup rows if grouping
  # depended on the ordering of the levels rather than on their identity.
  wide_int  = function(x) as.integer(factor(x)) * 7L
)
ref_fit <- run_guided_clustered_model(d, gee_spec("gee_gaussian", "y", "continuous"))
for (nm in names(id_types)) {
  ok(sprintf("%s cluster id gives the same fit", nm), {
    dd <- d
    dd$hhid <- id_types[[nm]](d$hhid)
    r <- run_guided_clustered_model(dd, gee_spec("gee_gaussian", "y", "continuous"))
    stopifnot(all(is.finite(se_of(r))), all(se_of(r) > 1e-3),
              field_value(r$fit_stats, "clusters") == n_hh,
              all.equal(r$coefficients$Estimate, ref_fit$coefficients$Estimate,
                        tolerance = TOL_STAT),
              all.equal(se_of(r), se_of(ref_fit), tolerance = TOL_STAT))
  })
}

ok("rows with a missing cluster id are dropped, not grouped together", {
  dd <- d
  dd$hhid[c(1, 5, 9)] <- NA_character_
  r <- run_guided_clustered_model(dd, gee_spec("gee_gaussian", "y", "continuous"))
  stopifnot(field_value(r$fit_stats, "n used") == nrow(d) - 3L,
            all(is.finite(se_of(r))), all(se_of(r) > 1e-3))
})

cat("\n== a numeric cluster id is unaffected by the coercion ==\n")

ok("numeric cluster id gives the same fit as before the id was coded", {
  dn <- d
  dn$hhid <- dn$hhid_code                     # already-numeric household key
  r <- run_guided_clustered_model(dn, gee_spec("gee_gaussian", "y", "continuous"))
  ref_d <- dn[order(dn$hhid), , drop = FALSE]
  ref <- geepack::geeglm(y ~ x, id = ref_d$hhid, data = ref_d,
                         family = stats::gaussian(), corstr = "exchangeable")
  s <- summary(ref)$coefficients
  stopifnot(all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(se_of(r), unname(s[, 2]), tolerance = TOL_STAT))
})

# --- optional check against the real ARCH 2.0 extract ----------------------
# Opt in with:
#   SABALI_ARCH2_DTA="/path/to/120225_All 133 households.dta" Rscript tests/test_gee_cluster_id.R
# The file holds participant data, so it is never committed and the suite skips
# silently when the variable is unset.
arch2 <- Sys.getenv("SABALI_ARCH2_DTA", "")
if (nzchar(arch2) && file.exists(arch2)) {
  cat("\n== real ARCH 2.0 household extract ==\n")
  real <- read_any_format(arch2, basename(arch2))
  needed <- c("RC_adcolonization", "ad_antibiotic", "adAge", "hhid")
  if (all(needed %in% names(real))) {
    ok("household colonisation GEE agrees with the GLMM on the same data", {
      g <- run_guided_clustered_model(real, list(
        outcome = "RC_adcolonization", outcome_type = "binary",
        predictors = c("ad_antibiotic", "adAge"), method = "gee_binomial",
        cluster = "hhid", corstr = "exchangeable"))
      m <- run_guided_clustered_model(real, list(
        outcome = "RC_adcolonization", outcome_type = "binary",
        predictors = c("ad_antibiotic", "adAge"), method = "glmm_binomial",
        cluster = "hhid"))
      # Population-average and subject-specific estimates differ in size, so
      # the contract is only that neither is degenerate and that they agree on
      # what is and is not significant at the 5% level.
      stopifnot(all(se_of(g) > 1e-3), all(se_of(m) > 1e-3),
                identical(p_of(g)[-1] < 0.05, p_of(m)[-1] < 0.05))
    })
  } else {
    cat("  SKIP  ARCH 2.0 file lacks the expected columns\n")
  }
} else {
  cat("\n  (set SABALI_ARCH2_DTA to also run the real-data checks)\n")
}

report_and_exit()
