# tests/test_survey_design.R -------------------------------------------------
# Design-based (survey-weighted) estimation.
#
# A complex sample analysed as a simple random sample gets both the estimate and
# its uncertainty wrong: unequal selection probabilities bias the estimate unless
# weighted, and clustering inflates the true variance so unweighted standard
# errors are too small. The ARCH 1.0 community analysis declares its design -
#   svyset hhid [pweight = ad_pw], fpc(fpc_hh) strata(rural_urban)
# - and the PPS protocol specifies a design effect for patients clustered within
# hospitals. The app had no way to honour either.
#
# The variance machinery is the `survey` package's, not ours, so what is tested
# here is the wiring: that the Stata-style declaration maps to the right
# svydesign, that the guards catch what an analyst will actually get wrong, and
# that clustering moves the standard errors in the direction it must.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

if (!requireNamespace("survey", quietly = TRUE)) {
  cat("\n  SKIP  the 'survey' package is not installed (see install_dependencies.R)\n")
  report_and_exit()
}
suppressPackageStartupMessages(library(survey))

# A clustered, stratified, weighted sample with a real within-cluster
# correlation, so the design genuinely matters.
set.seed(31)
n_psu <- 40L
per <- 12L
psu <- sprintf("hh%03d", seq_len(n_psu))
stratum <- rep(c("rural", "urban"), each = n_psu / 2)
u <- stats::rnorm(n_psu, 0, 1.1)          # cluster random effect
d <- do.call(rbind, lapply(seq_len(n_psu), function(i) data.frame(
  hhid = psu[i],
  rural_urban = stratum[i],
  # urban households are sampled at half the rate, so they carry twice the weight
  pw = if (stratum[i] == "urban") 20 else 10,
  fpc_hh = 0.1,
  x = stats::rnorm(per),
  stringsAsFactors = FALSE)))
d$y <- 1 + 0.5 * d$x + rep(u, each = per) + stats::rnorm(nrow(d), 0, 0.6)
d$col <- factor(stats::rbinom(nrow(d), 1, stats::plogis(-0.3 + 0.6 * d$x +
                  rep(u, each = per))), levels = c(0, 1), labels = c("No", "Yes"))
spec <- list(weights = "pw", strata = "rural_urban", psu = "hhid", fpc = "fpc_hh")

cat("\n== declaring a design ==\n")

ok("weights, strata or a PSU each count as a declaration; nothing does not", {
  stopifnot(survey_design_declared(spec),
            survey_design_declared(list(weights = "pw")),
            survey_design_declared(list(psu = "hhid")),
            survey_design_declared(list(strata = "rural_urban")),
            !survey_design_declared(list()),
            !survey_design_declared(list(fpc = "fpc_hh")),   # fpc alone is not
            !survey_design_declared(list(weights = "")),
            !survey_design_declared(NULL))
})

ok("the declaration maps onto the intended svydesign", {
  b <- build_survey_design(d, spec)
  stopifnot(b$n_used == nrow(d), b$n_dropped == 0L,
            length(unique(b$design$cluster[[1]])) == n_psu,
            length(unique(b$design$strata[[1]])) == 2L,
            all.equal(sum(stats::weights(b$design)), sum(d$pw)))
})

ok("the design summary reports what was declared", {
  s <- survey_design_summary(build_survey_design(d, spec))
  stopifnot(grepl("hhid", s$Value[s$Field == "Primary sampling units"]),
            grepl("rural_urban", s$Value[s$Field == "Strata"]),
            grepl("pw", s$Value[s$Field == "Sampling weight"]))
})

ok("rows missing a design column are dropped and counted, not absorbed", {
  dd <- d; dd$pw[1:5] <- NA
  b <- build_survey_design(dd, spec)
  stopifnot(b$n_used == nrow(d) - 5L, b$n_dropped == 5L,
            grepl("5 rows were dropped", .survey_note(b)))
})

cat("\n== the guards an analyst will actually hit ==\n")

ok("a non-existent design column is named in the error", {
  stopifnot(grepl("'nope' is not in the data",
                  friendly_error(build_survey_design(d, list(weights = "nope")))))
})

ok("zero or negative weights are refused with an explanation", {
  dd <- d; dd$pw[3] <- 0
  msg <- friendly_error(build_survey_design(dd, spec))
  stopifnot(grepl("zero or negative", msg), grepl("population units", msg))
  dd$pw[3] <- -1
  stopifnot(grepl("zero or negative", friendly_error(build_survey_design(dd, spec))))
})

ok("a non-numeric weight is refused", {
  dd <- d; dd$pw <- as.character(dd$pw); dd$pw[2] <- "heavy"
  stopifnot(grepl("not numbers", friendly_error(build_survey_design(dd, spec))))
})

ok("asking for an estimate with no design declared says so", {
  stopifnot(grepl("Declare at least a sampling weight",
                  friendly_error(build_survey_design(d, list()))))
})

ok("a stratum with a single PSU does not abort the estimate", {
  # Common in real designs; survey errors by default. The option is set locally
  # and must be restored afterwards.
  before <- getOption("survey.lonely.psu")
  dd <- d; dd$rural_urban[dd$hhid == "hh001"] <- "island"
  b <- build_survey_design(dd, spec)
  stopifnot(b$n_used == nrow(dd),
            identical(getOption("survey.lonely.psu"), before))
})

cat("\n== clustering must move the standard errors ==\n")

ok("the design-based SE exceeds the SRS SE for a clustered proportion", {
  pr <- survey_proportion(d, spec, "col")
  x <- d$col[!is.na(d$col)]
  ph <- prop.table(table(x))[["Yes"]]
  se_srs <- sqrt(ph * (1 - ph) / length(x))
  se_design <- pr[["Std. Error"]][1]
  stopifnot(is.finite(se_design), se_design > se_srs,
            all(pr[["Design effect"]] > 1))     # clustering costs precision
})

ok("proportions sum to one and carry a design-based interval", {
  pr <- survey_proportion(d, spec, "col")
  stopifnot(nrow(pr) == 2L,
            all.equal(sum(pr$Proportion), 1, tolerance = 1e-8),
            all(is.finite(pr[["CI lower"]])), all(is.finite(pr[["CI upper"]])),
            all(pr[["CI lower"]] < pr$Proportion),
            all(pr$Proportion < pr[["CI upper"]]))
})

ok("the weighted mean differs from the unweighted one when weights differ", {
  m <- survey_mean(d, spec, "y")
  stopifnot(nrow(m) == 1L, is.finite(m$Mean),
            !isTRUE(all.equal(m$Mean, mean(d$y), tolerance = 1e-6)),
            # and it equals the hand-computed weighted mean
            all.equal(m$Mean, sum(d$pw * d$y) / sum(d$pw), tolerance = 1e-8))
})

ok("estimates match survey's own functions called directly", {
  # The wrapper must not perturb the numbers it is wrapping.
  b <- build_survey_design(d, spec)
  ref <- survey::svymean(~y, b$design, na.rm = TRUE)
  m <- survey_mean(d, spec, "y")
  # as.numeric(), not unname(): SE() on an svystat returns a 1x1 matrix, and
  # all.equal compares a matrix to a scalar as different regardless of value.
  stopifnot(all.equal(m$Mean, unname(stats::coef(ref)), tolerance = TOL_STAT),
            all.equal(m[["Std. Error"]], as.numeric(survey::SE(ref)),
                      tolerance = TOL_STAT))
})

ok("a by-group estimate returns one row per group", {
  g <- survey_proportion(d, spec, "col", by = "rural_urban")
  stopifnot(nrow(g) == 2L, !is.null(attr(g, "design")))
})

cat("\n== two-stage designs ==\n")

# Households within villages, then people within households. Stata writes this
# `svyset village, ... || household, ...`; DHS, MICS and most national household
# surveys are shaped this way, as are the WSU community extracts.
set.seed(52)
d2 <- do.call(rbind, lapply(seq_len(30L), function(v) do.call(rbind, lapply(seq_len(4L),
  function(h) data.frame(
    village = sprintf("v%02d", v), hh = sprintf("v%02d-h%d", v, h),
    pw = 12, fpc_v = 0.2, fpc_h = 0.5,
    y = stats::rnorm(3, v * 0.1, 1), stringsAsFactors = FALSE)))))
two <- list(weights = "pw", psu = "village", psu2 = "hh",
            fpc = "fpc_v", fpc2 = "fpc_h")

ok("a second sampling stage is accepted and reported", {
  b <- build_survey_design(d2, two)
  stopifnot(identical(b$psu2, "hh"), identical(b$fpc2, "fpc_h"),
            b$n_used == nrow(d2))
  s <- survey_design_summary(b)
  stopifnot(grepl("hh", s$Value[s$Field == "Second sampling stage"]),
            grepl("then hh within it", .survey_note(b)))
})

ok("the two-stage design matches survey called directly", {
  b <- build_survey_design(d2, two)
  ref <- survey::svydesign(ids = ~village + hh, strata = NULL, weights = ~pw,
                           fpc = ~fpc_v + fpc_h, data = d2, nest = TRUE)
  m <- survey_mean(d2, two, "y")
  r <- survey::svymean(~y, ref, na.rm = TRUE)
  stopifnot(all.equal(m$Mean, unname(stats::coef(r)), tolerance = TOL_STAT),
            all.equal(m[["Std. Error"]], as.numeric(survey::SE(r)),
                      tolerance = TOL_STAT))
})

ok("a single-stage design is unchanged by the second-stage support", {
  s1 <- survey_design_summary(build_survey_design(d, spec))
  stopifnot(grepl("single-stage", s1$Value[s1$Field == "Second sampling stage"]))
})

ok("a second stage without a first, or an fpc2 without it, is refused", {
  stopifnot(grepl("needs a first-stage PSU",
                  friendly_error(build_survey_design(d2, list(weights = "pw",
                                                              psu2 = "hh")))),
            grepl("needs a second-stage unit",
                  friendly_error(build_survey_design(d2, list(weights = "pw",
                                        psu = "village", fpc2 = "fpc_h")))))
})

cat("\n== design-based regression ==\n")

svy_spec <- function(outcome, type, method, preds = "x", ...)
  c(list(outcome = outcome, outcome_type = type, predictors = preds,
         method = method), spec, list(...))

ok("the offered method matches the outcome type", {
  stopifnot(identical(survey_available_methods("continuous"), "svy_gaussian"),
            identical(survey_available_methods("binary"), "svy_binomial"),
            identical(survey_available_methods("count"), "svy_poisson"),
            identical(survey_available_methods("nominal"), character(0)))
})

ok("svyglm coefficients match survey called directly", {
  r <- run_survey_model(d, svy_spec("y", "continuous", "svy_gaussian"))
  b <- build_survey_design(d, spec)
  ref <- survey::svyglm(y ~ x, design = b$design, family = stats::gaussian())
  s <- summary(ref)$coefficients
  stopifnot(all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(r$coefficients[["Std. Error"]], unname(s[, 2]),
                      tolerance = TOL_STAT))
})

ok("residual degrees of freedom come from the PSUs, not the rows", {
  # This is the heart of design-based inference: 40 clusters and 2 parameters
  # give 38 df, not 480 - 2. Getting this wrong is what makes naive SEs too small.
  r <- run_survey_model(d, svy_spec("y", "continuous", "svy_gaussian"))
  df_res <- field_value(r$fit_stats, "residual degrees of freedom")
  stopifnot(df_res < n_psu, df_res > 0,
            field_value(r$fit_stats, "primary sampling units") == n_psu)
})

ok("a design-based logistic model reports odds ratios", {
  r <- run_survey_model(d, svy_spec("col", "binary", "svy_binomial"))
  stopifnot("Odds ratio" %in% names(r$coefficients),
            identical(r$effect_name, "Odds ratio"),
            all(is.finite(r$coefficients[["Std. Error"]])))
})

ok("design-based SEs are usually larger than the naive ones here", {
  s <- run_survey_model(d, svy_spec("y", "continuous", "svy_gaussian"))
  u <- run_guided_model(d, list(outcome = "y", outcome_type = "continuous",
                                predictors = "x", method = "ols"))
  stopifnot(s$coefficients[["Std. Error"]][1] > u$coefficients[["Std. Error"]][1])
})

ok("interactions work in a design-based model too", {
  dd <- d; dd$g <- factor(rep(c("a", "b"), length.out = nrow(d)))
  r <- run_survey_model(dd, svy_spec("y", "continuous", "svy_gaussian",
                                     preds = c("x", "g"), interactions = "x:g"))
  stopifnot("x:gb" %in% as.character(r$coefficients$Term))
})

ok("likelihood-based fit statistics are not reported for a weighted fit", {
  # The likelihood of a weighted complex sample is not the likelihood of these
  # rows, so an AIC would be meaningless rather than merely approximate.
  r <- run_survey_model(d, svy_spec("y", "continuous", "svy_gaussian"))
  stopifnot(!any(c("AIC", "BIC", "log-likelihood") %in%
                   as.character(r$fit_stats$Metric)),
            any(grepl("Likelihood-based fit statistics",
                      vapply(r$assumptions, function(a) a$note %||% "",
                             character(1)))))
})

ok("a method that does not fit the outcome type is refused", {
  stopifnot(grepl("does not apply to a binary outcome",
                  friendly_error(run_survey_model(d,
                    svy_spec("col", "binary", "svy_gaussian")))),
            grepl("not a design-based regression method",
                  friendly_error(run_survey_model(d,
                    svy_spec("y", "continuous", "ols")))))
})

ok("the survey methods are in the shared label vector", {
  # diagnostic_plot(), the report and the planner all look labels up there; a
  # missing entry took the whole fit down with a subscript error.
  stopifnot(all(c("svy_gaussian", "svy_binomial", "svy_poisson") %in%
                  names(MODEL_METHOD_LABELS)),
            identical(unname(SURVEY_METHOD_LABELS[["svy_binomial"]]),
                      unname(MODEL_METHOD_LABELS[["svy_binomial"]])))
})

cat("\n== replicate-weight designs ==\n")

# Build a genuine clustered design, then derive JK1 replicate weights FROM it,
# so both routes describe exactly the same sample and must agree.
set.seed(8)
rep_src <- do.call(rbind, lapply(seq_len(30L), function(i) data.frame(
  psu = sprintf("p%02d", i), w = 5,
  y = stats::rnorm(10, i * 0.05, 1),
  b = factor(stats::rbinom(10, 1, 0.4), levels = c(0, 1), labels = c("No", "Yes")),
  stringsAsFactors = FALSE)))
taylor <- survey::svydesign(ids = ~psu, weights = ~w, data = rep_src)
rep_ref <- suppressWarnings(survey::as.svrepdesign(taylor, type = "JK1"))
rw <- stats::weights(rep_ref, type = "analysis")
colnames(rw) <- sprintf("rep%03d", seq_len(ncol(rw)))
rep_d <- cbind(rep_src, as.data.frame(rw))
rep_spec <- list(weights = "w", repweights = "^rep[0-9]+$", reptype = "JK1")

ok("replicate columns count as a declared design on their own", {
  stopifnot(survey_design_declared(list(repweights = "^rep")),
            survey_is_replicate(rep_spec),
            !survey_is_replicate(spec))
})

ok("a name pattern resolves the whole replicate block, in order", {
  cols <- survey_resolve_repweights(rep_d, "^rep[0-9]+$")
  stopifnot(length(cols) == 30L, identical(cols, sort(cols)),
            identical(cols[[1]], "rep001"))
  # an explicit list works too
  stopifnot(identical(survey_resolve_repweights(rep_d, c("rep002", "rep001")),
                      c("rep001", "rep002")))
})

ok("a pattern matching nothing, or a named column that is absent, is refused", {
  stopifnot(grepl("No columns match the replicate-weight pattern",
                  friendly_error(survey_resolve_repweights(rep_d, "^nope"))),
            grepl("not in the data",
                  friendly_error(survey_resolve_repweights(rep_d,
                                                           c("rep001", "ghost")))))
})

ok("the replicate design reproduces survey's own estimate and SE", {
  m <- survey_mean(rep_d, rep_spec, "y")
  r <- survey::svymean(~y, rep_ref)
  stopifnot(all.equal(m$Mean, unname(stats::coef(r)), tolerance = TOL_STAT),
            all.equal(m[["Std. Error"]], as.numeric(survey::SE(r)),
                      tolerance = TOL_STAT))
})

ok("the replicate SE exceeds the SE that ignores the design", {
  # This is the whole point: averaging the replicate weights away, or using the
  # base weight alone, gives standard errors that are far too small.
  m <- survey_mean(rep_d, rep_spec, "y")
  naive <- stats::sd(rep_d$y) / sqrt(nrow(rep_d))
  stopifnot(m[["Std. Error"]] > naive * 1.2)
})

ok("the design summary describes the replicate block, not PSUs", {
  s <- survey_design_summary(build_survey_design(rep_d, rep_spec))
  stopifnot(grepl("replicate weights, JK1", s$Value[s$Field == "Design type"]),
            grepl("30 columns", s$Value[s$Field == "Replicate weights"]),
            !"Primary sampling units" %in% s$Field)
})

ok("the note explains where the standard errors come from", {
  b <- build_survey_design(rep_d, rep_spec)
  n <- .survey_note(b)
  stopifnot(grepl("30 JK1 replicate weights", n),
            grepl("carried inside those weights", n),
            # an undeclared scale factor is disclosed, not left to a console warning
            grepl("No scale factor was declared", n))
})

ok("a declared scale factor is used and reported", {
  b <- build_survey_design(rep_d, c(rep_spec, list(scale = 0.9)))
  stopifnot(grepl("Scale factor 0.9, as declared", .survey_note(b)))
})

ok("design-based regression works from replicate weights", {
  r <- run_survey_model(rep_d, c(list(outcome = "y", outcome_type = "continuous",
                                      predictors = "b", method = "svy_gaussian"),
                                 rep_spec))
  ref <- survey::svyglm(y ~ b, design = rep_ref, family = stats::gaussian())
  s <- summary(ref)$coefficients
  stopifnot(all.equal(r$coefficients$Estimate, unname(s[, 1]), tolerance = TOL_STAT),
            all.equal(r$coefficients[["Std. Error"]], unname(s[, 2]),
                      tolerance = TOL_STAT))
})

ok("an unknown scheme, too few replicates, or Fay without rho is refused", {
  stopifnot(grepl("is not a replicate scheme",
                  friendly_error(build_survey_design(rep_d,
                    c(rep_spec[c("weights", "repweights")], list(reptype = "magic"))))),
            grepl("at least two replicate columns",
                  friendly_error(build_survey_design(rep_d,
                    list(weights = "w", repweights = "rep001", reptype = "JK1")))),
            grepl("needs its rho constant",
                  friendly_error(build_survey_design(rep_d,
                    c(rep_spec[c("weights", "repweights")], list(reptype = "Fay"))))))
})

ok("a non-numeric replicate weight is refused by name", {
  bad <- rep_d; bad$rep005 <- as.character(bad$rep005); bad$rep005[1] <- "heavy"
  stopifnot(grepl("'rep005' contains values that are not numbers",
                  friendly_error(build_survey_design(bad, rep_spec))))
})

report_and_exit()
