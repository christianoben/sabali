# tests/test_path_analysis.R -------------------------------------------------
# Observed-variable path analysis and mediation.
#
# The ARCH 1.0 community analysis asks a question a regression cannot answer.
# Its model is
#   svy: gsem (AMU_DAYS_Week -> ESCrE_vitek) (HS_indiv_all -> AMU_DAYS_Week)
#            (HS_indiv_all -> ESCrE_vitek) (sex -> ...) ... nocapslatent
# - household and individual exposures acting on ESCrE colonisation both directly
# and THROUGH healthcare seeking and antibiotic-use days. Adjusting for a
# mediator in a single regression does not decompose that effect; it removes it.
#
# lavaan does the estimation, so what is tested here is what is ours: the model
# syntax (including the two-step routes an ordered mediator chain creates), the
# indirect and total effects, the mapping from the declared survey design
# including the estimator conflicts lavaan imposes, and the guards.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

if (!requireNamespace("lavaan", quietly = TRUE)) {
  cat("\n  SKIP  the 'lavaan' package is not installed (see install_dependencies.R)\n")
  report_and_exit()
}

cat("\n== model syntax ==\n")

ok("the outcome equation carries mediators and direct paths, both labelled", {
  syn <- path_model_syntax("y", "m", c("x1", "x2"))
  stopifnot(grepl("y ~ b1*m + c1*x1 + c2*x2", syn, fixed = TRUE),
            grepl("m ~ a1_1*x1 + a1_2*x2", syn, fixed = TRUE))
})

ok("each exposure gets an indirect effect per mediator and one total", {
  syn <- path_model_syntax("y", c("m1", "m2"), "x")
  stopifnot(grepl("ind_x_m1 := a1_1*b1", syn, fixed = TRUE),
            grepl("ind_x_m2 := a2_1*b2", syn, fixed = TRUE),
            grepl("total_x :=", syn, fixed = TRUE))
})

ok("an ordered mediator chain produces the two-step route", {
  # This is the community model's shape: exposure -> healthcare seeking ->
  # antibiotic days -> colonisation. Without the d-path the chain is invisible.
  syn <- path_model_syntax("y", c("m1", "m2"), "x")
  stopifnot(grepl("m2 ~ a2_1*x + d2_1*m1", syn, fixed = TRUE),
            grepl("ind_x_m1_m2 := a1_1*d2_1*b2", syn, fixed = TRUE),
            # and the total includes every route, direct and both indirect
            grepl("total_x := c1 + a1_1*b1 + a2_1*b2 + a1_1*d2_1*b2",
                  syn, fixed = TRUE))
})

ok("covariates enter every equation but get no indirect effects", {
  syn <- path_model_syntax("y", "m", "x", covariates = "age")
  stopifnot(grepl("y ~ b1*m + c1*x + age", syn, fixed = TRUE),
            grepl("m ~ a1_1*x + age", syn, fixed = TRUE),
            !grepl("ind_age", syn, fixed = TRUE),
            !grepl("total_age", syn, fixed = TRUE))
})

ok("non-syntactic variable names are quoted and given safe parameter labels", {
  syn <- path_model_syntax("1_out", "1_med", "1_exp")
  stopifnot(grepl("`1_out` ~ b1*`1_med` + c1*`1_exp`", syn, fixed = TRUE),
            # a lavaan label must be syntactic, so the tag drops the punctuation
            grepl("ind_1exp_1med := a1_1*b1", syn, fixed = TRUE))
})

cat("\n== the guards ==\n")

ok("a model with no mediator is refused, and says why", {
  msg <- friendly_error(path_model_syntax("y", character(0), "x"))
  stopifnot(grepl("needs at least one mediator", msg),
            grepl("Regression tab already fits", msg))
})
ok("a model with no exposure is refused", {
  stopifnot(grepl("at least one exposure",
                  friendly_error(path_model_syntax("y", "m", character(0)))))
})
ok("a variable cannot hold two roles at once", {
  stopifnot(grepl("cannot be both an exposure and a mediator",
                  friendly_error(path_model_syntax("y", "m", c("x", "m")))),
            grepl("outcome cannot also be",
                  friendly_error(path_model_syntax("y", "m", c("x", "y")))),
            grepl("covariate cannot also be",
                  friendly_error(path_model_syntax("y", "m", "x", "x"))))
})

# --- a fixture with a real indirect path -----------------------------------
# x acts on y ONLY through m, so the indirect effect must be found and the direct
# one must not. The outcome's error is residualised against x, z and the
# mediator's error so the direct path is zero IN SAMPLE, not merely in
# expectation: with a plain rnorm() draw the sample direct effect at seed 77 came
# out at -0.115 (z = -2.7) purely by chance, which `lm()` reproduces exactly - so
# asserting a null result on one random draw tests the seed, not the code.
set.seed(77)
n <- 800L
d <- data.frame(x = stats::rnorm(n), z = stats::rnorm(n))
e_m <- stats::rnorm(n, 0, 0.8)
d$m <- 0.6 * d$x + 0.2 * d$z + e_m
e_y <- stats::rnorm(n, 0, 0.9)
e_y <- stats::resid(stats::lm(e_y ~ d$x + d$z + e_m))
d$y <- 0.7 * d$m + 0.1 * d$z + e_y                       # no direct x -> y
d$hh <- rep(sprintf("h%03d", seq_len(n / 4)), each = 4)
d$w <- rep(c(1, 2), length.out = n)
d$ybin <- as.integer(d$y > stats::median(d$y))

base <- list(outcome = "y", mediators = "m", exposures = "x")

cat("\n== fitting ==\n")

ok("a purely indirect effect is recovered, and no direct effect invented", {
  # z causes BOTH m and y in the fixture, so it has to be adjusted for; see the
  # confounding test below for what happens when it is not.
  r <- run_path_model(d, c(base, list(covariates = "z")))
  e <- r$effects
  ind <- e[e$Effect == "ind_x_m", ]
  stopifnot(nrow(ind) == 1L, ind$`p-value` < 0.001,
            # planted as 0.6 * 0.7 = 0.42
            abs(ind$Estimate - 0.42) < 0.08)
  direct <- r$coefficients[r$coefficients$Term == "y ← x", ]
  # Zero in sample by construction, so this is a real assertion rather than a
  # 1-in-20 coin flip.
  stopifnot(nrow(direct) == 1L, abs(direct$Estimate) < 0.02,
            abs(direct$Statistic) < 3)
})

ok("omitting a mediator-outcome confounder biases the decomposition", {
  # The module's own caveat says an unmeasured cause of both a mediator and the
  # outcome biases the split. This pins that the caveat is real and reproducible
  # rather than boilerplate: dropping z from the same data inflates the indirect
  # effect and manufactures a direct one that does not exist.
  adj  <- run_path_model(d, c(base, list(covariates = "z")))
  unadj <- run_path_model(d, base)
  ind_adj <- adj$effects$Estimate[adj$effects$Effect == "ind_x_m"]
  ind_un  <- unadj$effects$Estimate[unadj$effects$Effect == "ind_x_m"]
  dir_un  <- unadj$coefficients[unadj$coefficients$Term == "y ← x", ]
  stopifnot(ind_un > ind_adj,                       # indirect inflated
            abs(dir_un$Estimate) > abs(adj$coefficients$Estimate[
              adj$coefficients$Term == "y ← x"]))   # spurious direct path
  note <- paste(vapply(unadj$assumptions, function(a) a$note %||% "",
                       character(1)), collapse = " ")
  stopifnot(grepl("unmeasured cause of both a mediator and the outcome", note))
})

ok("path estimates match lavaan called directly", {
  r <- run_path_model(d, base)
  ref <- lavaan::sem(path_model_syntax("y", "m", "x"), data = d)
  pe <- lavaan::parameterEstimates(ref)
  pe <- pe[pe$op == "~", ]
  stopifnot(all.equal(sort(r$coefficients$Estimate), sort(pe$est),
                      tolerance = TOL_STAT))
})

ok("the total effect equals the direct plus indirect routes", {
  r <- run_path_model(d, base)
  e <- r$effects
  tot <- e$Estimate[e$Effect == "total_x"]
  ind <- e$Estimate[e$Effect == "ind_x_m"]
  dir <- r$coefficients$Estimate[r$coefficients$Term == "y ← x"]
  stopifnot(all.equal(tot, dir + ind, tolerance = TOL_EFFECT))
})

ok("a two-mediator chain reports the two-step route", {
  d2 <- d
  d2$m2 <- 0.5 * d2$m + 0.3 * d2$x + stats::rnorm(nrow(d2), 0, 0.7)
  d2$y <- 0.5 * d2$m2 + 0.3 * d2$m + stats::rnorm(nrow(d2), 0, 0.8)
  r <- run_path_model(d2, list(outcome = "y", mediators = c("m", "m2"),
                               exposures = "x"))
  stopifnot("ind_x_m_m2" %in% r$effects$Effect,
            r$effects$`p-value`[r$effects$Effect == "ind_x_m_m2"] < 0.01)
})

ok("a binary outcome is modelled as ordered (probit) by default", {
  r <- run_path_model(d, list(outcome = "ybin", mediators = "m",
                              exposures = "x"))
  note <- vapply(r$assumptions, function(a) a$note %||% "", character(1))
  stopifnot(any(grepl("ordered \\(probit\\)", note)),
            identical(path_ordered_vars(d, c("ybin", "m")), "ybin"))
})

ok("the declared type wins over the observed shape when deciding ordered", {
  tp <- c(y = "continuous", m = "continuous")
  stopifnot(identical(path_ordered_vars(d, c("y", "m"), tp), character(0)))
  # a 0/1 column with no declared type is still recognised
  stopifnot(identical(path_ordered_vars(d, "ybin"), "ybin"))
})

cat("\n== the survey design, and the conflicts lavaan imposes ==\n")

ok("a continuous outcome takes the cluster and the weight", {
  r <- run_path_model(d, c(base, list(psu = "hh", weights = "w")))
  note <- paste(vapply(r$assumptions, function(a) a$note %||% "", character(1)),
                collapse = " ")
  stopifnot(grepl("cluster-robust", note), grepl("Weighted by w", note),
            !grepl("could NOT be fully applied", note))
})

ok("a probit model reports that the design could not be applied", {
  # lavaan cannot combine a threshold model with weights or clustering. The
  # probit model is kept - it is the right estimand - and the loss is stated
  # rather than the design being silently dropped.
  r <- run_path_model(d, list(outcome = "ybin", mediators = "m",
                              exposures = "x", psu = "hh", weights = "w"))
  note <- paste(vapply(r$assumptions, function(a) a$note %||% "", character(1)),
                collapse = " ")
  stopifnot(grepl("could NOT be fully applied", note),
            grepl("cluster-robust standard errors", note),
            grepl("sampling weight", note),
            grepl("treat binary variables as continuous", note))
})

ok("linear = TRUE applies the design and says what it costs", {
  r <- run_path_model(d, list(outcome = "ybin", mediators = "m",
                              exposures = "x", psu = "hh", weights = "w",
                              linear = TRUE))
  note <- paste(vapply(r$assumptions, function(a) a$note %||% "", character(1)),
                collapse = " ")
  stopifnot(grepl("cluster-robust", note), grepl("Weighted by w", note),
            grepl("risk-difference", note),
            !grepl("could NOT be fully applied", note))
})

cat("\n== a saturated model must not look well-fitting ==\n")

ok("a saturated model's perfect fit indices are called out, not reported", {
  # Every arrow drawn means df = 0, so CFI/TLI/RMSEA are 1/1/0 by construction.
  # Presenting those unqualified would read as strong evidence of a good model.
  r <- run_path_model(d, base)
  stopifnot(field_value(r$fit_stats, "df") < 1)
  note <- paste(vapply(r$assumptions, function(a) a$note %||% "", character(1)),
                collapse = " ")
  stopifnot(grepl("saturated", note), grepl("perfect by construction", note),
            grepl("saturated", r$interpretation))
})

cat("\n== the write-up and the result contract ==\n")

ok("the write-up leads on mediation and keeps the causal caveat", {
  r <- run_path_model(d, base)
  stopifnot(grepl("mediation", r$interpretation),
            grepl("mediator ordering is assumed", r$interpretation),
            grepl("not proof of causation", r$interpretation))
})

ok("the result carries the effects table and the syntax it fitted", {
  r <- run_path_model(d, base)
  stopifnot(is.data.frame(r$effects), nrow(r$effects) >= 2L,
            all(c("Effect", "Estimate", "p-value") %in% names(r$effects)),
            is.character(r$syntax), grepl("ind_x_m", r$syntax),
            # no residual plot: it is not meaningful for a path model
            is.null(r$diagnostic_plot))
})

ok("a many-category factor is refused by name rather than coerced", {
  dd <- d
  dd$g <- factor(sample(c("a", "b", "c"), nrow(dd), TRUE))
  msg <- friendly_error(run_path_model(dd, list(outcome = "y", mediators = "m",
                                                exposures = "g")))
  stopifnot(grepl("has 3 categories", msg), grepl("indicator variables", msg))
})

ok("a two-level factor is converted rather than refused", {
  dd <- d
  dd$g <- factor(ifelse(dd$x > 0, "hi", "lo"))
  r <- run_path_model(dd, list(outcome = "y", mediators = "m", exposures = "g"))
  stopifnot("ind_g_m" %in% r$effects$Effect)
})

ok("a variable not in the data is named in the error", {
  stopifnot(grepl("not in the data",
                  friendly_error(run_path_model(d, list(outcome = "y",
                    mediators = "m", exposures = "nope")))))
})

# --- against the real community extract ------------------------------------
# Opt in with SABALI_COMMUNITY_DTA. Skipped otherwise, so no participant data is
# carried by the suite.
comm <- Sys.getenv("SABALI_COMMUNITY_DTA", "")
if (nzchar(comm) && file.exists(comm)) {
  cat("\n== real ARCH 1.0 community path model ==\n")
  ok("the community gsem model fits and finds mediation", {
    real <- read_any_format(comm, basename(comm))
    spec <- list(outcome = "ESCrE_vitek",
                 mediators = c("HS_indiv_all", "AMU_DAYS_Week"),
                 exposures = c("sex", "hh_toilettype_risk", "tend_poultry",
                               "rural_urban", "indiv_waterfrom_risk",
                               "adultchild"),
                 psu = "hhid")
    stopifnot(all(unlist(spec[c("outcome", "mediators", "exposures")]) %in%
                    names(real)))
    r <- run_path_model(real, spec)
    ind <- r$effects[startsWith(r$effects$Effect, "ind_"), ]
    cat(sprintf("      %d indirect routes, %d with p < 0.05; n = %d\n",
                nrow(ind), sum(ind$`p-value` < 0.05, na.rm = TRUE),
                field_value(r$fit_stats, "n used")))
    stopifnot(nrow(ind) == 18L, all(is.finite(ind$Estimate)),
              any(ind$`p-value` < 0.05, na.rm = TRUE))
  })
} else {
  cat("\n  (set SABALI_COMMUNITY_DTA to also run the real-data check)\n")
}

report_and_exit()
