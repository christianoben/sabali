# tests/test_assumptions_catalogue.R -----------------------------------------
# The reorganised assumption layer: the catalogue and its identifiers, the
# graded verdicts, the magnitude-based (not p-value-based) normality rule, and
# the four checks that were missing - linearity, autocorrelation, sphericity and
# zero-inflation.
#
# Run:  Rscript tests/test_assumptions_catalogue.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# ============================================================================
# Catalogue and identifiers
# ============================================================================
cat_all <- assumption_catalogue()
ids <- vapply(cat_all, function(a) a$id, character(1))

ok("catalogue is non-empty with unique ids and known families", {
  stopifnot(length(ids) >= 15, !any(duplicated(ids)),
            all(vapply(cat_all, function(a) a$family, character(1)) %in%
                  names(ASSUMPTION_FAMILY_LABELS)))
})
ok("the four checks that were missing are now in the catalogue", {
  stopifnot(all(c("linearity", "independence", "sphericity", "zero_inflation") %in% ids))
})
ok("every check the planner can name has an entry", {
  # These are the ids the planner's own catalogues use, so the two layers join.
  stopifnot(all(c("normality", "normality_residuals", "variance_homogeneity",
                  "homoscedasticity", "expected_counts", "overdispersion",
                  "influence", "multicollinearity", "proportional_hazards",
                  "events_per_variable", "singular_fit") %in% ids))
})
ok("assumption_check / assumption_ids look entries up and filter by analysis", {
  stopifnot(identical(assumption_check("sphericity")$family, "structure"),
            is.null(assumption_check("astrology")),
            "proportional_hazards" %in% assumption_ids("cox_ph"),
            !("sphericity" %in% assumption_ids("cox_ph")),
            "sphericity" %in% assumption_ids("rm_anova"),
            length(assumption_ids()) == length(ids))
})
ok("every remedy names an action, and any id it cites really exists", {
  tf <- if (exists("transform_method_ids")) transform_method_ids() else character(0)
  tests <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  methods <- c(vapply(planner_regression_catalogue(), function(t) t$id, character(1)),
               vapply(planner_advanced_catalogue(), function(t) t$id, character(1)))
  for (a in cat_all) for (r in a$remedies) {
    stopifnot(nzchar(r$what %||% ""))
    if (!is.null(r$transform)) stopifnot(r$transform %in% tf)
    if (!is.null(r$test))      stopifnot(r$test %in% tests)
    if (!is.null(r$method))    stopifnot(r$method %in% methods)
  }
})
ok("remedies are offered for problems and withheld when there is nothing to do", {
  stopifnot(length(assumption_remedies("normality", "violated")) > 0,
            length(assumption_remedies("normality", "review")) > 0,
            length(assumption_remedies("normality", "ok")) == 0,
            length(assumption_remedies("normality", "untested")) == 0)
})

# ============================================================================
# Graded verdicts
# ============================================================================
ok("severity vocabulary is ordered and labelled", {
  stopifnot(identical(ASSUMPTION_SEVERITY,
                      c("ok", "review", "violated", "untested")),
            all(ASSUMPTION_SEVERITY %in% names(ASSUMPTION_SEVERITY_LABELS)))
})
ok("assumption_worse takes the more serious of two verdicts", {
  stopifnot(identical(assumption_worse("ok", "review"), "review"),
            identical(assumption_worse("violated", "review"), "violated"),
            identical(assumption_worse("ok", "ok"), "ok"),
            identical(assumption_worse("untested", "review"), "review"),
            identical(assumption_worse("untested", "untested"), "untested"))
})
ok("a result carries its id, verdict, remedy and a legacy ok flag", {
  r <- assumption_result("normality", "violated", "note text", statistic = 3)
  stopifnot(r$check == "normality", r$severity == "violated",
            isFALSE(r$ok), length(r$remedy) > 0,
            r$label == "Normality of the values", r$family == "distribution")
  stopifnot(isTRUE(assumption_result("normality", "ok", "n")$ok),
            is.na(assumption_result("normality", "untested", "n")$ok))
})
ok("assumption_summary counts verdicts and reports the worst", {
  rs <- list(assumption_result("normality", "ok", "a"),
             assumption_result("linearity", "review", "b"),
             assumption_result("independence", "violated", "c"),
             assumption_result("sphericity", "untested", "d"))
  s <- assumption_summary(rs)
  stopifnot(s$counts[["ok"]] == 1, s$counts[["review"]] == 1,
            s$counts[["violated"]] == 1, s$counts[["untested"]] == 1,
            s$worst == "violated", grepl("1 of 4", s$headline),
            s$n == 4)
  s2 <- assumption_summary(list(assumption_result("normality", "ok", "a")))
  stopifnot(s2$worst == "ok", grepl("satisfied", s2$headline))
  stopifnot(assumption_summary(list())$worst == "untested")
})

# ============================================================================
# Magnitude, not significance
# ============================================================================
ok("normality is judged on shape, so a huge sample is not condemned by its p-value", {
  set.seed(11)
  # 5000 draws from a mildly skewed distribution: Shapiro rejects, shape is fine.
  x <- qnorm(ppoints(3000)) + 0.15 * qnorm(ppoints(3000))^2
  r <- check_normality(x)
  stopifnot(abs(r$detail$skewness) < 1, r$severity %in% c("ok", "review"),
            !identical(r$severity, "violated"))
})
ok("a well-transformed variable is reported as satisfied - the case that failed before", {
  dc <- if (file.exists("data/lvct_cohort_repeated.csv"))
    utils::read.csv("data/lvct_cohort_repeated.csv", stringsAsFactors = FALSE) else NULL
  if (!is.null(dc)) {
  raw <- check_normality(dc$viral_load)
  lg  <- check_normality(log10(dc$viral_load))
  # The old p-value rule called BOTH of these failures (Shapiro p < 2e-16 for
  # each). Now the skew is what matters: 6.36 raw versus 0.75 after log10, so
  # the transformation is recognised as having worked.
  stopifnot(raw$severity == "violated", abs(raw$detail$skewness) > 5,
            abs(lg$detail$skewness) < 1,
            is.finite(lg$p), lg$p < 0.05,   # the test still rejects; we no longer care
            lg$severity != "violated")
  # It is not fully "ok" either, and that is correct: this cohort's viral load
  # is genuinely bimodal - a suppressed cluster near 10^2.5 and a failing
  # cluster near 10^4.5 - which the bimodality detector picks up. Skew alone
  # cannot see that, which is exactly why the detector was added.
  stopifnot(lg$severity == "review", isTRUE(lg$detail$bimodal),
            grepl("two peaks", lg$note))
  }
})
ok("the formal test and its exact p-value are still reported", {
  set.seed(12); x <- rnorm(50)
  r <- check_normality(x)
  stopifnot(identical(r$test, "Shapiro-Wilk"),
            abs(r$p - stats::shapiro.test(x)$p.value) < 1e-12,
            grepl("Shapiro-Wilk p =", r$note))
})
ok("below n = 30 a rejecting test can still worsen the verdict (no CLT cover)", {
  # Shape alone would pass this; with n < 30 the formal test is allowed in.
  x <- c(rnorm(24), 6, 6.2)
  r <- check_normality(x)
  stopifnot(r$detail$n < 30, r$severity %in% c("review", "violated"))
})
ok("a heavy-tailed but symmetric sample is caught by kurtosis, not skew", {
  # Symmetric contamination on purpose: skew stays near zero, tails do not.
  set.seed(13); x <- c(rnorm(200), 12, -12, 13, -13, 14, -14)
  r <- check_normality(x)
  stopifnot(abs(r$detail$skewness) < 1.2, r$detail$kurtosis > 3,
            r$severity %in% c("review", "violated"))
})
ok("a huge sample with no normality dependency available is still graded", {
  # Previously n > 5000 without {nortest} returned a blanket ok = TRUE.
  set.seed(14); x <- rexp(6000, 1)
  r <- check_normality(x)
  stopifnot(r$severity == "violated",           # skew of an exponential is ~2
            r$detail$n == 6000)
})
ok("too few values is 'untested', not a silent pass", {
  r <- check_normality(c(1, 2))
  stopifnot(r$severity == "untested", is.na(r$ok), identical(r$test, "none"))
})
ok("group normality reports the worst group and names the offenders", {
  set.seed(15)
  x <- c(rnorm(40), rexp(40, 1) * 5); g <- rep(c("a", "b"), each = 40)
  r <- check_normality_by_group(x, g)
  stopifnot(r$severity %in% c("review", "violated"), "b" %in% r$detail$flagged,
            !("a" %in% r$detail$flagged), length(r$by_group) == 2)
})
ok("variance homogeneity is judged on the variance ratio, not Levene's p", {
  set.seed(16)
  # Equal variances at large n: p can wobble, the ratio should not.
  x1 <- c(rnorm(400, 0, 1), rnorm(400, 0, 1.05)); g1 <- rep(c("a", "b"), each = 400)
  r1 <- check_variance_homogeneity(x1, g1)
  stopifnot(r1$severity == "ok", r1$statistic < 2)
  x2 <- c(rnorm(200, 0, 1), rnorm(200, 0, 5)); g2 <- rep(c("a", "b"), each = 200)
  r2 <- check_variance_homogeneity(x2, g2)
  stopifnot(r2$severity == "violated", r2$statistic > 4,
            grepl("Welch", r2$note), is.finite(r2$p))
})

# ============================================================================
# New: linearity
# ============================================================================
ok("a straight relationship passes", {
  set.seed(21); x <- seq(0, 10, length.out = 120); y <- 2 + 3 * x + rnorm(120, 0, 2)
  r <- check_linearity(x, y)
  stopifnot(r$check == "linearity", r$severity == "ok", r$statistic <= 0.01,
            grepl("looks straight", r$note))
})
ok("a strongly curved relationship is flagged with a remedy", {
  set.seed(22); x <- seq(-5, 5, length.out = 150); y <- x^2 + rnorm(150, 0, 1)
  r <- check_linearity(x, y)
  stopifnot(r$severity == "violated", r$statistic > 0.05,
            length(r$remedy) > 0, grepl("not straight", r$note),
            r$detail$r2_curved > r$detail$r2_linear)
})
ok("linearity declines to judge too few distinct predictor values", {
  r <- check_linearity(rep(c(1, 2), 30), rnorm(60))
  stopifnot(r$severity == "untested", grepl("distinct", r$note))
  stopifnot(check_linearity(1:5, 1:5)$severity == "untested")
})
ok("linearity survives missing values and non-numeric input", {
  set.seed(23); x <- c(seq(0, 10, length.out = 100), NA); y <- c(2 * seq(0, 10, length.out = 100), NA)
  stopifnot(check_linearity(x, y)$severity %in% ASSUMPTION_SEVERITY)
  stopifnot(check_linearity(as.character(seq(0, 10, length.out = 60)),
                            rnorm(60))$severity %in% ASSUMPTION_SEVERITY)
})

# ============================================================================
# New: autocorrelation
# ============================================================================
ok("independent residuals pass; a random walk is flagged", {
  set.seed(31)
  r1 <- check_autocorrelation(rnorm(200))
  stopifnot(r1$check == "independence", r1$severity == "ok",
            abs(r1$statistic) <= 0.2,
            abs(r1$detail$durbin_watson - 2) < 0.4)
  r2 <- check_autocorrelation(cumsum(rnorm(200)))
  stopifnot(r2$severity == "violated", r2$statistic > 0.4,
            r2$detail$durbin_watson < 1, length(r2$remedy) > 0,
            grepl("too small", r2$note))
})
ok("negative autocorrelation is described as negative", {
  set.seed(32); e <- as.numeric(stats::arima.sim(list(ar = -0.8), 200))
  r <- check_autocorrelation(e)
  stopifnot(r$statistic < -0.4, r$severity == "violated",
            grepl("negative", r$note), r$detail$durbin_watson > 2)
})
ok("autocorrelation declines on a series that is too short", {
  stopifnot(check_autocorrelation(rnorm(5))$severity == "untested")
})
ok("a fitted model yields an exact Durbin-Watson p-value when lmtest is present", {
  set.seed(33); x <- 1:120; y <- cumsum(rnorm(120))
  fit <- stats::lm(y ~ x)
  r <- check_autocorrelation(stats::residuals(fit), fit)
  if (requireNamespace("lmtest", quietly = TRUE))
    stopifnot(is.finite(r$p), grepl("Durbin-Watson p", r$note))
  stopifnot(r$severity == "violated")
})

# ============================================================================
# New: sphericity
# ============================================================================
ok("two conditions satisfy sphericity automatically", {
  r <- check_sphericity(rnorm(40), rep(1:20, 2), rep(c("t1", "t2"), each = 20))
  stopifnot(r$check == "sphericity", r$severity == "ok", r$statistic == 1,
            grepl("automatically", r$note))
})
ok("compound-symmetric data gives epsilon near 1", {
  # The epsilon estimator is noisy at modest n: with 60 subjects a perfectly
  # spherical sample can still draw 0.86. 200 keeps the fixture about the
  # property being tested rather than about the luck of the draw.
  set.seed(41); n <- 200
  subj <- rnorm(n, 0, 2)
  x <- c(subj + rnorm(n), subj + rnorm(n), subj + rnorm(n))
  r <- check_sphericity(x, rep(1:n, 3), rep(c("t1", "t2", "t3"), each = n))
  stopifnot(r$severity == "ok", r$statistic > 0.9,
            r$detail$conditions == 3, r$detail$subjects == n)
})
ok("wildly unequal difference variances drive epsilon down and are flagged", {
  set.seed(42); n <- 60
  subj <- rnorm(n, 0, 1)
  # The third condition has a far larger variance, breaking sphericity.
  x <- c(subj + rnorm(n, 0, 0.3), subj + rnorm(n, 0, 0.3), subj + rnorm(n, 0, 8))
  r <- check_sphericity(x, rep(1:n, 3), rep(c("t1", "t2", "t3"), each = n))
  stopifnot(r$severity %in% c("review", "violated"), r$statistic < 0.95,
            length(r$remedy) > 0)
})
ok("epsilon is bounded by its theoretical limits and Mauchly is reported", {
  set.seed(43); n <- 40; k <- 4
  subj <- rnorm(n)
  x <- unlist(lapply(1:k, function(i) subj + rnorm(n, 0, i)))
  r <- check_sphericity(x, rep(1:n, k), rep(paste0("t", 1:k), each = n))
  stopifnot(r$statistic >= 1 / (k - 1) - 1e-9, r$statistic <= 1 + 1e-9)
  if (is.finite(r$p)) stopifnot(grepl("Mauchly p", r$note))
})
ok("sphericity declines with too few complete subjects, and on empty input", {
  r <- check_sphericity(rnorm(6), rep(1:2, 3), rep(c("a", "b", "c"), each = 2))
  stopifnot(r$severity == "untested", grepl("too few", r$note))
  stopifnot(check_sphericity(numeric(0), character(0), character(0))$severity == "untested")
})
ok("[data] sphericity runs on the real cohort's three visits", {
  dc <- if (file.exists("data/lvct_cohort_repeated.csv"))
    utils::read.csv("data/lvct_cohort_repeated.csv", stringsAsFactors = FALSE) else NULL
  if (!is.null(dc)) {
  r <- check_sphericity(log10(dc$viral_load), dc$patient_id, dc$visit)
  stopifnot(r$severity %in% c("ok", "review", "violated"),
            r$detail$conditions == 3, r$detail$subjects == 250,
            r$statistic >= 0.5, r$statistic <= 1)
  }
})

# ============================================================================
# New: zero-inflation
# ============================================================================
ok("Poisson counts with the expected number of zeros pass", {
  set.seed(51); y <- rpois(500, 3)
  r <- check_zero_inflation(y, rep(3, 500))
  stopifnot(r$check == "zero_inflation", r$severity == "ok", r$statistic <= 1.25)
})
ok("injected excess zeros are caught with a remedy", {
  set.seed(52); y <- rpois(500, 4); y[sample(500, 150)] <- 0
  r <- check_zero_inflation(y, rep(4, 500))
  stopifnot(r$severity == "violated", r$statistic > 2, length(r$remedy) > 0,
            grepl("more zeros", r$note), r$detail$observed > r$detail$expected)
})
ok("the negative-binomial expectation tolerates more zeros than Poisson", {
  set.seed(53); y <- rnbinom(500, size = 1, mu = 4)
  pois <- check_zero_inflation(y, rep(4, 500))
  nb   <- check_zero_inflation(y, rep(4, 500), theta = 1)
  stopifnot(nb$detail$expected > pois$detail$expected,
            .as_rank(nb$severity) <= .as_rank(pois$severity),
            nb$detail$model == "negative binomial")
})
ok("zero-inflation falls back to the observed mean when no fit is supplied", {
  set.seed(54); y <- rpois(300, 2)
  r <- check_zero_inflation(y)
  stopifnot(r$severity %in% ASSUMPTION_SEVERITY, is.finite(r$statistic))
})
ok("zero-inflation refuses negatives and empty input", {
  stopifnot(check_zero_inflation(c(-1, 2, 3))$severity == "untested",
            check_zero_inflation(numeric(0))$severity == "untested")
})
ok("zeros where none are expected are flagged rather than dividing by zero", {
  r <- check_zero_inflation(c(0, 0, 900, 950), rep(900, 4))
  stopifnot(r$severity == "violated", grepl("essentially none", r$note))
})
ok("[data] missed_visits in the real cohort is assessed for excess zeros", {
  dc <- if (file.exists("data/lvct_cohort_repeated.csv"))
    utils::read.csv("data/lvct_cohort_repeated.csv", stringsAsFactors = FALSE) else NULL
  if (!is.null(dc)) {
  r <- check_zero_inflation(dc$missed_visits)
  stopifnot(r$severity %in% ASSUMPTION_SEVERITY, is.finite(r$detail$observed))
  }
})

# ============================================================================
# The verdict survives to the model layer
# ============================================================================
set.seed(61)
md <- data.frame(y = rnorm(120, 10, 2), x1 = rnorm(120), x2 = rnorm(120))
md$x3 <- md$x1 + rnorm(120, 0, 0.001)          # near-duplicate predictor
fit_ols <- stats::lm(y ~ x1 + x2 + x3, data = md)
ass_ols <- model_assumptions(fit_ols, "ols", "continuous")

ok("model_assumptions returns graded results, not bare notes", {
  stopifnot(length(ass_ols) > 0,
            all(vapply(ass_ols, function(a) !is.null(a$severity), logical(1))),
            all(vapply(ass_ols, function(a) a$severity %in% ASSUMPTION_SEVERITY, logical(1))),
            all(vapply(ass_ols, function(a) !is.null(a$check), logical(1))))
})
ok("every result still carries a note, so the old renderers cannot break", {
  stopifnot(all(vapply(ass_ols, function(a)
    is.character(a$note) && nzchar(a$note), logical(1))))
})
ok("checks are named by id - no more 'Diagnostic 1, Diagnostic 2'", {
  checks <- vapply(ass_ols, function(a) a$check, character(1))
  stopifnot(all(c("normality_residuals", "homoscedasticity",
                  "multicollinearity") %in% checks),
            !any(grepl("^Diagnostic", vapply(ass_ols, function(a) a$label, character(1)))))
})
ok("the collinearity we planted is caught and graded violated", {
  vif <- Filter(function(a) a$check == "multicollinearity", ass_ols)[[1]]
  stopifnot(vif$severity == "violated", vif$max_vif > 10, length(vif$remedy) > 0)
})
ok("an OLS fit also gets a linearity-independent autocorrelation check", {
  stopifnot("independence" %in% vapply(ass_ols, function(a) a$check, character(1)))
})
ok("the diagnostics table now has a verdict and an action column", {
  tab <- model_assumptions_df(ass_ols)
  stopifnot(all(c("Check", "Verdict", "Note", "What to do") %in% names(tab)),
            nrow(tab) == length(ass_ols),
            any(tab$Verdict == "Violated"),
            any(nzchar(tab$`What to do`)),
            !any(grepl("^Diagnostic [0-9]", tab$Check)))
  stopifnot(nrow(model_assumptions_df(list())) == 0)
})
ok("a count model gets both dispersion and zero-inflation checks", {
  set.seed(62)
  cd <- data.frame(y = rpois(200, 3), x = rnorm(200))
  cd$y[sample(200, 60)] <- 0
  cf <- stats::glm(y ~ x, family = stats::poisson, data = cd)
  a <- model_assumptions(cf, "poisson", "count")
  checks <- vapply(a, function(z) z$check, character(1))
  stopifnot(all(c("overdispersion", "zero_inflation") %in% checks))
  zi <- Filter(function(z) z$check == "zero_inflation", a)[[1]]
  stopifnot(zi$severity %in% c("review", "violated"))
})
ok("assumption_summary works on real model output", {
  s <- assumption_summary(ass_ols)
  stopifnot(s$n == length(ass_ols), s$worst == "violated",
            nzchar(s$headline), sum(s$counts) == s$n)
})

# ============================================================================
# Rendering
# ============================================================================
ok("assumption_ui shows the headline, a verdict badge and the remedy", {
  html <- html_text(htmltools::renderTags(assumption_ui(ass_ols))$html)
  stopifnot(grepl("violated", html, ignore.case = TRUE),
            grepl("What to do", html),
            grepl("Violated", html),
            grepl("ridge", html, ignore.case = TRUE))   # a collinearity remedy
})
ok("assumption_ui handles an empty set with the caller's own wording", {
  html <- html_text(htmltools::renderTags(assumption_ui(list(), empty = "nothing here"))$html)
  stopifnot(grepl("nothing here", html))
})
ok("contextual notes render without a misleading verdict badge", {
  a <- model_assumptions(stats::lm(y ~ x1, data = md), "quantile", "continuous")
  ctx <- Filter(function(z) z$check == "context", a)
  stopifnot(length(ctx) == 1)
  html <- html_text(htmltools::renderTags(assumption_ui(ctx))$html)
  stopifnot(grepl("quantile", html), !grepl("Satisfied", html))
})
ok("assumption_df renders a standalone result set", {
  tab <- assumption_df(list(check_linearity(seq(-5, 5, length.out = 100),
                                           seq(-5, 5, length.out = 100)^2)))
  stopifnot(nrow(tab) == 1, tab$Verdict == "Violated", nzchar(tab$`What to do`),
            nrow(assumption_df(list())) == 0)
})

# ============================================================================
# Pre-analysis screening (the Analysis readiness tab)
# ============================================================================
ok("the pre-fit / post-fit split is complete and disjoint", {
  stopifnot(length(intersect(ASSUMPTION_PREFIT, ASSUMPTION_POSTFIT)) == 0,
            setequal(c(ASSUMPTION_PREFIT, ASSUMPTION_POSTFIT), ids),
            all(c("normality", "linearity", "sphericity") %in% ASSUMPTION_PREFIT),
            all(c("normality_residuals", "homoscedasticity",
                  "proportional_hazards") %in% ASSUMPTION_POSTFIT))
})
set.seed(71)
rd <- data.frame(
  id = rep(1:60, each = 3),
  visit = rep(c("A", "B", "C"), 60),
  date = as.Date("2024-01-01") + rep(c(0, 180, 365), 60) + rep(0:59, each = 3),
  y = exp(rnorm(180, 5, 1.6)),
  g = rep(c("m", "f"), 90),
  x1 = rnorm(180), stringsAsFactors = FALSE)
rd$x2 <- rd$x1 + rnorm(180, 0, 0.001)      # near-duplicate predictor
rty <- c(id = "identifier", visit = "nominal", date = "datetime",
         y = "continuous", g = "binary", x1 = "continuous", x2 = "continuous")
rpn <- list(kind = "repeated", id = "id", time = "date")

ok("the screen runs the checks that fit the chosen roles", {
  r <- readiness_assumptions(rd, rty, "y", c("g", "x1", "x2"), rpn)
  checks <- vapply(r, function(z) z$check, character(1))
  stopifnot(length(r) > 0,
            "normality" %in% checks,            # continuous outcome
            "normality_groups" %in% checks,     # x a binary predictor
            "variance_homogeneity" %in% checks,
            "linearity" %in% checks,            # x a numeric predictor
            "multicollinearity" %in% checks,    # two numeric predictors
            "sphericity" %in% checks)           # repeated panel
  stopifnot(all(vapply(r, function(z) nzchar(z$scope), logical(1))))
})
ok("no post-fit check leaks into the screen", {
  r <- readiness_assumptions(rd, rty, "y", c("g", "x1"), rpn)
  stopifnot(!any(vapply(r, function(z) z$check, character(1)) %in% ASSUMPTION_POSTFIT))
})
ok("collinearity is found without fitting an outcome model", {
  r <- readiness_assumptions(rd, rty, "y", c("x1", "x2"), rpn)
  v <- Filter(function(z) z$check == "multicollinearity", r)[[1]]
  stopifnot(v$severity == "violated", v$statistic > 10)
})
ok("the within-subject factor is the visit occasion, not the calendar date", {
  # Every subject has their own dates here, so using the date itself would give
  # one "condition" per row and sphericity could never be computed.
  stopifnot(length(unique(rd$date)) > 100)
  r <- readiness_assumptions(rd, rty, "y", "g", rpn)
  sph <- Filter(function(z) z$check == "sphericity", r)[[1]]
  stopifnot(sph$severity != "untested", sph$detail$conditions == 3,
            sph$detail$subjects == 60, grepl("repeated visits", sph$scope))
})
ok("the occasion helper ranks each subject's visits in time order", {
  occ <- .as_occasion(rd$id, rd$date)
  stopifnot(length(unique(occ)) == 3,
            all(occ[rd$visit == "A"] == "Visit 1"),
            all(occ[rd$visit == "C"] == "Visit 3"))
})
ok("a categorical outcome gets the expected-cell-count rule instead", {
  r <- readiness_assumptions(rd, rty, "g", c("visit"), rpn)
  checks <- vapply(r, function(z) z$check, character(1))
  stopifnot("expected_counts" %in% checks, !("normality" %in% checks))
})
ok("a count outcome is screened for excess zeros", {
  cd <- rd; cd$cnt <- rpois(180, 2); cd$cnt[sample(180, 60)] <- 0
  cty <- c(rty, cnt = "count")
  r <- readiness_assumptions(cd, cty, "cnt", "g", rpn)
  stopifnot("zero_inflation" %in% vapply(r, function(z) z$check, character(1)))
})
ok("a time series is screened for autocorrelation", {
  td <- data.frame(t = 1:150, y = cumsum(rnorm(150)), x = rnorm(150))
  tty <- c(t = "datetime", y = "continuous", x = "continuous")
  r <- readiness_assumptions(td, tty, "y", "x",
                             list(kind = "time_series", time = "t"))
  a <- Filter(function(z) z$check == "independence", r)
  stopifnot(length(a) == 1, a[[1]]$severity == "violated")
})
ok("no outcome, no data, or an unknown outcome yields nothing rather than an error", {
  stopifnot(length(readiness_assumptions(rd, rty, NULL, c("g"))) == 0,
            length(readiness_assumptions(NULL, rty, "y", "g")) == 0,
            length(readiness_assumptions(rd[0, ], rty, "y", "g")) == 0)
  # An outcome not in the data is simply not screened.
  stopifnot(length(readiness_assumptions(rd, rty, "ghost", "g")) <= 1)
})
ok("the number of results stays readable on a wide dataset", {
  wide <- rd
  for (i in 1:20) wide[[paste0("p", i)]] <- rnorm(180)
  wty <- c(rty, stats::setNames(rep("continuous", 20), paste0("p", 1:20)))
  r <- readiness_assumptions(wide, wty, "y", paste0("p", 1:20), rpn,
                             max_per_check = 5)
  stopifnot(sum(vapply(r, function(z) z$check, character(1)) == "linearity") <= 5)
})

# --- the loop: screen, fix, re-screen ---------------------------------------
ok("applying the remedy the screen names actually clears the violations", {
  before <- readiness_assumptions(rd, rty, "y", c("g", "x1"), rpn)
  nviol <- function(z) sum(vapply(z, function(w) w$severity == "violated", logical(1)))
  stopifnot(nviol(before) > 0)
  # The normality result must offer a transformation this app can run.
  norm <- Filter(function(z) z$check == "normality", before)[[1]]
  tids <- Filter(nzchar, vapply(norm$remedy, function(r) r$transform %||% "",
                                character(1)))
  stopifnot("log10" %in% tids)
  res <- apply_transforms(rd, list(transform_spec("log10", variable = "y")), rty, rpn)
  ty2 <- rty; for (rec in res$records) for (nm in rec$output) ty2[[nm]] <- rec$produces
  after <- readiness_assumptions(res$df, ty2, "y_log10", c("g", "x1"), rpn)
  stopifnot(nviol(after) < nviol(before))
})

# ============================================================================
# The planner's assumptions become runnable checks
# ============================================================================
ok("free text resolves to catalogue check ids", {
  stopifnot(
    assumption_match_check("Normality of residuals") == "normality_residuals",
    assumption_match_check("Homoscedasticity of residuals") == "homoscedasticity",
    assumption_match_check("Sphericity for rm_anova if used") == "sphericity",
    assumption_match_check("Independence of observations at the cluster level") == "independence",
    assumption_match_check("Linear relationship between predictors and outcome") == "linearity",
    assumption_match_check("No influential outliers") == "influence",
    assumption_match_check("multicollinearity") == "multicollinearity",
    assumption_match_check("") == "", assumption_match_check(NULL) == "")
})
ok("every catalogue id resolves to itself", {
  for (i in ids) stopifnot(identical(assumption_match_check(i), i))
})
ok("a plan's assumptions split into screenable now and needs-a-model", {
  # The exact list the cohort dataset produced.
  plan <- list(assumptions_to_check = list(
    "Normality of residuals (viral_load is right-skewed)",
    "Homoscedasticity of residuals",
    "Linear relationship between predictors and outcome",
    "Independence of observations at the cluster level",
    "Sphericity for rm_anova if used",
    "No influential outliers given extreme maximum viral load",
    "Random effects structure for LMM"))
  r <- plan_assumption_ids(plan)
  stopifnot(all(c("linearity", "independence", "sphericity") %in% r$prefit),
            all(c("normality_residuals", "homoscedasticity", "influence",
                  "singular_fit") %in% r$postfit),
            length(intersect(r$prefit, r$postfit)) == 0)
})
ok("text that matches nothing is reported as advice rather than dropped", {
  r <- plan_assumption_ids(list(assumptions_to_check = list(
    "normality", "consult a subject-matter expert about the sampling frame")))
  stopifnot("normality" %in% r$prefit, length(r$unmatched) == 1)
})
ok("the planner prompt advertises the assumption vocabulary", {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    m <- build_planner_metadata(
      data.frame(a = 1:5, b = rnorm(5)), c(a = "count", b = "continuous"))
    pr <- planner_prompt("Risk factors?", m)
    stopifnot(grepl("available_assumptions", pr$user, fixed = TRUE),
              grepl("available_assumptions", pr$system, fixed = TRUE),
              grepl("sphericity", pr$user, fixed = TRUE))
  }
})
ok("assumption_df carries the variables column for a screen", {
  r <- readiness_assumptions(rd, rty, "y", c("g", "x1"), rpn)
  tab <- assumption_df(r)
  stopifnot("Variables" %in% names(tab), nrow(tab) == length(r),
            all(nzchar(tab$Variables)),
            nrow(assumption_df(list())) == 0)
})
ok("the screen renders with scopes and remedies", {
  r <- readiness_assumptions(rd, rty, "y", c("g", "x1"), rpn)
  html <- html_text(htmltools::renderTags(assumption_ui(r))$html)
  stopifnot(grepl("\\[y\\]", html) || grepl("y by g", html),
            grepl("What to do", html))
})

report_and_exit()
