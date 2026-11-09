# tests/test_planner.R -------------------------------------------------------
# The AI analysis planner (fct_planner.R). AI-ONLY: no offline fallback. Tests
# the controlled-vocabulary catalogues, the metadata builder, the validator,
# and that ai_plan() refuses when no AI endpoint is configured.
#
# Run:  Rscript tests/test_planner.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

.clear_ai_env <- function()
  Sys.unsetenv(c("AI_INTERPRET_ENDPOINT", "AI_INTERPRET_KEY",
                 "AI_INTERPRET_MODEL", "AI_INTERPRET_THINKING", "OPENCODE_API_KEY"))
.clear_ai_env()

# --- catalogues -------------------------------------------------------------
tests_cat <- planner_test_catalogue()
test_ids  <- vapply(tests_cat, function(t) t$id, character(1))

ok("test catalogue is non-empty with unique ids", {
  stopifnot(length(test_ids) >= 18, !any(duplicated(test_ids)))
})
ok("catalogue includes the repeated-measures binary tests (Cochran's Q, McNemar)", {
  stopifnot(all(c("cochran_q", "mcnemar") %in% test_ids))
})
ok("catalogue includes one-sample and repeated-measures correlation", {
  stopifnot(all(c("one_sample_t", "rm_correlation") %in% test_ids))
})
ok("regression + advanced catalogues both offer 'none'", {
  reg <- vapply(planner_regression_catalogue(), function(t) t$id, character(1))
  adv <- vapply(planner_advanced_catalogue(),  function(t) t$id, character(1))
  stopifnot("none" %in% reg, "none" %in% adv, "logistic" %in% reg, "cox_ph" %in% adv)
})

# --- metadata builder -------------------------------------------------------
df <- data.frame(
  viral_suppression = factor(c("suppressed", "not", "suppressed", "not")),
  age = c(30, 42, 55, 61),
  stringsAsFactors = FALSE)
types <- c(viral_suppression = "binary", age = "continuous")
meta <- build_planner_metadata(df, types, outcomes = "viral_suppression")

ok("metadata reports row count and every variable", {
  stopifnot(meta$n_rows == 4, length(meta$variables) == 2)
})
ok("binary variable carries its factor levels; numeric carries a range", {
  vs <- Filter(function(v) v$name == "viral_suppression", meta$variables)[[1]]
  ag <- Filter(function(v) v$name == "age", meta$variables)[[1]]
  stopifnot(vs$type == "binary", length(vs$levels) == 2,
            ag$type == "continuous", length(ag$range) == 2)
})
ok("user-selected outcome is recorded", {
  stopifnot(identical(unlist(meta$outcomes), "viral_suppression"))
})

# --- labelled formats (SPSS/Stata/SAS) carry through ------------------------
# Simulate a haven-labelled import: variable label + value labels as attributes
# on the UNCOERCED column (coercion strips them, so metadata reads `raw`).
raw_lab <- data.frame(q1 = c(1, 0, 1, 0), age = c(30, 42, 55, 61))
attr(raw_lab$q1, "label")  <- "Currently on ART"
attr(raw_lab$q1, "labels") <- c(No = 0, Yes = 1)   # names = text, values = codes
df_lab   <- data.frame(q1 = factor(c(1, 0, 1, 0)), age = c(30, 42, 55, 61))
types_l  <- c(q1 = "binary", age = "continuous")
meta_lab <- build_planner_metadata(df_lab, types_l, raw = raw_lab)

ok("variable label is carried into the metadata", {
  v <- Filter(function(x) x$name == "q1", meta_lab$variables)[[1]]
  stopifnot(identical(v$label, "Currently on ART"))
})
ok("value labels are carried as 'code = label' entries", {
  v <- Filter(function(x) x$name == "q1", meta_lab$variables)[[1]]
  vl <- unlist(v$value_labels)
  stopifnot(any(grepl("1 = Yes", vl)), any(grepl("0 = No", vl)))
})
ok("unlabelled variables carry no label/value_labels keys", {
  v <- Filter(function(x) x$name == "age", meta_lab$variables)[[1]]
  stopifnot(is.null(v$label), is.null(v$value_labels))
})
ok("metadata works with no raw supplied (labels simply absent)", {
  m <- build_planner_metadata(df_lab, types_l)   # raw = NULL
  v <- Filter(function(x) x$name == "q1", m$variables)[[1]]
  stopifnot(is.null(v$label))
})

# --- validator --------------------------------------------------------------
good <- list(
  allowable_tests = list(list(test = "chi_square", applies_to = list("age"))),
  recommended_test = "chi_square", regression_method = "logistic",
  advanced_regression = "none")
bad <- list(
  allowable_tests = list(list(test = "made_up_test", applies_to = list("age"))),
  recommended_test = "made_up_test", regression_method = "sorcery",
  advanced_regression = "none")

ok("validate_plan accepts catalogue ids", {
  stopifnot(length(validate_plan(good)) == 0)
})
ok("validate_plan rejects unknown ids", {
  errs <- validate_plan(bad)
  stopifnot(length(errs) >= 2, any(grepl("made_up_test", errs)))
})

# --- prompt assembly (needs jsonlite) ---------------------------------------
if (requireNamespace("jsonlite", quietly = TRUE)) {
  p <- planner_prompt("What is associated with viral suppression?", meta)
  ok("planner_prompt returns a system + user message", {
    stopifnot(is.list(p), nchar(p$system) > 0, grepl("available_tests", p$user))
  })
}

# --- AI-ONLY: no fallback when unconfigured ---------------------------------
ok("ai_plan() ERRORS when no AI endpoint is configured (no offline fallback)", {
  .clear_ai_env()
  err <- tryCatch({ ai_plan("q", df, types, outcomes = "viral_suppression"); NA },
                  error = function(e) conditionMessage(e))
  stopifnot(is.character(err), grepl("no AI endpoint", err, ignore.case = TRUE))
})
ok("the offline planner has been removed", {
  stopifnot(!exists("plan_offline"))
})

# --- plan-gate helpers (used by the analysis tabs) --------------------------
p_binary <- list(
  allowable_tests = list(list(test = "chi_square"), list(test = "fisher_exact"),
                         list(test = "point_biserial"), list(test = "welch_t")),
  recommended_test = "chi_square", regression_method = "logistic",
  advanced_regression = "none")
p_repeat <- list(
  allowable_tests = list(list(test = "rm_anova"), list(test = "mixed_anova"),
                         list(test = "pearson")),
  recommended_test = NULL, regression_method = "none",
  advanced_regression = "lmm")
p_surv <- list(
  allowable_tests = list(list(test = "chi_square")),
  recommended_test = "chi_square", regression_method = "none",
  advanced_regression = "cox_ph")

ok("plan_test_families maps ids to Tests-tab families", {
  fams <- plan_test_families(p_binary)
  stopifnot(all(c("categorical", "point_biserial", "location") %in% fams))
})
ok("plan_regression_id returns the recommended method, or empty for 'none'", {
  stopifnot(identical(plan_regression_id(p_binary), "logistic"),
            length(plan_regression_id(p_repeat)) == 0)
})
ok("plan_advanced_family/method map advanced ids correctly", {
  stopifnot(identical(plan_advanced_family(p_repeat), "clustered"),
            identical(plan_advanced_family(p_surv), "survival"),
            identical(plan_survival_method(p_surv), "cox"),
            identical(plan_advanced_id(p_repeat), "lmm"))
})
ok("gate_choices narrows when active, keeps all when inactive or empty-allowed", {
  feas <- c("location", "correlation", "cochran_q")
  stopifnot(
    setequal(gate_choices(feas, plan_test_families(p_repeat), TRUE),
             c("location", "correlation")),                 # narrowed
    setequal(gate_choices(feas, plan_test_families(p_repeat), FALSE), feas), # override
    setequal(gate_choices(feas, character(0), TRUE), feas),  # nothing recommended
    setequal(gate_choices(c("ols", "ridge"),
                          plan_regression_id(p_repeat), TRUE),
             c("ols", "ridge")))                             # reg 'none' -> no narrowing
})
ok("gate never empties the menu when the recommendation is infeasible here", {
  stopifnot(setequal(gate_choices(c("ols", "ridge"), "logistic", TRUE),
                     c("ols", "ridge")))
})
ok("plan_test_designs maps tests to design keys and gates the Design selector", {
  # p_repeat lists rm_anova + mixed_anova + pearson -> paired_repeated, mixed, standard
  stopifnot(setequal(plan_test_designs(p_repeat),
                     c("paired_repeated", "mixed", "standard")))
  loc_designs <- c("one_sample", "two_independent", "multi_independent",
                   "paired_repeated", "factorial", "mixed", "ancova")
  stopifnot(
    setequal(gate_choices(loc_designs, plan_test_designs(p_repeat), TRUE),
             c("paired_repeated", "mixed")),                       # narrowed
    setequal(gate_choices(loc_designs, plan_test_designs(p_repeat), FALSE),
             loc_designs),                                         # override keeps all
    setequal(gate_choices(loc_designs, plan_test_designs(NULL), TRUE),
             loc_designs))                                         # no plan -> all
  # a chi-square plan narrows the categorical design to 'association'
  stopifnot(setequal(gate_choices(c("association", "repeated_binary"),
                                  plan_test_designs(good), TRUE), "association"))
})

# ============================================================================
# Repair: salvage a plan instead of discarding it
# ============================================================================
# The reply data/amr_surveillance_timeseries.csv actually produced: two ts_*
# diagnostics filed under allowable_tests. They are real ids the app can run -
# they simply belong in the advanced slot - and under the old all-or-nothing
# validator they destroyed the whole plan.
ts_reply <- list(
  allowable_tests = list(
    list(test = "ts_stationarity", applies_to = list("ecoli_cipro_pct")),
    list(test = "ts_trend",        applies_to = list("ecoli_cipro_pct")),
    list(test = "pearson",         applies_to = list("carbapenem_ddd_per1000"))),
  recommended_test = NULL, regression_method = "ols",
  advanced_regression = "none")

ok("repair moves a ts_* diagnostic out of allowable_tests into the advanced slot", {
  r <- repair_plan(ts_reply)
  stopifnot(all(c("ts_stationarity", "ts_trend") %in% r$relocated),
            identical(r$plan$advanced_regression, "ts_stationarity"),
            length(r$plan$allowable_tests) == 1,
            identical(r$plan$allowable_tests[[1]]$test, "pearson"))
})
ok("the repaired time-series plan validates - the old validator rejected it", {
  stopifnot(length(validate_plan(ts_reply)) >= 2,
            length(validate_plan(repair_plan(ts_reply)$plan)) == 0)
})
ok("relocation never clobbers an advanced pick the AI already made", {
  p <- ts_reply; p$advanced_regression <- "ts_arima"
  r <- repair_plan(p)
  stopifnot(identical(r$plan$advanced_regression, "ts_arima"),
            "ts_stationarity" %in% r$relocated)
})
ok("an id that exists nowhere is dropped and reported, the rest survives", {
  p <- list(allowable_tests = list(list(test = "sorcery"), list(test = "chi_square")),
            regression_method = "logistic", advanced_regression = "none")
  r <- repair_plan(p)
  stopifnot(identical(r$dropped, "sorcery"), length(r$relocated) == 0,
            length(r$plan$allowable_tests) == 1,
            identical(r$plan$allowable_tests[[1]]$test, "chi_square"))
})
ok("a recommended_test that is really an advanced id is relocated, not fatal", {
  p <- list(allowable_tests = list(), recommended_test = "cox_ph",
            regression_method = "none", advanced_regression = "none")
  r <- repair_plan(p)
  stopifnot(identical(r$plan$advanced_regression, "cox_ph"),
            is.null(r$plan$recommended_test),
            "cox_ph" %in% r$relocated,
            length(validate_plan(r$plan)) == 0)
})
ok("a recommended_test that is pure invention is cleared and reported", {
  p <- list(allowable_tests = list(), recommended_test = "vibes_test",
            regression_method = "none", advanced_regression = "none")
  r <- repair_plan(p)
  stopifnot(identical(r$dropped, "vibes_test"), is.null(r$plan$recommended_test))
})
ok("repair leaves a already-valid plan untouched", {
  r <- repair_plan(good)
  stopifnot(length(r$relocated) == 0, length(r$dropped) == 0,
            identical(r$plan$allowable_tests[[1]]$test, "chi_square"))
})

# --- repair edge cases ------------------------------------------------------
ok("repair handles NULL, empty tests, and entries with no test key", {
  stopifnot(is.null(repair_plan(NULL)$plan))
  e <- repair_plan(list(allowable_tests = list(), regression_method = "none",
                        advanced_regression = "none"))
  stopifnot(length(e$plan$allowable_tests) == 0, length(e$dropped) == 0)
  n <- repair_plan(list(allowable_tests = list(list(applies_to = list("age"))),
                        regression_method = "none", advanced_regression = "none"))
  stopifnot(length(n$plan$allowable_tests) == 1, length(n$dropped) == 0)
})
ok("the same bogus id twice is reported once", {
  r <- repair_plan(list(
    allowable_tests = list(list(test = "sorcery"), list(test = "sorcery")),
    regression_method = "none", advanced_regression = "none"))
  stopifnot(identical(r$dropped, "sorcery"))
})
ok("validate_plan reports a missing plan rather than erroring", {
  stopifnot(length(validate_plan(NULL)) == 1)
})
ok("malformed JSON shapes from the model do not crash the pipeline", {
  # jsonlite hands back whatever the model sent: bare strings, length-1 lists,
  # nulls nested inside lists, NA. None of these may take the panel down.
  for (v in list(NULL, list(), list(NULL), NA, NA_character_, "", list("")))
    stopifnot(identical(.plan_id(v), ""))
  stopifnot(identical(.plan_id(list("chi_square")), "chi_square"),
            identical(.plan_id("chi_square"), "chi_square"),
            identical(.plan_id(list("chi_square", "welch_t")), "chi_square"))
  weird <- list(allowable_tests = list(
                  list(test = list("chi_square"), verdict = list("appropriate")),
                  list(test = NULL), list(test = list(NULL))),
                recommended_test = list(), regression_method = list("logistic"),
                advanced_regression = NULL)
  r <- repair_plan(weird)
  e <- enforce_plan_structure(r$plan, list(available = TRUE, clustered = TRUE,
                                           time_series = FALSE, survival = FALSE))
  stopifnot(is.list(e$plan), length(r$dropped) == 0)
})

# ============================================================================
# Structural enforcement: the data overrules the AI
# ============================================================================
st_clustered <- list(available = TRUE, kind = "longitudinal", clustered = TRUE,
                     time_series = FALSE, survival = FALSE)
st_survival  <- list(available = TRUE, kind = "survival", clustered = FALSE,
                     time_series = FALSE, survival = TRUE)
st_ts        <- list(available = TRUE, kind = "time_series", clustered = FALSE,
                     time_series = TRUE, survival = FALSE)
st_none      <- list(available = FALSE, kind = "none", clustered = FALSE,
                     time_series = FALSE, survival = FALSE)

# The lvct_cohort_repeated reply: independence-assuming tests offered as
# 'appropriate' with the objection buried in the prose.
clustered_reply <- list(
  outcome_variable = "viral_load", outcome_type = "continuous",
  predictors = list("visit", "county", "sex", "age_years", "who_stage", "on_art",
                    "cd4_count", "adherence_pct", "missed_visits", "regimen"),
  allowable_tests = list(
    list(test = "welch_t", verdict = "appropriate",
         reason = "though does not account for clustering"),
    list(test = "pearson", verdict = "appropriate", reason = "marginal correlation"),
    list(test = "rm_anova", verdict = "appropriate", reason = "handles within-subject"),
    list(test = "rm_correlation", verdict = "appropriate", reason = "preferred here")),
  recommended_test = "rm_correlation", regression_method = "robust",
  advanced_regression = "lmm")

ok("clustered data: independence-assuming tests are forced to invalid_here", {
  e <- enforce_plan_structure(clustered_reply, st_clustered)
  v <- vapply(e$plan$allowable_tests, function(it) it$verdict, character(1))
  names(v) <- vapply(e$plan$allowable_tests, function(it) it$test, character(1))
  stopifnot(v[["welch_t"]] == "invalid_here", v[["pearson"]] == "invalid_here",
            v[["rm_anova"]] == "appropriate", v[["rm_correlation"]] == "appropriate",
            setequal(e$demoted, c("welch_t", "pearson")))
})
ok("a demoted test states why, keeping the AI's own wording", {
  e <- enforce_plan_structure(clustered_reply, st_clustered)
  w <- Filter(function(it) it$test == "welch_t", e$plan$allowable_tests)[[1]]
  stopifnot(grepl("Downgraded by the app", w$reason, fixed = TRUE),
            grepl("repeated measures", w$reason),
            grepl("does not account for clustering", w$reason))
})
ok("survival data: censoring-blind tests become secondary, not invalid", {
  p <- list(allowable_tests = list(
              list(test = "chi_square", verdict = "appropriate"),
              list(test = "fisher_exact", verdict = "appropriate")),
            recommended_test = "chi_square", regression_method = "logistic",
            advanced_regression = "cox_ph")
  e <- enforce_plan_structure(p, st_survival)
  v <- vapply(e$plan$allowable_tests, function(it) it$verdict, character(1))
  stopifnot(all(v == "secondary"), setequal(e$demoted, c("chi_square", "fisher_exact")),
            grepl("censoring", e$plan$allowable_tests[[1]]$reason))
})
ok("time series: independence-assuming tests are invalid_here", {
  p <- list(allowable_tests = list(list(test = "pearson", verdict = "appropriate")),
            regression_method = "ols", advanced_regression = "ts_trend")
  e <- enforce_plan_structure(p, st_ts)
  stopifnot(e$plan$allowable_tests[[1]]$verdict == "invalid_here",
            grepl("time series", e$plan$allowable_tests[[1]]$reason))
})
ok("cross-sectional data demotes nothing", {
  p <- list(allowable_tests = list(list(test = "welch_t", verdict = "appropriate"),
                                   list(test = "pearson", verdict = "appropriate")),
            regression_method = "logistic", advanced_regression = "none")
  for (s in list(st_none, NULL)) {
    e <- enforce_plan_structure(p, s)
    stopifnot(length(e$demoted) == 0,
              all(vapply(e$plan$allowable_tests, function(it) it$verdict,
                         character(1)) == "appropriate"))
  }
})
ok("enforcement only ever demotes - an AI 'invalid_here' is never promoted", {
  p <- list(allowable_tests = list(list(test = "rm_anova", verdict = "invalid_here",
                                        reason = "not enough visits")),
            regression_method = "none", advanced_regression = "lmm")
  e <- enforce_plan_structure(p, st_clustered)
  stopifnot(e$plan$allowable_tests[[1]]$verdict == "invalid_here",
            length(e$demoted) == 0,
            !grepl("Downgraded", e$plan$allowable_tests[[1]]$reason))
})
ok("a missing or nonsense verdict is treated as 'appropriate', then capped", {
  p <- list(allowable_tests = list(list(test = "welch_t"),
                                   list(test = "pearson", verdict = "excellent")),
            regression_method = "none", advanced_regression = "lmm")
  e <- enforce_plan_structure(p, st_clustered)
  stopifnot(all(vapply(e$plan$allowable_tests, function(it) it$verdict,
                       character(1)) == "invalid_here"))
  e2 <- enforce_plan_structure(p, st_none)
  stopifnot(all(vapply(e2$plan$allowable_tests, function(it) it$verdict,
                       character(1)) == "appropriate"))
})
ok("a structurally invalid recommended_test is cleared", {
  p <- list(allowable_tests = list(), recommended_test = "pearson",
            regression_method = "none", advanced_regression = "lmm")
  stopifnot(is.null(enforce_plan_structure(p, st_clustered)$plan$recommended_test),
            identical(enforce_plan_structure(p, st_none)$plan$recommended_test, "pearson"))
})
ok("a standard regression beside an advanced model is flagged as secondary", {
  e <- enforce_plan_structure(clustered_reply, st_clustered)
  stopifnot(!is.null(e$plan$regression_secondary_reason),
            grepl("independent rows", e$plan$regression_secondary_reason))
  e2 <- enforce_plan_structure(clustered_reply, st_none)
  stopifnot(is.null(e2$plan$regression_secondary_reason))
})
ok("enforce_plan_structure handles a NULL plan", {
  stopifnot(is.null(enforce_plan_structure(NULL, st_clustered)$plan))
})

# ============================================================================
# Primary analysis: one answer, and the right one
# ============================================================================
ok("clustered: a bivariate primary is re-pointed at the mixed model", {
  p <- clustered_reply
  p$primary_analysis <- list(kind = "test", id = "rm_correlation", why = "handles clustering")
  e <- enforce_plan_structure(p, st_clustered)
  pa <- plan_primary_analysis(e$plan)
  stopifnot(pa$kind == "advanced", pa$id == "lmm", grepl("Chosen by the app", pa$why))
})
ok("survival: an omitted primary derives Cox rather than rendering 'none'", {
  # amr_timetoevent returned recommended_test = null, so the old UI's most
  # prominent line read "none" while cox_ph sat two badges lower.
  p <- list(allowable_tests = list(list(test = "chi_square", verdict = "appropriate")),
            recommended_test = NULL, regression_method = "logistic",
            advanced_regression = "cox_ph")
  e <- enforce_plan_structure(p, st_survival)
  pa <- plan_primary_analysis(e$plan)
  stopifnot(pa$kind == "advanced", pa$id == "cox_ph",
            grepl("Cox", pa$label))
})
ok("cross-sectional: an omitted primary derives the regression, not a test", {
  # Both AMR cross-sectional and the messy file headlined chi_square when the
  # question ("risk factors for X") is answered by logistic regression.
  p <- list(allowable_tests = list(list(test = "chi_square", verdict = "appropriate")),
            recommended_test = "chi_square", regression_method = "logistic",
            advanced_regression = "none")
  pa <- plan_primary_analysis(enforce_plan_structure(p, st_none)$plan)
  stopifnot(pa$kind == "regression", pa$id == "logistic")
})
ok("a bivariate primary yields when the plan itself lists many predictors", {
  # Even where the test is perfectly valid (rm_correlation on clustered data),
  # it cannot be the headline answer to a 10-predictor risk-factor question.
  p <- clustered_reply
  p$primary_analysis <- list(kind = "test", id = "rm_correlation", why = "handles clustering")
  pa <- plan_primary_analysis(enforce_plan_structure(p, st_clustered)$plan)
  stopifnot(pa$kind == "advanced", pa$id == "lmm",
            grepl("10 candidate predictors", pa$why))
  # Two predictors is a scope a single test can genuinely answer.
  p2 <- p; p2$predictors <- list("visit", "arm")
  pa2 <- plan_primary_analysis(enforce_plan_structure(p2, st_clustered)$plan)
  stopifnot(pa2$kind == "test", pa2$id == "rm_correlation")
})
ok("the multivariable rule needs somewhere to go - no model, no override", {
  p <- list(predictors = as.list(letters[1:5]),
            primary_analysis = list(kind = "test", id = "kruskal", why = "k groups"),
            allowable_tests = list(list(test = "kruskal", verdict = "appropriate")),
            regression_method = "none", advanced_regression = "none")
  pa <- plan_primary_analysis(enforce_plan_structure(p, st_none)$plan)
  stopifnot(pa$kind == "test", pa$id == "kruskal")
})
ok("a defensible AI primary is honoured, not overruled on style", {
  # "Is viral load different between the arms?" is a question a test answers.
  p <- list(allowable_tests = list(list(test = "welch_t", verdict = "appropriate")),
            primary_analysis = list(kind = "test", id = "welch_t", why = "two arms"),
            recommended_test = "welch_t", regression_method = "ols",
            advanced_regression = "none")
  pa <- plan_primary_analysis(enforce_plan_structure(p, st_none)$plan)
  stopifnot(pa$kind == "test", pa$id == "welch_t", pa$why == "two arms")
})
ok("an independence-assuming regression primary yields to the advanced model", {
  p <- clustered_reply
  p$primary_analysis <- list(kind = "regression", id = "robust", why = "skewed")
  pa <- plan_primary_analysis(enforce_plan_structure(p, st_clustered)$plan)
  stopifnot(pa$kind == "advanced", pa$id == "lmm")
})
ok("an unknown primary id or kind falls back to a derived one", {
  base <- list(allowable_tests = list(), regression_method = "logistic",
               advanced_regression = "none")
  for (pa_in in list(list(kind = "advanced", id = "telepathy", why = ""),
                     list(kind = "haruspicy", id = "logistic", why = ""),
                     list(kind = "test", id = "not_a_test", why = ""))) {
    p <- base; p$primary_analysis <- pa_in
    pa <- plan_primary_analysis(enforce_plan_structure(p, st_none)$plan)
    stopifnot(pa$kind == "regression", pa$id == "logistic")
  }
})
ok("primary falls through advanced -> regression -> recommended -> first fit", {
  mk <- function(...) enforce_plan_structure(list(...), st_none)$plan
  stopifnot(
    plan_primary_analysis(mk(allowable_tests = list(), regression_method = "none",
                             advanced_regression = "lmm"))$id == "lmm",
    plan_primary_analysis(mk(allowable_tests = list(), regression_method = "ols",
                             advanced_regression = "none"))$id == "ols",
    plan_primary_analysis(mk(allowable_tests = list(), recommended_test = "kruskal",
                             regression_method = "none",
                             advanced_regression = "none"))$id == "kruskal",
    plan_primary_analysis(mk(
      allowable_tests = list(list(test = "friedman", verdict = "appropriate")),
      regression_method = "none", advanced_regression = "none"))$id == "friedman")
})
ok("a plan naming nothing runnable yields no primary rather than erroring", {
  p <- enforce_plan_structure(list(allowable_tests = list(),
                                   regression_method = "none",
                                   advanced_regression = "none"), st_none)$plan
  stopifnot(is.null(plan_primary_analysis(p)), is.null(plan_primary_analysis(NULL)))
})
ok("a primary with a kind but no id is treated as absent", {
  p <- list(allowable_tests = list(), primary_analysis = list(kind = "advanced", id = ""),
            regression_method = "logistic", advanced_regression = "none")
  stopifnot(plan_primary_analysis(enforce_plan_structure(p, st_none)$plan)$id == "logistic")
})

# ============================================================================
# The gate now narrows to what is defensible
# ============================================================================
ok("plan_test_ids drops invalid_here but keeps secondary", {
  p <- list(allowable_tests = list(
    list(test = "welch_t",  verdict = "invalid_here"),
    list(test = "kruskal",  verdict = "secondary"),
    list(test = "rm_anova", verdict = "appropriate")))
  stopifnot(setequal(plan_test_ids(p), c("kruskal", "rm_anova")),
            setequal(plan_test_ids(p, PLANNER_VERDICTS),
                     c("welch_t", "kruskal", "rm_anova")),
            setequal(plan_test_ids(p, "appropriate"), "rm_anova"))
})
ok("plans with no verdicts still gate exactly as before", {
  stopifnot(setequal(plan_test_ids(p_binary),
                     c("chi_square", "fisher_exact", "point_biserial", "welch_t")))
})
ok("the enforced clustered plan gates the Tests tab away from the invalid designs", {
  e <- enforce_plan_structure(clustered_reply, st_clustered)
  designs <- plan_test_designs(e$plan)
  stopifnot("paired_repeated" %in% designs,      # rm_anova survives
            !("two_independent" %in% designs),   # welch_t was ruled out
            !("standard" %in% designs))          # pearson was ruled out
})
ok("PLANNER_VERDICTS is ordered best-to-worst and covers the rule sets", {
  stopifnot(identical(PLANNER_VERDICTS,
                      c("appropriate", "secondary", "invalid_here")),
            all(PLANNER_INDEPENDENCE_TESTS %in% test_ids),
            all(PLANNER_CENSORING_BLIND_TESTS %in% test_ids),
            all(PLANNER_INDEPENDENCE_REGRESSIONS %in%
                  vapply(planner_regression_catalogue(), function(t) t$id, character(1))))
})

# ============================================================================
# The prompt asks for the new fields
# ============================================================================
if (requireNamespace("jsonlite", quietly = TRUE)) {
  pr <- planner_prompt("What are the risk factors for viral load?", meta)
  ok("the prompt demands verdicts, a primary analysis and honest exclusions", {
    stopifnot(grepl("invalid_here", pr$system), grepl("primary_analysis", pr$system),
              grepl("excluded_variables", pr$system),
              grepl("Never put them in allowable_tests", pr$system, fixed = TRUE))
  })
  ok("the schema advertises every new field", {
    for (f in c("primary_analysis", "excluded_variables", "transformations",
                "verdict", "confidence"))
      stopifnot(grepl(f, pr$user, fixed = TRUE))
  })
}

# ============================================================================
# Against the real datasets in data/ - structure detection drives the rules
# ============================================================================
# No AI call: the structure is computed from the actual files, then the rules
# are applied to a plan of the shape those files provoked.
.dataset <- function(f) {
  p <- file.path("data", f)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

d_cohort <- .dataset("lvct_cohort_repeated.csv")
if (!is.null(d_cohort)) {
  ok("[data] lvct_cohort_repeated is detected as clustered, and rules fire", {
    ty <- detect_all_types(d_cohort)
    st <- advanced_data_structure(d_cohort, ty,
            list(kind = "repeated", id = "patient_id", time = "visit"))
    stopifnot(isTRUE(st$clustered), identical(st$kind, "longitudinal"))
    e <- enforce_plan_structure(clustered_reply, st)
    stopifnot(setequal(e$demoted, c("welch_t", "pearson")),
              plan_primary_analysis(e$plan)$id == "lmm")
  })
  ok("[data] the cohort panel is balanced: 250 subjects x 3 visits", {
    stopifnot(nrow(d_cohort) == 750,
              length(unique(d_cohort$patient_id)) == 250,
              all(table(d_cohort$patient_id) == 3))
  })
  ok("[data] viral_load is skewed enough to warrant the transformation prompt", {
    stopifnot(max(d_cohort$viral_load, na.rm = TRUE) /
                stats::median(d_cohort$viral_load, na.rm = TRUE) > 100)
  })
}

d_tte <- .dataset("amr_timetoevent.csv")
if (!is.null(d_tte)) {
  ok("[data] amr_timetoevent is detected as survival and censoring is real", {
    ty <- detect_all_types(d_tte)
    st <- advanced_data_structure(d_tte, ty,
            list(kind = "cross_sectional", id = "patient_id", survival = TRUE,
                 surv_time = "followup_days", surv_event = "death_30d"))
    stopifnot(isTRUE(st$survival), identical(st$kind, "survival"))
    # Patients alive but followed for less than the 30-day horizon are censored:
    # a plain binary test on death_30d silently discards them.
    stopifnot(sum(d_tte$death_30d == 0 & d_tte$followup_days < 30) > 0)
    p <- list(allowable_tests = list(list(test = "chi_square", verdict = "appropriate")),
              recommended_test = NULL, regression_method = "logistic",
              advanced_regression = "cox_ph")
    e <- enforce_plan_structure(p, st)
    stopifnot(e$plan$allowable_tests[[1]]$verdict == "secondary",
              plan_primary_analysis(e$plan)$id == "cox_ph")
  })
}

d_ts <- .dataset("amr_surveillance_timeseries.csv")
if (!is.null(d_ts)) {
  ok("[data] amr_surveillance_timeseries survives end-to-end after repair", {
    ty <- detect_all_types(d_ts)
    st <- advanced_data_structure(d_ts, ty,
            list(kind = "time_series", time = "month"))
    stopifnot(isTRUE(st$time_series))
    r <- repair_plan(ts_reply)
    stopifnot(length(validate_plan(r$plan)) == 0)
    e <- enforce_plan_structure(r$plan, st)
    stopifnot(e$plan$allowable_tests[[1]]$verdict == "invalid_here",   # pearson
              plan_primary_analysis(e$plan)$kind == "advanced",
              plan_advanced_family(e$plan) == "time_series")
  })
}

d_amr <- .dataset("amr_isolates_crosssectional.csv")
if (!is.null(d_amr)) {
  ok("[data] amr_isolates_crosssectional stays unconstrained", {
    ty <- detect_all_types(d_amr)
    st <- advanced_data_structure(d_amr, ty, list(kind = "cross_sectional"))
    stopifnot(!isTRUE(st$available))
    p <- list(allowable_tests = list(list(test = "chi_square", verdict = "appropriate"),
                                     list(test = "welch_t", verdict = "appropriate")),
              recommended_test = "chi_square", regression_method = "logistic",
              advanced_regression = "none")
    e <- enforce_plan_structure(p, st)
    stopifnot(length(e$demoted) == 0, plan_primary_analysis(e$plan)$id == "logistic")
  })
}

d_messy <- .dataset("messy_cross_sectional_dataset.csv")
if (!is.null(d_messy)) {
  ok("[data] the messy file builds metadata without erroring", {
    ty <- detect_all_types(d_messy)
    m <- build_planner_metadata(d_messy, ty, outcomes = "hypertension",
                                panel = list(kind = "cross_sectional"))
    stopifnot(m$n_rows == nrow(d_messy), length(m$variables) == ncol(d_messy))
  })
}

# ============================================================================
# Rendering
# ============================================================================
ok("the plan UI leads with the primary analysis and folds away the ruled-out", {
  e <- enforce_plan_structure(clustered_reply, st_clustered)
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = e$plan, source = "test-model (AI)",
         repaired = list(relocated = character(0), dropped = character(0)),
         demoted = e$demoted)))$html)
  stopifnot(
    grepl("Start here", html), grepl("Linear mixed-effects", html),
    grepl("ruled out for this dataset", html),
    grepl("<details", html),                       # invalid tests are collapsed
    grepl("The app corrected this plan", html),
    grepl("Welch", html))                          # still explained, not deleted
})
ok("the UI reports relocations and drops rather than hiding them", {
  r <- repair_plan(ts_reply)
  e <- enforce_plan_structure(r$plan, st_ts)
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = e$plan, source = "test-model (AI)",
         repaired = list(relocated = r$relocated, dropped = "sorcery"),
         demoted = e$demoted)))$html)
  stopifnot(grepl("ts_stationarity", html), grepl("advanced-model slot", html),
            grepl("sorcery", html), grepl("not something this app can run", html))
})
ok("a plan with no corrections shows no correction banner", {
  p <- enforce_plan_structure(good, st_none)$plan
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = p, source = "test-model (AI)")))$html)
  stopifnot(!grepl("The app corrected this plan", html))
})
ok("transformations and exclusions are rendered when present", {
  p <- good
  p$transformations <- list(list(variable = "viral_load", transform = "log10",
                                 reason = "heavily right-skewed"))
  p$excluded_variables <- list(list(variable = "heart_rate", reason = "61 missing"))
  p <- enforce_plan_structure(p, st_none)$plan
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = p, source = "test-model (AI)")))$html)
  stopifnot(grepl("Prepare the data first", html), grepl("log10", html),
            grepl("left out of the predictors", html), grepl("heart_rate", html))
})
ok("a plan whose every test is ruled out says so instead of showing an empty list", {
  p <- list(allowable_tests = list(list(test = "pearson", verdict = "appropriate")),
            regression_method = "none", advanced_regression = "lmm")
  e <- enforce_plan_structure(p, st_clustered)
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = e$plan, source = "test-model (AI)", demoted = e$demoted)))$html)
  stopifnot(grepl("No bivariate test is a defensible choice", html))
})
ok("an unexpected primary kind renders instead of taking the panel down", {
  # planner_plan_ui may be handed a plan that never went through enforcement.
  p <- list(allowable_tests = list(),
            primary_analysis = list(kind = "augury", id = "lmm", why = "?"),
            regression_method = "none", advanced_regression = "lmm")
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = p, source = "test-model (AI)")))$html)
  stopifnot(grepl("Analysis", html), grepl("Linear mixed-effects", html))
})
ok("stated confidence reaches the UI (it was collected and discarded before)", {
  p <- good; p$confidence <- "medium"
  html <- html_text(htmltools::renderTags(planner_plan_ui(
    list(plan = enforce_plan_structure(p, st_none)$plan,
         source = "test-model (AI)")))$html)
  stopifnot(grepl("stated confidence: medium", html))
})

report_and_exit()
