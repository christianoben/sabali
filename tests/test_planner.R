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
                 "AI_INTERPRET_MODEL", "OPENCODE_API_KEY"))
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

report_and_exit()
