# fct_planner.R -------------------------------------------------------------
# AI analysis planner (Section 1 add-on). The user pastes a plain-language
# research question or set of objectives; the app assembles compact metadata
# about the dataset (variable names, measurement types, factor levels, missing-
# ness, chosen roles) and a catalogue of the tests / regressions this app can
# actually run, then asks an AI to return a STRUCTURED analysis plan:
#
#   outcome variable, outcome type, candidate predictors, allowable statistical
#   tests (each tied to the predictor it applies to), a recommended test, a
#   recommended regression method, any advanced regression, assumptions to
#   check, confidence and notes.
#
# The plan is a SUGGESTION for the analyst to confirm - it never runs anything
# on its own. Downstream, the returned ids can be used to pre-select / gate the
# options offered on the Tests (4) and Regression (5/6) tabs.
#
# GOVERNANCE: exactly as fct_interpret.R, only aggregate METADATA (never row-
# level patient data) is sent to any endpoint. The planner is AI-only: when no
# approved endpoint is configured it remains disabled. The catalogues
# below are the single source of truth the AI must choose from; extend them as
# the app grows and the AI's choices expand automatically.

# --- catalogues (the controlled vocabulary handed to the AI) ----------------

#' Statistical tests the app can run. `id` is the stable key the AI must return.
planner_test_catalogue <- function() {
  list(
    list(id = "one_sample_t",   name = "One-sample t-test",              applies_to = "single numeric variable vs a reference mean (Wilcoxon if non-normal)"),
    list(id = "welch_t",        name = "Welch's t-test",                 applies_to = "numeric outcome across 2 independent groups"),
    list(id = "mann_whitney",   name = "Mann-Whitney U test",            applies_to = "numeric outcome across 2 groups (non-parametric)"),
    list(id = "one_way_anova",  name = "One-way ANOVA",                  applies_to = "numeric outcome across 3+ independent groups"),
    list(id = "kruskal",        name = "Kruskal-Wallis test",            applies_to = "numeric outcome across 3+ groups (non-parametric)"),
    list(id = "paired_t",       name = "Paired t-test",                  applies_to = "numeric outcome, two measurements on the same subjects"),
    list(id = "wilcoxon",       name = "Wilcoxon signed-rank test",      applies_to = "paired numeric outcome (non-parametric)"),
    list(id = "rm_anova",       name = "Repeated-measures ANOVA",        applies_to = "numeric outcome, same subjects over 3+ conditions"),
    list(id = "friedman",       name = "Friedman test",                  applies_to = "repeated numeric outcome (non-parametric)"),
    list(id = "factorial_anova",name = "Factorial ANOVA",                applies_to = "numeric outcome with 2-3 crossed categorical factors"),
    list(id = "mixed_anova",    name = "Mixed-model ANOVA",              applies_to = "numeric outcome with between- and within-subject factors"),
    list(id = "ancova",         name = "ANCOVA",                         applies_to = "numeric outcome, group comparison adjusted for covariates"),
    list(id = "chi_square",     name = "Chi-square test of independence",applies_to = "categorical outcome vs categorical predictor"),
    list(id = "fisher_exact",   name = "Fisher's exact test",            applies_to = "categorical vs categorical, sparse cells (expected < 5)"),
    list(id = "mcnemar",        name = "McNemar's test",                 applies_to = "paired binary outcome, two matched conditions (2x2)"),
    list(id = "point_biserial", name = "Point-biserial correlation",     applies_to = "binary variable vs numeric variable"),
    list(id = "pearson",        name = "Pearson correlation",            applies_to = "numeric vs numeric (linear)"),
    list(id = "spearman",       name = "Spearman correlation",           applies_to = "numeric vs numeric (monotonic, non-parametric)"),
    list(id = "rm_correlation", name = "Repeated-measures correlation",  applies_to = "two numeric variables measured repeatedly within subjects"),
    list(id = "one_proportion", name = "One-proportion test",            applies_to = "single binary outcome vs a reference proportion"),
    list(id = "cochran_q",      name = "Cochran's Q test",               applies_to = "repeated binary outcome across 3+ conditions"),
    list(id = "effect_measures", name = "Effect measures (risk difference, risk ratio, odds ratio, NNT)", applies_to = "binary outcome across TWO groups - quantifies how much they differ, not just whether"),
    list(id = "trend_test", name = "Cochran-Armitage test for trend", applies_to = "binary outcome across an ORDERED exposure (dose, quintile, age band) - the dose-response question"),
    list(id = "diagnostic_accuracy", name = "Diagnostic / screening test accuracy", applies_to = "a binary test against a binary REFERENCE STANDARD - sensitivity, specificity, predictive values, likelihood ratios; directional, unlike agreement"),
    list(id = "fleiss_kappa",   name = "Fleiss' kappa (agreement among 3+ raters)", applies_to = "three or more raters classifying the same subjects - panel agreement, not the average of pairwise kappas"),
    list(id = "cohens_kappa",   name = "Cohen's kappa (agreement)",      applies_to = "two categorical measurements of the SAME cases - concordance/agreement, not association; weighted kappa for ordered categories")
  )
}

#' Standard (independent-rows) regression methods, keyed to MODEL_METHOD_LABELS.
planner_regression_catalogue <- function() {
  ids <- c("ols", "robust", "quantile", "lasso", "ridge", "pls",
           "logistic", "poisson", "negative_binomial", "none")
  labs <- c(MODEL_METHOD_LABELS, none = "None (no regression indicated)")
  lapply(ids, function(id) list(id = id, name = unname(labs[[id]])))
}

#' Advanced regression methods (clustered / longitudinal / survival / series).
planner_advanced_catalogue <- function() {
  ids <- c("gee_gaussian", "gee_binomial", "gee_poisson",
           "lmm", "glmm_binomial", "glmm_poisson")
  base <- lapply(ids, function(id) list(id = id, name = unname(MODEL_METHOD_LABELS[[id]])))
  ts <- lapply(names(TS_METHOD_LABELS),
               function(id) list(id = id, name = unname(TS_METHOD_LABELS[[id]])))
  c(base, list(
    list(id = "cox_ph",       name = "Cox proportional-hazards regression (time-to-event)"),
    list(id = "kaplan_meier", name = "Kaplan-Meier survival estimate / log-rank test")),
    ts,
    list(list(id = "none",    name = "None (no advanced model indicated)")))
}

# --- structural rules (the deterministic safety net) ------------------------
# The AI is asked to mark each suggested test with a verdict, but a suggestion
# is only as good as the model that produced it. These rules re-derive the
# verdict from the data structure the app has ALREADY established, so a plan is
# corrected whatever the endpoint replies. They only ever demote, never promote.

#' Verdicts a test may carry, most to least usable.
PLANNER_VERDICTS <- c("appropriate", "secondary", "invalid_here")

#' Tests whose inference assumes independent rows. Under clustering (repeated
#' measures) or serial dependence (a time series) they inflate the Type I error
#' rate, so they are not a defensible choice however plausible they look.
PLANNER_INDEPENDENCE_TESTS <- c(
  "one_sample_t", "welch_t", "mann_whitney", "one_way_anova", "kruskal",
  "factorial_anova", "ancova", "pearson", "spearman", "point_biserial",
  "chi_square", "fisher_exact", "one_proportion")

#' Tests that treat the event indicator as a plain binary outcome. With
#' censored follow-up they discard the time dimension and everyone still at
#' risk, so they are screening tools at best - never the primary analysis.
PLANNER_CENSORING_BLIND_TESTS <- c(
  "chi_square", "fisher_exact", "mcnemar", "cohens_kappa", "effect_measures",
  "trend_test", "point_biserial", "welch_t",
  "mann_whitney", "one_way_anova", "kruskal", "one_proportion", "cochran_q",
  "pearson", "spearman")

#' Standard regression methods that assume independent rows, for the same
#' reason as PLANNER_INDEPENDENCE_TESTS.
PLANNER_INDEPENDENCE_REGRESSIONS <- c(
  "ols", "robust", "quantile", "lasso", "ridge", "pls",
  "logistic", "poisson", "negative_binomial")

# --- metadata builder (what actually gets handed over) ----------------------

# Human-readable variable label carried by labelled formats (SPSS/Stata/SAS),
# imported via rio/haven as attr(x, "label"). NULL when absent.
.planner_var_label <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab) || !is.character(lab) || !nzchar(lab[1])) return(NULL)
  as.character(lab)[1]
}

# Value labels for a labelled column: haven stores attr(x, "labels") as a named
# vector whose NAMES are the text labels and whose VALUES are the codes. Return
# a compact "code = label" list (capped), or NULL when absent.
.planner_value_labels <- function(x) {
  vl <- attr(x, "labels", exact = TRUE)
  if (is.null(vl) || !length(vl) || is.null(names(vl))) return(NULL)
  out <- paste0(as.character(unname(vl)), " = ", names(vl))
  as.list(utils::head(out, 25))
}

#' Compact, privacy-safe description of the loaded dataset.
#'
#' @param df           the type-coerced data frame
#' @param types        named character vector: variable -> measurement type
#' @param base_levels  named character vector: variable -> chosen base level ("" = default)
#' @param outcomes     character vector of user-selected outcome variables
#' @param structure    result of advanced_data_structure() (list) or NULL
#' @param panel        the panel/role list from the data module, or NULL
#' @param raw          the UNCOERCED import (optional). Labelled formats
#'   (SPSS/Stata/SAS) carry variable + value labels as attributes that coercion
#'   strips; when supplied, those human-readable labels are added to the payload
#'   so the AI can interpret cryptic column names and coded categories.
build_planner_metadata <- function(df, types, base_levels = NULL,
                                    outcomes = character(0),
                                    structure = NULL, panel = NULL,
                                    raw = NULL) {
  base_levels <- base_levels %||% stats::setNames(character(0), character(0))
  role_of <- function(nm) {
    if (is.null(panel)) return(NULL)
    roles <- character(0)
    if (!is.null(panel$id) && identical(panel$id, nm)) roles <- c(roles, "subject id")
    if (!is.null(panel$time) && identical(panel$time, nm)) roles <- c(roles, "time / visit")
    if (!is.null(panel$surv_time) && identical(panel$surv_time, nm)) roles <- c(roles, "follow-up time")
    if (!is.null(panel$surv_event) && identical(panel$surv_event, nm)) roles <- c(roles, "event indicator")
    if (nm %in% outcomes) roles <- c(roles, "outcome (user-selected)")
    if (length(roles)) paste(roles, collapse = ", ") else NULL
  }

  vars <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    # `[[` on a named ATOMIC vector throws for a missing name rather than
    # returning NULL, so it needs the same guarded lookup as base_levels below.
    tp <- if (!is.null(names(types)) && nm %in% names(types))
      types[[nm]] else "text"
    n_missing <- sum(is.na(x))
    v <- list(name = nm, type = tp, n_missing = as.integer(n_missing))
    if (is_numeric_type(tp)) {
      xx <- suppressWarnings(as.numeric(as.character(x)))
      xx <- xx[is.finite(xx)]
      if (length(xx)) {
        v$range <- c(round(min(xx), 3), round(max(xx), 3))
        v$mean  <- round(mean(xx), 3)
        v$sd    <- round(stats::sd(xx), 3)
      }
    } else if (is_categorical_type(tp)) {
      lv <- factor_levels_of(x)
      v$levels   <- as.list(utils::head(lv, 25))
      v$n_levels <- length(lv)
      # `base_levels` is optional and is often an unnamed/empty vector when
      # the analyst has not changed any reference levels.  `[[nm]]` throws a
      # subscript-out-of-bounds error for that normal case, so use a guarded
      # name lookup instead.
      ref <- if (!is.null(names(base_levels)) && nm %in% names(base_levels))
        base_levels[[nm]] else ""
      if (nzchar(ref)) v$base_level <- ref
    }
    # Human-readable labels from labelled formats (read from the uncoerced
    # import, since coercion drops these attributes).
    if (!is.null(raw) && nm %in% names(raw)) {
      lab <- .planner_var_label(raw[[nm]])
      if (!is.null(lab)) v$label <- lab
      vlab <- .planner_value_labels(raw[[nm]])
      if (!is.null(vlab)) v$value_labels <- vlab
    }
    r <- role_of(nm)
    if (!is.null(r)) v$role <- r
    v
  })

  meta <- list(
    n_rows    = nrow(df),
    data_kind = (panel$kind %||% "cross_sectional"),
    variables = vars,
    outcomes  = as.list(outcomes)
  )
  if (!is.null(structure)) {
    meta$structure <- list(
      advanced_available = isTRUE(structure$available),
      clustered   = isTRUE(structure$clustered),
      time_series = isTRUE(structure$time_series),
      survival    = isTRUE(structure$survival),
      reason      = structure$reason %||% ""
    )
  }
  meta
}

# --- prompt assembly --------------------------------------------------------

planner_prompt <- function(question, metadata) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite is needed to build the planner prompt.", call. = FALSE)
  payload <- list(
    research_question   = question,
    dataset_metadata    = metadata,
    available_tests     = planner_test_catalogue(),
    regression_methods  = planner_regression_catalogue(),
    advanced_regression = planner_advanced_catalogue(),
    # Named ids, not prose: a suggested transformation that matches the
    # catalogue becomes a button on the Data check tab rather than advice the
    # analyst has to carry out by hand.
    available_transformations = lapply(transform_catalogue(), function(m)
      list(id = m$id, name = m$name, family = m$family)),
    # Likewise for assumptions: named ids let the Data check tab actually RUN
    # the checks the plan promises, instead of printing them as prose.
    available_assumptions = lapply(assumption_catalogue(), function(a)
      list(id = a$id, name = a$name, statistic = a$statistic))
  )
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null")

  system <- paste(
    "You are a careful biostatistics analysis planner for a public-health tool.",
    "You are given a research question, compact dataset metadata (never row-level",
    "records) and catalogues of the tests and regressions this app can run.",
    "Produce a single analysis PLAN as JSON. Rules:",
    "- Choose outcome_variable from the dataset variables only.",
    "- You may ONLY use test ids that appear in available_tests, and regression",
    "  ids from regression_methods / advanced_regression. Never invent one.",
    "- Tie each allowable test to the specific predictor variable(s) it applies to.",
    "- Give every allowable test a verdict: 'appropriate' (defensible here),",
    "  'secondary' (usable as a crude screen only) or 'invalid_here' (would give",
    "  misleading inference on THIS dataset). Do not hide an objection in the",
    "  reason text while leaving the verdict at 'appropriate'. If dataset_metadata",
    "  reports structure.clustered or structure.time_series, every test assuming",
    "  independent rows is 'invalid_here'. If structure.survival is true, any test",
    "  that ignores censoring and follow-up time is at best 'secondary'.",
    "- Set primary_analysis to the ONE analysis that answers the research question,",
    "  naming its kind ('test', 'regression' or 'advanced') and its id from the",
    "  matching catalogue. A question about risk factors for an outcome is usually",
    "  answered by a regression or an advanced model, NOT by a bivariate test.",
    "- Time-series diagnostics (ts_*), survival models and clustered models are",
    "  advanced_regression ids. Never put them in allowable_tests.",
    "- List in excluded_variables any dataset variable you deliberately left out",
    "  of predictors, with the reason. Do not drop a variable silently.",
    "- Every entry in assumptions_to_check must be an id from",
    "  available_assumptions, so the app can run the check rather than only",
    "  printing it. Add any extra caveat to notes instead.",
    "- If the question is about effect modification - whether one predictor's",
    "  effect DEPENDS ON another ('is the effect of X different in group Y?',",
    "  'does A modify B?') - name the pair in interactions as \"a:b\". Both",
    "  variables must also appear in predictors, because an interaction without",
    "  its main effects does not mean what it appears to. Leave interactions",
    "  empty when the question is only about adjusted main effects.",
    "- Put any needed transformation in transformations, not only in the notes.",
    "  Set each one's `transform` to an id from available_transformations so the",
    "  app can offer it as a one-click action; use the plainest id that fits.",
    "- Some variables include a human-readable `label` and coded `value_labels`",
    "  (from SPSS/Stata/SAS). Use them to interpret cryptic column names and",
    "  coded categories when choosing the outcome and predictors.",
    "- If no listed test fits, return allowable_tests: [] and explain in notes.",
    "- Use UK English. Output ONLY the JSON object, no prose, no markdown fences."
  )
  schema <- paste(
    "Return exactly this shape:",
    '{',
    '  "outcome_variable": string,',
    '  "outcome_type": "binary"|"nominal"|"ordinal"|"continuous"|"count",',
    '  "predictors": [string],',
    '  "interactions": [string],',
    '  "excluded_variables": [{"variable": string, "reason": string}],',
    '  "primary_analysis": {"kind": "test"|"regression"|"advanced", "id": <id>, "why": string},',
    '  "allowable_tests": [{"test": <id>, "applies_to": [string],',
    '                       "verdict": "appropriate"|"secondary"|"invalid_here",',
    '                       "reason": string}],',
    '  "recommended_test": <id or null>,',
    '  "regression_method": <id>,',
    '  "advanced_regression": <id>,',
    '  "transformations": [{"variable": string, "transform": string, "reason": string}],',
    '  "assumptions_to_check": [string],',
    '  "confidence": "high"|"medium"|"low",',
    '  "notes": string',
    '}'
  )
  list(
    system = system,
    user   = sprintf("%s\n\nINPUT:\n%s", schema, json)
  )
}

# --- LLM backend (reuses fct_interpret.R config) ----------------------------
# AI-ONLY: there is no offline planner. When no endpoint is configured,
# ai_available() is FALSE, the planner UI is inactive, and ai_plan() errors.

plan_via_llm <- function(question, metadata, cfg) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("The httr2 package is needed for the AI planner backend.", call. = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("The jsonlite package is needed for the AI planner backend.", call. = FALSE)
  p <- planner_prompt(question, metadata)
  # Share the interpreter's transport (fct_interpret.R) rather than repeating the
  # request here: that carries the transient-failure retry, the optional thinking
  # field, and the retry-without-temperature fallback for providers that pin the
  # sampling temperature and reject an explicit value with HTTP 400.
  txt <- .llm_chat(cfg, p$system, p$user, temperature = 0.1,
                   timeout = AI_PLAN_TIMEOUT_S)
  # strip markdown fences if the model added them
  txt <- gsub("^```[a-zA-Z]*\\s*|\\s*```$", "", trimws(txt))
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# --- repair: salvage a plan rather than discarding it -----------------------

.planner_ids <- function(cat) vapply(cat, function(t) t$id, character(1))

# The single id carried by a plan field, or "" - the AI may send a bare string,
# a length-1 list, or NULL, and jsonlite gives us whichever it was.
.plan_id <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- unlist(x, use.names = FALSE)          # a nested NULL unlists to length 0
  if (!length(x)) return("")
  x <- x[[1]]
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) "" else as.character(x)
}

#' Repair a raw plan in place of rejecting it outright.
#'
#' Two failure modes are worth salvaging rather than discarding a whole plan:
#'
#'  * a test id that is really an ADVANCED id (the ts_* diagnostics, survival
#'    and clustered models). The app can run these - the AI merely filed them
#'    under the wrong heading - so they are moved to advanced_regression when
#'    that slot is still empty, instead of being thrown away.
#'  * a test id that exists nowhere. That single entry is dropped and reported;
#'    the rest of the plan stands.
#'
#' @return list(plan, relocated, dropped) - the character vectors name what
#'   moved and what was removed, for an honest note in the UI.
repair_plan <- function(plan) {
  if (is.null(plan)) return(list(plan = NULL, relocated = character(0),
                                 dropped = character(0)))
  test_ids <- .planner_ids(planner_test_catalogue())
  adv_ids  <- .planner_ids(planner_advanced_catalogue())
  relocated <- dropped <- character(0)

  keep <- list()
  for (it in (plan$allowable_tests %||% list())) {
    tid <- .plan_id(it$test %||% it[["test"]])
    if (!nzchar(tid) || tid %in% test_ids) { keep[[length(keep) + 1L]] <- it; next }
    if (tid %in% adv_ids) {
      # Right analysis, wrong slot: promote it if nothing better is claimed.
      if (!nzchar(.plan_id(plan$advanced_regression)) ||
          identical(.plan_id(plan$advanced_regression), "none")) {
        plan$advanced_regression <- tid
      }
      relocated <- c(relocated, tid)
    } else {
      dropped <- c(dropped, tid)
    }
  }
  plan$allowable_tests <- keep

  # A recommended_test that is not a test cannot be honoured; clear it rather
  # than fail the plan, and let it inform the advanced slot on the way out.
  rt <- .plan_id(plan$recommended_test)
  if (nzchar(rt) && !(rt %in% test_ids)) {
    if (rt %in% adv_ids) {
      if (!nzchar(.plan_id(plan$advanced_regression)) ||
          identical(.plan_id(plan$advanced_regression), "none"))
        plan$advanced_regression <- rt
      relocated <- c(relocated, rt)
    } else {
      dropped <- c(dropped, rt)
    }
    plan$recommended_test <- NULL
  }
  list(plan = plan, relocated = unique(relocated), dropped = unique(dropped))
}

# --- validation: what remains must come from the catalogues -----------------
# Runs AFTER repair_plan(), so anything it still flags is a genuine failure the
# user needs to know about (a hallucinated regression method), not a slip that
# could be salvaged.

validate_plan <- function(plan) {
  if (is.null(plan)) return("the AI returned no plan")
  test_ids <- .planner_ids(planner_test_catalogue())
  reg_ids  <- .planner_ids(planner_regression_catalogue())
  adv_ids  <- .planner_ids(planner_advanced_catalogue())
  errs <- character(0)
  for (it in (plan$allowable_tests %||% list())) {
    tid <- it$test %||% it[["test"]]
    if (!is.null(tid) && !(tid %in% test_ids)) errs <- c(errs, sprintf("unknown test id '%s'", tid))
  }
  rt <- plan$recommended_test
  if (!is.null(rt) && length(rt) && nzchar(rt) && !(rt %in% test_ids))
    errs <- c(errs, sprintf("unknown recommended_test '%s'", rt))
  rm <- plan$regression_method %||% "none"
  if (!(rm %in% reg_ids)) errs <- c(errs, sprintf("unknown regression_method '%s'", rm))
  ar <- plan$advanced_regression %||% "none"
  if (!(ar %in% adv_ids)) errs <- c(errs, sprintf("unknown advanced_regression '%s'", ar))
  errs
}

# --- structural enforcement: the plan must match the data -------------------

#' Choose the single analysis that answers the question.
#'
#' Honours the AI's own primary_analysis when it is structurally defensible, and
#' otherwise re-points it. The correction is deliberately narrow: it fires when
#' the nominated analysis is INVALID for the data (a bivariate test on clustered
#' data, an independence-assuming regression alongside a fitted advanced model),
#' not merely because a test is bivariate. "Is viral load different between the
#' arms?" is a question a test genuinely answers, and the app must not overrule
#' the model on style.
#'
#' When the AI omits the field entirely, the primary is derived in the order the
#' app itself would defend: advanced model, then regression, then the
#' recommended test, then any test still marked appropriate.
.planner_fix_primary <- function(plan, structure = NULL) {
  adv <- .plan_id(plan$advanced_regression)
  reg <- .plan_id(plan$regression_method)
  has_adv <- nzchar(adv) && !identical(adv, "none")
  has_reg <- nzchar(reg) && !identical(reg, "none")
  constrained <- !is.null(structure) && isTRUE(structure$available) &&
    (isTRUE(structure$clustered) || isTRUE(structure$time_series) ||
     isTRUE(structure$survival))

  derive <- function() {
    if (has_adv) return(list(kind = "advanced", id = adv, why = ""))
    if (has_reg) return(list(kind = "regression", id = reg, why = ""))
    rt <- .plan_id(plan$recommended_test)
    if (nzchar(rt)) return(list(kind = "test", id = rt, why = ""))
    fit <- Filter(function(it) identical(.plan_id(it$verdict) %||% "", "appropriate"),
                  plan$allowable_tests %||% list())
    if (length(fit)) return(list(kind = "test", id = .plan_id(fit[[1]]$test), why = ""))
    NULL
  }

  pa <- plan$primary_analysis
  kind <- .plan_id(pa$kind); id <- .plan_id(pa$id)
  if (!nzchar(kind) || !nzchar(id)) return(derive())

  # A bivariate test cannot be the headline answer to a question the plan itself
  # scopes at several candidate predictors - that is what buried the mixed model
  # under a correlation, and logistic regression under a chi-square test.
  n_pred <- length(unlist(plan$predictors %||% list()))
  multivariable <- n_pred >= 3L && (has_adv || has_reg)

  why_not <- ""
  bad <- switch(
    kind,
    test = {
      ceil <- .planner_ceiling(id, structure)
      if (!(id %in% .planner_ids(planner_test_catalogue()))) {
        why_not <- "that test is not one this app can run"; TRUE
      } else if (identical(ceil, "invalid_here") ||
                 (identical(ceil, "secondary") && has_adv)) {
        why_not <- "a bivariate test does not hold for this data structure"; TRUE
      } else if (multivariable) {
        why_not <- sprintf("one bivariate test cannot answer a question with %d candidate predictors",
                           n_pred); TRUE
      } else FALSE
    },
    regression = {
      if (constrained && has_adv && id %in% PLANNER_INDEPENDENCE_REGRESSIONS) {
        why_not <- "that regression assumes independent rows"; TRUE
      } else FALSE
    },
    advanced = {
      if (!(id %in% .planner_ids(planner_advanced_catalogue()))) {
        why_not <- "that model is not one this app can run"; TRUE
      } else FALSE
    },
    { why_not <- "the AI named an analysis type this app does not have"; TRUE })

  if (isTRUE(bad)) {
    d <- derive()
    if (!is.null(d)) d$why <- trimws(sprintf(
      "Chosen by the app: %s. %s", why_not, .plan_id(pa$why)))
    return(d)
  }
  list(kind = kind, id = id, why = .plan_id(pa$why))
}

#' Turn a plan's `transformations` into runnable specs.
#'
#' This is what makes the planner's advice actionable: each suggestion is
#' resolved to a catalogue method, checked against the actual column, and
#' returned with everything the Data check tab needs to render either an Apply
#' button or an honest explanation of why it cannot be offered.
#'
#' @return a list of entries with variable, method, label, reason (the AI's own
#'   words), spec, ok and why_not.
plan_transform_specs <- function(plan, df = NULL, types = NULL, panel = NULL) {
  items <- plan$transformations %||% list()
  if (!length(items)) return(list())
  out <- list()
  for (it in items) {
    var <- .plan_id(it$variable)
    txt <- .plan_id(it$transform)
    why <- .plan_id(it$reason)
    method <- transform_match_method(txt)
    entry <- list(variable = var, method = method, described = txt, reason = why,
                  spec = NULL, ok = FALSE, why_not = "")
    if (!nzchar(method)) {
      entry$why_not <- "This app has no matching transformation, so treat it as advice."
    } else if (is.null(df) || !(var %in% names(df))) {
      entry$why_not <- sprintf("Column '%s' is not in the current dataset.", var)
    } else {
      sp <- transform_spec(method, variable = var)
      ty <- if (!is.null(types) && var %in% names(types)) unname(types[[var]]) else NULL
      g <- transform_applicable(method, df[[var]], ty, sp, df, panel)
      entry$spec <- sp
      entry$ok <- isTRUE(g$ok)
      entry$why_not <- if (isTRUE(g$ok)) "" else g$reason
    }
    m <- if (nzchar(method)) transform_method(method) else NULL
    entry$label <- if (!is.null(m)) m$name else txt
    out[[length(out) + 1L]] <- entry
  }
  out
}

#' The plan's primary analysis, resolved to a label the UI can print.
#' Returns NULL when a plan names nothing runnable.
plan_primary_analysis <- function(plan) {
  if (is.null(plan)) return(NULL)
  pa <- plan$primary_analysis %||% .planner_fix_primary(plan, NULL)
  if (is.null(pa)) return(NULL)
  kind <- .plan_id(pa$kind); id <- .plan_id(pa$id)
  if (!nzchar(kind) || !nzchar(id)) return(NULL)
  label <- if (identical(kind, "test")) .planner_test_label(id) else .planner_reg_label(id)
  list(kind = kind, id = id, label = label, why = .plan_id(pa$why))
}

#' The verdict a test may hold at most, given the data structure.
#' Returns "appropriate" when nothing constrains it.
.planner_ceiling <- function(test_id, structure) {
  if (is.null(structure) || !isTRUE(structure$available)) return("appropriate")
  if ((isTRUE(structure$clustered) || isTRUE(structure$time_series)) &&
      test_id %in% PLANNER_INDEPENDENCE_TESTS)
    return("invalid_here")
  if (isTRUE(structure$survival) && test_id %in% PLANNER_CENSORING_BLIND_TESTS)
    return("secondary")
  "appropriate"
}

#' Why a test was demoted, in words the analyst can act on.
.planner_demotion_reason <- function(structure) {
  if (isTRUE(structure$clustered))
    "assumes independent rows, but this dataset has repeated measures per subject"
  else if (isTRUE(structure$time_series))
    "assumes independent rows, but these observations form a time series"
  else if (isTRUE(structure$survival))
    "ignores censoring and follow-up time"
  else "does not match this dataset's structure"
}

#' Apply the structural rules to a plan, demoting anything the data forbids.
#'
#' This is the guarantee under the AI: it runs on every plan regardless of what
#' the endpoint said, and it only ever demotes. A test the model called
#' 'appropriate' on clustered data becomes 'invalid_here' with a stated reason;
#' a primary analysis that is no longer defensible is re-pointed at the advanced
#' or regression slot the structure calls for.
#'
#' @return list(plan, demoted) where demoted names the tests that were downgraded.
enforce_plan_structure <- function(plan, structure = NULL) {
  if (is.null(plan)) return(list(plan = NULL, demoted = character(0)))
  demoted <- character(0)
  rank <- function(v) match(v, PLANNER_VERDICTS, nomatch = 1L)

  items <- plan$allowable_tests %||% list()
  for (i in seq_along(items)) {
    it <- items[[i]]
    tid <- .plan_id(it$test %||% it[["test"]])
    given <- .plan_id(it$verdict)
    if (!nzchar(given) || !(given %in% PLANNER_VERDICTS)) given <- "appropriate"
    ceiling <- .planner_ceiling(tid, structure)
    if (rank(ceiling) > rank(given)) {   # only ever demote
      demoted <- c(demoted, tid)
      it$verdict <- ceiling
      it$reason <- paste0(
        if (!is.null(it$reason) && nzchar(it$reason)) paste0(it$reason, " ") else "",
        sprintf("[Downgraded by the app: %s.]", .planner_demotion_reason(structure)))
    } else {
      it$verdict <- given
    }
    items[[i]] <- it
  }
  plan$allowable_tests <- items
  demoted <- unique(demoted)

  # A standard regression on clustered / serially dependent data has the same
  # problem as an independence-assuming test: point at the advanced model.
  if (!is.null(structure) && isTRUE(structure$available) &&
      (isTRUE(structure$clustered) || isTRUE(structure$time_series) ||
       isTRUE(structure$survival))) {
    reg <- .plan_id(plan$regression_method)
    if (nzchar(reg) && reg %in% PLANNER_INDEPENDENCE_REGRESSIONS &&
        nzchar(.plan_id(plan$advanced_regression)) &&
        !identical(.plan_id(plan$advanced_regression), "none"))
      plan$regression_secondary_reason <- sprintf(
        "%s assumes independent rows; the advanced model below is the primary one.",
        .planner_reg_label(reg))
  }

  # The recommended (bivariate) test must not be one the structure forbids.
  rt <- .plan_id(plan$recommended_test)
  if (nzchar(rt) && identical(.planner_ceiling(rt, structure), "invalid_here"))
    plan$recommended_test <- NULL

  plan$primary_analysis <- .planner_fix_primary(plan, structure)
  list(plan = plan, demoted = demoted)
}

# --- plan-gate helpers (used by the Tests / Regression / Advanced tabs) -----
# These map the plan's catalogue ids onto the keys each analysis tab uses, so a
# tab can narrow its offered analyses to what the AI recommended. The gate
# STACKS ON TOP of each tab's existing feasibility gate (it only ever narrows,
# never widens) and is bypassed by the per-tab "include un-recommended" override.

# catalogue test id -> Tests-tab family key (see FAMILY_LABELS)
PLANNER_TEST_FAMILY <- c(
  one_sample_t = "location", welch_t = "location", mann_whitney = "location",
  one_way_anova = "location", kruskal = "location", paired_t = "location",
  wilcoxon = "location", rm_anova = "location", friedman = "location",
  factorial_anova = "location", mixed_anova = "location", ancova = "location",
  chi_square = "categorical", fisher_exact = "categorical", mcnemar = "categorical",
  point_biserial = "point_biserial", pearson = "correlation",
  spearman = "correlation", rm_correlation = "correlation",
  one_proportion = "proportion", cochran_q = "cochran_q",
  cohens_kappa = "categorical", fleiss_kappa = "categorical",
  diagnostic_accuracy = "diagnostic",
  effect_measures = "effects", trend_test = "trend")

#' Interaction terms a plan asks for, as "a:b" strings.
#'
#' Only interactions whose every component is also in the plan's predictors are
#' returned. An interaction fitted without its main effects does not mean what it
#' appears to, and the model engine refuses one anyway (see
#' `validate_interactions()`), so a plan naming a stray variable is narrowed here
#' rather than being allowed to fail at fit time.
plan_interactions <- function(plan) {
  if (is.null(plan)) return(character(0))
  raw <- plan$interactions %||% list()
  ix <- unlist(lapply(raw, function(z) as.character(z %||% "")), use.names = FALSE)
  ix <- unique(trimws(ix[nzchar(trimws(ix))]))
  if (!length(ix)) return(character(0))
  preds <- unlist(lapply(plan$predictors %||% list(), as.character),
                  use.names = FALSE)
  keep <- vapply(interaction_parts(ix), function(p)
    length(p) >= 2L && all(p %in% preds) && !anyDuplicated(p), logical(1))
  ix[keep]
}

#' All test ids named in a plan (allowable + recommended), de-duplicated.
#'
#' Tests marked 'invalid_here' are excluded by default, so the Tests-tab gate
#' narrows to what is actually defensible rather than to everything the AI could
#' think of. A test with no verdict counts as 'appropriate' - plans predating
#' the verdict field, and hand-built plans in the tests, keep working unchanged.
#'
#' @param verdicts verdict values to include; pass PLANNER_VERDICTS for all.
plan_test_ids <- function(plan, verdicts = c("appropriate", "secondary")) {
  if (is.null(plan)) return(character(0))
  items <- plan$allowable_tests %||% list()
  ids <- vapply(items, function(it) {
    v <- .plan_id(it$verdict)
    if (!nzchar(v)) v <- "appropriate"
    if (v %in% verdicts) .plan_id(it$test %||% it[["test"]]) else NA_character_
  }, character(1))
  rec <- plan$recommended_test
  ids <- c(ids[!is.na(ids) & nzchar(ids)],
           if (!is.null(rec) && length(rec) && nzchar(rec)) rec else NULL)
  unique(ids)
}

#' Tests-tab family keys implied by a plan's tests.
plan_test_families <- function(plan) {
  ids <- plan_test_ids(plan)
  unique(unname(PLANNER_TEST_FAMILY[ids[ids %in% names(PLANNER_TEST_FAMILY)]]))
}

# catalogue test id -> Tests-tab DESIGN key (the sub-choice within a family:
# location / categorical / correlation designs). Lets the AI-plan gate narrow
# the design selector the same way it narrows the family selector.
PLANNER_TEST_DESIGN <- c(
  one_sample_t = "one_sample",
  welch_t = "two_independent", mann_whitney = "two_independent",
  one_way_anova = "multi_independent", kruskal = "multi_independent",
  paired_t = "paired_repeated", wilcoxon = "paired_repeated",
  rm_anova = "paired_repeated", friedman = "paired_repeated",
  factorial_anova = "factorial", mixed_anova = "mixed", ancova = "ancova",
  chi_square = "association", fisher_exact = "association",
  mcnemar = "repeated_binary", cochran_q = "repeated_binary",
  cohens_kappa = "agreement", fleiss_kappa = "agreement",
  pearson = "standard", spearman = "standard", rm_correlation = "repeated")

#' Tests-tab design keys implied by a plan's tests (used to gate the design
#' selector on top of the layout gate, mirroring plan_test_families()).
plan_test_designs <- function(plan) {
  ids <- plan_test_ids(plan)
  unique(unname(PLANNER_TEST_DESIGN[ids[ids %in% names(PLANNER_TEST_DESIGN)]]))
}

#' The standard regression method id a plan recommends ("" -> none / advanced).
plan_regression_id <- function(plan) {
  if (is.null(plan)) return(character(0))
  r <- plan$regression_method %||% "none"
  if (!length(r) || !nzchar(r) || identical(r, "none")) character(0) else r
}

#' The advanced regression id a plan recommends.
plan_advanced_id <- function(plan) {
  if (is.null(plan)) return(character(0))
  a <- plan$advanced_regression %||% "none"
  if (!length(a) || !nzchar(a) || identical(a, "none")) character(0) else a
}

#' Advanced-tab analysis family ("clustered" / "survival" / "time_series")
#' implied by a plan.
plan_advanced_family <- function(plan) {
  a <- plan_advanced_id(plan)
  if (!length(a)) return(character(0))
  if (a %in% c("cox_ph", "kaplan_meier")) "survival"
  else if (a %in% c("gee_gaussian", "gee_binomial", "gee_poisson",
                    "lmm", "glmm_binomial", "glmm_poisson")) "clustered"
  else if (a %in% names(TS_METHOD_LABELS)) "time_series"
  else character(0)
}

#' Survival-method key ("cox" / "kaplan_meier") implied by a plan, if any.
plan_survival_method <- function(plan) {
  switch(plan_advanced_id(plan) %||% "",
         cox_ph = "cox", kaplan_meier = "kaplan_meier", character(0))
}

#' Time-series method id (ts_*) implied by a plan, if the advanced recommendation
#' is a time-series analysis; used to gate the method menu on the Advanced tab.
plan_ts_method <- function(plan) {
  a <- plan_advanced_id(plan)
  if (length(a) && a %in% names(TS_METHOD_LABELS)) a else character(0)
}

#' Apply the gate: intersect `feasible` with `allowed`, but if that would leave
#' nothing (the AI recommended something not feasible here), keep `feasible`
#' unchanged rather than emptying the menu. `active` FALSE returns `feasible`.
gate_choices <- function(feasible, allowed, active = TRUE) {
  if (!isTRUE(active) || !length(allowed)) return(feasible)
  keep <- intersect(feasible, allowed)
  if (length(keep)) keep else feasible
}

# --- entry point used by mod_data -------------------------------------------

#' Produce a structured analysis plan for a research question.
#' AI-ONLY: requires a configured endpoint (ai_available()); there is no offline
#' fallback. A network/parse failure is allowed to propagate so the caller can
#' surface it.
#'
#' The reply is put through three stages before it is shown:
#'
#'   repair    - salvage misfiled ids (a ts_* diagnostic listed as a test is
#'               moved to the advanced slot) and drop ids that exist nowhere.
#'   validate  - whatever survives must come from the catalogues, or the plan is
#'               refused. Only a genuinely unusable reply fails here.
#'   enforce   - re-derive each verdict from the data structure the app already
#'               established, demoting anything the data forbids and re-pointing
#'               the primary analysis. This runs whatever the endpoint said.
#'
#' Returns list(plan, source, metadata, valid, errors, repaired, demoted), where
#' repaired/demoted describe the corrections so the UI can report them honestly.
ai_plan <- function(question, df, types, base_levels = NULL,
                    outcomes = character(0), panel = NULL, raw = NULL) {
  cfg <- ai_backend_config()
  if (is.null(cfg))
    stop("The AI planner is unavailable: no AI endpoint is configured.",
         call. = FALSE)

  structure <- tryCatch(advanced_data_structure(df, types, panel),
                        error = function(e) NULL)
  metadata <- build_planner_metadata(df, types, base_levels, outcomes,
                                     structure, panel, raw = raw)

  raw_plan <- plan_via_llm(question, metadata, cfg)   # may error -> propagate
  rep  <- repair_plan(raw_plan)
  errs <- validate_plan(rep$plan)
  enf  <- if (length(errs)) list(plan = NULL, demoted = character(0))
          else enforce_plan_structure(rep$plan, structure)

  list(plan = enf$plan,
       source = sprintf("%s (AI)", cfg$model),
       metadata = metadata,
       valid = length(errs) == 0,
       errors = errs,
       repaired = list(relocated = rep$relocated, dropped = rep$dropped),
       demoted = enf$demoted)
}

# --- rendering helpers ------------------------------------------------------

.planner_test_label <- function(id) {
  cat <- planner_test_catalogue()
  hit <- Filter(function(t) identical(t$id, id), cat)
  if (length(hit)) hit[[1]]$name else id
}
.planner_reg_label <- function(id) {
  cat <- c(planner_regression_catalogue(), planner_advanced_catalogue())
  hit <- Filter(function(t) identical(t$id, id), cat)
  if (length(hit)) hit[[1]]$name else id
}

#' Split a plan's tests into the three zones the UI shows separately.
.planner_zones <- function(plan) {
  items <- plan$allowable_tests %||% list()
  verdict_of <- function(it) {
    v <- .plan_id(it$verdict)
    if (nzchar(v) && v %in% PLANNER_VERDICTS) v else "appropriate"
  }
  list(appropriate = Filter(function(it) verdict_of(it) == "appropriate", items),
       secondary   = Filter(function(it) verdict_of(it) == "secondary",   items),
       invalid     = Filter(function(it) verdict_of(it) == "invalid_here", items))
}

# One test as a list item. `muted` greys out the entries in the "not valid"
# block so they read as an explanation rather than as something to click.
.planner_test_item <- function(it, muted = FALSE) {
  tid <- .plan_id(it$test %||% it[["test"]])
  ap <- paste(unlist(it$applies_to), collapse = ", ")
  shiny::tags$li(
    style = if (muted) "color:#7b8f8d;" else NULL,
    shiny::strong(.planner_test_label(tid)),
    if (nzchar(ap)) sprintf(" - %s", ap) else "",
    shiny::tags$br(),
    shiny::tags$small(style = "color:#4a6360;", it$reason %||% ""))
}

#' Render a plan as a Shiny UI block.
#'
#' Structured so the analyst reads ONE recommendation first and everything else
#' as supporting detail: the primary analysis is a card at the top, secondary
#' tests are a plain list, and tests that would mislead on this dataset are
#' folded away - visible for teaching, but never presented as a choice.
planner_plan_ui <- function(res) {
  plan <- res$plan
  ns_badge <- function(txt) shiny::tags$span(
    class = "label", style = "background:#018786;font-weight:600;", txt)

  zones <- .planner_zones(plan)
  prim  <- plan_primary_analysis(plan)
  assum <- unlist(plan$assumptions_to_check %||% list())
  excl  <- plan$excluded_variables %||% list()
  trans <- plan$transformations %||% list()
  rec   <- .plan_id(plan$recommended_test)
  conf  <- .plan_id(plan$confidence)

  # Single-bracket lookup: `[[` on an absent name throws, and an unexpected kind
  # must not take the whole plan panel down with it.
  kind_word <- function(k) {
    w <- unname(c(test = "Test", regression = "Regression",
                  advanced = "Advanced model")[k])
    if (is.na(w)) "Analysis" else w
  }

  shiny::div(
    class = "well", style = "background:#f2fbfa;border-color:#cfe9e7;",
    shiny::h5(shiny::icon("magic"), " Suggested analysis plan"),

    # --- the one thing to do -------------------------------------------------
    if (!is.null(prim)) shiny::div(
      style = paste("background:#ffffff;border-left:4px solid #018786;",
                    "padding:10px 12px;margin:8px 0 12px 0;"),
      shiny::div(shiny::tags$small(
        style = "text-transform:uppercase;letter-spacing:.05em;color:#018786;",
        "Start here")),
      shiny::div(style = "font-size:1.15em;font-weight:600;margin:2px 0;",
                 prim$label),
      shiny::tags$small(style = "color:#4a6360;",
        sprintf("%s. %s", kind_word(prim$kind), prim$why))),

    shiny::tags$p(
      ns_badge("Outcome"), " ", shiny::strong(plan$outcome_variable),
      shiny::tags$small(sprintf(" (%s)", plan$outcome_type))),
    shiny::tags$p(ns_badge("Predictors"), " ",
                  paste(unlist(plan$predictors), collapse = ", ")),

    # --- prepare the data first ----------------------------------------------
    if (length(trans)) shiny::div(
      shiny::tags$p(shiny::strong("Prepare the data first:")),
      shiny::tags$ul(lapply(trans, function(t) shiny::tags$li(
        shiny::strong(.plan_id(t$variable)), " - ", .plan_id(t$transform),
        shiny::tags$br(),
        shiny::tags$small(style = "color:#4a6360;", .plan_id(t$reason)))))),

    # --- supporting tests ----------------------------------------------------
    if (length(zones$appropriate)) shiny::div(
      shiny::tags$p(shiny::strong("Also defensible here:")),
      shiny::tags$ul(lapply(zones$appropriate, .planner_test_item))),
    if (length(zones$secondary)) shiny::div(
      shiny::tags$p(shiny::strong("Crude screens only "),
                    shiny::tags$small("(do not report these as the main result)")),
      shiny::tags$ul(lapply(zones$secondary, .planner_test_item))),
    if (!length(zones$appropriate) && !length(zones$secondary))
      shiny::tags$p(shiny::em(
        "No bivariate test is a defensible choice here - use the analysis above.")),

    # --- what was ruled out, folded away -------------------------------------
    if (length(zones$invalid)) shiny::tags$details(
      style = "margin:6px 0;",
      shiny::tags$summary(
        style = "cursor:pointer;color:#7b8f8d;",
        sprintf("%d test%s ruled out for this dataset - why",
                length(zones$invalid), if (length(zones$invalid) == 1) "" else "s")),
      shiny::tags$ul(lapply(zones$invalid, .planner_test_item, muted = TRUE))),

    if (length(excl)) shiny::tags$details(
      style = "margin:6px 0;",
      shiny::tags$summary(
        style = "cursor:pointer;color:#7b8f8d;",
        sprintf("%d variable%s left out of the predictors - why",
                length(excl), if (length(excl) == 1) "" else "s")),
      shiny::tags$ul(lapply(excl, function(e) shiny::tags$li(
        style = "color:#7b8f8d;",
        shiny::strong(.plan_id(e$variable)), " - ", .plan_id(e$reason))))),

    # --- the model slots -----------------------------------------------------
    shiny::tags$p(ns_badge("Regression"), " ",
                  .planner_reg_label(plan$regression_method %||% "none"),
                  if (!is.null(plan$regression_secondary_reason))
                    shiny::tags$small(style = "color:#8a6d3b;", " - ",
                                      plan$regression_secondary_reason)),
    shiny::tags$p(ns_badge("Advanced regression"), " ",
                  .planner_reg_label(plan$advanced_regression %||% "none")),
    if (nzchar(rec)) shiny::tags$p(
      ns_badge("Best bivariate screen"), " ", .planner_test_label(rec)),

    if (length(assum)) shiny::div(
      shiny::tags$p(shiny::strong("Assumptions to check:")),
      shiny::tags$ul(lapply(assum, shiny::tags$li))),

    if (!is.null(plan$notes) && nzchar(plan$notes))
      shiny::tags$p(shiny::em(plan$notes)),

    # --- honest reporting of the app's own corrections -----------------------
    .planner_corrections_ui(res),

    shiny::tags$hr(),
    shiny::tags$small(style = "color:#4a6360;",
      sprintf("Source: %s%s. This is a suggestion - confirm each choice on the Tests and Regression tabs before running. Association is not causation.",
              res$source,
              if (nzchar(conf)) sprintf(", stated confidence: %s", conf) else ""))
  )
}

#' A short, plain note whenever the app overrode the AI, so a corrected plan
#' never looks like the plan the model actually returned.
.planner_corrections_ui <- function(res) {
  rel  <- res$repaired$relocated %||% character(0)
  drop <- res$repaired$dropped %||% character(0)
  dem  <- res$demoted %||% character(0)
  if (!length(rel) && !length(drop) && !length(dem)) return(NULL)
  bits <- c(
    if (length(rel)) sprintf("moved %s to the advanced-model slot, where this app runs %s",
                             paste(rel, collapse = ", "),
                             if (length(rel) == 1) "it" else "them"),
    if (length(drop)) sprintf("removed %s (not something this app can run)",
                              paste(drop, collapse = ", ")),
    if (length(dem)) sprintf("downgraded %s as unsuitable for this dataset's structure",
                             paste(dem, collapse = ", ")))
  shiny::div(
    class = "alert alert-warning", style = "padding:6px 10px;margin:8px 0;",
    shiny::tags$small(shiny::strong("The app corrected this plan: "),
                      paste(bits, collapse = "; "), "."))
}
