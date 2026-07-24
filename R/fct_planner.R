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
    list(id = "cochran_q",      name = "Cochran's Q test",               applies_to = "repeated binary outcome across 3+ conditions")
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
    tp <- types[[nm]] %||% "text"
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
    advanced_regression = planner_advanced_catalogue()
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
    '  "allowable_tests": [{"test": <id>, "applies_to": [string], "reason": string}],',
    '  "recommended_test": <id or null>,',
    '  "regression_method": <id>,',
    '  "advanced_regression": <id>,',
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
  req <- httr2::request(cfg$endpoint)
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  if (nzchar(cfg$key)) req <- httr2::req_auth_bearer_token(req, cfg$key)
  req <- httr2::req_body_json(req, list(
    model = cfg$model,
    messages = list(
      list(role = "system", content = p$system),
      list(role = "user",   content = p$user)),
    temperature = 0.1))
  req <- httr2::req_timeout(req, 60)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  txt <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt))
    stop("The AI endpoint returned no usable text.", call. = FALSE)
  # strip markdown fences if the model added them
  txt <- gsub("^```[a-zA-Z]*\\s*|\\s*```$", "", trimws(txt))
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# --- validation: reject anything outside the catalogues ---------------------

validate_plan <- function(plan) {
  test_ids <- vapply(planner_test_catalogue(), function(t) t$id, character(1))
  reg_ids  <- vapply(planner_regression_catalogue(), function(t) t$id, character(1))
  adv_ids  <- vapply(planner_advanced_catalogue(), function(t) t$id, character(1))
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
  one_proportion = "proportion", cochran_q = "cochran_q")

#' All test ids named in a plan (allowable + recommended), de-duplicated.
plan_test_ids <- function(plan) {
  if (is.null(plan)) return(character(0))
  ids <- vapply(plan$allowable_tests %||% list(),
                function(it) (it$test %||% NA_character_)[[1]], character(1))
  rec <- plan$recommended_test
  ids <- c(ids[!is.na(ids)],
           if (!is.null(rec) && length(rec) && nzchar(rec)) rec else NULL)
  unique(ids)
}

#' Tests-tab family keys implied by a plan's tests.
plan_test_families <- function(plan) {
  ids <- plan_test_ids(plan)
  unique(unname(PLANNER_TEST_FAMILY[ids[ids %in% names(PLANNER_TEST_FAMILY)]]))
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
#' surface it. If the AI replies with ids outside the catalogue, the result is
#' returned with valid = FALSE and the offending ids in errors (no fallback).
#' Returns list(plan, source, metadata, valid, errors).
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

  plan <- plan_via_llm(question, metadata, cfg)   # may error -> propagate
  errs <- validate_plan(plan)
  list(plan = if (length(errs)) NULL else plan,
       source = sprintf("%s (AI)", cfg$model),
       metadata = metadata,
       valid = length(errs) == 0,
       errors = errs)
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

#' Render a plan as a Shiny UI block (readable, with the raw ids underneath).
planner_plan_ui <- function(res) {
  plan <- res$plan
  ns_badge <- function(txt) shiny::tags$span(
    class = "label", style = "background:#018786;font-weight:600;", txt)

  tests <- plan$allowable_tests %||% list()
  test_items <- if (length(tests)) lapply(tests, function(it) {
    tid <- it$test %||% it[["test"]]
    ap <- paste(unlist(it$applies_to), collapse = ", ")
    shiny::tags$li(shiny::strong(.planner_test_label(tid)),
                   if (nzchar(ap)) sprintf(" - %s", ap) else "",
                   shiny::tags$br(),
                   shiny::tags$small(style = "color:#4a6360;", it$reason %||% ""))
  }) else list(shiny::tags$li(shiny::em("No listed test fits - see notes.")))

  rec <- plan$recommended_test
  assum <- unlist(plan$assumptions_to_check %||% list())

  shiny::div(
    class = "well", style = "background:#f2fbfa;border-color:#cfe9e7;",
    shiny::h5(shiny::icon("magic"), " Suggested analysis plan"),
    shiny::tags$p(
      ns_badge("Outcome"), " ", shiny::strong(plan$outcome_variable),
      shiny::tags$small(sprintf(" (%s)", plan$outcome_type))),
    shiny::tags$p(ns_badge("Predictors"), " ",
                  paste(unlist(plan$predictors), collapse = ", ")),
    shiny::tags$p(ns_badge("Recommended test"), " ",
                  if (!is.null(rec) && length(rec) && nzchar(rec))
                    shiny::strong(.planner_test_label(rec)) else shiny::em("none")),
    shiny::tags$p(shiny::strong("Allowable tests:")),
    shiny::tags$ul(test_items),
    shiny::tags$p(ns_badge("Regression"), " ",
                  .planner_reg_label(plan$regression_method %||% "none")),
    shiny::tags$p(ns_badge("Advanced regression"), " ",
                  .planner_reg_label(plan$advanced_regression %||% "none")),
    if (length(assum)) shiny::tags$p(shiny::strong("Assumptions to check: "),
                                     paste(assum, collapse = "; ")),
    if (!is.null(plan$notes) && nzchar(plan$notes))
      shiny::tags$p(shiny::em(plan$notes)),
    shiny::tags$hr(),
    shiny::tags$small(style = "color:#4a6360;",
      sprintf("Source: %s. This is a suggestion - confirm each choice on the Tests and Regression tabs before running. Association is not causation.",
              res$source))
  )
}
