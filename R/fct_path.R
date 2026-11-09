# fct_path.R -----------------------------------------------------------------
# Path analysis and mediation for observed variables.
#
# Why this exists. A regression answers "is X associated with Y, adjusting for
# the rest". It cannot answer "does X act THROUGH M", which is a different
# question and a common one in epidemiology: does an intervention work via
# adherence, does deprivation act via access to care, does a risk factor act via
# an intermediate biomarker, does water source act via hygiene behaviour.
# Adjusting for a mediator in a single regression does not decompose that
# effect - it removes it.
#
# Scope, stated plainly. This fits OBSERVED-variable path models in the
# exposures -> mediators -> outcome shape, which covers the standard mediation
# designs. It does not fit latent factors, and it is not a general
# structural-equation editor: there is no arbitrary arrow-drawing, because a
# path model an analyst can draw freely is a path model they can silently
# mis-specify.
#
# The estimation is lavaan's. What is here is the model-syntax construction, the
# indirect and total effects (which are the point of the exercise), the mapping
# from the app's declared survey design, and the guards.

#' Build lavaan model syntax for an exposures -> mediators -> outcome model.
#'
#' Every structural path is labelled so the indirect effects can be written as
#' products of named parameters rather than recovered by hand afterwards:
#'   outcome ~ b<m>*mediator + c<e>*exposure
#'   mediator ~ a<m><e>*exposure
#' and for each exposure-mediator pair, `ind_<e>_<m> := a<m><e> * b<m>`, plus a
#' total effect per exposure (its direct path plus every indirect route).
#'
#' Mediators are also regressed on each other in the order given, so a chain
#' (healthcare seeking -> antibiotic days -> colonisation) is expressible: a
#' later mediator may depend on an earlier one. That ordering is the analyst's
#' causal claim, so it comes from the order they listed them, not from the data.
#'
#' @param outcome    Final outcome variable name.
#' @param mediators  Mediator names, in causal order (earliest first).
#' @param exposures  Exposure names.
#' @param covariates Adjusted for in every equation, with no indirect effects
#'   computed for them.
#' @return A single string of lavaan model syntax.
path_model_syntax <- function(outcome, mediators, exposures,
                              covariates = character(0)) {
  if (is.null(outcome) || !length(outcome) || !nzchar(outcome))
    stop("Choose the final outcome variable for the path model.", call. = FALSE)
  mediators <- unique(mediators[nzchar(mediators)])
  exposures <- unique(exposures[nzchar(exposures)])
  covariates <- unique(covariates[nzchar(covariates)])
  if (!length(mediators))
    stop("A path model needs at least one mediator - the variable the exposure is thought to act through. With no mediator this is an ordinary regression, which the Regression tab already fits.",
         call. = FALSE)
  if (!length(exposures))
    stop("Choose at least one exposure - the variable whose effect you want to decompose.",
         call. = FALSE)
  overlap <- intersect(mediators, exposures)
  if (length(overlap))
    stop(sprintf("A variable cannot be both an exposure and a mediator: %s.",
                 paste(overlap, collapse = ", ")), call. = FALSE)
  if (outcome %in% c(mediators, exposures, covariates))
    stop("The outcome cannot also be an exposure, mediator or covariate.",
         call. = FALSE)
  if (length(intersect(covariates, c(mediators, exposures))))
    stop("A covariate cannot also be an exposure or a mediator.", call. = FALSE)

  q <- .quote_term
  lines <- character(0)
  # --- outcome equation: mediators (b), exposures (c, the direct effects) ----
  b_lab <- sprintf("b%d", seq_along(mediators))
  c_lab <- sprintf("c%d", seq_along(exposures))
  rhs <- c(sprintf("%s*%s", b_lab, q(mediators)),
           sprintf("%s*%s", c_lab, q(exposures)))
  if (length(covariates)) rhs <- c(rhs, q(covariates))
  lines <- c(lines, sprintf("%s ~ %s", q(outcome), paste(rhs, collapse = " + ")))

  # --- mediator equations ---------------------------------------------------
  a_lab <- matrix(NA_character_, nrow = length(mediators),
                  ncol = length(exposures))
  # d_lab[m2, m1]: an earlier mediator's path into a later one.
  d_lab <- matrix(NA_character_, nrow = length(mediators),
                  ncol = length(mediators))
  for (m in seq_along(mediators)) {
    a_lab[m, ] <- sprintf("a%d_%d", m, seq_along(exposures))
    mrhs <- sprintf("%s*%s", a_lab[m, ], q(exposures))
    if (m > 1L) {
      d_lab[m, seq_len(m - 1L)] <- sprintf("d%d_%d", m, seq_len(m - 1L))
      mrhs <- c(mrhs, sprintf("%s*%s", d_lab[m, seq_len(m - 1L)],
                              q(mediators[seq_len(m - 1L)])))
    }
    if (length(covariates)) mrhs <- c(mrhs, q(covariates))
    lines <- c(lines, sprintf("%s ~ %s", q(mediators[m]),
                              paste(mrhs, collapse = " + ")))
  }

  # --- indirect and total effects -------------------------------------------
  # Single-mediator routes for every exposure-mediator pair, then the two-step
  # routes an ordered mediator chain creates, then a total per exposure.
  eff <- character(0)
  for (e in seq_along(exposures)) {
    routes <- character(0)
    for (m in seq_along(mediators)) {
      nm <- sprintf("ind_%s_%s", .path_tag(exposures[e]), .path_tag(mediators[m]))
      eff <- c(eff, sprintf("%s := %s*%s", nm, a_lab[m, e], b_lab[m]))
      routes <- c(routes, sprintf("%s*%s", a_lab[m, e], b_lab[m]))
      # exposure -> earlier mediator -> this mediator -> outcome
      if (m > 1L) for (k in seq_len(m - 1L)) {
        nm2 <- sprintf("ind_%s_%s_%s", .path_tag(exposures[e]),
                       .path_tag(mediators[k]), .path_tag(mediators[m]))
        eff <- c(eff, sprintf("%s := %s*%s*%s", nm2, a_lab[k, e],
                              d_lab[m, k], b_lab[m]))
        routes <- c(routes, sprintf("%s*%s*%s", a_lab[k, e], d_lab[m, k],
                                    b_lab[m]))
      }
    }
    eff <- c(eff, sprintf("total_%s := %s + %s", .path_tag(exposures[e]),
                          c_lab[e], paste(routes, collapse = " + ")))
  }
  paste(c(lines, eff), collapse = "\n")
}

# lavaan parameter labels must be syntactic, so a variable name that is not
# becomes a safe tag. Kept deterministic so the same model always produces the
# same labels.
.path_tag <- function(x) {
  t <- gsub("[^A-Za-z0-9]+", "", x)
  ifelse(nzchar(t), t, paste0("v", seq_along(x)))
}

#' Which path-model variables must be treated as ordered (probit) rather than
#' continuous.
#'
#' A binary or ordinal outcome or mediator modelled as continuous is a linear
#' probability model - fittable, and sometimes deliberately chosen, but it is not
#' what the analyst usually means. lavaan handles these with a probit threshold
#' model when they are declared ordered.
path_ordered_vars <- function(df, vars, types = NULL) {
  vars <- intersect(vars, names(df))
  vars[vapply(vars, function(v) {
    tp <- if (!is.null(types) && !is.null(names(types)) && v %in% names(types))
      types[[v]] else NULL
    if (!is.null(tp) && tp %in% c("binary", "ordinal")) return(TRUE)
    x <- df[[v]]
    if (is.ordered(x)) return(TRUE)
    length(unique(stats::na.omit(x))) == 2L
  }, logical(1))]
}

#' Fit a path / mediation model.
#'
#' @param df   Analysis data.
#' @param spec A list with `outcome`, `mediators`, `exposures`, optional
#'   `covariates`, optional `ordered` (character vector; defaults to the binary
#'   and ordinal variables in the model), and the survey-design fields
#'   (`weights`, `psu`) when one is declared on the Data tab.
#' @return The app's model-result contract, with the path estimates, the
#'   indirect and total effects, and the global fit indices.
run_path_model <- function(df, spec) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    stop("Path analysis needs the 'lavaan' package. Run install_dependencies.R.",
         call. = FALSE)
  outcome <- spec[["outcome"]]
  mediators <- spec[["mediators"]] %||% character(0)
  exposures <- spec[["exposures"]] %||% character(0)
  covariates <- spec[["covariates"]] %||% character(0)
  syntax <- path_model_syntax(outcome, mediators, exposures, covariates)

  vars <- unique(c(outcome, mediators, exposures, covariates))
  missing <- setdiff(vars, names(df))
  if (length(missing))
    stop(sprintf("These path-model variables are not in the data: %s.",
                 paste(missing, collapse = ", ")), call. = FALSE)

  # lavaan needs numeric input for every modelled variable. A two-level factor
  # is converted (and then declared ordered); anything with more levels is
  # refused by name rather than silently coerced to its level codes, which would
  # impose an ordering the analyst never claimed.
  d <- df[, vars, drop = FALSE]
  for (v in vars) {
    x <- d[[v]]
    if (is.factor(x) || is.character(x)) {
      lv <- levels(droplevels(as.factor(x)))
      if (length(lv) > 2L)
        stop(sprintf("'%s' has %d categories. A path model needs numeric or two-level variables; split it into indicator variables on the Data-check tab first.",
                     v, length(lv)), call. = FALSE)
      d[[v]] <- as.numeric(droplevels(as.factor(x)) == lv[length(lv)])
    } else {
      d[[v]] <- suppressWarnings(as.numeric(x))
    }
  }
  n_total <- nrow(d)
  keep <- stats::complete.cases(d)
  d <- d[keep, , drop = FALSE]
  if (nrow(d) < 10L)
    stop(sprintf("Only %d rows have every path-model variable observed; at least 10 are needed.",
                 nrow(d)), call. = FALSE)

  ordered <- spec[["ordered"]] %||% path_ordered_vars(d, c(outcome, mediators),
                                                 spec[["types"]])
  ordered <- intersect(ordered, c(outcome, mediators))
  # `linear = TRUE` is the analyst's deliberate choice to model a binary outcome
  # as continuous - a linear probability model. It is the only way to get
  # sampling weights or cluster-robust standard errors out of lavaan for a binary
  # outcome (see below), so it is offered rather than hidden, and its cost is
  # stated wherever it is used.
  if (isTRUE(spec[["linear"]])) ordered <- character(0)

  # --- the declared survey design ------------------------------------------
  # lavaan takes cluster-robust standard errors, and sampling weights, but the
  # weights need the ML estimator: they cannot be combined with the ordered
  # (WLSMV threshold) estimator. Rather than silently dropping one of them, the
  # conflict is reported and the ordered model kept, because treating a binary
  # outcome as continuous changes what is being estimated.
  design_note <- character(0)
  args <- list(model = syntax, data = d)
  used <- function(k) {
    v <- spec[[k]]
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(v) && v %in% names(df)
  }
  # lavaan supports a threshold (probit) model for binary variables, and it
  # supports sampling weights and cluster-robust standard errors - but not the
  # threshold model TOGETHER with either of those. That is a real limitation, not
  # a preference, so the conflict is reported and the probit model kept: treating
  # a binary outcome as continuous silently changes the estimand from a probit
  # coefficient to a risk difference, which is a bigger change than losing a
  # variance adjustment. `linear = TRUE` reverses that choice explicitly.
  blocked <- character(0)
  if (used("psu")) {
    if (length(ordered)) {
      blocked <- c(blocked, sprintf("cluster-robust standard errors for '%s'",
                                    spec[["psu"]]))
    } else {
      d[[".__cluster"]] <- df[[spec[["psu"]]]][keep]
      args$cluster <- ".__cluster"
      design_note <- c(design_note, sprintf(
        "Standard errors are cluster-robust for %s.", spec[["psu"]]))
    }
  }
  if (used("weights")) {
    if (length(ordered)) {
      blocked <- c(blocked, sprintf("the sampling weight '%s'", spec[["weights"]]))
    } else {
      d[[".__wts"]] <- suppressWarnings(as.numeric(df[[spec[["weights"]]]][keep]))
      args$sampling.weights <- ".__wts"
      design_note <- c(design_note, sprintf("Weighted by %s.", spec[["weights"]]))
    }
  }
  if (length(blocked))
    design_note <- c(design_note, sprintf(
      "The declared design could NOT be fully applied: %s %s not available because %s %s modelled as ordered (probit), which lavaan cannot combine with either. The path estimates are on the probit scale and unadjusted for the design. To apply the design instead, tick \"treat binary variables as continuous\" - the estimates then become linear-probability (risk-difference) effects.",
      paste(blocked, collapse = " and "),
      if (length(blocked) == 1L) "was" else "were",
      paste(ordered, collapse = ", "),
      if (length(ordered) == 1L) "is" else "are"))
  if (isTRUE(spec[["linear"]]) && length(path_ordered_vars(d, c(outcome, mediators),
                                                      spec[["types"]])))
    design_note <- c(design_note,
      "Binary variables are modelled as continuous at your request, so these are linear-probability (risk-difference) effects, not probit coefficients.")
  args$data <- d
  if (length(ordered)) args$ordered <- ordered

  fit <- tryCatch(do.call(lavaan::sem, args),
    error = function(e) stop("The path model could not be fitted: ",
                             conditionMessage(e), call. = FALSE))
  if (!isTRUE(lavaan::lavInspect(fit, "converged")))
    stop("The path model did not converge. Simplify it - fewer exposures, or one mediator at a time - and check that no two variables are near-duplicates.",
         call. = FALSE)

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  structural <- pe[pe$op == "~", , drop = FALSE]
  effects <- pe[pe$op == ":=", , drop = FALSE]

  coef_tab <- data.frame(
    Term = sprintf("%s ← %s", structural$lhs, structural$rhs),
    Estimate = structural$est,
    `Std. Error` = structural$se,
    Statistic = structural$z,
    `p-value` = structural$pvalue,
    `CI lower` = structural$ci.lower,
    `CI upper` = structural$ci.upper,
    Standardised = structural$std.all,
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)

  effect_tab <- if (nrow(effects)) data.frame(
    Effect = effects$label,
    Estimate = effects$est,
    `Std. Error` = effects$se,
    `p-value` = effects$pvalue,
    `CI lower` = effects$ci.lower,
    `CI upper` = effects$ci.upper,
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) else NULL

  fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) numeric(0))
  pick <- function(nm) if (nm %in% names(fm)) unname(fm[[nm]]) else NA_real_
  metrics <- c(
    "n used" = nrow(d),
    "parameters" = pick("npar"),
    "chi-square" = pick("chisq"),
    "df" = pick("df"),
    "chi-square p" = pick("pvalue"),
    "CFI" = pick("cfi"),
    "TLI" = pick("tli"),
    "RMSEA" = pick("rmsea"),
    "SRMR" = pick("srmr"))

  # A path model with every arrow drawn is SATURATED: df = 0, and CFI/TLI/RMSEA
  # are then 1/1/0 by construction rather than by fitting well. Reporting them
  # unqualified would read as strong evidence of a correct model when the model
  # has not in fact been tested at all.
  df_model <- pick("df")
  saturated <- is.finite(df_model) && df_model < 1
  fit_note <- if (saturated)
    "This model is saturated (0 degrees of freedom): every variable is connected to every other, so there is nothing left over to test. CFI, TLI, RMSEA and SRMR are perfect by construction here and say nothing about whether the model is right - ignore them. The path and indirect estimates are still interpretable; the global fit is not. To get a testable model, leave out a path you believe is absent."
  else sprintf("The model has %d degrees of freedom, so its global fit is testable: chi-square p above 0.05, CFI/TLI at or above 0.95 and RMSEA at or below 0.06 are the usual thresholds.",
               as.integer(df_model))

  assumptions <- list(list(
    check = "context", label = "About this method", family = "structure",
    severity = "ok", ok = TRUE, statistic = NA_real_, p = NA_real_, test = "",
    note = paste(c(
      "A path model decomposes an exposure's effect into a direct path and the routes running through the mediators; the indirect effects are products of path coefficients, so they inherit the assumptions of every equation on the route.",
      "The mediator ordering is the analyst's causal claim, not something estimated from the data, and an unmeasured cause of both a mediator and the outcome biases the decomposition.",
      fit_note,
      if (length(ordered)) sprintf("Modelled as ordered (probit): %s.",
                                   paste(ordered, collapse = ", ")),
      design_note), collapse = " "),
    remedy = list(), detail = list()))

  res <- model_result("Path analysis (observed-variable mediation)", "path",
                      stats::as.formula(paste(.quote_term(outcome), "~ .")),
                      fit, coef_tab, named_metric_table(metrics), assumptions,
                      "continuous", n_total, nrow(d),
                      recommendation = paste(
                        "Read the indirect effects: they are what a path model adds over a regression.",
                        paste(design_note, collapse = " ")))
  res$effects <- effect_tab
  res$syntax <- syntax
  res$diagnostic_plot <- NULL   # a residual plot is not meaningful here
  res$interpretation <- paste(
    .path_interpretation(coef_tab, effect_tab, outcome),
    if (saturated) "The model is saturated, so its fit indices are perfect by construction and carry no information." else "")
  res
}

# Plain-language summary. Deliberately leads on the indirect effects, since the
# direct paths are what an ordinary regression would already have shown.
.path_interpretation <- function(coef_tab, effect_tab, outcome) {
  bits <- character(0)
  if (!is.null(effect_tab) && nrow(effect_tab)) {
    ind <- effect_tab[startsWith(effect_tab$Effect, "ind_"), , drop = FALSE]
    sig <- ind[is.finite(ind$`p-value`) & ind$`p-value` < 0.05, , drop = FALSE]
    bits <- c(bits, if (nrow(sig))
      sprintf("Evidence of mediation on %d of %d route%s: %s.",
              nrow(sig), nrow(ind), if (nrow(ind) == 1L) "" else "s",
              paste(utils::head(sig$Effect, 3), collapse = ", "))
      else sprintf("No indirect route reached p < 0.05, so there is no evidence here that the exposures act on %s through the chosen mediators.",
                   outcome))
  }
  direct <- coef_tab[coef_tab$Term %in%
                       grep(sprintf("^%s ", outcome), coef_tab$Term, value = TRUE), ,
                     drop = FALSE]
  ds <- direct[is.finite(direct$`p-value`) & direct$`p-value` < 0.05, , drop = FALSE]
  bits <- c(bits, if (nrow(ds))
    sprintf("Direct paths into %s below 0.05: %s.", outcome,
            paste(utils::head(ds$Term, 3), collapse = "; "))
    else sprintf("No direct path into %s reached p < 0.05.", outcome))
  bits <- c(bits, "An indirect effect is a product of path coefficients, so it is only as sound as the weakest equation on its route, and the mediator ordering is assumed rather than tested. Associations are not proof of causation.")
  paste(bits, collapse = " ")
}
