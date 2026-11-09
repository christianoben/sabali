# fct_survey.R ---------------------------------------------------------------
# Design-based (survey-weighted) estimation.
#
# Why this exists. A complex sample is not a simple random sample, and analysing
# it as one gets both the point estimate and its uncertainty wrong: unequal
# selection probabilities bias the estimate unless weighted, and clustering
# inflates the true variance so unweighted standard errors are too small. Most
# population-based public-health data arrives this way - DHS and MICS, national
# health surveys, cluster-randomised trials, point-prevalence surveys sampling
# patients within hospitals, and any household survey with a sampling frame.
#
# The declaration vocabulary follows Stata's `svyset` (weight, stratum, PSU,
# finite population correction, and a second sampling stage), because that is
# what most analysts will be reading off an existing analysis plan. An example
# of the shape it has to accept:
#
#   svyset hhid [pweight = w], fpc(fpc_hh) strata(rural_urban) || person, fpc(fpc_p)
#
# This wraps the `survey` package rather than reimplementing design-based
# variance: Taylor linearisation with strata, PSUs and finite-population
# corrections is exactly the kind of thing that should not be hand-rolled. The
# work here is the mapping from that declaration to a svydesign, the guards that
# turn survey's terse errors into something an analyst can act on, and the
# estimation wrappers that return the app's standard result contracts.
#
# Replicate-weight designs are supported too. Some producers - NHANES, several
# DHS recodes, many national statistics offices - publish a set of replicate
# weight columns (BRR, jackknife or bootstrap) INSTEAD of the PSU and stratum
# identifiers, precisely so the design can be honoured without disclosing which
# cluster a household belongs to. Declaring the replicate columns is then the
# only way to get correct standard errors, and averaging the replicate weights
# or using the base weight alone gives standard errors that are far too small.

SURVEY_ROLE_LABELS <- c(
  weights = "Sampling weight (pweight)",
  strata  = "Stratum",
  psu     = "Primary sampling unit / cluster (PSU)",
  fpc     = "Finite population correction (fpc)",
  psu2    = "Second-stage unit (optional)",
  fpc2    = "Second-stage finite population correction (optional)",
  repweights = "Replicate weight columns",
  reptype = "Replicate type"
)

# Replicate-weight schemes survey::svrepdesign() understands, with the labels an
# analyst will recognise from a data dictionary.
SURVEY_REP_TYPES <- c(
  "Balanced repeated replication (BRR)" = "BRR",
  "Fay's BRR (needs rho)"               = "Fay",
  "Jackknife, one PSU per stratum (JK1)" = "JK1",
  "Jackknife, several PSUs per stratum (JKn)" = "JKn",
  "Bootstrap"                            = "bootstrap",
  "Successive difference (SDR)"          = "successive-difference"
)

# The design-based methods, taken from the shared label vector so the two can
# never drift apart.
SURVEY_METHOD_LABELS <- MODEL_METHOD_LABELS[
  c("svy_gaussian", "svy_binomial", "svy_poisson")]

#' Has a survey design actually been declared?
#'
#' Weights alone are a design (unequal probabilities of selection); so is
#' clustering alone (a self-weighting cluster sample). Either is enough to make
#' design-based estimation the right thing to do.
survey_design_declared <- function(spec) {
  if (is.null(spec) || !is.list(spec)) return(FALSE)
  if (length(spec[["repweights"]] %||% character(0)) > 0L) return(TRUE)
  any(vapply(c("weights", "strata", "psu"), function(k) {
    v <- spec[[k]]
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  }, logical(1)))
}

#' Is this a replicate-weight declaration rather than a PSU/stratum one?
survey_is_replicate <- function(spec) {
  length((spec[["repweights"]] %||% character(0))) > 0L
}

#' Resolve replicate weight columns from an explicit list or a name pattern.
#'
#' Producers ship these as a numbered block - BRR001..BRR080, wgt1..wgt50 - so a
#' pattern is usually far more practical than ticking eighty boxes. The resolved
#' columns are returned in name order so the set is reproducible.
survey_resolve_repweights <- function(df, repweights) {
  rw <- repweights %||% character(0)
  rw <- rw[!is.na(rw) & nzchar(rw)]
  if (!length(rw)) return(character(0))
  if (length(rw) == 1L && !rw %in% names(df)) {
    # A single value that is not itself a column is treated as a pattern.
    hit <- tryCatch(grep(rw, names(df), value = TRUE),
                    error = function(e) character(0))
    if (!length(hit))
      stop(sprintf("No columns match the replicate-weight pattern '%s'.", rw),
           call. = FALSE)
    return(sort(hit))
  }
  missing <- setdiff(rw, names(df))
  if (length(missing))
    stop(sprintf("These replicate-weight columns are not in the data: %s.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  sort(rw)
}

.survey_field <- function(spec, key, df = NULL) {
  v <- spec[[key]]
  if (is.null(v) || !length(v) || is.na(v[[1]]) || !nzchar(v[[1]])) return(NULL)
  v <- as.character(v[[1]])
  if (!is.null(df) && !v %in% names(df))
    stop(sprintf("The %s column '%s' is not in the data.",
                 tolower(SURVEY_ROLE_LABELS[[key]]), v), call. = FALSE)
  v
}

#' Available design-based regression methods for an outcome type.
survey_available_methods <- function(outcome_type) {
  switch(outcome_type,
    continuous = "svy_gaussian",
    binary     = "svy_binomial",
    count      = "svy_poisson",
    character(0))
}

#' Build a survey design object from a declaration.
#'
#' Rows missing any declared design column are dropped, with the count returned
#' rather than silently absorbed: `survey` would either error or quietly change
#' the population being described, and the analyst needs to know which happened.
#'
#' @param df   The analysis data.
#' @param spec A list with any of `weights`, `strata`, `psu`, `fpc` (column
#'   names), plus optional `nest` (default TRUE, matching Stata, where PSU ids
#'   are numbered within stratum).
#' @return A list with `design` (the svydesign), `n_used`, `n_dropped`, and the
#'   resolved column names.
build_survey_design <- function(df, spec) {
  if (!requireNamespace("survey", quietly = TRUE))
    stop("Design-based estimation needs the 'survey' package. Run install_dependencies.R.",
         call. = FALSE)
  if (!survey_design_declared(spec))
    stop("Declare at least a sampling weight, a stratum or a PSU before asking for a design-based estimate.",
         call. = FALSE)
  if (survey_is_replicate(spec)) return(.build_replicate_design(df, spec))
  w  <- .survey_field(spec, "weights", df)
  st <- .survey_field(spec, "strata", df)
  ps <- .survey_field(spec, "psu", df)
  fp <- .survey_field(spec, "fpc", df)
  # A second sampling stage: households sampled within villages, then people
  # within households. Stata writes it `svyset psu, ... || ssu, ...`; survey
  # takes both stages on one formula. Ignored unless a first stage exists,
  # because a second stage without a first is not a design.
  ps2 <- .survey_field(spec, "psu2", df)
  fp2 <- .survey_field(spec, "fpc2", df)
  if (!is.null(ps2) && is.null(ps))
    stop("A second-stage unit needs a first-stage PSU as well.", call. = FALSE)
  if (!is.null(fp2) && is.null(ps2))
    stop("A second-stage finite population correction needs a second-stage unit.",
         call. = FALSE)

  needed <- c(w, st, ps, fp, ps2, fp2)
  keep <- stats::complete.cases(df[, needed, drop = FALSE])
  n_dropped <- sum(!keep)
  d <- df[keep, , drop = FALSE]
  if (!nrow(d))
    stop("Every row is missing at least one of the declared design columns.",
         call. = FALSE)

  if (!is.null(w)) {
    wv <- suppressWarnings(as.numeric(d[[w]]))
    if (any(!is.finite(wv)))
      stop(sprintf("The sampling weight '%s' contains values that are not numbers.", w),
           call. = FALSE)
    if (any(wv <= 0))
      stop(sprintf("The sampling weight '%s' contains zero or negative values; a weight is a number of population units each row represents.", w),
           call. = FALSE)
    d[[w]] <- wv
  }
  # A stratum containing a single PSU has no within-stratum variance to
  # estimate. Erroring on that is unhelpful - it is common in real designs - so
  # the standard "adjust" behaviour (centre on the grand mean) is used and
  # reported. Set locally, then restored, so a global option is never left changed.
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)

  ids <- if (is.null(ps)) ~1 else stats::as.formula(paste(
    "~", paste(.quote_term(c(ps, ps2)), collapse = " + ")))
  args <- list(ids = ids, data = d,
               nest = if (is.null(spec[["nest"]])) TRUE else isTRUE(spec[["nest"]]))
  if (!is.null(st)) args$strata <- stats::as.formula(paste("~", .quote_term(st)))
  if (!is.null(w))  args$weights <- stats::as.formula(paste("~", .quote_term(w)))
  if (!is.null(fp)) args$fpc <- stats::as.formula(paste(
    "~", paste(.quote_term(c(fp, fp2)), collapse = " + ")))

  design <- tryCatch(do.call(survey::svydesign, args),
    error = function(e) stop("The survey design could not be built: ",
                             conditionMessage(e), call. = FALSE))
  list(design = design, data = d, n_used = nrow(d),
       n_dropped = as.integer(n_dropped),
       weights = w, strata = st, psu = ps, fpc = fp, psu2 = ps2, fpc2 = fp2)
}

# Replicate-weight design. Kept separate from the Taylor-linearisation branch
# because almost nothing is shared: there are no PSUs or strata to declare, the
# variance comes from how the replicate weights vary, and the scheme (BRR,
# jackknife, bootstrap) has to be stated because it sets the scale factor.
.build_replicate_design <- function(df, spec) {
  reps <- survey_resolve_repweights(df, spec[["repweights"]])
  if (length(reps) < 2L)
    stop(sprintf("A replicate-weight design needs at least two replicate columns; %d was resolved.",
                 length(reps)), call. = FALSE)
  w <- .survey_field(spec, "weights", df)
  type <- spec[["reptype"]] %||% "BRR"
  if (!type %in% SURVEY_REP_TYPES)
    stop(sprintf("'%s' is not a replicate scheme this app knows (%s).", type,
                 paste(unname(SURVEY_REP_TYPES), collapse = ", ")), call. = FALSE)
  if (identical(type, "Fay") && !is.finite(suppressWarnings(as.numeric(spec[["rho"]] %||% NA))))
    stop("Fay's BRR needs its rho constant; the data dictionary states it (commonly 0.3).",
         call. = FALSE)

  needed <- c(w, reps)
  keep <- stats::complete.cases(df[, needed, drop = FALSE])
  n_dropped <- sum(!keep)
  d <- df[keep, , drop = FALSE]
  if (!nrow(d))
    stop("Every row is missing at least one of the declared weight columns.",
         call. = FALSE)
  for (nm in needed) {
    v <- suppressWarnings(as.numeric(d[[nm]]))
    if (any(!is.finite(v)))
      stop(sprintf("The weight column '%s' contains values that are not numbers.", nm),
           call. = FALSE)
    d[[nm]] <- v
  }

  args <- list(
    data = d,
    repweights = d[, reps, drop = FALSE],
    type = type,
    combined.weights = TRUE)
  if (!is.null(w)) args$weights <- stats::as.formula(paste("~", .quote_term(w)))
  if (identical(type, "Fay")) args$rho <- as.numeric(spec[["rho"]])
  # The scale factor is part of the scheme, and the data dictionary states it.
  # When it is not supplied, survey infers one from the number of replicates -
  # right for a standard JK1/BRR block, wrong if the producer used a different
  # constant - so what happened is recorded rather than left to a console warning
  # the analyst will never see.
  scale_note <- NULL
  if (!is.null(spec[["scale"]]) && is.finite(suppressWarnings(as.numeric(spec[["scale"]])))) {
    args$scale <- as.numeric(spec[["scale"]])
    scale_note <- sprintf("Scale factor %s, as declared.", args$scale)
  } else if (!type %in% c("bootstrap", "Fay")) {
    scale_note <- "No scale factor was declared, so it was inferred from the number of replicate columns - correct for a standard block, but check the data dictionary if the producer used a different constant."
  }

  design <- tryCatch(withCallingHandlers(
      do.call(survey::svrepdesign, args),
      warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) stop("The replicate-weight design could not be built: ",
                             conditionMessage(e), call. = FALSE))
  list(design = design, data = d, n_used = nrow(d),
       n_dropped = as.integer(n_dropped),
       weights = w, strata = NULL, psu = NULL, fpc = NULL,
       psu2 = NULL, fpc2 = NULL,
       repweights = reps, reptype = type, scale_note = scale_note,
       rho = if (identical(type, "Fay")) as.numeric(spec[["rho"]]) else NULL)
}

#' Human-readable description of a declared design, for the UI and the report.
survey_design_summary <- function(built) {
  if (!is.null(built[["repweights"]])) {
    return(data.frame(
      Field = c("Observations used", "Rows dropped (missing a weight column)",
                "Design type", "Replicate weights", "Base weight"),
      Value = c(format(built[["n_used"]], big.mark = ","),
                format(built[["n_dropped"]], big.mark = ","),
                sprintf("replicate weights, %s%s", built[["reptype"]],
                        if (is.null(built[["rho"]])) "" else sprintf(" (rho = %s)", built[["rho"]])),
                sprintf("%d columns (%s ... %s)", length(built[["repweights"]]),
                        built[["repweights"]][[1]],
                        built[["repweights"]][[length(built[["repweights"]])]]),
                built[["weights"]] %||% "none (replicate weights carry it)"),
      stringsAsFactors = FALSE, row.names = NULL))
  }
  d <- built[["design"]]
  n_psu <- tryCatch(length(unique(stats::na.omit(d$cluster[[1]]))),
                    error = function(e) NA_integer_)
  n_str <- tryCatch(length(unique(stats::na.omit(d$strata[[1]]))),
                    error = function(e) NA_integer_)
  wsum <- tryCatch(sum(stats::weights(d)), error = function(e) NA_real_)
  rows <- list(
    c("Observations used", format(built[["n_used"]], big.mark = ",")),
    c("Rows dropped (missing a design column)",
      format(built[["n_dropped"]], big.mark = ",")),
    c("Primary sampling units",
      if (is.null(built[["psu"]])) "not declared (rows treated as independent)"
      else sprintf("%s (%s)", format(n_psu, big.mark = ","), built[["psu"]])),
    c("Strata", if (is.null(built[["strata"]])) "not declared"
      else sprintf("%s (%s)", format(n_str, big.mark = ","), built[["strata"]])),
    c("Sampling weight", if (is.null(built[["weights"]])) "none (equal weights)"
      else sprintf("%s; weights sum to %s", built[["weights"]],
                   format(round(wsum), big.mark = ","))),
    c("Second sampling stage",
      if (is.null(built[["psu2"]])) "none (single-stage design)" else built[["psu2"]]),
    c("Finite population correction",
      if (is.null(built[["fpc"]])) "none" else
        paste(c(built[["fpc"]], built[["fpc2"]]), collapse = ", ")))
  data.frame(Field = vapply(rows, `[`, character(1), 1),
             Value = vapply(rows, `[`, character(1), 2),
             stringsAsFactors = FALSE, row.names = NULL)
}

# Design effect as survey reports it, guarded: deff is unavailable for some
# designs and must not take the whole table down with it.
.survey_deff <- function(est) {
  tryCatch({
    v <- survey::deff(est)
    if (length(v)) as.numeric(v) else NA_real_
  }, error = function(e) NA_real_)
}

#' Design-based proportions for a categorical variable, optionally by group.
#'
#' This is `svy: proportion` - the estimate the WASH analysis reports. The design
#' effect is shown alongside, because it is the number that says how much the
#' clustering cost: a deff of 2 means the sample carries the information of one
#' half its size.
survey_proportion <- function(df, spec, var, by = NULL, conf = 0.95) {
  built <- build_survey_design(df, spec)
  if (!var %in% names(built[["data"]]))
    stop(sprintf("'%s' is not in the data.", var), call. = FALSE)
  f <- stats::as.formula(paste("~ factor(", .quote_term(var), ")"))
  est <- if (is.null(by) || !nzchar(by)) {
    survey::svymean(f, built[["design"]], na.rm = TRUE, deff = TRUE)
  } else {
    if (!by %in% names(built[["data"]]))
      stop(sprintf("The grouping variable '%s' is not in the data.", by),
           call. = FALSE)
    survey::svyby(f, stats::as.formula(paste("~", .quote_term(by))),
                  built[["design"]], survey::svymean, na.rm = TRUE, deff = TRUE,
                  vartype = c("se", "ci"))
  }
  .survey_tidy(est, built, conf, is_grouped = !(is.null(by) || !nzchar(by)),
               label = "Proportion")
}

#' Design-based means for a numeric variable, optionally by group.
survey_mean <- function(df, spec, var, by = NULL, conf = 0.95) {
  built <- build_survey_design(df, spec)
  if (!var %in% names(built[["data"]]))
    stop(sprintf("'%s' is not in the data.", var), call. = FALSE)
  f <- stats::as.formula(paste("~", .quote_term(var)))
  est <- if (is.null(by) || !nzchar(by)) {
    survey::svymean(f, built[["design"]], na.rm = TRUE, deff = TRUE)
  } else {
    survey::svyby(f, stats::as.formula(paste("~", .quote_term(by))),
                  built[["design"]], survey::svymean, na.rm = TRUE, deff = TRUE,
                  vartype = c("se", "ci"))
  }
  .survey_tidy(est, built, conf, is_grouped = !(is.null(by) || !nzchar(by)),
               label = "Mean")
}

# Flatten a survey estimate into the app's Field/Value display shape.
.survey_tidy <- function(est, built, conf, is_grouped, label) {
  if (is_grouped) {
    tab <- as.data.frame(est)
    names(tab) <- sub("^factor\\(.*\\)", "", names(tab))
    out <- tab
  } else {
    ci <- tryCatch(stats::confint(est, level = conf),
                   error = function(e) matrix(NA_real_, nrow = length(est), ncol = 2))
    nm <- names(stats::coef(est))
    nm <- sub("^factor\\([^)]*\\)", "", nm)
    out <- data.frame(
      Level = nm,
      Estimate = as.numeric(stats::coef(est)),
      `Std. Error` = as.numeric(survey::SE(est)),
      `CI lower` = ci[, 1], `CI upper` = ci[, 2],
      `Design effect` = .survey_deff(est),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
    names(out)[2] <- label
  }
  attr(out, "design") <- survey_design_summary(built)
  attr(out, "note") <- .survey_note(built)
  out
}

.survey_note <- function(built) {
  bits <- character(0)
  if (!is.null(built[["repweights"]]))
    return(sprintf("Standard errors come from %d %s replicate weights, as the data producer intended: the clustering and stratification are carried inside those weights rather than declared separately.%s%s",
                   length(built[["repweights"]]), built[["reptype"]],
                   if (!is.null(built[["scale_note"]])) paste0(" ", built[["scale_note"]]) else "",
                   if (built[["n_dropped"]] > 0) sprintf(" %s row%s dropped for a missing weight.",
                     format(built[["n_dropped"]], big.mark = ","),
                     if (built[["n_dropped"]] == 1L) " was" else "s were") else ""))
  if (!is.null(built[["psu"]]))
    bits <- c(bits, sprintf("Standard errors account for clustering within %s%s.",
                            built[["psu"]],
                            if (is.null(built[["psu2"]])) "" else
                              sprintf(", then %s within it", built[["psu2"]])))
  if (!is.null(built[["weights"]]))
    bits <- c(bits, sprintf("Estimates are weighted by %s, so they describe the population the sample represents rather than the sample itself.",
                            built[["weights"]]))
  if (!is.null(built[["strata"]]))
    bits <- c(bits, sprintf("Stratified by %s.", built[["strata"]]))
  if (is.null(built[["fpc"]]))
    bits <- c(bits, "No finite population correction was declared, so sampling is treated as with replacement - slightly conservative.")
  if (built[["n_dropped"]] > 0)
    bits <- c(bits, sprintf("%s row%s dropped for missing a design column.",
                            format(built[["n_dropped"]], big.mark = ","),
                            if (built[["n_dropped"]] == 1L) " was" else "s were"))
  paste(bits, collapse = " ")
}

#' Fit a design-based regression (svyglm), in the app's model result contract.
#'
#' The coefficients are the same quantities as the unweighted GLM's; the standard
#' errors are not - they are design-based, and are usually larger. Model
#' selection statistics that assume independent observations (AIC/BIC computed
#' from a likelihood) are deliberately not reported, because the likelihood is
#' not the sample's.
run_survey_model <- function(df, spec) {
  if (!requireNamespace("survey", quietly = TRUE))
    stop("Design-based regression needs the 'survey' package. Run install_dependencies.R.",
         call. = FALSE)
  method <- spec[["method"]]
  if (!method %in% names(SURVEY_METHOD_LABELS))
    stop("That is not a design-based regression method.", call. = FALSE)
  outcome_type <- spec[["outcome_type"]]
  if (!identical(survey_available_methods(outcome_type), method))
    stop(sprintf("%s does not apply to a %s outcome.",
                 SURVEY_METHOD_LABELS[[method]], outcome_type), call. = FALSE)
  if (is.null(spec[["outcome"]]) || !nzchar(spec[["outcome"]]))
    stop("Choose a valid outcome variable.", call. = FALSE)
  if (!length(spec[["predictors"]] %||% character(0)))
    stop("Choose at least one predictor.", call. = FALSE)
  if (spec[["outcome"]] %in% spec[["predictors"]])
    stop("The outcome cannot also be selected as a predictor.", call. = FALSE)

  interactions <- validate_interactions(spec[["interactions"]] %||% character(0),
                                        spec[["predictors"]])
  formula <- model_formula(spec[["outcome"]], spec[["predictors"]], interactions)
  design_cols <- unlist(lapply(c("weights", "strata", "psu", "fpc",
                                 "psu2", "fpc2"),
                               function(k) .survey_field(spec, k, df)))
  if (survey_is_replicate(spec))
    design_cols <- c(design_cols, survey_resolve_repweights(df, spec[["repweights"]]))
  n_total <- nrow(df)
  mf <- model_frame(df, formula, spec[["outcome"]], spec[["outcome_type"]],
                    extra_vars = design_cols)
  built <- build_survey_design(mf, spec)

  family <- switch(outcome_type,
    continuous = stats::gaussian(),
    binary     = stats::quasibinomial(),   # avoids survey's spurious "non-integer" warning
    count      = stats::quasipoisson())
  gee_formula <- formula
  fit_data <- built[["design"]]
  if (identical(outcome_type, "binary")) {
    y <- built[["data"]][[spec[["outcome"]]]]
    built[["design"]]$variables[[".__svy_y"]] <-
      as.numeric(droplevels(as.factor(y)) == levels(droplevels(as.factor(y)))[2])
    rhs <- paste(deparse(formula[[3]]), collapse = " ")
    gee_formula <- stats::as.formula(paste(".__svy_y ~", rhs))
    fit_data <- built[["design"]]
  }
  fit <- tryCatch(survey::svyglm(gee_formula, design = fit_data, family = family),
    error = function(e) stop("The design-based model could not be fitted: ",
                             conditionMessage(e), call. = FALSE))
  co <- summary(fit)$coefficients
  effect_name <- switch(outcome_type, binary = "Odds ratio",
                        count = "Rate ratio", NULL)
  ci <- tryCatch(stats::confint(fit), error = function(e)
    cbind(co[, 1] - stats::qnorm(0.975) * co[, 2],
          co[, 1] + stats::qnorm(0.975) * co[, 2]))
  coef_tab <- matrix_to_coefficient_table(co, ci[, 1], ci[, 2],
                                          p_col = colnames(co)[4],
                                          effect_name = effect_name)
  metrics <- c(
    "n used" = built[["n_used"]],
    "primary sampling units" = tryCatch(
      length(unique(stats::na.omit(built[["design"]]$cluster[[1]]))),
      error = function(e) NA_integer_),
    "residual degrees of freedom" = tryCatch(stats::df.residual(fit),
                                             error = function(e) NA_real_),
    "dispersion" = tryCatch(summary(fit)$dispersion[[1]],
                            error = function(e) NA_real_))
  assumptions <- list(list(
    check = "context", label = "About this method", family = "structure",
    severity = "ok", ok = TRUE, statistic = NA_real_, p = NA_real_, test = "",
    note = paste("Standard errors are design-based (Taylor linearisation), so they account for the declared weights, strata and clustering.",
                 "Likelihood-based fit statistics such as AIC are not reported, because the likelihood of a weighted complex sample is not the likelihood of these rows.",
                 .survey_note(built)),
    remedy = list(), detail = list()))
  model_result(SURVEY_METHOD_LABELS[[method]], method, formula, fit,
               coef_tab, named_metric_table(metrics), assumptions,
               outcome_type, n_total, built[["n_used"]],
               effect_name = effect_name,
               recommendation = sprintf(
                 "%s estimates population-level effects for the design declared on the Data tab. %s",
                 SURVEY_METHOD_LABELS[[method]], .survey_note(built)))
}
