# fct_rates.R ----------------------------------------------------------------
# Incidence rates over person-time, and age (or any other) standardisation.
#
# Why this exists. Two questions the app could not answer, both routine.
#
# 1. RATES. A proportion divides cases by people; a rate divides cases by
#    person-TIME. When people are followed for different lengths - a cohort with
#    staggered enrolment, a surveillance system, a programme with dropout - the
#    proportion is not interpretable, because someone followed for a month and
#    someone followed for three years are not equivalent denominators. Rates are
#    also what surveillance reports in: cases per 1,000 person-years.
#
# 2. STANDARDISATION. Comparing crude rates between two populations of different
#    age structure compares their age structures as much as their risk. A
#    district with more older residents will show more cancer whatever its
#    exposure. Direct standardisation applies both populations' rates to one
#    reference structure; indirect standardisation (the SMR) applies a reference
#    population's rates to the observed structure, and is what to use when the
#    stratum-specific rates are too sparse to be stable.
#
# Intervals are exact (Poisson/gamma) rather than normal-approximation, because
# rates in public health are frequently built on small numerators where the
# normal approximation is poor and can put the lower limit below zero.

# A note on spec access. Everything here reads the spec with `[[`, never `$`.
# R's `$` does PARTIAL matching on lists: when the exact name is absent it
# silently returns a field whose name merely starts with it. `spec$per` returned
# the value of `spec$person_time` - the string "pt" - so the rate was computed as
# "pt" * events, and `spec$reference` would have picked up `reference_rates` the
# same way. `[[` is exact, so the whole class of error disappears.

#' Exact (Garwood) Poisson confidence interval for a count.
#'
#' Uses the gamma relationship rather than a normal approximation: with 3 events
#' a Wald interval is badly skewed and its lower limit can be negative, which is
#' not a possible number of events.
poisson_exact_ci <- function(x, conf = 0.95) {
  if (!is.finite(x) || x < 0)
    return(c(lower = NA_real_, upper = NA_real_))
  a <- (1 - conf) / 2
  c(lower = if (x == 0) 0 else stats::qgamma(a, shape = x),
    upper = stats::qgamma(1 - a, shape = x + 1))
}

#' Incidence rate over person-time, with an exact interval.
#'
#' @param events      Number of events (a count, or a vector to be summed).
#' @param person_time Total person-time at risk, in the same unit throughout.
#' @param per         Report the rate per this many units (1,000 by default).
incidence_rate <- function(events, person_time, per = 1000, conf = 0.95) {
  ev <- sum(events, na.rm = TRUE)
  pt <- sum(person_time, na.rm = TRUE)
  if (!is.finite(pt) || pt <= 0)
    stop("Person-time must be a positive total; check that the follow-up column is not empty or zero.",
         call. = FALSE)
  if (!is.finite(ev) || ev < 0)
    stop("The event count must be zero or more.", call. = FALSE)
  ci <- poisson_exact_ci(ev, conf)
  list(events = ev, person_time = pt, per = per, conf = conf,
       rate = per * ev / pt,
       lower = per * ci[["lower"]] / pt,
       upper = per * ci[["upper"]] / pt)
}

#' Incidence rates by group, and the rate ratio between two of them.
#'
#' The rate-ratio interval is exact, conditioning on the total number of events -
#' the standard approach, and the one that stays sensible when a group has very
#' few events, where a log-normal interval does not.
#'
#' @param events      Event counts per group.
#' @param person_time Person-time per group.
#' @param groups      Group labels.
#' @param reference   Group the ratios are measured against.
rate_comparison <- function(events, person_time, groups, reference = NULL,
                            per = 1000, conf = 0.95) {
  d <- data.frame(g = as.character(groups),
                  e = as.numeric(events),
                  t = as.numeric(person_time), stringsAsFactors = FALSE)
  d <- d[!is.na(d$g) & is.finite(d$e) & is.finite(d$t), , drop = FALSE]
  if (!nrow(d)) stop("No usable rows for a rate comparison.", call. = FALSE)
  agg <- stats::aggregate(cbind(e, t) ~ g, data = d, FUN = sum)
  if (any(agg$t <= 0))
    stop(sprintf("These groups have no person-time at risk: %s.",
                 paste(agg$g[agg$t <= 0], collapse = ", ")), call. = FALSE)
  if (nrow(agg) < 2L)
    stop("A rate comparison needs at least two groups.", call. = FALSE)
  ref <- reference %||% agg$g[[1]]
  if (!ref %in% agg$g)
    stop(sprintf("'%s' is not one of the groups (%s).", ref,
                 paste(agg$g, collapse = ", ")), call. = FALSE)

  rates <- do.call(rbind, lapply(seq_len(nrow(agg)), function(i) {
    r <- incidence_rate(agg$e[i], agg$t[i], per = per, conf = conf)
    data.frame(Group = agg$g[i], Events = agg$e[i], `Person-time` = agg$t[i],
               Rate = r$rate, `CI lower` = r$lower, `CI upper` = r$upper,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))

  e0 <- agg$e[agg$g == ref]; t0 <- agg$t[agg$g == ref]
  rr <- do.call(rbind, lapply(seq_len(nrow(agg)), function(i) {
    if (identical(agg$g[i], ref))
      return(data.frame(Group = ref, `Rate ratio` = 1, `CI lower` = NA_real_,
                        `CI upper` = NA_real_, `p-value` = NA_real_,
                        check.names = FALSE, stringsAsFactors = FALSE))
    e1 <- agg$e[i]; t1 <- agg$t[i]
    est <- (e1 / t1) / (e0 / t0)
    # Exact interval conditional on the event total: the number of events in the
    # index group is binomial given the total, so binom.test gives an exact
    # interval for the ratio without a normal approximation.
    bt <- tryCatch(stats::binom.test(round(e1), round(e1 + e0),
                                     p = t1 / (t1 + t0), conf.level = conf),
                   error = function(e) NULL)
    if (is.null(bt))
      return(data.frame(Group = agg$g[i], `Rate ratio` = est,
                        `CI lower` = NA_real_, `CI upper` = NA_real_,
                        `p-value` = NA_real_, check.names = FALSE,
                        stringsAsFactors = FALSE))
    to_rr <- function(p) (p / (1 - p)) * (t0 / t1)
    data.frame(Group = agg$g[i], `Rate ratio` = est,
               `CI lower` = to_rr(bt$conf.int[1]),
               `CI upper` = to_rr(bt$conf.int[2]),
               `p-value` = bt$p.value,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  list(rates = rates, rate_ratios = rr, reference = ref, per = per, conf = conf)
}

#' Directly standardised rate.
#'
#' Applies the study population's stratum-specific rates to a REFERENCE
#' population's structure, so two populations can be compared without their
#' different age structures doing the comparing. The variance is the usual
#' weighted sum of stratum Poisson variances.
#'
#' @param events        Events per stratum.
#' @param person_time   Person-time per stratum.
#' @param std_population Reference population size per stratum, in the same
#'   stratum order. Only the relative sizes matter.
direct_standardise <- function(events, person_time, std_population,
                               per = 1000, conf = 0.95) {
  e <- as.numeric(events); t <- as.numeric(person_time)
  w <- as.numeric(std_population)
  if (length(unique(c(length(e), length(t), length(w)))) != 1L)
    stop(sprintf("Events (%d), person-time (%d) and the standard population (%d) must have one value per stratum.",
                 length(e), length(t), length(w)), call. = FALSE)
  ok <- is.finite(e) & is.finite(t) & is.finite(w) & t > 0 & w >= 0
  if (!any(ok))
    stop("No stratum has both person-time at risk and a standard population.",
         call. = FALSE)
  n_dropped <- sum(!ok)
  e <- e[ok]; t <- t[ok]; w <- w[ok]
  wt <- w / sum(w)
  rates <- e / t
  dsr <- sum(wt * rates)
  var_dsr <- sum(wt^2 * e / t^2)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  se <- sqrt(var_dsr)
  list(rate = per * dsr, lower = per * max(0, dsr - z * se),
       upper = per * (dsr + z * se), se = per * se, per = per, conf = conf,
       strata_used = length(e), strata_dropped = as.integer(n_dropped),
       crude = per * sum(e) / sum(t))
}

#' Indirectly standardised ratio (SMR) with an exact interval.
#'
#' Applies a REFERENCE population's stratum rates to the study population's own
#' structure to get the number of events expected, then compares that with the
#' number observed. This is the right choice when the study population's own
#' stratum rates are too sparse for direct standardisation to be stable - which
#' is usually the case for a small district or a rare outcome.
indirect_standardise <- function(events, person_time, reference_rates,
                                 conf = 0.95) {
  e <- as.numeric(events); t <- as.numeric(person_time)
  r <- as.numeric(reference_rates)
  if (length(unique(c(length(e), length(t), length(r)))) != 1L)
    stop(sprintf("Events (%d), person-time (%d) and the reference rates (%d) must have one value per stratum.",
                 length(e), length(t), length(r)), call. = FALSE)
  ok <- is.finite(e) & is.finite(t) & is.finite(r) & t > 0
  if (!any(ok))
    stop("No stratum has person-time at risk and a reference rate.", call. = FALSE)
  n_dropped <- sum(!ok)
  obs <- sum(e[ok]); exp_ <- sum(t[ok] * r[ok])
  if (exp_ <= 0)
    stop("The expected count is zero; check that the reference rates are on the same time unit as the person-time.",
         call. = FALSE)
  ci <- poisson_exact_ci(obs, conf)
  list(observed = obs, expected = exp_, smr = obs / exp_,
       lower = ci[["lower"]] / exp_, upper = ci[["upper"]] / exp_,
       conf = conf, strata_used = sum(ok),
       strata_dropped = as.integer(n_dropped))
}

#' Rates and standardisation as a guided-analysis result.
#'
#' @param df   Data with one row per stratum (or per person, aggregated first).
#' @param spec `events`, `person_time`, optional `group`, optional `strata` and
#'   `std_population` (direct) or `reference_rates` (indirect), and `per`.
run_rate_analysis <- function(df, spec) {
  need <- c(spec[["events"]], spec[["person_time"]])
  missing <- setdiff(need, names(df))
  if (length(missing))
    stop(sprintf("These columns are not in the data: %s.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  per <- spec[["per"]] %||% 1000
  unit <- spec[["time_unit"]] %||% "person-time"

  if (!is.null(spec[["group"]]) && nzchar(spec[["group"]])) {
    cmp <- rate_comparison(df[[spec[["events"]]]], df[[spec[["person_time"]]]],
                           df[[spec[["group"]]]], reference = spec[["reference"]],
                           per = per)
    top <- cmp$rates
    highest <- top$Group[which.max(top$Rate)]
    interp <- sprintf(
      "Rates per %s %s ranged from %.2f to %.2f, highest in %s. Ratios are against %s, with exact intervals conditional on the total number of events.",
      format(per, big.mark = ","), unit, min(top$Rate), max(top$Rate),
      highest, cmp$reference)
    interp <- paste(interp,
      "A rate divides events by person-TIME, so it stays interpretable when people are followed for different lengths - which a proportion does not.")
    return(list(
      primary = mk_result(sprintf("Incidence rates per %s %s",
                                  format(per, big.mark = ","), unit),
        c("groups" = nrow(top), "total events" = sum(top$Events),
          "total person-time" = sum(top$`Person-time`)),
        p = NA_real_, interpretation = interp,
        note = "Exact Poisson intervals for the rates; the rate-ratio interval conditions on the event total, which stays sensible when a group has few events."),
      alternative = NULL, recommended = "primary", assumptions = list(),
      rates = top, rate_ratios = cmp$rate_ratios,
      rationale = "Person-time denominators make groups with different follow-up comparable; the rate ratio quantifies the difference between them."))
  }

  # Standardisation, when a reference structure or reference rates are supplied.
  if (!is.null(spec[["std_population"]]) && nzchar(spec[["std_population"]])) {
    ds <- direct_standardise(df[[spec[["events"]]]], df[[spec[["person_time"]]]],
                             df[[spec[["std_population"]]]], per = per)
    interp <- sprintf(
      "The directly standardised rate is %.2f per %s %s (%.2f to %.2f), against a crude rate of %.2f. Standardisation applies this population's stratum rates to the reference structure, so a comparison is not driven by the two populations having different structures.",
      ds$rate, format(per, big.mark = ","), unit, ds$lower, ds$upper, ds$crude)
    if (ds$strata_dropped > 0)
      interp <- paste(interp, sprintf("%d stratum/strata had no person-time and were excluded.",
                                      ds$strata_dropped))
    return(list(
      primary = mk_result("Directly standardised rate",
        c("standardised rate" = round(ds$rate, 4),
          "CI lower" = round(ds$lower, 4), "CI upper" = round(ds$upper, 4),
          "crude rate" = round(ds$crude, 4), "strata used" = ds$strata_used),
        p = NA_real_, interpretation = interp,
        note = "Direct standardisation needs the study population's own stratum rates to be stable; when they are sparse, use indirect standardisation (SMR) instead."),
      alternative = NULL, recommended = "primary", assumptions = list(),
      rationale = "Comparing crude rates between populations with different age structures compares the structures as much as the risk; standardisation removes that."))
  }

  if (!is.null(spec[["reference_rates"]]) && nzchar(spec[["reference_rates"]])) {
    ind <- indirect_standardise(df[[spec[["events"]]]], df[[spec[["person_time"]]]],
                                df[[spec[["reference_rates"]]]])
    verdict <- if (ind$lower > 1) "more events than expected"
               else if (ind$upper < 1) "fewer events than expected"
               else "no more events than expected"
    interp <- sprintf(
      "%d events were observed against %.1f expected from the reference rates, giving an SMR of %.3f (%.3f to %.3f) - %s. The interval is exact (Poisson) on the observed count.",
      ind$observed, ind$expected, ind$smr, ind$lower, ind$upper, verdict)
    interp <- paste(interp,
      "An SMR is standardised to this population's own structure, so two SMRs are each comparable with the reference but not strictly with each other.")
    return(list(
      primary = mk_result("Standardised incidence/mortality ratio (SMR)",
        c("observed" = ind$observed, "expected" = round(ind$expected, 3),
          "SMR" = round(ind$smr, 4), "CI lower" = round(ind$lower, 4),
          "CI upper" = round(ind$upper, 4), "strata used" = ind$strata_used),
        p = NA_real_, interpretation = interp,
        note = "The reference rates must be on the same time unit as the person-time column."),
      alternative = NULL, recommended = "primary", assumptions = list(),
      rationale = "Indirect standardisation compares the events observed with those a reference population's rates would produce in this population's structure, and is stable where direct standardisation is not."))
  }

  r <- incidence_rate(df[[spec[["events"]]]], df[[spec[["person_time"]]]], per = per)
  interp <- sprintf(
    "%d events over %s %s gives an incidence rate of %.3f per %s (%.3f to %.3f, exact Poisson interval).",
    r$events, format(round(r$person_time, 1), big.mark = ","), unit,
    r$rate, format(per, big.mark = ","), r$lower, r$upper)
  list(primary = mk_result(sprintf("Incidence rate per %s %s",
                                   format(per, big.mark = ","), unit),
         c("events" = r$events, "person-time" = round(r$person_time, 2),
           "rate" = round(r$rate, 4), "CI lower" = round(r$lower, 4),
           "CI upper" = round(r$upper, 4)),
         p = NA_real_, interpretation = interp,
         note = "The interval is exact (Poisson), not a normal approximation, because rates are often built on small numerators where the approximation puts the lower limit below zero."),
       alternative = NULL, recommended = "primary", assumptions = list(),
       rationale = "A rate divides events by person-time, so it remains interpretable when follow-up varies between people.")
}
