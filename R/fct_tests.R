# fct_tests.R ---------------------------------------------------------------
# The hypothesis-test engine (Section 5). Each family runner computes BOTH the
# primary (assumptions-met) test and its alternative, attaches effect sizes,
# CIs and assumption verdicts, and marks which is recommended - but never hides
# either. Multi-group and repeated designs also attach a multiplicity-adjusted
# posthoc table for the recommended analysis path. Results use a common shape
# so the UI and report render them uniformly.

# --- result helpers --------------------------------------------------------
mk_result <- function(label, stats_named, p, effect = NULL,
                      interpretation = "", note = "") {
  list(label = label,
       table = data.frame(statistic = names(stats_named),
                          value = unname(stats_named),
                          stringsAsFactors = FALSE),
       p_value = p, effect = effect,
       interpretation = interpretation, note = note)
}

sig_phrase <- function(p, alpha = 0.05, what = "difference") {
  if (is.na(p)) return("The test could not be evaluated.")
  if (p < alpha)
    sprintf("There is a statistically significant %s (p = %s).", what, p_fmt(p))
  else
    sprintf("No statistically significant %s was detected (p = %s).", what, p_fmt(p))
}

# --- effect sizes ----------------------------------------------------------
cohens_d_two <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]; x2 <- x2[is.finite(x2)]
  n1 <- length(x1); n2 <- length(x2)
  sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
  if (sp == 0) return(NA_real_)
  (mean(x1) - mean(x2)) / sp
}
hedges_g <- function(x1, x2) {
  d <- cohens_d_two(x1, x2)
  n <- length(x1[is.finite(x1)]) + length(x2[is.finite(x2)])
  d * (1 - 3 / (4 * n - 9))
}
eta_sq_aov <- function(fit) {
  a <- stats::anova(fit); ss <- a$`Sum Sq`
  if (length(ss) < 2) return(NA_real_)
  ss[1] / sum(ss)
}
eta_sq_kruskal <- function(H, k, n) (H - k + 1) / (n - k)  # H-based eta-squared
rank_biserial_mw <- function(W, n1, n2) 1 - 2 * W / (n1 * n2)
cramers_v <- function(tab) {
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab); k <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(chi / (n * (k - 1))))
}

# reshape long -> complete subjects x within-levels matrix
reshape_wide <- function(x, within, subject) {
  within <- factor(within); subject <- factor(subject)
  m <- tapply(x, list(subject, within), function(v) v[1])
  m <- m[stats::complete.cases(m), , drop = FALSE]
  m
}

# --- posthoc result helpers ------------------------------------------------
# Posthoc tables use a common set of human-readable columns where possible.
# Tukey and Games-Howell provide multiplicity-adjusted p-values directly;
# Dunn and paired follow-ups retain both the raw and Holm-adjusted p-values.
mk_posthoc <- function(label, table, global_label, global_p,
                       adjustment, alpha = 0.05, note = NULL) {
  omnibus_significant <- isTRUE(!is.na(global_p) && global_p < alpha)
  context <- if (is.na(global_p)) {
    sprintf("%s. The omnibus %s could not be evaluated; interpret pairwise results cautiously.",
            label, global_label)
  } else if (omnibus_significant) {
    sprintf("%s. The omnibus %s was statistically significant (p = %s). Pairwise p-values use %s.",
            label, global_label, p_fmt(global_p), adjustment)
  } else {
    sprintf("%s. The omnibus %s was not statistically significant (p = %s); treat individual pairwise results as exploratory. Pairwise p-values use %s.",
            label, global_label, p_fmt(global_p), adjustment)
  }
  list(label = label, table = table, global_label = global_label,
       global_p = global_p, adjustment = adjustment, alpha = alpha,
       omnibus_significant = omnibus_significant,
       note = if (is.null(note)) context else paste(context, note))
}

# Tukey HSD for the equal-variance one-way ANOVA path. stats::TukeyHSD()
# already supplies simultaneous confidence intervals and adjusted p-values.
posthoc_tukey <- function(d, global_label, global_p, alpha = 0.05) {
  fit <- stats::aov(x ~ g, data = d)
  tk <- stats::TukeyHSD(fit, which = "g", conf.level = 1 - alpha)$g
  tab <- data.frame(
    Comparison = rownames(tk),
    Estimate = unname(tk[, "diff"]),
    `CI lower` = unname(tk[, "lwr"]),
    `CI upper` = unname(tk[, "upr"]),
    Statistic = NA_real_,
    df = NA_real_,
    `p-value` = NA_real_,
    `p-adjusted` = unname(tk[, "p adj"]),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )
  mk_posthoc("Tukey HSD", tab, global_label, global_p,
             adjustment = "Tukey's studentized-range adjustment", alpha = alpha)
}

# Games-Howell follow-up for Welch's ANOVA. This uses the studentized-range
# distribution (ptukey/qtukey) and Welch-Satterthwaite degrees of freedom, so
# it does not require equal variances or a third-party package.
posthoc_games_howell <- function(d, global_label, global_p, alpha = 0.05) {
  lv <- levels(droplevels(d$g)); k <- length(lv)
  pieces <- lapply(lv, function(z) d$x[d$g == z])
  ns <- vapply(pieces, length, integer(1))
  means <- vapply(pieces, mean, numeric(1))
  vars <- vapply(pieces, function(z) stats::var(z), numeric(1))
  pairs <- utils::combn(seq_len(k), 2)

  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    i <- pairs[1, j]; h <- pairs[2, j]
    v1 <- vars[i] / ns[i]; v2 <- vars[h] / ns[h]
    se <- sqrt(v1 + v2)
    df <- if (is.finite(se) && se > 0 && ns[i] > 1 && ns[h] > 1)
      (v1 + v2)^2 / (v1^2 / (ns[i] - 1) + v2^2 / (ns[h] - 1)) else NA_real_
    estimate <- means[i] - means[h]
    q <- if (is.finite(se) && se > 0) abs(estimate) / (se / sqrt(2)) else NA_real_
    p <- if (is.finite(q) && is.finite(df))
      tryCatch(stats::ptukey(q * sqrt(2), nmeans = k, df = df,
                             lower.tail = FALSE), error = function(e) NA_real_)
    else NA_real_
    crit <- if (is.finite(df))
      tryCatch(stats::qtukey(1 - alpha, nmeans = k, df = df) / sqrt(2),
               error = function(e) NA_real_) else NA_real_
    data.frame(
      Comparison = sprintf("%s vs %s", lv[i], lv[h]),
      Estimate = estimate,
      `CI lower` = estimate - crit * se,
      `CI upper` = estimate + crit * se,
      Statistic = q,
      df = df,
      `p-value` = p,
      `p-adjusted` = p,
      n1 = ns[i], n2 = ns[h],
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  mk_posthoc("Games-Howell comparisons", tab, global_label, global_p,
             adjustment = "Games-Howell studentized-range adjustment", alpha = alpha)
}

# Dunn's test for a Kruskal-Wallis follow-up, implemented with pooled ranks
# and the standard tie correction. Holm adjustment controls the family-wise
# error rate across all pairwise comparisons.
posthoc_dunn <- function(d, global_label, global_p, alpha = 0.05) {
  x <- d$x; g <- droplevels(d$g); n <- length(x)
  lv <- levels(g); k <- length(lv)
  ranks <- rank(x, ties.method = "average")
  tie_sizes <- table(x)
  tie_correction <- if (n > 1) 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n) else 0
  rank_var <- n * (n + 1) / 12 * tie_correction
  mean_ranks <- tapply(ranks, g, mean)
  ns <- table(g)
  pairs <- utils::combn(seq_len(k), 2)

  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    i <- pairs[1, j]; h <- pairs[2, j]
    se <- sqrt(rank_var * (1 / ns[i] + 1 / ns[h]))
    estimate <- unname(mean_ranks[i] - mean_ranks[h])
    z <- if (is.finite(se) && se > 0) estimate / se else NA_real_
    p <- if (is.finite(z)) 2 * stats::pnorm(-abs(z)) else NA_real_
    data.frame(
      Comparison = sprintf("%s vs %s", lv[i], lv[h]),
      Estimate = estimate,
      `CI lower` = NA_real_,
      `CI upper` = NA_real_,
      Statistic = z,
      df = NA_real_,
      `p-value` = p,
      n1 = unname(as.integer(ns[i])), n2 = unname(as.integer(ns[h])),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  tab[["p-adjusted"]] <- stats::p.adjust(tab[["p-value"]], method = "holm")
  tab <- tab[, c("Comparison", "Estimate", "CI lower", "CI upper",
                 "Statistic", "df", "p-value", "p-adjusted", "n1", "n2")]
  mk_posthoc("Dunn's test", tab, global_label, global_p,
             adjustment = "Holm", alpha = alpha,
             note = "The estimate is the difference in mean ranks; confidence intervals are not provided for this rank-based follow-up.")
}

# Pairwise repeated-measures follow-ups. Each comparison uses only subjects
# observed in both conditions; p-values are then adjusted across all pairs.
posthoc_paired_t <- function(m, global_label, global_p, alpha = 0.05) {
  lv <- colnames(m); if (is.null(lv)) lv <- paste0("Condition ", seq_len(ncol(m)))
  pairs <- utils::combn(seq_len(ncol(m)), 2)
  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    i <- pairs[1, j]; h <- pairs[2, j]
    ok <- is.finite(m[, i]) & is.finite(m[, h])
    a <- m[ok, i]; b <- m[ok, h]
    tt <- tryCatch(stats::t.test(a, b, paired = TRUE),
                   error = function(e) NULL)
    data.frame(
      Comparison = sprintf("%s vs %s", lv[i], lv[h]),
      Estimate = mean(a - b),
      `CI lower` = if (is.null(tt)) NA_real_ else unname(tt$conf.int[1]),
      `CI upper` = if (is.null(tt)) NA_real_ else unname(tt$conf.int[2]),
      Statistic = if (is.null(tt)) NA_real_ else unname(tt$statistic),
      df = if (is.null(tt)) NA_real_ else unname(tt$parameter),
      `p-value` = if (is.null(tt)) NA_real_ else tt$p.value,
      `n pairs` = length(a),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  tab[["p-adjusted"]] <- stats::p.adjust(tab[["p-value"]], method = "holm")
  tab <- tab[, c("Comparison", "Estimate", "CI lower", "CI upper",
                 "Statistic", "df", "p-value", "p-adjusted", "n pairs")]
  mk_posthoc("Pairwise paired t-tests", tab, global_label, global_p,
             adjustment = "Holm", alpha = alpha)
}

posthoc_paired_wilcox <- function(m, global_label, global_p, alpha = 0.05) {
  lv <- colnames(m); if (is.null(lv)) lv <- paste0("Condition ", seq_len(ncol(m)))
  pairs <- utils::combn(seq_len(ncol(m)), 2)
  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    i <- pairs[1, j]; h <- pairs[2, j]
    ok <- is.finite(m[, i]) & is.finite(m[, h])
    a <- m[ok, i]; b <- m[ok, h]
    diffs <- a - b
    nonzero <- diffs[diffs != 0]
    exact_ok <- length(nonzero) < 50 && length(unique(abs(nonzero))) == length(nonzero)
    wt <- tryCatch(
      suppressWarnings(stats::wilcox.test(a, b, paired = TRUE,
                                           exact = exact_ok)),
      error = function(e) NULL)
    data.frame(
      Comparison = sprintf("%s vs %s", lv[i], lv[h]),
      Estimate = stats::median(diffs),
      `CI lower` = NA_real_,
      `CI upper` = NA_real_,
      Statistic = if (is.null(wt)) NA_real_ else unname(wt$statistic),
      df = NA_real_,
      `p-value` = if (is.null(wt)) NA_real_ else wt$p.value,
      `n pairs` = length(a),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  tab[["p-adjusted"]] <- stats::p.adjust(tab[["p-value"]], method = "holm")
  tab <- tab[, c("Comparison", "Estimate", "CI lower", "CI upper",
                 "Statistic", "df", "p-value", "p-adjusted", "n pairs")]
  mk_posthoc("Pairwise Wilcoxon signed-rank tests", tab,
             global_label, global_p, adjustment = "Holm", alpha = alpha,
             note = "The estimate is the median within-subject difference; confidence intervals are not provided for this rank-based follow-up.")
}

# Pairwise McNemar follow-ups for a significant or exploratory Cochran's Q
# result. Holm adjustment controls the family-wise error rate across condition
# pairs, while the global Q result remains the primary decision.
posthoc_paired_mcnemar <- function(m, global_label, global_p, alpha = 0.05) {
  lv <- colnames(m); if (is.null(lv)) lv <- paste0("Condition ", seq_len(ncol(m)))
  pairs <- utils::combn(seq_len(ncol(m)), 2)
  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    i <- pairs[1, j]; h <- pairs[2, j]
    a <- m[, i]; b <- m[, h]
    tab <- table(factor(a, levels = c(0, 1)), factor(b, levels = c(0, 1)))
    mc <- tryCatch(stats::mcnemar.test(tab), error = function(e) NULL)
    data.frame(
      Comparison = sprintf("%s vs %s", lv[i], lv[h]),
      Estimate = mean(a) - mean(b),
      `CI lower` = NA_real_,
      `CI upper` = NA_real_,
      Statistic = if (is.null(mc)) NA_real_ else unname(mc$statistic),
      df = if (is.null(mc)) NA_real_ else unname(mc$parameter),
      `p-value` = if (is.null(mc)) NA_real_ else mc$p.value,
      `n pairs` = length(a),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  tab[["p-adjusted"]] <- stats::p.adjust(tab[["p-value"]], method = "holm")
  tab <- tab[, c("Comparison", "Estimate", "CI lower", "CI upper",
                 "Statistic", "df", "p-value", "p-adjusted", "n pairs")]
  mk_posthoc("Pairwise McNemar tests", tab, global_label, global_p,
             adjustment = "Holm", alpha = alpha,
             note = "The estimate is the difference in observed success proportions between the two matched conditions; confidence intervals are not provided for McNemar's test.")
}

# Adjusted standardized (Pearson) residuals for a contingency table. Each cell
# residual is approximately standard normal under the null of independence, so
# it identifies WHICH row x column combinations deviate most from what
# independence predicts. This is the standard post-hoc for a chi-square / Fisher
# test of association. Equivalent to stats::chisq.test(tab)$stdres.
adjusted_std_residuals <- function(tab) {
  tab <- as.matrix(tab)
  n  <- sum(tab)
  rt <- rowSums(tab); ct <- colSums(tab)
  expected <- outer(rt, ct) / n
  (tab - expected) / sqrt(expected * outer(1 - rt / n, 1 - ct / n))
}

# Post-hoc for the chi-square / Fisher association test: per-cell adjusted
# standardized residuals with Holm-adjusted two-sided p-values, ordered by the
# most extreme deviation. Shares the shape produced by mk_posthoc().
posthoc_residuals <- function(tab, global_label, global_p, alpha = 0.05) {
  tab <- as.matrix(tab)
  n  <- sum(tab)
  rt <- rowSums(tab); ct <- colSums(tab)
  expected <- outer(rt, ct) / n
  res <- adjusted_std_residuals(tab)

  rn <- rownames(tab); if (is.null(rn)) rn <- paste0("row", seq_len(nrow(tab)))
  cn <- colnames(tab); if (is.null(cn)) cn <- paste0("col", seq_len(ncol(tab)))
  z <- as.vector(res)
  p_raw <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

  out <- data.frame(
    Cell            = paste(rep(rn, times = ncol(tab)), rep(cn, each = nrow(tab)), sep = " × "),
    Observed        = as.vector(tab),
    Expected        = round(as.vector(expected), 2),
    `Adj. residual` = round(z, 3),
    `p-value`       = p_raw,
    `p-adjusted`    = stats::p.adjust(p_raw, method = "holm"),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )
  out <- out[order(-abs(out[["Adj. residual"]])), , drop = FALSE]

  omnibus_significant <- isTRUE(!is.na(global_p) && global_p < alpha)
  crit <- stats::qnorm(1 - alpha / 2)
  flagged <- out$Cell[is.finite(out[["Adj. residual"]]) & abs(out[["Adj. residual"]]) > crit]
  context <- sprintf(
    "Adjusted standardized residuals for the %s. Each cell residual is approximately standard normal under independence; |residual| > %.2f flags a category combination that deviates from expectation at the %.0f%% level (per-cell p-values are Holm-adjusted for the %d cells). A positive residual means the cell has MORE observations than expected, a negative one FEWER.",
    global_label, crit, 100 * alpha, nrow(out))
  detail <- if (!omnibus_significant) {
    " The omnibus test was not statistically significant, so treat these cell-level signals as exploratory."
  } else if (length(flagged)) {
    sprintf(" The cells deviating most from independence are: %s.",
            paste(utils::head(flagged, 4), collapse = "; "))
  } else {
    " No single cell exceeds the critical value despite the overall association."
  }

  list(label = "Adjusted standardized residuals (post-hoc)",
       table = out, global_label = global_label, global_p = global_p,
       adjustment = "Holm", alpha = alpha,
       omnibus_significant = omnibus_significant,
       note = paste0(context, detail))
}

# ===========================================================================
# LOCATION COMPARISONS (continuous / ordinal outcomes)
# ===========================================================================

run_one_sample <- function(x, mu = 0, alpha = 0.05) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 2L)
    stop("A one-sample comparison needs at least two finite observations.", call. = FALSE)
  if (stats::sd(x) == 0)
    stop("A one-sample comparison cannot estimate uncertainty because every observed value is identical.", call. = FALSE)
  norm <- check_normality(x)
  tt <- stats::t.test(x, mu = mu)
  ws <- suppressWarnings(stats::wilcox.test(x, mu = mu))
  d  <- (mean(x) - mu) / stats::sd(x)

  primary <- mk_result(
    "One-sample t-test",
    c("n" = n, "mean" = round(mean(x), 3), "test value" = mu,
      "t" = round(unname(tt$statistic), 3), "df" = round(unname(tt$parameter), 1),
      "95% CI lower" = round(tt$conf.int[1], 3),
      "95% CI upper" = round(tt$conf.int[2], 3)),
    p = tt$p.value, effect = list("Cohen's d" = round(d, 3)),
    interpretation = sig_phrase(tt$p.value, alpha,
      sprintf("difference of the mean from %s", mu)))
  alt <- mk_result(
    "Wilcoxon signed-rank test",
    c("n" = n, "V" = unname(ws$statistic)),
    p = ws$p.value,
    interpretation = sig_phrase(ws$p.value, alpha, "difference of the median"))

  rec <- if (isTRUE(norm$ok) || n >= 30) "primary" else "alternative"
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(normality = norm),
       rationale = sprintf("Recommended: %s. %s",
         if (rec == "primary") "one-sample t-test" else "Wilcoxon signed-rank", norm$note))
}

run_two_independent <- function(x, g, alpha = 0.05) {
  d <- stats::na.omit(data.frame(x = x, g = droplevels(as.factor(g))))
  if (nlevels(d$g) != 2)
    stop(sprintf(
      "A two-group comparison needs exactly two groups, but the grouping variable has %d. Pick a two-level variable, or use the 'more than two groups' comparison.",
      nlevels(d$g)))
  lv <- levels(d$g); x1 <- d$x[d$g == lv[1]]; x2 <- d$x[d$g == lv[2]]
  if (length(x1) < 2L || length(x2) < 2L)
    stop("Each group needs at least two complete observations for an independent comparison.", call. = FALSE)
  if (stats::sd(x1) == 0 && stats::sd(x2) == 0 && identical(mean(x1), mean(x2)))
    stop("The two groups have no measurable variation, so a comparison statistic is undefined.", call. = FALSE)

  norm <- check_normality_by_group(d$x, d$g)
  vare <- check_variance_homogeneity(d$x, d$g)
  welch <- stats::t.test(x ~ g, data = d)                 # Welch by default
  mw    <- suppressWarnings(stats::wilcox.test(x ~ g, data = d))
  g_es  <- hedges_g(x1, x2)
  rb    <- rank_biserial_mw(unname(mw$statistic), length(x1), length(x2))

  primary <- mk_result(
    "Welch's t-test (independent samples)",
    c("n1" = length(x1), "n2" = length(x2),
      "mean1" = round(mean(x1), 3), "mean2" = round(mean(x2), 3),
      "t" = round(unname(welch$statistic), 3),
      "df" = round(unname(welch$parameter), 1),
      "95% CI lower" = round(welch$conf.int[1], 3),
      "95% CI upper" = round(welch$conf.int[2], 3)),
    p = welch$p.value, effect = list("Hedges g" = round(g_es, 3)),
    interpretation = sig_phrase(welch$p.value, alpha,
      sprintf("difference between %s and %s", lv[1], lv[2])),
    note = "Welch is the default (does not assume equal variances). A pooled t-test is available if variances are equal.")
  alt <- mk_result(
    "Mann-Whitney U test",
    c("W" = unname(mw$statistic)), p = mw$p.value,
    effect = list("rank-biserial r" = round(rb, 3)),
    interpretation = sig_phrase(mw$p.value, alpha, "difference in distributions"))

  rec <- if (isTRUE(norm$ok) || min(length(x1), length(x2)) >= 30)
    "primary" else "alternative"
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(normality = norm, variance = vare),
       rationale = sprintf("Recommended: %s. %s %s",
         if (rec == "primary") "Welch's t-test" else "Mann-Whitney U",
         norm$note, vare$note))
}

run_two_paired <- function(x, within, subject, alpha = 0.05) {
  m <- reshape_wide(x, within, subject)
  if (ncol(m) != 2)
    stop(sprintf(
      "A paired comparison needs exactly two conditions, but the condition variable has %d. Use the repeated-measures design for three or more conditions.",
      ncol(m)))
  d1 <- m[, 1]; d2 <- m[, 2]; diff <- d1 - d2
  if (length(diff) < 2L)
    stop("A paired comparison needs at least two complete subject pairs.", call. = FALSE)
  if (stats::sd(diff) == 0)
    stop("A paired comparison cannot estimate uncertainty because every within-subject difference is identical.", call. = FALSE)
  norm <- check_normality(diff); n <- length(diff)
  pt <- stats::t.test(d1, d2, paired = TRUE)
  ws <- suppressWarnings(stats::wilcox.test(d1, d2, paired = TRUE))
  dz <- mean(diff) / stats::sd(diff)

  primary <- mk_result(
    "Paired t-test",
    c("n pairs" = n, "mean diff" = round(mean(diff), 3),
      "t" = round(unname(pt$statistic), 3), "df" = unname(pt$parameter),
      "95% CI lower" = round(pt$conf.int[1], 3),
      "95% CI upper" = round(pt$conf.int[2], 3)),
    p = pt$p.value, effect = list("Cohen's dz" = round(dz, 3)),
    interpretation = sig_phrase(pt$p.value, alpha, "mean within-subject change"))
  alt <- mk_result(
    "Wilcoxon signed-rank test (paired)",
    c("n pairs" = n, "V" = unname(ws$statistic)), p = ws$p.value,
    interpretation = sig_phrase(ws$p.value, alpha, "median within-subject change"))

  rec <- if (isTRUE(norm$ok) || n >= 30) "primary" else "alternative"
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(normality_of_differences = norm),
       rationale = sprintf("Recommended: %s. %s",
         if (rec == "primary") "paired t-test" else "Wilcoxon signed-rank", norm$note))
}

run_anova_oneway <- function(x, g, alpha = 0.05) {
  d <- stats::na.omit(data.frame(x = x, g = droplevels(as.factor(g))))
  k <- nlevels(d$g); n <- nrow(d)
  if (k < 2L)
    stop("A one-way comparison needs at least two observed groups.", call. = FALSE)
  if (any(table(d$g) < 2L))
    stop("Each group needs at least two complete observations for a one-way comparison.", call. = FALSE)
  norm <- check_normality_by_group(d$x, d$g)
  vare <- check_variance_homogeneity(d$x, d$g)

  fit  <- stats::aov(x ~ g, data = d); a <- stats::anova(fit)
  welch <- stats::oneway.test(x ~ g, data = d, var.equal = FALSE)
  kw   <- stats::kruskal.test(x ~ g, data = d)
  eta2 <- eta_sq_aov(fit)
  eta2H <- eta_sq_kruskal(unname(kw$statistic), k, n)

  use_welch <- isFALSE(vare$ok)
  primary <- if (use_welch)
    mk_result("Welch's one-way ANOVA (unequal variances)",
      c("groups" = k, "F" = round(unname(welch$statistic), 3),
        "num df" = round(unname(welch$parameter[1]), 1),
        "den df" = round(unname(welch$parameter[2]), 1)),
      p = welch$p.value, effect = list("eta-squared" = round(eta2, 3)),
      interpretation = sig_phrase(welch$p.value, alpha, "difference among group means"),
      note = "Use Games-Howell post-hoc comparisons with Welch ANOVA.")
  else
    mk_result("One-way ANOVA",
      c("groups" = k, "F" = round(a$`F value`[1], 3),
        "df between" = a$Df[1], "df within" = a$Df[2]),
      p = a$`Pr(>F)`[1], effect = list("eta-squared" = round(eta2, 3)),
      interpretation = sig_phrase(a$`Pr(>F)`[1], alpha, "difference among group means"),
      note = "Follow a significant result with Tukey HSD post-hoc comparisons.")

  alt <- mk_result("Kruskal-Wallis test",
    c("groups" = k, "chi-squared" = round(unname(kw$statistic), 3),
      "df" = unname(kw$parameter)), p = kw$p.value,
    effect = list("eta-squared (H)" = round(eta2H, 3)),
    interpretation = sig_phrase(kw$p.value, alpha, "difference among groups"),
    note = "Follow a significant result with Dunn's post-hoc test.")

  rec <- if (isTRUE(norm$ok)) "primary" else "alternative"
  posthoc_primary <- if (use_welch)
    posthoc_games_howell(d, primary$label, primary$p_value, alpha = alpha)
  else
    posthoc_tukey(d, primary$label, primary$p_value, alpha = alpha)
  posthoc_alternative <- posthoc_dunn(d, alt$label, alt$p_value, alpha = alpha)
  posthoc <- if (rec == "primary") posthoc_primary else posthoc_alternative
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(normality = norm, variance = vare),
       posthoc = posthoc,
       rationale = sprintf("Recommended: %s. %s %s",
         if (rec == "primary") primary$label else "Kruskal-Wallis",
         norm$note, vare$note))
}

run_repeated <- function(x, within, subject, alpha = 0.05) {
  m <- reshape_wide(x, within, subject)
  k <- ncol(m); n <- nrow(m)
  if (k < 2L)
    stop("A repeated-measures comparison needs at least two conditions.", call. = FALSE)
  if (n < 2L)
    stop("A repeated-measures comparison needs at least two complete subjects.", call. = FALSE)
  long <- data.frame(
    x = as.vector(m),
    within = factor(rep(colnames(m), each = n)),
    subject = factor(rep(rownames(m), times = k)))

  fried <- suppressWarnings(stats::friedman.test(m))
  rm <- tryCatch({
    fit <- stats::aov(x ~ within + Error(subject / within), data = long)
    s <- summary(fit)
    tab <- NULL
    for (stratum in s) {
      df <- stratum[[1]]
      rn <- trimws(rownames(df))
      if ("within" %in% rn) { tab <- df[rn == "within", , drop = FALSE]; break }
    }
    tab
  }, error = function(e) NULL)

  primary <- if (!is.null(rm))
    mk_result("Repeated-measures ANOVA",
      c("subjects" = n, "conditions" = k,
        "F" = round(rm$`F value`[1], 3),
        "df1" = rm$Df[1], "df2" = (n - 1) * (k - 1)),
      p = rm$`Pr(>F)`[1],
      interpretation = sig_phrase(rm$`Pr(>F)`[1], alpha, "difference across conditions"),
      note = "Check sphericity (Mauchly); apply Greenhouse-Geisser correction if violated.")
  else
    mk_result("Repeated-measures ANOVA",
      c("subjects" = n, "conditions" = k), p = NA_real_,
      interpretation = "Repeated-measures ANOVA could not be fitted; use the Friedman test.")

  alt <- mk_result("Friedman test",
    c("subjects" = n, "conditions" = k,
      "chi-squared" = round(unname(fried$statistic), 3),
      "df" = unname(fried$parameter)), p = fried$p.value,
    interpretation = sig_phrase(fried$p.value, alpha, "difference across conditions"))

  rec <- if (!is.null(rm)) "primary" else "alternative"
  posthoc_primary <- posthoc_paired_t(
    m, primary$label, primary$p_value, alpha = alpha)
  posthoc_alternative <- posthoc_paired_wilcox(
    m, alt$label, alt$p_value, alpha = alpha)
  posthoc <- if (rec == "primary") posthoc_primary else posthoc_alternative
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(),
       posthoc = posthoc,
       rationale = "Repeated-measures ANOVA assumes normal, spherical data; Friedman is its nonparametric alternative.")
}

# ===========================================================================
# FACTORIAL AND MIXED-MODEL LOCATION DESIGNS
# ===========================================================================

# Quote user-selected column names when constructing formulas. This keeps the
# guided interface safe for names containing spaces or punctuation.
.formula_name <- function(x) paste0("`", gsub("`", "``", x, fixed = TRUE), "`")

.factorial_formula <- function(outcome, factors) {
  stats::as.formula(sprintf("%s ~ %s",
                           .formula_name(outcome),
                           paste(vapply(factors, .formula_name, character(1)),
                                 collapse = " * ")))
}

.anova_terms <- function(tab, residual_df = NULL) {
  rn <- rownames(tab)
  keep <- !is.na(rn) & rn != "Residuals"
  ss_error <- if ("Residuals" %in% rn) tab["Residuals", "Sum Sq"] else NA_real_
  data.frame(
    term = rn[keep],
    df = unname(tab[keep, "Df"]),
    sum_sq = unname(tab[keep, "Sum Sq"]),
    F = unname(tab[keep, "F value"]),
    p = unname(tab[keep, "Pr(>F)"]),
    partial_eta_sq = if (is.finite(ss_error))
      unname(tab[keep, "Sum Sq"]) /
        (unname(tab[keep, "Sum Sq"]) + ss_error) else NA_real_,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

.highest_order_interaction <- function(terms) {
  interaction <- grepl(":", terms$term, fixed = TRUE)
  if (!any(interaction)) return(terms$term[[length(terms$term)]])
  candidates <- terms$term[interaction]
  candidates[[which.max(vapply(strsplit(candidates, ":", fixed = TRUE),
                              length, integer(1)))]]
}

.term_row <- function(terms, term) {
  hit <- which(identical(terms$term, term) | terms$term == term)
  if (!length(hit)) return(NULL)
  terms[hit[[1]], , drop = FALSE]
}

.terms_to_result_rows <- function(terms) {
  if (is.null(terms) || !nrow(terms)) return(NULL)
  data.frame(
    Field = paste0("Term: ", terms$term),
    Value = vapply(seq_len(nrow(terms)), function(i) {
      sprintf("df = %s; F = %s; p = %s; partial eta-squared = %s",
              terms$df[[i]], p_fmt(terms$F[[i]]), p_fmt(terms$p[[i]]),
              p_fmt(terms$partial_eta_sq[[i]]))
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

# An aligned-rank-transform alternative implemented without making ARTool a
# hard dependency. Each target term is aligned against the nuisance terms,
# ranked, and then analysed with the same factorial model.
.run_art <- function(d, outcome, factors, target, alpha = 0.05) {
  full_formula <- .factorial_formula(outcome, factors)
  labels <- attr(stats::terms(full_formula), "term.labels")
  nuisance <- setdiff(labels, target)
  nuisance_rhs <- if (length(nuisance)) paste(nuisance, collapse = " + ") else "1"
  nuisance_formula <- stats::as.formula(sprintf("%s ~ %s",
                                                .formula_name(outcome), nuisance_rhs))
  aligned <- stats::residuals(stats::lm(nuisance_formula, data = d))
  d$.__art_rank <- rank(aligned, ties.method = "average")
  rank_formula <- stats::as.formula(sprintf(".__art_rank ~ %s",
                                            paste(vapply(factors, .formula_name, character(1)),
                                                  collapse = " * ")))
  fit <- stats::aov(rank_formula, data = d)
  terms <- .anova_terms(stats::anova(fit))
  target_row <- .term_row(terms, target)
  if (is.null(target_row))
    stop("The aligned-rank factorial term could not be evaluated.", call. = FALSE)
  res <- mk_result(
    sprintf("Aligned Rank Transform ANOVA (ART; %s-factor)", length(factors)),
    c("observations" = nrow(d), "factors" = length(factors),
      "F (highest-order interaction)" = target_row$F,
      "df numerator" = target_row$df),
    p = target_row$p,
    effect = list("partial eta-squared (highest-order interaction)" =
                    round(target_row$partial_eta_sq, 3)),
    interpretation = sig_phrase(target_row$p, alpha,
                                "rank-transformed factorial interaction"),
    note = "ART aligns each target effect against the remaining factorial terms before ranking; inspect the term table for main effects and interactions.")
  res$terms_table <- terms
  res
}

# Scheirer–Ray–Hare's two-way rank ANOVA. This follows the published
# rcompanion convention: rank the response, fit the factorial linear model,
# convert term sums of squares to H statistics, and apply the tie correction.
# The two-way method is deliberately kept separate from ART because they make
# different rank/model assumptions and are useful sensitivity analyses of one
# another.
run_scheirer_ray_hare <- function(df, outcome, factors, alpha = 0.05,
                                  tie_correct = TRUE) {
  if (length(factors) != 2)
    stop("Scheirer–Ray–Hare requires exactly two factors.", call. = FALSE)
  if (anyDuplicated(factors) || any(!factors %in% names(df)) ||
      !outcome %in% names(df))
    stop("Choose an outcome and two distinct factor variables present in the data.", call. = FALSE)

  d <- df[, unique(c(outcome, factors)), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[[outcome]] <- suppressWarnings(as.numeric(as.character(d[[outcome]])))
  if (!all(is.finite(d[[outcome]])))
    stop("The Scheirer–Ray–Hare outcome must contain finite numeric values.", call. = FALSE)
  for (v in factors) d[[v]] <- droplevels(as.factor(d[[v]]))
  if (any(vapply(d[factors], nlevels, integer(1)) < 2))
    stop("Each Scheirer–Ray–Hare factor needs at least two observed levels.", call. = FALSE)
  if (nrow(d) <= 4)
    stop("The two-way rank ANOVA does not have enough complete observations.", call. = FALSE)

  d$.__srh_rank_y <- rank(d[[outcome]], ties.method = "average")
  d$.__srh_f1 <- d[[factors[[1]]]]
  d$.__srh_f2 <- d[[factors[[2]]]]
  fit_i <- stats::lm(.__srh_rank_y ~ .__srh_f1 + .__srh_f2 +
                       .__srh_f1:.__srh_f2, data = d)
  an_i <- stats::anova(fit_i)

  # Match the standard type-II convention used by rcompanion for the first
  # factor while retaining the balanced-design result for the other terms.
  fit_ii <- stats::lm(.__srh_rank_y ~ .__srh_f2 + .__srh_f1 +
                        .__srh_f2:.__srh_f1, data = d)
  an_ii <- stats::anova(fit_ii)
  df_terms <- unname(an_i$Df[1:3]); ss_terms <- unname(an_i$`Sum Sq`[1:3])
  df_terms[[1]] <- unname(an_ii$Df[2]); ss_terms[[1]] <- unname(an_ii$`Sum Sq`[2])
  n <- nrow(d)
  ties <- table(d$.__srh_rank_y)
  correction <- if (isTRUE(tie_correct) && n > 1)
    1 - sum(ties^3 - ties) / (n^3 - n) else 1
  if (!is.finite(correction) || correction <= 0)
    correction <- 1
  h <- (ss_terms / (n * (n + 1) / 12)) / correction
  p <- stats::pchisq(h, df = df_terms, lower.tail = FALSE)
  terms <- data.frame(
    term = c(factors[[1]], factors[[2]], paste(factors, collapse = ":")),
    df = df_terms, sum_sq = ss_terms, H = h, p = p,
    stringsAsFactors = FALSE, row.names = NULL
  )
  target <- terms[3, , drop = FALSE]
  cells <- table(d$.__srh_f1, d$.__srh_f2)
  primary <- mk_result(
    "Scheirer–Ray–Hare test",
    c("observations" = n, "factors" = 2,
      "H (interaction)" = target$H, "df (interaction)" = target$df),
    p = target$p,
    effect = list("rank eta-squared (interaction)" =
                    round(target$H / max(n - 1, 1), 3)),
    interpretation = sig_phrase(target$p, alpha,
                                "rank-based factorial interaction"),
    note = sprintf("The tie-corrected Scheirer–Ray–Hare table reports both main effects and the interaction. The smallest observed cell contains %d observation%s; balanced designs with adequate cell counts are preferred.",
                   min(cells), if (min(cells) == 1) "" else "s"))
  primary$terms_table <- terms
  list(primary = primary, terms = terms, tie_correction = correction,
       rationale = "Scheirer–Ray–Hare is the two-way rank-based alternative; inspect the term table for the two main effects and their interaction.")
}

# Two-way and three-way ANOVA. The primary p-value is the highest-order
# interaction so the result has a single, meaningful omnibus recommendation;
# the attached term table retains every main effect and interaction.
run_factorial_anova <- function(df, outcome, factors, alpha = 0.05) {
  if (length(factors) < 2 || length(factors) > 3)
    stop("A factorial ANOVA needs exactly two or three factors.", call. = FALSE)
  if (anyDuplicated(factors) || any(!factors %in% names(df)))
    stop("Choose two or three distinct factor variables present in the data.", call. = FALSE)
  if (!outcome %in% names(df)) stop("Choose a valid numeric outcome variable.", call. = FALSE)

  d <- df[, unique(c(outcome, factors)), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[[outcome]] <- suppressWarnings(as.numeric(as.character(d[[outcome]])))
  if (!all(is.finite(d[[outcome]])))
    stop("The factorial outcome must contain finite numeric values.", call. = FALSE)
  for (v in factors) d[[v]] <- droplevels(as.factor(d[[v]]))
  if (any(vapply(d[factors], nlevels, integer(1)) < 2))
    stop("Each factorial variable needs at least two observed levels.", call. = FALSE)
  if (nrow(d) <= length(factors) + 1)
    stop("The factorial design does not have enough complete observations.", call. = FALSE)

  fit <- stats::aov(.factorial_formula(outcome, factors), data = d)
  terms <- .anova_terms(stats::anova(fit))
  target <- .highest_order_interaction(terms)
  target_row <- .term_row(terms, target)
  if (is.null(target_row))
    stop("The factorial interaction could not be evaluated.", call. = FALSE)

  primary <- mk_result(
    sprintf("%s-way factorial ANOVA", if (length(factors) == 2) "Two" else "Three"),
    c("observations" = nrow(d), "factors" = length(factors),
      "F (highest-order interaction)" = target_row$F,
      "df numerator" = target_row$df,
      "df residual" = tail(stats::anova(fit)$Df, 1)),
    p = target_row$p,
    effect = list("partial eta-squared (highest-order interaction)" =
                    round(target_row$partial_eta_sq, 3)),
    interpretation = sig_phrase(target_row$p, alpha,
                                "highest-order factorial interaction"),
    note = "The term table reports every main effect and interaction. For unbalanced data, interpret Type-I sums of squares in light of the factor order.")
  primary$terms_table <- terms
  art <- .run_art(d, outcome, factors, target, alpha = alpha)
  srh <- if (length(factors) == 2)
    run_scheirer_ray_hare(d, outcome, factors, alpha = alpha) else NULL
  alternative <- if (!is.null(srh)) srh$primary else art
  list(primary = primary, alternative = alternative,
       extra = if (!is.null(srh)) art else NULL, recommended = "primary",
       assumptions = list(), terms = terms,
       rationale = sprintf("Recommended: %s. %s",
                           primary$label,
                           if (!is.null(srh))
                             "Scheirer–Ray–Hare and aligned-rank-transform ANOVA are reported as nonparametric sensitivity analyses."
                           else
                             "The aligned-rank-transform ANOVA is the nonparametric alternative for three factors."))
}

.mixed_terms <- function(fit) {
  est <- lme4::fixef(fit)
  vc <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(vc)); statistic <- est / se
  p <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  keep <- names(est) != "(Intercept)"
  data.frame(term = names(est)[keep], estimate = unname(est[keep]),
             se = unname(se[keep]), statistic = unname(statistic[keep]),
             p = unname(p[keep]), stringsAsFactors = FALSE, row.names = NULL)
}

.mixed_fit <- function(y, group, within, subject) {
  d <- data.frame(y = y, group = droplevels(as.factor(group)),
                  within = droplevels(as.factor(within)),
                  subject = droplevels(as.factor(subject)))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nlevels(d$group) < 2 || nlevels(d$within) < 2 || nlevels(d$subject) < 2)
    stop("A mixed-model ANOVA needs at least two groups, two within-subject conditions, and two subjects.", call. = FALSE)
  fit <- tryCatch(
    lme4::lmer(y ~ group * within + (1 | subject), data = d, REML = FALSE),
    error = function(e) stop("Mixed-model ANOVA could not be fitted: ", conditionMessage(e), call. = FALSE)
  )
  list(data = d, fit = fit, terms = .mixed_terms(fit))
}

run_mixed_anova <- function(df, outcome, group, within, subject, alpha = 0.05) {
  if (any(!c(outcome, group, within, subject) %in% names(df)))
    stop("Choose outcome, between-group, within-subject, and subject variables present in the data.", call. = FALSE)
  if (length(unique(c(outcome, group, within, subject))) != 4)
    stop("The mixed-model roles must use four distinct variables.", call. = FALSE)
  y <- suppressWarnings(as.numeric(as.character(df[[outcome]])))
  if (any(!is.na(df[[outcome]]) & !is.finite(y)))
    stop("The mixed-model outcome must be numeric.", call. = FALSE)
  fitted <- .mixed_fit(y, df[[group]], df[[within]], df[[subject]])
  d <- fitted$data; terms <- fitted$terms
  interaction <- terms[grepl(":", terms$term, fixed = TRUE), , drop = FALSE]
  if (!nrow(interaction)) stop("The mixed-model interaction could not be evaluated.", call. = FALSE)
  target <- interaction[1, , drop = FALSE]
  primary <- mk_result(
    "Mixed-model ANOVA (linear mixed model)",
    c("observations" = nrow(d), "between groups" = nlevels(d$group),
      "within conditions" = nlevels(d$within), "subjects" = nlevels(d$subject),
      "interaction statistic" = target$statistic),
    p = target$p,
    interpretation = sig_phrase(target$p, alpha, "between-by-within interaction"),
    note = "A random intercept accounts for repeated observations within subjects; coefficient-level Wald p-values are shown for the fixed effects.")
  primary$terms_table <- terms

  ranked <- .mixed_fit(rank(d$y, ties.method = "average"), d$group,
                       d$within, d$subject)
  rterms <- ranked$terms
  rinteraction <- rterms[grepl(":", rterms$term, fixed = TRUE), , drop = FALSE]
  alt_target <- rinteraction[1, , drop = FALSE]
  alternative <- mk_result(
    "Rank-transformed mixed-model ANOVA",
    c("observations" = nrow(d), "between groups" = nlevels(d$group),
      "within conditions" = nlevels(d$within), "subjects" = nlevels(d$subject),
      "interaction statistic" = alt_target$statistic),
    p = alt_target$p,
    interpretation = sig_phrase(alt_target$p, alpha,
                                "rank-transformed between-by-within interaction"),
    note = "This rank-based sensitivity analysis is an approximation; report the linear mixed model as the primary analysis when its assumptions are defensible.")
  alternative$terms_table <- rterms
  list(primary = primary, alternative = alternative, recommended = "primary",
       assumptions = list(), terms = terms,
       rationale = "The mixed-model ANOVA estimates between-group, within-subject, and interaction effects while accounting for subject-level clustering with a random intercept.")
}

# Quade's rank ANCOVA follows the nonparametric ANCOVA definition: rank the
# response and covariates, regress ranked response on ranked covariates, then
# run the group F-test on those residuals.
run_quade_ancova <- function(df, outcome, group, covariates, alpha = 0.05) {
  covariates <- unique(covariates[!is.na(covariates) & nzchar(covariates)])
  if (!length(covariates) || !outcome %in% names(df) ||
      is.null(group) || !nzchar(group) || !group %in% names(df) ||
      any(!covariates %in% names(df)))
    stop("Choose a valid outcome, grouping variable, and at least one covariate for Quade ANCOVA.", call. = FALSE)
  d <- df[, unique(c(outcome, group, covariates)), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[[outcome]] <- suppressWarnings(as.numeric(as.character(d[[outcome]])))
  if (!all(is.finite(d[[outcome]])))
    stop("The Quade ANCOVA outcome must contain finite numeric values.", call. = FALSE)
  d[[group]] <- droplevels(as.factor(d[[group]]))
  for (cv in covariates) {
    d[[cv]] <- suppressWarnings(as.numeric(as.character(d[[cv]])))
    if (!all(is.finite(d[[cv]])))
      stop(sprintf("The Quade ANCOVA covariate '%s' must be numeric.", cv), call. = FALSE)
  }
  if (nlevels(d[[group]]) < 2)
    stop("Quade ANCOVA needs a grouping variable with at least two observed groups.", call. = FALSE)
  if (nrow(d) < nlevels(d[[group]]) + length(covariates) + 2)
    stop("Not enough complete observations to fit Quade ANCOVA.", call. = FALSE)

  d$.__quade_rank_y <- rank(d[[outcome]], ties.method = "average")
  rank_covariates <- character(length(covariates))
  for (i in seq_along(covariates)) {
    rank_covariates[[i]] <- paste0(".__quade_rank_cov_", i)
    d[[rank_covariates[[i]]]] <- rank(d[[covariates[[i]]]], ties.method = "average")
  }
  d$.__quade_group <- d[[group]]
  rank_fit <- stats::lm(stats::as.formula(sprintf(
    ".__quade_rank_y ~ %s", paste(rank_covariates, collapse = " + "))), data = d)
  d$.__quade_residual <- stats::residuals(rank_fit)
  group_fit <- stats::lm(.__quade_residual ~ .__quade_group, data = d)
  an <- stats::anova(group_fit)
  Fval <- an$`F value`[1]; df1 <- an$Df[1]; df2 <- an$Df[2]; pval <- an$`Pr(>F)`[1]
  ss_group <- an$`Sum Sq`[1]; ss_res <- an$`Sum Sq`[2]
  effect <- if (is.finite(ss_group + ss_res) && ss_group + ss_res > 0)
    ss_group / (ss_group + ss_res) else NA_real_
  primary <- mk_result(
    "Quade rank ANCOVA",
    c("groups" = nlevels(d[[group]]), "covariates" = length(covariates),
      "n used" = nrow(d), "F" = round(Fval, 3), "df1" = df1, "df2" = df2),
    p = pval,
    effect = list("rank partial eta-squared" = round(effect, 3)),
    interpretation = sig_phrase(pval, alpha,
      sprintf("rank-based difference in %s between groups after adjusting for %s",
              outcome, paste(covariates, collapse = ", "))),
    note = "Quade ranks the response and covariates, removes the ranked covariate effects, and tests the grouping effect on the residuals.")
  list(primary = primary, recommended = "primary", assumptions = list(),
       rationale = "Quade rank ANCOVA is the nonparametric sensitivity analysis for the covariate-adjusted group effect.")
}

# ANCOVA: compare group means on a continuous outcome after adjusting for one
# or more continuous covariates. The group effect is the F-test for adding the
# grouping factor to a model that already contains the covariate(s); a
# homogeneity-of-slopes check (group x covariate interaction) is reported.
run_ancova <- function(df, outcome, group, covariates, alpha = 0.05) {
  covariates <- unique(covariates[!is.na(covariates) & nzchar(covariates)])
  if (!length(covariates))
    stop("ANCOVA needs at least one continuous covariate to adjust for.", call. = FALSE)
  if (!outcome %in% names(df) || is.null(group) || !nzchar(group) || !group %in% names(df))
    stop("Choose a valid outcome and grouping variable for the ANCOVA.", call. = FALSE)
  if (any(!covariates %in% names(df)))
    stop("One or more covariates are not present in the data.", call. = FALSE)
  if (outcome %in% c(group, covariates) || group %in% covariates)
    stop("The outcome, grouping variable, and covariates must all be different variables.", call. = FALSE)

  d <- df[, unique(c(outcome, group, covariates)), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[[outcome]] <- suppressWarnings(as.numeric(as.character(d[[outcome]])))
  if (!all(is.finite(d[[outcome]])))
    stop("The ANCOVA outcome must contain finite numeric values.", call. = FALSE)
  for (cv in covariates) {
    d[[cv]] <- suppressWarnings(as.numeric(as.character(d[[cv]])))
    if (!all(is.finite(d[[cv]])))
      stop(sprintf("The covariate '%s' must be numeric.", cv), call. = FALSE)
  }
  d[[group]] <- droplevels(as.factor(d[[group]]))
  k <- nlevels(d[[group]])
  if (k < 2)
    stop("ANCOVA needs a grouping variable with at least two observed groups.", call. = FALSE)
  if (nrow(d) < k + length(covariates) + 2)
    stop("Not enough complete observations to fit the ANCOVA.", call. = FALSE)

  bt <- function(v) sprintf("`%s`", v)
  covs_rhs <- paste(vapply(covariates, bt, character(1)), collapse = " + ")
  f_reduced <- stats::as.formula(sprintf("%s ~ %s", bt(outcome), covs_rhs))
  f_full    <- stats::as.formula(sprintf("%s ~ %s + %s", bt(outcome), covs_rhs, bt(group)))
  f_inter   <- stats::as.formula(sprintf("%s ~ %s * %s", bt(outcome), covs_rhs, bt(group)))

  full <- stats::lm(f_full, data = d)
  reduced <- stats::lm(f_reduced, data = d)
  an <- stats::anova(reduced, full)             # F-test for the group effect
  Fval <- an$F[2]; df1 <- an$Df[2]; df2 <- an$Res.Df[2]; pval <- an$`Pr(>F)`[2]
  ss_group <- an$`Sum of Sq`[2]; ss_res <- an$RSS[2]
  partial_eta <- if (is.finite(ss_group + ss_res) && (ss_group + ss_res) > 0)
    ss_group / (ss_group + ss_res) else NA_real_

  # Homogeneity-of-regression-slopes assumption (group x covariate interaction).
  slope_p <- tryCatch({
    inter <- stats::lm(f_inter, data = d)
    slope_anova <- stats::anova(full, inter)
    p <- slope_anova$`Pr(>F)`[2]
    # Floating-point round-off can turn an exactly parallel slope comparison
    # into a tiny negative extra sum of squares, which base R reports as NA.
    # Treat that numerical zero as p = 1 rather than telling the analyst the
    # assumption could not be evaluated.
    if (is.na(p) && nrow(slope_anova) >= 2L &&
        is.finite(slope_anova$`Sum of Sq`[2]) &&
        abs(slope_anova$`Sum of Sq`[2]) <= 1e-10) p <- 1
    p
  }, error = function(e) NA_real_)
  res_norm <- check_normality(stats::residuals(full))

  covs_label <- paste(covariates, collapse = ", ")
  primary <- mk_result(
    "ANCOVA (group effect adjusted for covariates)",
    c("groups" = k, "covariates" = length(covariates), "n used" = nrow(d),
      "F" = round(Fval, 3), "df1" = df1, "df2" = df2),
    p = pval,
    effect = list("partial eta-squared" = round(partial_eta, 3)),
    interpretation = sig_phrase(pval, alpha,
      sprintf("difference in %s between groups after adjusting for %s", outcome, covs_label)),
    note = sprintf("The group effect is adjusted for %s.", covs_label))
  primary$terms_table <- .anova_terms(stats::anova(full))

  slope_note <- if (is.finite(slope_p))
    sprintf("Homogeneity-of-slopes check: group x covariate interaction p = %s. %s",
            p_fmt(slope_p),
            if (slope_p < alpha)
              "Slopes appear to differ between groups, so a single adjusted effect may be misleading - consider modelling separate slopes."
            else
              "No strong evidence that the covariate-outcome slope differs by group, so the common-slope ANCOVA is reasonable.")
  else "Homogeneity-of-slopes check could not be evaluated."

  alternative <- run_quade_ancova(d, outcome, group, covariates, alpha = alpha)$primary
  rec <- if (isTRUE(res_norm$ok) || nrow(d) >= 30) "primary" else "alternative"
  list(primary = primary, alternative = alternative, recommended = rec,
       assumptions = list(homogeneity_of_slopes = list(note = slope_note),
                          residual_normality = list(note = paste("Residual normality:", res_norm$note))),
       rationale = sprintf("ANCOVA compares %s across the %d groups of '%s' after adjusting for %s. Recommended: %s. Check the homogeneity-of-slopes assumption before trusting the single adjusted effect.",
                           outcome, k, group, covs_label,
                           if (rec == "primary") primary$label else alternative$label))
}

# ===========================================================================
# CATEGORICAL OUTCOMES (proportions / associations)
# ===========================================================================

run_proportion_test <- function(x, ref_prop = 0.5, success = NULL, alpha = 0.05) {
  if (!is.numeric(ref_prop) || length(ref_prop) != 1 || !is.finite(ref_prop) ||
      ref_prop <= 0 || ref_prop >= 1)
    stop("The reference proportion must be a single number strictly between 0 and 1.", call. = FALSE)
  observed <- x[!is.na(x)]
  lev <- levels(droplevels(as.factor(observed)))
  if (length(lev) != 2)
    stop("A one-proportion test needs a binary outcome with exactly two observed levels.", call. = FALSE)
  if (is.null(success) || !length(success) || !nzchar(as.character(success)))
    success <- lev[[2]]
  success <- as.character(success)[1]
  if (!success %in% lev)
    stop(sprintf("Success level '%s' is not one of the observed outcome levels.", success), call. = FALSE)
  successes <- sum(as.character(observed) == success)
  n <- length(observed); failures <- n - successes
  exact <- stats::binom.test(successes, n, p = ref_prop)
  approx <- suppressWarnings(stats::prop.test(successes, n, p = ref_prop,
                                               correct = FALSE))
  primary <- mk_result(
    "Exact binomial test",
    c("successes" = successes, "failures" = failures, "n" = n,
      "observed proportion" = successes / n, "reference proportion" = ref_prop,
      "95% CI lower" = exact$conf.int[1], "95% CI upper" = exact$conf.int[2]),
    p = exact$p.value,
    effect = list("proportion difference" = successes / n - ref_prop),
    interpretation = sig_phrase(exact$p.value, alpha,
                                sprintf("difference from the reference proportion of %.3f", ref_prop)),
    note = sprintf("The success level is '%s'. The exact binomial test is the primary result.", success))
  alternative <- mk_result(
    "One-sample proportion z-test",
    c("successes" = successes, "failures" = failures, "n" = n,
      "observed proportion" = successes / n,
      "chi-squared" = unname(approx$statistic)),
    p = approx$p.value,
    effect = list("proportion difference" = successes / n - ref_prop),
    interpretation = sig_phrase(approx$p.value, alpha,
                                sprintf("difference from the reference proportion of %.3f", ref_prop)),
    note = "The normal approximation is shown as a sensitivity analysis; use the exact binomial result for small samples or extreme proportions.")
  list(primary = primary, alternative = alternative, recommended = "primary",
       assumptions = list(), rationale = "The exact binomial test compares one binary outcome proportion with the selected reference value; the normal-approximation proportion test is also reported.")
}

run_categorical <- function(x, y, paired = FALSE, alpha = 0.05) {
  tab <- table(x, y)
  if (!length(tab) || any(dim(tab) < 2L))
    stop("A categorical association needs two variables with at least two observed levels each.", call. = FALSE)
  is2x2 <- all(dim(tab) == 2)

  if (paired && is2x2) {
    mc <- stats::mcnemar.test(tab)
    primary <- mk_result("McNemar's test (paired 2x2)",
      c("chi-squared" = round(unname(mc$statistic), 3), "df" = unname(mc$parameter)),
      p = mc$p.value,
      interpretation = sig_phrase(mc$p.value, alpha, "change in paired proportions"))
    return(list(primary = primary, alternative = NULL, recommended = "primary",
                assumptions = list(),
                rationale = "Paired 2x2 table: McNemar's test compares the discordant pairs."))
  }

  exp_ok <- check_expected_counts(tab)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  fisher <- tryCatch(stats::fisher.test(tab),
                     error = function(e) stats::fisher.test(tab, simulate.p.value = TRUE))
  v <- cramers_v(tab)

  primary <- mk_result("Chi-square test of independence",
    c("chi-squared" = round(unname(chi$statistic), 3),
      "df" = unname(chi$parameter), "n" = sum(tab)),
    p = chi$p.value, effect = list("Cramer's V" = round(v, 3)),
    interpretation = sig_phrase(chi$p.value, alpha, "association between the variables"))
  alt <- mk_result("Fisher's exact test",
    c("n" = sum(tab)), p = fisher$p.value,
    effect = if (is2x2 && !is.null(fisher$estimate))
      list("odds ratio" = round(unname(fisher$estimate), 3)) else NULL,
    interpretation = sig_phrase(fisher$p.value, alpha, "association between the variables"))

  rec <- if (isTRUE(exp_ok$ok)) "primary" else "alternative"
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(expected_counts = exp_ok),
       posthoc = posthoc_residuals(tab, "chi-square test of independence",
                                   chi$p.value, alpha = alpha),
       rationale = sprintf("Recommended: %s. %s",
         if (rec == "primary") "chi-square test" else "Fisher's exact test", exp_ok$note))
}

# Cochran's Q test for a binary outcome measured on the same subjects under
# three or more conditions/time points. This is the binary repeated-measures
# analogue of the Friedman test. The implementation is dependency-free and
# uses the standard chi-squared approximation (df = k - 1).
run_cochran_q <- function(x, within, subject, success = NULL, alpha = 0.05) {
  if (length(x) != length(within) || length(x) != length(subject))
    stop("Cochran's Q needs outcome, condition, and subject vectors of equal length.",
         call. = FALSE)

  d <- data.frame(
    outcome = x,
    condition = within,
    subject = subject,
    stringsAsFactors = FALSE
  )
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!nrow(d))
    stop("Cochran's Q needs complete outcome, condition, and subject observations.",
         call. = FALSE)

  d$condition <- droplevels(as.factor(d$condition))
  d$subject <- droplevels(as.factor(d$subject))
  if (nlevels(d$condition) < 3)
    stop("Cochran's Q needs at least three repeated conditions or time points.",
         call. = FALSE)
  if (nlevels(d$subject) < 2)
    stop("Cochran's Q needs at least two subjects.", call. = FALSE)

  key <- paste(as.character(d$subject), as.character(d$condition), sep = "\r")
  if (anyDuplicated(key))
    stop("Cochran's Q requires one outcome per subject and condition/time point; duplicate subject-time rows were found.",
         call. = FALSE)

  observed <- as.character(d$outcome)
  outcome_levels <- if (is.factor(d$outcome)) {
    levels(droplevels(d$outcome))
  } else {
    unique(observed)
  }
  if (length(outcome_levels) != 2)
    stop("Cochran's Q needs a binary outcome with exactly two observed levels.",
         call. = FALSE)
  success <- if (is.null(success) || !length(success) ||
                 !nzchar(as.character(success)[1])) outcome_levels[[2]]
             else as.character(success)[1]
  if (!success %in% outcome_levels)
    stop(sprintf("Success level '%s' is not one of the observed outcome levels.", success),
         call. = FALSE)
  d$.__binary <- as.integer(as.character(d$outcome) == success)

  # tapply retains NA for missing subject-condition combinations; those rows
  # are incomplete blocks and are excluded, as required by Cochran's Q.
  m <- tapply(d$.__binary, list(d$subject, d$condition), function(v) v[[1]])
  complete <- stats::complete.cases(m)
  m <- m[complete, , drop = FALSE]
  if (nrow(m) < 2)
    stop("Cochran's Q needs at least two subjects with complete observations at every condition.",
         call. = FALSE)

  k <- ncol(m)
  column_totals <- colSums(m)
  row_totals <- rowSums(m)
  total <- sum(m)
  denominator <- sum(row_totals * (k - row_totals))
  if (!is.finite(denominator) || denominator <= 0)
    stop("Cochran's Q is undefined because all subjects have the same response pattern.",
         call. = FALSE)
  q_stat <- (k - 1) * (k * sum(column_totals^2) - total^2) / denominator
  q_stat <- max(0, as.numeric(q_stat))
  df_q <- k - 1
  p_value <- stats::pchisq(q_stat, df = df_q, lower.tail = FALSE)
  rates <- colMeans(m)

  primary <- mk_result(
    "Cochran's Q test (repeated binary outcome)",
    c("Q" = round(q_stat, 3), "df" = df_q,
      "complete subjects" = nrow(m), "conditions" = k),
    p = p_value,
    effect = list("minimum success rate" = round(min(rates), 3),
                  "maximum success rate" = round(max(rates), 3),
                  "success level" = success),
    interpretation = sig_phrase(p_value, alpha,
                                "difference in paired binary proportions"),
    note = sprintf(
      "Success level: '%s'. %d complete subject blocks were analysed; %d incomplete subject block(s) were excluded.",
      success, nrow(m), sum(!complete)))

  posthoc <- posthoc_paired_mcnemar(m, primary$label, p_value, alpha = alpha)

  list(
    primary = primary,
    alternative = NULL,
    recommended = "primary",
    posthoc = posthoc,
    assumptions = list(
      complete_blocks = list(note = sprintf(
        "Cochran's Q used %d complete subjects across %d conditions; %d incomplete subject block(s) were excluded.",
        nrow(m), k, sum(!complete)))
    ),
    rationale = "Cochran's Q tests whether the success proportions differ across three or more matched conditions/time points for the same subjects. If the global test is significant, follow up with pairwise McNemar tests using multiplicity adjustment."
  )
}

# Shared longitudinal binary route: McNemar for two repeated conditions and
# Cochran's Q for three or more. Long data are reshaped to complete subject x
# condition blocks before the appropriate categorical test is called.
run_repeated_binary <- function(x, within, subject, success = NULL, alpha = 0.05) {
  if (length(x) != length(within) || length(x) != length(subject))
    stop("Repeated binary analysis needs outcome, condition, and subject vectors of equal length.",
         call. = FALSE)
  d <- data.frame(outcome = x, condition = within, subject = subject,
                  stringsAsFactors = FALSE)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!nrow(d))
    stop("Repeated binary analysis needs complete outcome, condition, and subject observations.",
         call. = FALSE)
  d$condition <- droplevels(as.factor(d$condition))
  if (nlevels(d$condition) < 2)
    stop("Repeated binary analysis needs at least two conditions or time points.",
         call. = FALSE)
  if (anyDuplicated(paste(as.character(d$subject), as.character(d$condition), sep = "\r")))
    stop("Repeated binary analysis requires one outcome per subject and condition/time point; duplicate subject-time rows were found.",
         call. = FALSE)
  observed <- as.character(d$outcome)
  outcome_levels <- if (is.factor(d$outcome)) levels(droplevels(d$outcome)) else unique(observed)
  if (length(outcome_levels) != 2)
    stop("Repeated binary analysis needs a binary outcome with exactly two observed levels.",
         call. = FALSE)
  success <- if (is.null(success) || !length(success) ||
                 !nzchar(as.character(success)[1])) outcome_levels[[2]]
             else as.character(success)[1]
  if (!success %in% outcome_levels)
    stop(sprintf("Success level '%s' is not one of the observed outcome levels.", success),
         call. = FALSE)
  d$.__binary <- as.integer(as.character(d$outcome) == success)
  d$subject <- droplevels(as.factor(d$subject))
  m <- tapply(d$.__binary, list(d$subject, d$condition), function(v) v[[1]])
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (nrow(m) < 2)
    stop("Repeated binary analysis needs at least two subjects with complete observations at every condition.",
         call. = FALSE)
  if (ncol(m) >= 3)
    return(run_cochran_q(x, within, subject, success = success, alpha = alpha))

  res <- run_categorical(factor(m[, 1], levels = c(0, 1)),
                         factor(m[, 2], levels = c(0, 1)),
                         paired = TRUE, alpha = alpha)
  res$rationale <- paste(
    res$rationale,
    "The repeated-binary longitudinal route used the two-condition McNemar test."
  )
  res
}

# ===========================================================================
# CORRELATION
# ===========================================================================

run_correlation <- function(x, y, alpha = 0.05) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3L)
    stop("A correlation needs at least three complete observations.", call. = FALSE)
  if (stats::sd(x) == 0 || stats::sd(y) == 0)
    stop("A correlation cannot be computed when either variable has no variation.", call. = FALSE)
  nx <- check_normality(x); ny <- check_normality(y)
  both_normal <- isTRUE(nx$ok) && isTRUE(ny$ok)

  pear <- suppressWarnings(stats::cor.test(x, y, method = "pearson"))
  spear <- suppressWarnings(stats::cor.test(x, y, method = "spearman"))
  ken  <- suppressWarnings(stats::cor.test(x, y, method = "kendall"))

  primary <- mk_result("Pearson correlation",
    c("n" = n, "r" = round(unname(pear$estimate), 3),
      "95% CI lower" = round(pear$conf.int[1], 3),
      "95% CI upper" = round(pear$conf.int[2], 3)),
    p = pear$p.value, effect = list("r" = round(unname(pear$estimate), 3)),
    interpretation = sig_phrase(pear$p.value, alpha, "linear correlation"))
  alt <- mk_result("Spearman correlation",
    c("n" = n, "rho" = round(unname(spear$estimate), 3)),
    p = spear$p.value, effect = list("rho" = round(unname(spear$estimate), 3)),
    interpretation = sig_phrase(spear$p.value, alpha, "monotonic correlation"))
  ken_res <- mk_result("Kendall's tau",
    c("n" = n, "tau" = round(unname(ken$estimate), 3)),
    p = ken$p.value, effect = list("tau" = round(unname(ken$estimate), 3)),
    interpretation = sig_phrase(ken$p.value, alpha, "monotonic correlation"))

  rec <- if (both_normal) "primary" else "alternative"
  list(primary = primary, alternative = alt, extra = ken_res, recommended = rec,
       assumptions = list(normality_x = nx, normality_y = ny),
       rationale = sprintf("Recommended: %s. %s A scatter plot should be checked for linearity first.",
         if (rec == "primary") "Pearson (both variables ~normal)" else "Spearman (non-normal or non-linear)",
         paste(nx$note, ny$note)))
}

# Repeated-measures correlation (rmcorr). This is the common within-subject
# association from the fixed-effects model y ~ subject + x. The implementation
# mirrors the CRAN rmcorr method without adding a runtime package dependency.
run_repeated_correlation <- function(x, y, subject, alpha = 0.05) {
  if (length(x) != length(y) || length(x) != length(subject))
    stop("Repeated-measures correlation needs X, Y, and subject vectors of equal length.",
         call. = FALSE)
  d <- data.frame(
    x = suppressWarnings(as.numeric(as.character(x))),
    y = suppressWarnings(as.numeric(as.character(y))),
    subject = subject,
    stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$subject) & is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  if (nrow(d) < 4)
    stop("Repeated-measures correlation needs at least four complete observations.",
         call. = FALSE)
  d$subject <- droplevels(as.factor(d$subject))
  if (nlevels(d$subject) < 2)
    stop("Repeated-measures correlation needs at least two subjects.", call. = FALSE)
  counts <- table(d$subject)
  if (any(counts < 2))
    stop("Each subject needs at least two complete paired observations for repeated-measures correlation.",
         call. = FALSE)

  fit <- stats::lm(y ~ subject + x, data = d,
                   contrasts = list(subject = "contr.sum"))
  if (!is.finite(fit$df.residual) || fit$df.residual <= 0)
    stop("Repeated-measures correlation has no residual degrees of freedom.",
         call. = FALSE)
  drop <- stats::drop1(fit, . ~ ., test = "F")
  x_row <- match("x", rownames(drop))
  if (is.na(x_row))
    stop("Repeated-measures correlation could not estimate the within-subject association.",
         call. = FALSE)
  ss_x <- unname(drop[["Sum of Sq"]][x_row])
  rss <- unname(drop[["RSS"]][1])
  slope <- unname(stats::coef(fit)[["x"]])
  r_rm <- sign(slope) * sqrt(ss_x / (ss_x + rss))
  p_value <- unname(drop[["Pr(>F)"]][x_row])
  df_resid <- unname(fit$df.residual)

  # Fisher-z analytic interval, using the rmcorr effective sample size
  # (residual df + 2), as in the reference implementation.
  n_effective <- df_resid + 2
  if (is.finite(r_rm) && abs(r_rm) < 1 && n_effective > 3) {
    z <- atanh(r_rm)
    margin <- stats::qnorm(1 - alpha / 2) / sqrt(n_effective - 3)
    ci <- tanh(z + c(-1, 1) * margin)
  } else {
    ci <- c(NA_real_, NA_real_)
  }

  primary <- mk_result(
    "Repeated-measures correlation (rmcorr)",
    c("n observations" = nrow(d), "subjects" = nlevels(d$subject),
      "r_rm" = round(r_rm, 3), "df" = df_resid,
      "95% CI lower" = round(ci[1], 3),
      "95% CI upper" = round(ci[2], 3)),
    p = p_value,
    effect = list("repeated-measures r" = round(r_rm, 3)),
    interpretation = sig_phrase(p_value, alpha,
                                "within-subject linear association"),
    note = "The subject fixed effects remove between-subject level differences; the reported association is the common within-subject relationship.")
  list(
    primary = primary,
    alternative = NULL,
    recommended = "primary",
    assumptions = list(
      repeated_subjects = list(note = sprintf(
        "%d subjects contributed at least two complete paired observations; inference assumes a common within-subject linear slope.",
        nlevels(d$subject)))
    ),
    rationale = "Repeated-measures correlation estimates the common within-subject association between two numeric variables while accounting for subject-level differences."
  )
}

# Point-biserial correlation: the association between a binary variable and a
# continuous variable. It equals Pearson's r with the binary variable coded
# 0/1, and its significance test is identical to the two-sample t-test. The
# rank-biserial correlation (from the Mann-Whitney U) is offered as the
# nonparametric alternative.
run_point_biserial <- function(x, y, alpha = 0.05) {
  y <- suppressWarnings(as.numeric(as.character(y)))
  ok <- !is.na(x) & is.finite(y)
  xf <- droplevels(as.factor(x[ok])); y <- y[ok]
  if (nlevels(xf) != 2)
    stop("Point-biserial correlation needs a binary variable with exactly two observed levels.", call. = FALSE)
  n <- length(y)
  if (n < 3) stop("Point-biserial correlation needs at least three complete observations.", call. = FALSE)
  lv <- levels(xf); x01 <- as.integer(xf == lv[2])
  if (stats::sd(x01) == 0 || stats::sd(y) == 0)
    stop("Point-biserial correlation cannot be computed when a variable has no variation.", call. = FALSE)

  ct <- suppressWarnings(stats::cor.test(x01, y, method = "pearson"))
  r <- unname(ct$estimate)
  norm <- check_normality(y)

  primary <- mk_result(
    "Point-biserial correlation",
    c("n" = n, "reference level (coded 1)" = lv[2],
      "r_pb" = round(r, 3),
      "95% CI lower" = round(ct$conf.int[1], 3),
      "95% CI upper" = round(ct$conf.int[2], 3)),
    p = ct$p.value, effect = list("point-biserial r" = round(r, 3)),
    interpretation = sig_phrase(ct$p.value, alpha,
      sprintf("association between the binary variable and the numeric variable (r_pb = %.3f)", r)),
    note = "Point-biserial r equals Pearson's r with the binary variable coded 0/1; its p-value matches the two-sample t-test.")

  mw <- suppressWarnings(stats::wilcox.test(y ~ xf))
  n1 <- sum(x01 == 0); n2 <- sum(x01 == 1)
  rb <- rank_biserial_mw(unname(mw$statistic), n1, n2)
  alt <- mk_result(
    "Rank-biserial correlation (Mann-Whitney U)",
    c("n" = n, "W" = unname(mw$statistic)),
    p = mw$p.value, effect = list("rank-biserial r" = round(rb, 3)),
    interpretation = sig_phrase(mw$p.value, alpha, "difference in ranks between the two groups"))

  rec <- if (isTRUE(norm$ok) || n >= 30) "primary" else "alternative"
  list(primary = primary, alternative = alt, recommended = rec,
       assumptions = list(normality = norm),
       rationale = sprintf("Point-biserial correlation quantifies how strongly the binary split relates to the numeric variable. Recommended: %s. %s",
         if (rec == "primary") "point-biserial (parametric)" else "rank-biserial (nonparametric)",
         norm$note))
}
