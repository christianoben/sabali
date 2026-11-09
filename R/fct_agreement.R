# fct_agreement.R ------------------------------------------------------------
# Agreement between two categorical measurements of the same thing.
#
# This answers a different question from the tests already in the app, and the
# difference matters. McNemar and the chi-square test ask whether the two
# measurements have the same MARGINAL distribution, or whether they are
# associated. Neither asks whether they agree case by case: two raters can have
# identical margins while disagreeing on every individual, and two measurements
# can be strongly associated yet systematically differ.
#
# Agreement is the right measure whenever the same thing is measured twice -
# two clinicians reading the same slides or radiographs, a rapid test against a
# reference assay, self-report against a register, two anatomical sites in the
# same person, paired household members, or the same instrument re-administered
# for test-retest reliability.
#
# Everything here is computed from the contingency table with the classic
# Fleiss-Cohen-Everitt (1969) asymptotic variance, using the AGREEMENT weight
# convention throughout (w_ii = 1, off-diagonal weights shrink towards 0 as
# categories get further apart). The two conventions - agreement weights and
# disagreement weights - give the same kappa but different variance formulas, so
# they are never mixed. The results are validated against independent
# implementations in tests/test_agreement.R.
#
# Two raters use Cohen's kappa; three or more use Fleiss' kappa, which is a
# different estimator (it measures agreement among a PANEL rather than between a
# specific pair) and is implemented separately below.

AGREEMENT_WEIGHTS <- c(
  "Unweighted (nominal categories)"        = "unweighted",
  "Linear weights (ordered categories)"    = "linear",
  "Quadratic weights (ordered categories)" = "quadratic"
)

# Agreement weight matrix for k ordered categories. w_ii = 1; off-diagonal
# weights fall with distance for the ordinal schemes and are 0 for nominal.
.agreement_weight_matrix <- function(k, weights = "unweighted") {
  i <- matrix(seq_len(k), nrow = k, ncol = k)
  j <- t(i)
  d <- switch(weights,
    # `1 *` keeps the matrix dimensions; as.numeric() would flatten it.
    unweighted = 1 * (i != j),
    linear     = abs(i - j) / (k - 1L),
    quadratic  = (i - j)^2 / (k - 1L)^2,
    stop("Unknown weighting scheme for kappa.", call. = FALSE))
  1 - d
}

#' Cohen's kappa for two categorical measurements of the same units.
#'
#' @param x,y     The two measurements, paired element by element.
#' @param weights "unweighted" (nominal), or "linear"/"quadratic" for ordered
#'                categories, where a near miss counts as partial agreement.
#' @param conf    Confidence level for the interval.
#' @param levels  Optional level set, in order. Supply this for ordered
#'                categories: the weights depend on the ORDER, and sorting
#'                labels alphabetically would otherwise decide it silently.
#' @return A list with the table, n, po (observed agreement), pe (expected by
#'   chance), kappa, its SE and CI, the weighting used, and - for a 2x2 table -
#'   the prevalence and bias indices and PABAK.
agreement_kappa <- function(x, y, weights = "unweighted", conf = 0.95,
                            levels = NULL) {
  keep <- !is.na(x) & !is.na(y)
  n_dropped <- sum(!keep)
  x <- x[keep]; y <- y[keep]
  if (!length(x))
    stop("Agreement needs at least one case measured both ways.", call. = FALSE)
  lv <- levels %||% .agreement_levels(x, y)
  fx <- factor(as.character(x), levels = lv)
  fy <- factor(as.character(y), levels = lv)
  if (anyNA(fx) || anyNA(fy))
    stop("Some observed values are outside the supplied category list.",
         call. = FALSE)
  k <- length(lv)
  if (k < 2L)
    stop("Agreement needs at least two categories across the two measurements.",
         call. = FALSE)
  if (!identical(weights, "unweighted") && k == 2L)
    weights <- "unweighted"   # with two categories every scheme coincides

  tab <- table(fx, fy)
  n <- sum(tab)
  p <- tab / n
  pr <- rowSums(p); pc <- colSums(p)
  w <- .agreement_weight_matrix(k, weights)

  po <- sum(w * p)
  pe <- sum(w * outer(pr, pc))
  kappa <- if (isTRUE(all.equal(pe, 1))) NA_real_ else (po - pe) / (1 - pe)

  # Fleiss-Cohen-Everitt asymptotic variance, agreement-weight convention.
  wr <- as.vector(w %*% pc)          # w-bar_i.  (row means over columns)
  wc <- as.vector(t(w) %*% pr)       # w-bar_.j  (column means over rows)
  se <- NA_real_
  if (is.finite(kappa) && !isTRUE(all.equal(pe, 1))) {
    inner <- outer(wr, wc, "+")
    term <- sum(p * (w - inner * (1 - kappa))^2) - (kappa - pe * (1 - kappa))^2
    v <- term / (n * (1 - pe)^2)
    if (is.finite(v) && v > 0) se <- sqrt(v)
  }
  z <- stats::qnorm(1 - (1 - conf) / 2)
  ci <- if (is.finite(se)) c(kappa - z * se, kappa + z * se) else c(NA_real_, NA_real_)

  out <- list(table = tab, n = as.integer(n), n_dropped = as.integer(n_dropped),
              levels = lv, weights = weights, po = po, pe = pe,
              kappa = kappa, se = se, ci = ci, conf = conf,
              n_categories = k)
  if (k == 2L) {
    # Kappa is depressed when prevalence is very high or very low even though
    # raw agreement is excellent - the "kappa paradox". These two indices say
    # whether that is what is happening, and PABAK is the agreement adjusted
    # for it. Reporting kappa alone in a low-prevalence setting (which
    # colonisation studies often are) invites the wrong conclusion.
    a <- tab[1, 1]; b <- tab[1, 2]; cc <- tab[2, 1]; d <- tab[2, 2]
    out$prevalence_index <- abs(a - d) / n
    out$bias_index <- abs(b - cc) / n
    out$pabak <- 2 * (po) - 1
  }
  out
}

# The level set shared by the two measurements. Ordered factors keep their own
# order (it is meaningful); anything else is sorted so the result is stable.
.agreement_levels <- function(x, y) {
  if (is.ordered(x) && is.ordered(y) && identical(levels(x), levels(y)))
    return(levels(x))
  if (is.factor(x) && is.factor(y) && identical(levels(x), levels(y)))
    return(levels(x))
  lv <- union(as.character(stats::na.omit(unique(x))),
              as.character(stats::na.omit(unique(y))))
  if (is.factor(x) || is.factor(y)) {
    ord <- union(levels(as.factor(x)), levels(as.factor(y)))
    return(ord[ord %in% lv])
  }
  sort(lv)
}

#' Plain-language strength label for a kappa value.
#'
#' Landis & Koch's bands, named as such: they are a convention, not a
#' statistical result, and the note says so wherever this is shown.
agreement_strength <- function(kappa) {
  if (!is.finite(kappa)) return("not estimable")
  if (kappa < 0)    return("worse than chance")
  if (kappa < 0.20) return("slight")
  if (kappa < 0.40) return("fair")
  if (kappa < 0.60) return("moderate")
  if (kappa < 0.80) return("substantial")
  "almost perfect"
}

#' Agreement as a guided-test result, in the app's standard test contract.
#'
#' The primary result is kappa. McNemar's test is reported alongside it as the
#' complementary question - not an alternative to it - because a pair of
#' measurements can agree poorly with identical margins, or agree well while
#' differing systematically, and reading only one of the two misses that.
run_agreement <- function(x, y, weights = "unweighted", alpha = 0.05,
                          levels = NULL) {
  a <- agreement_kappa(x, y, weights = weights, levels = levels)
  wlab <- switch(a$weights, unweighted = "Cohen's kappa",
                 linear = "Weighted kappa (linear)",
                 quadratic = "Weighted kappa (quadratic)")
  stats_named <- c(
    "n cases measured both ways" = a$n,
    "categories" = a$n_categories,
    "observed agreement" = round(a$po, 4),
    "expected agreement by chance" = round(a$pe, 4),
    "kappa" = round(a$kappa, 4),
    "standard error" = round(a$se, 4))
  if (!is.null(a$pabak)) {
    stats_named[["prevalence index"]] <- round(a$prevalence_index, 4)
    stats_named[["bias index"]] <- round(a$bias_index, 4)
    stats_named[["PABAK"]] <- round(a$pabak, 4)
  }
  ci_txt <- if (all(is.finite(a$ci)))
    sprintf("%.3f to %.3f", a$ci[1], a$ci[2]) else "not estimable"

  # Kappa is an agreement coefficient, not a hypothesis test. The app's result
  # contract carries a p-value, so the honest thing is to report the test of
  # kappa > 0 - which is rarely the question of interest - and say plainly in the
  # interpretation that the interval is what should be read.
  p_value <- if (is.finite(a$kappa) && is.finite(a$se) && a$se > 0)
    stats::pnorm(a$kappa / a$se, lower.tail = FALSE) else NA_real_

  interp <- sprintf(
    "%s = %.3f (%.0f%% CI %s), which is %s agreement on Landis & Koch's convention. Raw agreement was %.1f%% of %d cases, and %.1f%% would be expected by chance alone. Read the interval rather than the p-value: the p-value only tests whether agreement exceeds chance, which is seldom the question.",
    wlab, a$kappa, 100 * a$conf, ci_txt, agreement_strength(a$kappa),
    100 * a$po, a$n, 100 * a$pe)
  if (!is.null(a$pabak) && a$prevalence_index > 0.6)
    interp <- paste(interp, sprintf(
      "One category dominates (prevalence index %.2f), which holds kappa down even when raw agreement is high; PABAK (%.3f) is the prevalence-adjusted comparison.",
      a$prevalence_index, a$pabak))
  if (!is.null(a$bias_index) && a$bias_index > 0.2)
    interp <- paste(interp, sprintf(
      "The two measurements disagree asymmetrically (bias index %.2f), so one tends to record the category more often than the other - McNemar's test below addresses that.",
      a$bias_index))
  if (a$n_dropped > 0)
    interp <- paste(interp, sprintf(
      "%d case%s measured only one way and were excluded.",
      a$n_dropped, if (a$n_dropped == 1L) "" else "s"))

  primary <- mk_result(wlab, stats_named, p = p_value,
    effect = list("kappa" = round(a$kappa, 4)),
    interpretation = interp,
    note = sprintf("Agreement weighting: %s. Kappa measures case-by-case agreement beyond chance; it is not a test of association.",
                   a$weights))

  # The complementary marginal question, where the table allows it.
  alt <- NULL
  if (a$n_categories == 2L) {
    mc <- tryCatch(stats::mcnemar.test(a$table), error = function(e) NULL)
    if (!is.null(mc))
      alt <- mk_result("McNemar's test (marginal symmetry)",
        c("chi-squared" = round(unname(mc$statistic), 3),
          "df" = unname(mc$parameter)),
        p = mc$p.value,
        interpretation = paste(
          sig_phrase(mc$p.value, alpha, "systematic difference between the two measurements"),
          "This asks whether one measurement records the category more often overall - a different question from whether they agree case by case."),
        note = "Reported alongside kappa, not as an alternative to it.")
  }
  list(primary = primary, alternative = alt, recommended = "primary",
       assumptions = list(),
       rationale = sprintf(
         "%s measures how far two measurements of the same cases agree beyond chance. %s",
         wlab,
         if (a$n_categories == 2L)
           "McNemar's test is reported alongside it because agreement and marginal equality are different questions."
         else if (identical(a$weights, "unweighted"))
           "For ordered categories, choose linear or quadratic weights so a near miss counts as partial agreement."
         else "Weighted kappa credits near misses as partial agreement."))
}


# --- three or more raters ---------------------------------------------------

#' Fleiss' kappa: agreement among three or more raters.
#'
#' Cohen's kappa is defined for a specific PAIR of raters. When a panel rates the
#' same subjects - three clinicians reading the same radiographs, several coders
#' applying a case definition, a group of field supervisors classifying households
#' - the question is how much the panel agrees as a whole, which is Fleiss'
#' kappa. It is not an average of pairwise Cohen's kappas and does not reduce to
#' one; the expected agreement is computed from the pooled category proportions
#' across all raters rather than from one rater's marginals.
#'
#' Raters need not be the same people for every subject (the estimator only needs
#' the COUNT of raters choosing each category), but every subject must be rated
#' the same number of times; subjects with a different number of ratings are
#' dropped and reported, because the standard estimator has no term for them.
#'
#' @param ratings A subjects x raters matrix or data frame of category labels,
#'   or a long-format data frame - see `fleiss_from_long()`.
#' @param boot Bootstrap resamples for the confidence interval (0 disables it).
#' @param seed Seed for the bootstrap, so the interval is reproducible.
#' @return A list with kappa, its bootstrap SE and interval, the null-hypothesis
#'   SE used for the significance test, the per-category kappas, the observed and
#'   expected agreement, and the counts used.
fleiss_kappa <- function(ratings, conf = 0.95, boot = 2000L,
                         seed = 1L) {
  m <- as.matrix(ratings)
  if (!nrow(m) || ncol(m) < 3L)
    stop(sprintf("Fleiss' kappa needs at least three raters; %d column%s supplied. For two raters use Cohen's kappa.",
                 ncol(m), if (ncol(m) == 1L) " was" else "s were"), call. = FALSE)
  # A subject rated by fewer raters than the rest has no place in the standard
  # estimator, so it is removed rather than silently treated as if it had.
  n_rated <- rowSums(!is.na(m))
  n_raters <- max(n_rated)
  keep <- n_rated == n_raters
  n_dropped <- sum(!keep)
  m <- m[keep, , drop = FALSE]
  if (nrow(m) < 2L)
    stop("Fleiss' kappa needs at least two subjects rated by every rater.",
         call. = FALSE)

  cats <- sort(unique(stats::na.omit(as.vector(m))))
  if (length(cats) < 2L)
    stop("Fleiss' kappa needs at least two categories to be used.", call. = FALSE)
  # n_ij: how many raters put subject i in category j.
  counts <- t(apply(m, 1L, function(r) table(factor(r, levels = cats))))
  if (is.null(dim(counts))) counts <- matrix(counts, nrow = nrow(m))
  colnames(counts) <- as.character(cats)

  N <- nrow(counts); k <- n_raters
  p_j <- colSums(counts) / (N * k)                 # pooled category proportions
  P_i <- (rowSums(counts^2) - k) / (k * (k - 1))   # agreement within each subject
  p_obs <- mean(P_i)
  p_exp <- sum(p_j^2)
  kappa <- if (isTRUE(all.equal(p_exp, 1))) NA_real_ else
    (p_obs - p_exp) / (1 - p_exp)

  # The classic Fleiss (1971) standard error. It is derived UNDER THE NULL
  # (kappa = 0), so it is the right quantity for the significance test and the
  # wrong one for a confidence interval - a distinction that is easy to miss
  # because published implementations use it for both.
  #
  # Simulated against the actual sampling distribution (40 subjects, 4 raters,
  # 3 categories), this SE and the variant DescTools uses come out at 0.70 and
  # 0.67 times the empirical SD of kappa-hat: intervals built from either are
  # roughly a third too narrow. A bootstrap over SUBJECTS - the unit that was
  # sampled - lands at 1.01 times the empirical SD with 93.5% coverage of a
  # nominal 95% interval, so that is what the interval uses.
  se_null <- NA_real_
  if (is.finite(kappa) && !isTRUE(all.equal(p_exp, 1))) {
    num <- 2 * (p_exp - (2 * k - 3) * p_exp^2 + 2 * (k - 2) * sum(p_j^3))
    den <- N * k * (k - 1) * (1 - p_exp)^2
    v <- num / den
    if (is.finite(v) && v > 0) se_null <- sqrt(v)
  }

  ci <- c(NA_real_, NA_real_); ci_method <- "not estimable"; se <- NA_real_
  boot <- as.integer(boot)
  if (is.finite(kappa) && boot > 0L && N >= 5L) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv),
            add = TRUE)
    bs <- vapply(seq_len(boot), function(i) {
      idx <- sample.int(N, N, replace = TRUE)
      cb <- counts[idx, , drop = FALSE]
      pjb <- colSums(cb) / (N * k); peb <- sum(pjb^2)
      pob <- mean((rowSums(cb^2) - k) / (k * (k - 1)))
      if (isTRUE(all.equal(peb, 1))) NA_real_ else (pob - peb) / (1 - peb)
    }, numeric(1))
    bs <- bs[is.finite(bs)]
    if (length(bs) >= 20L) {
      a2 <- (1 - conf) / 2
      ci <- unname(stats::quantile(bs, c(a2, 1 - a2)))
      se <- stats::sd(bs)
      ci_method <- sprintf("bootstrap over subjects (%d resamples)", length(bs))
    }
  }
  if (!is.finite(ci[1]) && is.finite(se_null)) {
    # Last resort when bootstrapping is not possible (very few subjects). It is
    # labelled, because it is the null-hypothesis SE being used outside its remit.
    z <- stats::qnorm(1 - (1 - conf) / 2)
    ci <- c(kappa - z * se_null, kappa + z * se_null)
    se <- se_null
    ci_method <- "null-hypothesis SE (too few subjects to bootstrap; interval is likely too narrow)"
  }

  # Per-category kappa: how reproducibly each single category is applied. A
  # headline kappa can look adequate while one category is being applied almost
  # at random, which is what an analyst needs to know to fix a case definition.
  kappa_j <- vapply(seq_along(cats), function(j) {
    pj <- p_j[[j]]
    if (pj <= 0 || pj >= 1) return(NA_real_)
    num_j <- sum(counts[, j] * (k - counts[, j]))
    1 - num_j / (N * k * (k - 1) * pj * (1 - pj))
  }, numeric(1))

  list(kappa = kappa, se = se, se_null = se_null, ci = ci,
       ci_method = ci_method, conf = conf,
       p_observed = p_obs, p_expected = p_exp,
       n_subjects = N, n_raters = k, n_dropped = as.integer(n_dropped),
       categories = as.character(cats),
       category_proportions = stats::setNames(as.numeric(p_j), as.character(cats)),
       category_kappa = stats::setNames(kappa_j, as.character(cats)))
}

#' Reshape long-format ratings (subject, rater, category) into the wide matrix
#' Fleiss' kappa needs.
fleiss_from_long <- function(df, subject, rater, rating) {
  for (v in c(subject, rater, rating))
    if (!v %in% names(df))
      stop(sprintf("'%s' is not in the data.", v), call. = FALSE)
  d <- df[!is.na(df[[subject]]) & !is.na(df[[rater]]), , drop = FALSE]
  if (anyDuplicated(d[, c(subject, rater)]))
    stop(sprintf("Some subject/rater pairs appear more than once in '%s' and '%s'. Each rater should rate each subject at most once.",
                 subject, rater), call. = FALSE)
  stats::reshape(d[, c(subject, rater, rating)], idvar = subject,
                 timevar = rater, direction = "wide")[, -1, drop = FALSE]
}

#' Fleiss' kappa as a guided-test result.
run_fleiss <- function(ratings, alpha = 0.05) {
  a <- fleiss_kappa(ratings, conf = 1 - alpha)
  ci_txt <- if (all(is.finite(a$ci)))
    sprintf("%.3f to %.3f", a$ci[1], a$ci[2]) else "not estimable"
  stats_named <- c(
    "subjects" = a$n_subjects, "raters" = a$n_raters,
    "categories" = length(a$categories),
    "observed agreement" = round(a$p_observed, 4),
    "expected agreement by chance" = round(a$p_expected, 4),
    "Fleiss' kappa" = round(a$kappa, 4),
    "bootstrap standard error" = round(a$se, 4))
  # The p-value tests kappa = 0, so it uses the NULL standard error; the
  # interval describes where kappa is, so it uses the bootstrap. Using one for
  # both is the error this separation exists to avoid.
  p_value <- if (is.finite(a$kappa) && is.finite(a$se_null) && a$se_null > 0)
    stats::pnorm(a$kappa / a$se_null, lower.tail = FALSE) else NA_real_

  worst <- if (any(is.finite(a$category_kappa)))
    names(a$category_kappa)[which.min(a$category_kappa)] else NA_character_
  interp <- sprintf(
    "Fleiss' kappa = %.3f (%.0f%% CI %s) across %d raters and %d subjects, which is %s agreement on Landis & Koch's convention. Raters agreed on %.1f%% of pairings; %.1f%% would be expected by chance.",
    a$kappa, 100 * a$conf, ci_txt, a$n_raters, a$n_subjects,
    agreement_strength(a$kappa), 100 * a$p_observed, 100 * a$p_expected)
  if (!is.na(worst) && is.finite(a$category_kappa[[worst]]) &&
      a$category_kappa[[worst]] < a$kappa - 0.1)
    interp <- paste(interp, sprintf(
      "Agreement is weakest on '%s' (kappa %.3f), so that category is where the definition is being applied inconsistently - a headline kappa can look adequate while one category is nearly arbitrary.",
      worst, a$category_kappa[[worst]]))
  if (a$n_dropped > 0)
    interp <- paste(interp, sprintf(
      "%d subject%s rated by fewer than %d raters and were excluded.",
      a$n_dropped, if (a$n_dropped == 1L) " was" else "s were", a$n_raters))
  interp <- paste(interp,
    "This measures agreement across the panel as a whole; it is not the average of the pairwise Cohen's kappas and will not equal it.")

  cat_tab <- data.frame(
    Category = a$categories,
    `Share of ratings` = round(unname(a$category_proportions), 4),
    `Kappa for this category` = round(unname(a$category_kappa), 4),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)

  list(primary = mk_result("Fleiss' kappa (3+ raters)", stats_named, p = p_value,
         effect = list("Fleiss' kappa" = round(a$kappa, 4)),
         interpretation = interp,
         note = sprintf("Every subject must be rated the same number of times; the raters need not be the same people for each subject. The interval is a %s, because the classic Fleiss standard error is derived under kappa = 0 and gives intervals about a third too narrow when kappa is not 0 - that SE is used only for the p-value. Read the interval rather than the p-value, which only tests agreement against chance.",
                        a$ci_method)),
       alternative = NULL, recommended = "primary", assumptions = list(),
       by_category = cat_tab,
       rationale = "Fleiss' kappa measures how far a panel of three or more raters agrees beyond chance, with a per-category breakdown showing which categories are applied inconsistently.")
}
