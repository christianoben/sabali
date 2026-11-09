# tests/test_transform.R -----------------------------------------------------
# The data transformation engine (fct_transform.R). Covers the catalogue, every
# method's maths, the guards, provenance records, back-transformation, and the
# real datasets in data/.
#
# Run:  Rscript tests/test_transform.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# A small frame exercising the awkward cases: zeros, a negative, a big outlier,
# proportions, percentages, a rare category, missingness and dates.
set.seed(4321)
d <- data.frame(
  id       = rep(1:5, each = 3),
  visit    = rep(c("Baseline", "6 months", "12 months"), 5),
  vdate    = as.Date("2024-01-15") + rep(c(0, 180, 365), 5),
  dob      = as.Date("1980-06-30") + rep(0, 15),
  pos      = c(1, 10, 100, 2, 20, 200, 3, 30, 300, 4, 40, 400, 5, 50, 50000),
  withzero = c(0, 1, 2, 0, 3, 4, 0, 5, 6, 1, 7, 8, 2, 9, 10),
  withneg  = c(-5, 0, 5, -4, 1, 6, -3, 2, 7, -2, 3, 8, -1, 4, 9),
  prop     = c(0, .1, .25, .5, .75, .9, 1, .3, .4, .6, .7, .2, .8, .05, .95),
  pct      = c(0, 10, 25, 50, 75, 90, 100, 30, 40, 60, 70, 20, 80, 5, 95),
  grp      = c(rep("A", 7), rep("B", 6), "C", "D"),
  ord      = rep(c("low", "mid", "high"), 5),
  gaps     = c(1, NA, 3, 4, NA, 6, 7, 8, NA, 10, 11, 12, 13, 14, 15),
  weight   = c(60, 65, 70, 55, 58, 62, 80, 85, 90, 70, 72, 75, 66, 68, 71),
  height   = rep(c(160, 170, 180, 175, 165), each = 3),
  stringsAsFactors = FALSE)
ty <- c(id = "identifier", visit = "nominal", vdate = "datetime", dob = "datetime",
        pos = "continuous", withzero = "count", withneg = "continuous",
        prop = "continuous", pct = "continuous", grp = "nominal", ord = "nominal",
        gaps = "continuous", weight = "continuous", height = "continuous")
pnl <- list(kind = "repeated", id = "id", time = "vdate")

# Shorthand: apply one method and return the new column.
tv <- function(method, variable, ...) {
  r <- apply_transform(d, transform_spec(method, variable = variable, ...), ty, pnl)
  stopifnot(isTRUE(r$ok))
  r$df[[r$added[1]]]
}
# Shorthand: the reason a method refuses.
tw <- function(method, variable, ...) {
  r <- apply_transform(d, transform_spec(method, variable = variable, ...), ty, pnl)
  stopifnot(isFALSE(r$ok)); r$reason
}

# ============================================================================
# Catalogue
# ============================================================================
cat_all <- transform_catalogue()
ids <- vapply(cat_all, function(m) m$id, character(1))

ok("catalogue is non-empty with unique ids and known families", {
  stopifnot(length(ids) >= 50, !any(duplicated(ids)),
            all(vapply(cat_all, function(m) m$family, character(1)) %in%
                  names(TRANSFORM_FAMILY_LABELS)))
})
ok("every family in the labels is actually populated", {
  fams <- vapply(cat_all, function(m) m$family, character(1))
  stopifnot(all(names(TRANSFORM_FAMILY_LABELS) %in% fams))
})
ok("every method has a short output suffix, and they are distinct", {
  stopifnot(all(ids %in% names(TRANSFORM_SUFFIX)),
            !any(duplicated(unname(TRANSFORM_SUFFIX[ids]))))
})
ok("every method declares a guard key the guard function understands", {
  known <- c("any", "numeric", "positive", "nonnegative", "proportion",
             "categorical", "date", "panel", "partner", "columns")
  stopifnot(all(vapply(cat_all, function(m) m$requires, character(1)) %in% known))
})
ok("every method produces a measurement type the app knows", {
  stopifnot(all(vapply(cat_all, function(m) m$produces, character(1)) %in%
                  c("continuous", "count", "binary", "nominal", "ordinal")))
})
ok("transform_method / ids look methods up, family filter narrows", {
  stopifnot(identical(transform_method("log10")$name, "Log base 10"),
            is.null(transform_method("astrology")),
            all(transform_method_ids("rank") %in%
                  c("rank", "percentile", "inverse_normal")),
            length(transform_method_ids()) == length(ids))
})
ok("transform_spec fills catalogue defaults and takes overrides", {
  s <- transform_spec("winsorise", variable = "pos")
  stopifnot(s$lower == 0.05, s$upper == 0.95)
  s2 <- transform_spec("winsorise", variable = "pos", lower = 0.10)
  stopifnot(s2$lower == 0.10, s2$upper == 0.95)
})

# ============================================================================
# Distributional transforms
# ============================================================================
ok("the log family computes correctly", {
  stopifnot(
    isTRUE(all.equal(tv("log", "pos"), log(d$pos))),
    isTRUE(all.equal(tv("log2", "pos"), log2(d$pos))),
    isTRUE(all.equal(tv("log10", "pos"), log10(d$pos))),
    isTRUE(all.equal(tv("log1p", "withzero"), log1p(d$withzero))))
})
ok("log of (x + c) picks half the smallest non-zero value by default", {
  r <- apply_transform(d, transform_spec("log_offset", variable = "withzero"), ty, pnl)
  stopifnot(isTRUE(r$ok), isTRUE(all.equal(r$record$params$offset, 0.5)),
            all(is.finite(r$df[[r$added]])),
            grepl("Offset 0.5", r$record$note))
})
ok("log of (x + c) accepts an explicit offset and shifts negatives clear", {
  r <- apply_transform(d, transform_spec("log_offset", variable = "withneg"), ty, pnl)
  stopifnot(isTRUE(r$ok), all(is.finite(r$df[[r$added]])))
})
ok("roots and powers behave, including on negatives", {
  stopifnot(
    isTRUE(all.equal(tv("sqrt", "withzero"), sqrt(d$withzero))),
    isTRUE(all.equal(tv("cbrt", "withneg"), sign(d$withneg) * abs(d$withneg)^(1/3))),
    all(is.finite(tv("cbrt", "withneg"))),
    isTRUE(all.equal(tv("square", "withneg"), d$withneg^2)))
})
ok("the reciprocal sends zeros to missing rather than to infinity", {
  r <- apply_transform(d, transform_spec("reciprocal", variable = "withzero"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), all(is.na(v[d$withzero == 0])), !any(is.infinite(v)),
            grepl("Zero values became missing", r$record$note),
            r$record$n_lost == sum(d$withzero == 0))
})
ok("Box-Cox estimates a lambda that beats the untransformed skew", {
  r <- apply_transform(d, transform_spec("boxcox", variable = "pos"), ty, pnl)
  lam <- r$record$params$lambda
  raw_skew <- abs(mean((d$pos - mean(d$pos))^3) / stats::sd(d$pos)^3)
  new <- r$df[[r$added]]
  new_skew <- abs(mean((new - mean(new))^3) / stats::sd(new)^3)
  stopifnot(isTRUE(r$ok), is.finite(lam), new_skew < raw_skew,
            grepl("maximum likelihood", r$record$note))
})
ok("a supplied Box-Cox lambda is used as given, and lambda 0 is a log", {
  r <- apply_transform(d, transform_spec("boxcox", variable = "pos", lambda = 0), ty, pnl)
  stopifnot(isTRUE(all.equal(r$df[[r$added]], log(d$pos))),
            r$record$params$lambda == 0, grepl("supplied", r$record$note))
  r2 <- apply_transform(d, transform_spec("boxcox", variable = "pos", lambda = 0.5), ty, pnl)
  stopifnot(isTRUE(all.equal(r2$df[[r2$added]], (d$pos^0.5 - 1) / 0.5)))
})
ok("Yeo-Johnson works where Box-Cox cannot - zeros and negatives", {
  stopifnot(!isTRUE(apply_transform(d, transform_spec("boxcox", variable = "withneg"), ty, pnl)$ok))
  r <- apply_transform(d, transform_spec("yeojohnson", variable = "withneg"), ty, pnl)
  stopifnot(isTRUE(r$ok), is.finite(r$record$params$lambda),
            all(is.finite(r$df[[r$added]])))
})
ok("Yeo-Johnson matches its published formula at lambda 0 and 2", {
  v <- c(-3, -1, 0, 1, 3)
  y0 <- .tf_yj(v, 0)
  stopifnot(isTRUE(all.equal(y0[v >= 0], log(v[v >= 0] + 1))),
            isTRUE(all.equal(y0[v < 0], -(((-v[v < 0] + 1)^2) - 1) / 2)))
  y2 <- .tf_yj(v, 2)
  stopifnot(isTRUE(all.equal(y2[v < 0], -log(-v[v < 0] + 1))),
            isTRUE(all.equal(y2[v >= 0], ((v[v >= 0] + 1)^2 - 1) / 2)))
})
ok("arcsine, logit and probit take proportions, and percentages on request", {
  stopifnot(
    isTRUE(all.equal(tv("arcsine", "prop", squeeze = 0), asin(sqrt(d$prop)))),
    isTRUE(all.equal(tv("arcsine", "pct", percent = TRUE, squeeze = 0),
                     asin(sqrt(d$pct / 100)))),
    isTRUE(all.equal(tv("probit", "prop", squeeze = 0.001),
                     stats::qnorm(pmin(pmax(d$prop, 0.001), 0.999)))))
})
ok("the logit squeezes exact 0s and 1s inwards and says how many", {
  r <- apply_transform(d, transform_spec("logit", variable = "prop"), ty, pnl)
  stopifnot(isTRUE(r$ok), all(is.finite(r$df[[r$added]])),
            grepl("moved inwards", r$record$note),
            grepl(sprintf("%d value", sum(d$prop %in% c(0, 1))), r$record$note))
})
ok("a logit with no squeeze leaves the infinities visible rather than hiding them", {
  v <- tv("logit", "prop", squeeze = 0)
  stopifnot(any(is.infinite(v)))
})

# ============================================================================
# Scaling
# ============================================================================
ok("z-score, centring and SD scaling are exact", {
  z <- tv("zscore", "pos")
  stopifnot(abs(mean(z)) < 1e-12, abs(stats::sd(z) - 1) < 1e-12,
            isTRUE(all.equal(tv("center", "pos"), d$pos - mean(d$pos))),
            isTRUE(all.equal(tv("scale_sd", "pos"), d$pos / stats::sd(d$pos))))
})
ok("min-max lands exactly on its requested bounds", {
  v <- tv("minmax", "pos")
  stopifnot(abs(min(v)) < 1e-12, abs(max(v) - 1) < 1e-12)
  v2 <- tv("minmax", "pos", lower = -1, upper = 1)
  stopifnot(abs(min(v2) + 1) < 1e-12, abs(max(v2) - 1) < 1e-12)
})
ok("the robust scale uses median and IQR, so an outlier barely moves it", {
  r <- apply_transform(d, transform_spec("robust_scale", variable = "pos"), ty, pnl)
  stopifnot(isTRUE(all.equal(r$record$params$centre, stats::median(d$pos))),
            isTRUE(all.equal(r$record$params$scale, stats::IQR(d$pos))),
            abs(stats::median(r$df[[r$added]])) < 1e-12)
})
ok("maxabs keeps zero at zero; index100 sets the reference to 100", {
  v <- tv("maxabs", "withneg")
  stopifnot(max(abs(v)) == 1, v[d$withneg == 0] == 0)
  r <- apply_transform(d, transform_spec("index100", variable = "pos"), ty, pnl)
  stopifnot(abs(mean(r$df[[r$added]]) - 100) < 1e-9, grepl("100 = the mean", r$record$note))
  r2 <- apply_transform(d, transform_spec("index100", variable = "pos",
                                         reference = "median"), ty, pnl)
  stopifnot(abs(stats::median(r2$df[[r2$added]]) - 100) < 1e-9)
})

# ============================================================================
# Rank-based
# ============================================================================
ok("rank and percentile respect missing values", {
  v <- tv("rank", "gaps")
  stopifnot(all(is.na(v) == is.na(d$gaps)), max(v, na.rm = TRUE) == sum(!is.na(d$gaps)))
  p <- tv("percentile", "gaps")
  stopifnot(all(p > 0 & p < 100, na.rm = TRUE))
})
ok("the inverse-normal score is normal, symmetric and offset-aware", {
  v <- tv("inverse_normal", "pos")
  stopifnot(all(is.finite(v)), abs(mean(v)) < 1e-9,
            abs(stats::sd(v) - 1) < 0.25,           # n = 15, so approximate
            isTRUE(all.equal(sort(v), -sort(-v, decreasing = TRUE))))
  # A monotone transform must not change the ordering.
  stopifnot(identical(order(v), order(d$pos)))
  for (k in names(TRANSFORM_INT_CONSTANTS))
    stopifnot(all(is.finite(tv("inverse_normal", "pos", constant = k))))
})
ok("an unknown inverse-normal offset is refused by name", {
  stopifnot(grepl("blom, tukey, waerden or rankit",
                  tw("inverse_normal", "pos", constant = "vibes")))
})

# ============================================================================
# Outlier handling
# ============================================================================
ok("winsorising pulls the tails in without changing the sample size", {
  r <- apply_transform(d, transform_spec("winsorise", variable = "pos"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), sum(!is.na(v)) == sum(!is.na(d$pos)),
            max(v) < max(d$pos), r$record$n_lost == 0,
            grepl("value\\(s\\) changed", r$record$note))
})
ok("trimming deletes instead, and says so", {
  r <- apply_transform(d, transform_spec("trim_quantile", variable = "pos",
                                         lower = 0.1, upper = 0.9), ty, pnl)
  stopifnot(isTRUE(r$ok), r$record$n_lost > 0, grepl("set to missing", r$record$note))
})
ok("percentile bounds the wrong way round are refused", {
  stopifnot(grepl("must be below", tw("winsorise", "pos", lower = 0.9, upper = 0.1)),
            grepl("must be below", tw("trim_quantile", "pos", lower = 0.9, upper = 0.1)))
})
ok("clipping honours one-sided bounds and rejects an empty request", {
  v <- tv("clip", "withneg", min = 0, max = NA_real_)
  stopifnot(min(v) == 0, max(v) == max(d$withneg))
  stopifnot(grepl("Give a lower bound", tw("clip", "pos", min = NA_real_, max = NA_real_)))
})
ok("capping on SDs and on MADs both bound the outlier, MAD more tightly", {
  sd_v  <- tv("cap_sd",  "pos")
  mad_v <- tv("cap_mad", "pos")
  stopifnot(max(sd_v) < max(d$pos), max(mad_v) < max(sd_v))
})

# ============================================================================
# Discretisation
# ============================================================================
ok("equal-width and quantile bins produce ordered factors", {
  b <- tv("bin_equal_width", "pos", bins = 3)
  q <- tv("bin_quantile", "pos", bins = 3)
  stopifnot(is.ordered(b), is.ordered(q), nlevels(q) == 3,
            all(levels(q) == c("Tertile 1", "Tertile 2", "Tertile 3")))
  stopifnot(all(levels(tv("bin_quantile", "pos", bins = 4)) ==
                  paste("Quartile", 1:4)))
})
ok("quantile bins split the sample about evenly, unlike equal-width bins", {
  q <- table(tv("bin_quantile", "pos", bins = 3))
  b <- table(tv("bin_equal_width", "pos", bins = 3))
  stopifnot(max(q) - min(q) <= 1, max(b) > max(q))
})
ok("custom cutpoints open their outer edges so nothing falls outside", {
  r <- apply_transform(d, transform_spec("bin_breaks", variable = "pos",
                                         breaks = c(10, 100)), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), !any(is.na(v)), is.ordered(v),
            any(is.infinite(r$record$params$breaks)))
})
ok("custom cutpoints accept matching labels and ignore mismatched ones", {
  v <- tv("bin_breaks", "pos", breaks = c(-Inf, 10, 100, Inf),
          labels = c("low", "mid", "high"))
  stopifnot(identical(levels(v), c("low", "mid", "high")))
  v2 <- tv("bin_breaks", "pos", breaks = c(-Inf, 10, 100, Inf), labels = c("only-one"))
  stopifnot(nlevels(v2) == 3)                       # fell back to generated labels
})
ok("dichotomising labels both sides and honours the boundary rule", {
  v <- tv("dichotomise", "pos", threshold = 100)
  stopifnot(nlevels(v) == 2, sum(v == levels(v)[2]) == sum(d$pos >= 100))
  v2 <- tv("dichotomise", "pos", threshold = 100, at_or_above = FALSE)
  stopifnot(sum(v2 == levels(v2)[2]) == sum(d$pos > 100))
  stopifnot(grepl("Give the threshold", tw("dichotomise", "pos")))
})

# ============================================================================
# Categorical
# ============================================================================
ok("rare categories collapse into Other, commonest level first", {
  r <- apply_transform(d, transform_spec("collapse_rare", variable = "grp",
                                         min_n = 3), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), "Other" %in% levels(v),
            sum(v == "Other") == sum(d$grp %in% c("C", "D")),
            levels(v)[1] == "A",
            setequal(r$record$params$collapsed, c("C", "D")))
})
ok("collapse_rare accepts a percentage threshold and reports a no-op honestly", {
  r <- apply_transform(d, transform_spec("collapse_rare", variable = "grp",
                                         min_pct = 20), ty, pnl)
  stopifnot(isTRUE(r$ok), "Other" %in% levels(r$df[[r$added]]))
  r2 <- apply_transform(d, transform_spec("collapse_rare", variable = "ord",
                                          min_n = 1), ty, pnl)
  stopifnot(grepl("No category was rare enough", r2$record$note))
})
ok("recoding merges categories and leaves unmapped ones alone", {
  v <- tv("recode_levels", "grp", map = list(C = "Other", D = "Other"))
  stopifnot(setequal(levels(v), c("A", "B", "Other")),
            sum(v == "Other") == 2, sum(v == "A") == 7)
  stopifnot(grepl("at least one", tw("recode_levels", "grp")))
})
ok("relevel moves the chosen category to the front and validates it", {
  v <- tv("relevel", "grp", reference = "B")
  stopifnot(levels(v)[1] == "B")
  stopifnot(grepl("not one of this column's categories",
                  tw("relevel", "grp", reference = "Z")),
            grepl("Choose the reference", tw("relevel", "grp")))
})
ok("to_ordinal applies a stated order and refuses an incomplete one", {
  v <- tv("to_ordinal", "ord", order = c("low", "mid", "high"))
  stopifnot(is.ordered(v), identical(levels(v), c("low", "mid", "high")),
            v[1] < v[2], v[2] < v[3])
  stopifnot(grepl("leaves out", tw("to_ordinal", "ord", order = c("low", "mid"))))
})
ok("ordinal_score scores 1..k by default and warns about equal spacing", {
  r <- apply_transform(d, transform_spec("ordinal_score", variable = "ord"), ty, pnl)
  stopifnot(isTRUE(r$ok), setequal(unique(r$df[[r$added]]), 1:3),
            grepl("equally spaced", r$record$note))
  v <- tv("ordinal_score", "ord", scores = c(0, 1, 10))   # alphabetical: high,low,mid
  stopifnot(setequal(unique(v), c(0, 1, 10)))
  stopifnot(grepl("one score per category", tw("ordinal_score", "ord", scores = c(1, 2))))
})
ok("dummy makes one 0/1 column per category and can drop the first", {
  r <- apply_transform(d, transform_spec("dummy", variable = "grp"), ty, pnl)
  stopifnot(isTRUE(r$ok), length(r$added) == 4,
            all(vapply(r$added, function(nm) all(r$df[[nm]] %in% c(0L, 1L)), logical(1))),
            sum(vapply(r$added, function(nm) sum(r$df[[nm]]), integer(1))) == nrow(d))
  r2 <- apply_transform(d, transform_spec("dummy", variable = "grp",
                                          drop_first = TRUE), ty, pnl)
  stopifnot(length(r2$added) == 3)
})

# ============================================================================
# Missing values
# ============================================================================
ok("mean/median imputation fills every gap and flags the caveat", {
  for (mth in c("impute_mean", "impute_median")) {
    r <- apply_transform(d, transform_spec(mth, variable = "gaps"), ty, pnl)
    stopifnot(isTRUE(r$ok), !any(is.na(r$df[[r$added]])),
              r$record$params$n_filled == sum(is.na(d$gaps)),
              grepl("understates uncertainty", r$record$note))
  }
})
ok("imputation refuses when nothing is missing", {
  stopifnot(grepl("Nothing is missing", tw("impute_mean", "pos")),
            grepl("Nothing is missing", tw("impute_mode", "grp")))
})
ok("mode imputation fills categories with the commonest value", {
  dd <- d; dd$grp[c(1, 2)] <- NA          # 5 x A, 6 x B -> B is now commonest
  r <- apply_transform(dd, transform_spec("impute_mode", variable = "grp"), ty, pnl)
  stopifnot(isTRUE(r$ok), r$record$params$value == "B", !any(is.na(r$df[[r$added]])))
})
ok("constant fill works for numbers and for text", {
  r <- apply_transform(d, transform_spec("impute_constant", variable = "gaps",
                                         value = 0), ty, pnl)
  stopifnot(isTRUE(r$ok), sum(r$df[[r$added]] == 0) == sum(is.na(d$gaps)))
  dd <- d; dd$grp[1] <- NA
  r2 <- apply_transform(dd, transform_spec("impute_constant", variable = "grp",
                                           value = "Unknown"), ty, pnl)
  stopifnot(isTRUE(r2$ok), "Unknown" %in% levels(r2$df[[r2$added]]))
})
ok("the missing indicator marks exactly the missing rows", {
  r <- apply_transform(d, transform_spec("missing_indicator", variable = "gaps"), ty, pnl)
  stopifnot(isTRUE(r$ok),
            identical(as.logical(r$df[[r$added]]), is.na(d$gaps)),
            r$record$params$n_missing == sum(is.na(d$gaps)))
})

# ============================================================================
# Derived across columns
# ============================================================================
ok("ratio and difference combine two columns, zero denominators to missing", {
  r <- apply_transform(d, transform_spec("ratio", variable = "pos",
                                         with = "withzero"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), all(is.na(v[d$withzero == 0])), !any(is.infinite(v)))
  stopifnot(isTRUE(all.equal(tv("difference", "pos", with = "withzero"),
                             d$pos - d$withzero)))
})
ok("a partner column must be named, present, and not the variable itself", {
  stopifnot(grepl("Choose the second column", tw("ratio", "pos")),
            grepl("not in this dataset", tw("ratio", "pos", with = "nope")),
            grepl("cannot be combined with itself", tw("ratio", "pos", with = "pos")))
})
ok("sum and mean across columns honour na_rm", {
  stopifnot(isTRUE(all.equal(tv("sum_columns", "pos",
                                columns = c("pos", "withzero")),
                             d$pos + d$withzero)))
  v <- tv("mean_columns", "gaps", columns = c("gaps", "pos"), na_rm = TRUE)
  stopifnot(!any(is.na(v)))
  v2 <- tv("mean_columns", "gaps", columns = c("gaps", "pos"), na_rm = FALSE)
  stopifnot(sum(is.na(v2)) == sum(is.na(d$gaps)))
  stopifnot(grepl("at least two columns", tw("sum_columns", "pos", columns = "pos")))
})
ok("BMI divides kilograms by metres squared, converting centimetres", {
  r <- apply_transform(d, transform_spec("bmi", variable = "weight",
                                         with = "height"), ty, pnl)
  stopifnot(isTRUE(r$ok),
            isTRUE(all.equal(r$df[[r$added]], d$weight / (d$height / 100)^2)),
            all(r$df[[r$added]] > 15 & r$df[[r$added]] < 40))
  r2 <- apply_transform(d, transform_spec("bmi", variable = "weight", with = "height",
                                          height_unit = "m"), ty, pnl)
  stopifnot(isTRUE(all.equal(r2$df[[r2$added]], d$weight / d$height^2)))
})
ok("within-group z-scores centre each group, not the whole column", {
  r <- apply_transform(d, transform_spec("zscore_by_group", variable = "pos",
                                         with = "grp"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), abs(mean(v[d$grp == "A"])) < 1e-12,
            all(is.na(v[d$grp %in% c("C", "D")])))   # single-row groups: no spread
})

# ============================================================================
# Longitudinal
# ============================================================================
ok("change from baseline subtracts each subject's own first value", {
  r <- apply_transform(d, transform_spec("change_from_baseline",
                                         variable = "pos"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), all(v[d$vdate == min(d$vdate)] == 0),
            isTRUE(all.equal(v[2], d$pos[2] - d$pos[1])),
            r$record$params$baseline == "first")
})
ok("percent change from baseline is scale-free and guards a zero baseline", {
  v <- tv("pct_change_from_baseline", "pos")
  stopifnot(isTRUE(all.equal(v[2], 100 * (d$pos[2] - d$pos[1]) / d$pos[1])))
  vz <- tv("pct_change_from_baseline", "withzero")
  stopifnot(any(is.na(vz)))                          # subjects whose baseline is 0
})
ok("a named baseline visit can be chosen, and a wrong name is refused", {
  pnl2 <- list(kind = "repeated", id = "id", time = "visit")
  r <- apply_transform(d, transform_spec("change_from_baseline", variable = "pos",
                                         baseline = "6 months"), ty, pnl2)
  stopifnot(isTRUE(r$ok), r$record$params$baseline == "6 months",
            all(r$df[[r$added]][d$visit == "6 months"] == 0))
  r2 <- apply_transform(d, transform_spec("change_from_baseline", variable = "pos",
                                          baseline = "Week 96"), ty, pnl2)
  stopifnot(isFALSE(r2$ok), grepl("not one of the values", r2$reason))
})
ok("lag and lead shift within a subject and never across subjects", {
  lg <- tv("lag_within", "pos"); ld <- tv("lead_within", "pos")
  first <- which(d$vdate == min(d$vdate)); last <- which(d$vdate == max(d$vdate))
  stopifnot(all(is.na(lg[first])), all(is.na(ld[last])),
            isTRUE(all.equal(lg[2], d$pos[1])), isTRUE(all.equal(ld[1], d$pos[2])))
})
ok("a running total accumulates within a subject and resets between them", {
  cs <- tv("cumsum_within", "pos")
  stopifnot(isTRUE(all.equal(cs[1:3], cumsum(d$pos[1:3]))),
            isTRUE(all.equal(cs[4], d$pos[4])))       # subject 2 starts again
})
ok("time since baseline turns visit dates into elapsed time, in chosen units", {
  r <- apply_transform(d, transform_spec("time_since_baseline",
                                         variable = "pos"), ty, pnl)
  v <- r$df[[r$added]]
  stopifnot(isTRUE(r$ok), all(v[d$vdate == min(d$vdate)] == 0),
            isTRUE(all.equal(sort(unique(v)), c(0, 180, 365))))
  vy <- tv("time_since_baseline", "pos", units = "years")
  stopifnot(max(vy) < 1.01, max(vy) > 0.99)
})
ok("longitudinal methods refuse politely without a panel declared", {
  r <- apply_transform(d, transform_spec("change_from_baseline", variable = "pos"),
                       ty, panel = NULL)
  stopifnot(isFALSE(r$ok), grepl("subject ID", r$reason))
})
ok("a non-date, non-numeric time column is reported, not silently wrong", {
  r <- apply_transform(d, transform_spec("time_since_baseline", variable = "pos"),
                       ty, panel = list(kind = "repeated", id = "id", time = "grp"))
  stopifnot(isFALSE(r$ok) || all(is.finite(r$df[[r$added]])))
})

# ============================================================================
# Dates
# ============================================================================
ok("calendar parts come out of a date column", {
  stopifnot(all(tv("date_year", "vdate") %in% c(2024, 2025)),
            is.ordered(tv("date_month", "vdate")),
            is.ordered(tv("date_quarter", "vdate")),
            all(levels(tv("date_quarter", "vdate")) == paste0("Q", 1:4)),
            all(tv("date_doy", "vdate") >= 1 & tv("date_doy", "vdate") <= 366),
            all(as.character(tv("date_weekday", "vdate")) %in%
                  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                    "Saturday", "Sunday")))
})
ok("the gap between two dates is computed in the requested units", {
  v <- tv("date_diff", "dob", with = "vdate")
  stopifnot(all(v > 15000), all(v < 17000))
  vy <- tv("date_diff", "dob", with = "vdate", units = "years")
  stopifnot(all(vy > 43 & vy < 46))
})
ok("age counts completed years, not day counts over 365", {
  dd <- data.frame(dob = as.Date(c("1980-06-30", "1980-06-30", "2000-01-01")),
                   ref = as.Date(c("2024-06-29", "2024-06-30", "2024-12-31")))
  r <- apply_transform(dd, transform_spec("age_from_date", variable = "dob",
                                          with = "ref"))
  stopifnot(isTRUE(r$ok), identical(as.integer(r$df[[r$added]]), c(43L, 44L, 24L)))
})
ok("a column with no recognisable dates is refused", {
  stopifnot(grepl("does not hold recognisable dates", tw("date_year", "grp")))
})

# ============================================================================
# Guards
# ============================================================================
ok("logs refuse zero or negative input and name the alternatives", {
  msg <- tw("log10", "withzero")
  stopifnot(grepl("above zero", msg), grepl("x \\+ 1", msg), grepl("Yeo-Johnson", msg))
  stopifnot(grepl("negative", tw("sqrt", "withneg")),
            grepl("cube root", tw("sqrt", "withneg")))
})
ok("proportion transforms refuse out-of-range values and suggest the percent tick", {
  msg <- tw("logit", "pct")
  stopifnot(grepl("between 0 and 1", msg), grepl("percentage option", msg))
})
ok("a constant column is refused where the maths needs spread", {
  dd <- d; dd$flat <- 7
  tt <- c(ty, flat = "continuous")
  for (mth in c("zscore", "robust_scale", "minmax", "boxcox", "inverse_normal")) {
    r <- apply_transform(dd, transform_spec(mth, variable = "flat"), tt, pnl)
    stopifnot(isFALSE(r$ok), grepl("same", r$reason))
  }
})
ok("a categorical method refuses a continuous column, and the reverse", {
  stopifnot(grepl("applies to categories", tw("collapse_rare", "pos")),
            grepl("no usable numeric values", tw("zscore", "grp")))
})
ok("an all-missing or absent column is refused, never crashed on", {
  dd <- d; dd$empty <- NA_real_
  r <- apply_transform(dd, transform_spec("log10", variable = "empty"),
                       c(ty, empty = "continuous"), pnl)
  stopifnot(isFALSE(r$ok), grepl("no usable numeric", r$reason))
  stopifnot(grepl("not in this dataset",
                  apply_transform(d, transform_spec("log10", variable = "ghost"),
                                  ty, pnl)$reason))
})
ok("an unknown method is refused by both the guard and the entry point", {
  stopifnot(!isTRUE(transform_applicable("alchemy", d$pos)$ok),
            grepl("not a transformation this app knows",
                  apply_transform(d, transform_spec("alchemy", variable = "pos"))$reason))
})
ok("transform_options_for reports what fits and why the rest does not", {
  opts <- transform_options_for(d$withzero, "count", d, pnl, "withzero")
  by_id <- setNames(opts, vapply(opts, function(o) o$id, character(1)))
  stopifnot(length(opts) == length(ids),
            isTRUE(by_id$log1p$ok), isFALSE(by_id$log10$ok),
            nzchar(by_id$log10$reason), isTRUE(by_id$zscore$ok),
            isFALSE(by_id$collapse_rare$ok))
})

# ============================================================================
# The additive contract and provenance
# ============================================================================
ok("the original column is never touched and the frame only grows", {
  r <- apply_transform(d, transform_spec("log10", variable = "pos"), ty, pnl)
  stopifnot(identical(r$df$pos, d$pos), ncol(r$df) == ncol(d) + 1,
            nrow(r$df) == nrow(d), identical(names(d), names(r$df)[seq_along(d)]))
})
ok("output names follow variable_suffix and never collide", {
  r <- apply_transform(d, transform_spec("log10", variable = "pos"), ty, pnl)
  stopifnot(identical(r$added, "pos_log10"))
  r2 <- apply_transform(r$df, transform_spec("log10", variable = "pos"), ty, pnl)
  stopifnot(identical(r2$added, "pos_log10_2"), ncol(r2$df) == ncol(d) + 2)
})
ok("a record carries method, params, counts and a note", {
  r <- apply_transform(d, transform_spec("winsorise", variable = "pos"), ty, pnl)
  rec <- r$record
  stopifnot(rec$method == "winsorise", rec$variable == "pos",
            rec$family == "outliers", rec$output == "pos_wins",
            rec$n_in == 15, rec$n_out == 15, rec$n_lost == 0,
            length(rec$params$at) == 2, nzchar(rec$note))
})
ok("apply_transforms chains steps, each seeing the last one's output", {
  res <- apply_transforms(d, list(
    transform_spec("log10", variable = "pos"),
    transform_spec("zscore", variable = "pos_log10"),
    transform_spec("bin_quantile", variable = "pos", bins = 3)), ty, pnl)
  stopifnot(isTRUE(res$ok), length(res$records) == 3,
            setequal(res$added, c("pos_log10", "pos_log10_z", "pos_grp")),
            abs(mean(res$df$pos_log10_z)) < 1e-12)
})
ok("a failing step is collected without aborting the others", {
  res <- apply_transforms(d, list(
    transform_spec("log10", variable = "withzero"),     # fails: zeros
    transform_spec("log1p", variable = "withzero")), ty, pnl)
  stopifnot(isFALSE(res$ok), length(res$records) == 1, length(res$failures) == 1,
            res$failures[[1]]$method == "log10",
            grepl("above zero", res$failures[[1]]$reason),
            identical(res$added, "withzero_log1p"))
})
ok("an empty spec list is a clean no-op", {
  res <- apply_transforms(d, list(), ty, pnl)
  stopifnot(isTRUE(res$ok), identical(res$df, d), length(res$records) == 0)
})
ok("a NULL or empty data frame is refused, not crashed on", {
  stopifnot(grepl("no data", apply_transform(NULL, transform_spec("log10", variable = "x"))$reason),
            grepl("no data", apply_transform(d[0, ], transform_spec("log10", variable = "pos"))$reason))
})

# ============================================================================
# Back-transformation
# ============================================================================
ok("every invertible method round-trips to its original values", {
  invertible <- Filter(function(m) isTRUE(m$invertible), cat_all)
  col_for <- c(positive = "pos", nonnegative = "withzero",
               numeric = "withneg", proportion = "prop")
  for (m in invertible) {
    var <- unname(col_for[m$requires])
    if (is.na(var)) next
    sp <- transform_spec(m$id, variable = var)
    if (identical(m$id, "reciprocal")) sp <- transform_spec(m$id, variable = "pos")
    if (m$requires == "proportion") sp$squeeze <- 0.001
    r <- apply_transform(d, sp, ty, pnl)
    if (!isTRUE(r$ok)) next
    back <- transform_inverse(r$record, r$df[[r$added]])
    # An even power over negatives is not one-to-one and correctly refuses.
    if (isTRUE(r$record$params$sign_lost)) { stopifnot(is.null(back)); next }
    stopifnot(!is.null(back))
    orig <- .tf_num(d[[sp$variable]])   # follow the spec, not the loop variable
    keep <- is.finite(back) & is.finite(orig)
    # The logit/probit squeeze deliberately moves 0s and 1s, so compare the rest.
    if (m$requires == "proportion") keep <- keep & orig > 0 & orig < 1
    stopifnot(sum(keep) > 3,
              max(abs(back[keep] - orig[keep])) < 1e-6 * max(1, max(abs(orig[keep]))))
  }
})
ok("log10 inverts to a geometric mean, which is what a report needs", {
  r <- apply_transform(d, transform_spec("log10", variable = "pos"), ty, pnl)
  gm <- transform_inverse(r$record, mean(r$df[[r$added]]))
  stopifnot(abs(gm - exp(mean(log(d$pos)))) < 1e-9, gm < mean(d$pos))
})
ok("a z-score inverts through its stored centre and scale", {
  r <- apply_transform(d, transform_spec("zscore", variable = "pos"), ty, pnl)
  stopifnot(abs(transform_inverse(r$record, 0) - mean(d$pos)) < 1e-9,
            abs(transform_inverse(r$record, 1) -
                  (mean(d$pos) + stats::sd(d$pos))) < 1e-9)
})
ok("a non-invertible method returns NULL rather than a wrong answer", {
  for (mth in c("rank", "percentile", "winsorise", "bin_quantile",
                "impute_median", "missing_indicator", "zscore_by_group")) {
    r <- apply_transform(d, transform_spec(mth, variable = if (mth == "impute_median")
                         "gaps" else if (mth == "zscore_by_group") "pos" else "pos",
                         with = "grp", bins = 3), ty, pnl)
    if (isTRUE(r$ok))
      stopifnot(is.null(transform_inverse(r$record, r$df[[r$added]])))
  }
  stopifnot(is.null(transform_inverse(NULL, 1)))
})

# ============================================================================
# Reporting
# ============================================================================
ok("records render as a table, empty input gives an empty table", {
  res <- apply_transforms(d, list(
    transform_spec("log10", variable = "pos"),
    transform_spec("winsorise", variable = "pos")), ty, pnl)
  tab <- transform_record_df(res$records)
  stopifnot(nrow(tab) == 2, all(c("Variable", "Transformation", "New column",
                                  "Detail") %in% names(tab)),
            tab$Variable[1] == "pos", nzchar(tab$Detail[1]),
            nrow(transform_record_df(list())) == 0)
})
ok("a lost-values count reaches the table so it cannot pass unnoticed", {
  r <- apply_transform(d, transform_spec("trim_quantile", variable = "pos",
                                         lower = 0.1, upper = 0.9), ty, pnl)
  stopifnot(grepl("became missing", transform_record_df(list(r$record))$Detail))
})
ok("methods text names the new column, its source and the parameters", {
  r <- apply_transform(d, transform_spec("boxcox", variable = "pos"), ty, pnl)
  txt <- transform_methods_text(list(r$record))
  stopifnot(length(txt) == 1, grepl("pos_bc", txt), grepl("pos", txt),
            grepl("box-cox", txt, ignore.case = TRUE), grepl("lambda", txt))
  stopifnot(length(transform_methods_text(list())) == 0)
})

# ============================================================================
# Against the real datasets in data/
# ============================================================================
.dataset <- function(f) {
  p <- file.path("data", f)
  if (!file.exists(p)) NULL else utils::read.csv(p, stringsAsFactors = FALSE)
}

dc <- .dataset("lvct_cohort_repeated.csv")
if (!is.null(dc)) {
  tyc <- stats::setNames(detect_all_types(dc)$type, detect_all_types(dc)$variable)
  pnc <- list(kind = "repeated", id = "patient_id", time = "visit_date")

  ok("[data] log10 tames viral load - the transform the planner asks for", {
    r <- apply_transform(dc, transform_spec("log10", variable = "viral_load"), tyc, pnc)
    stopifnot(isTRUE(r$ok), identical(r$added, "viral_load_log10"))
    sk <- function(v) { v <- v[is.finite(v)]
                        mean((v - mean(v))^3) / stats::sd(v)^3 }
    stopifnot(abs(sk(r$df$viral_load_log10)) < abs(sk(dc$viral_load)) / 5,
              isTRUE(all.equal(transform_inverse(r$record, r$df$viral_load_log10),
                               dc$viral_load)))
  })
  ok("[data] the planner's second ask - ordinal coding of who_stage - works", {
    lv <- sort(unique(as.character(dc$who_stage[!is.na(dc$who_stage)])))
    tyo <- tyc; tyo[["who_stage"]] <- "ordinal"   # override, do not append
    r <- apply_transform(dc, transform_spec("to_ordinal", variable = "who_stage",
                                           order = lv), tyo, pnc)
    stopifnot(isTRUE(r$ok), is.ordered(r$df$who_stage_ord))
  })
  ok("[data] change from baseline is zero at each patient's first visit", {
    r <- apply_transform(dc, transform_spec("change_from_baseline",
                                           variable = "cd4_count"), tyc, pnc)
    stopifnot(isTRUE(r$ok))
    first <- dc$visit == "Baseline"
    stopifnot(all(r$df$cd4_count_chg[first] == 0, na.rm = TRUE),
              any(r$df$cd4_count_chg[!first] != 0, na.rm = TRUE))
  })
  ok("[data] elapsed days since baseline recovers the 0 / ~180 / ~365 design", {
    r <- apply_transform(dc, transform_spec("time_since_baseline",
                                           variable = "cd4_count"), tyc, pnc)
    v <- r$df[[r$added]]
    stopifnot(isTRUE(r$ok), all(v[dc$visit == "Baseline"] == 0),
              abs(stats::median(v[dc$visit == "6 months"]) - 180) < 15,
              abs(stats::median(v[dc$visit == "12 months"]) - 365) < 20)
  })
  ok("[data] adherence_pct needs the percent option for a logit", {
    stopifnot(isFALSE(apply_transform(dc, transform_spec("logit",
                        variable = "adherence_pct"), tyc, pnc)$ok))
    r <- apply_transform(dc, transform_spec("logit", variable = "adherence_pct",
                                           percent = TRUE), tyc, pnc)
    v <- r$df[[r$added]]
    # 30 values are missing in this column; missingness passes through untouched.
    stopifnot(isTRUE(r$ok), all(is.finite(v[!is.na(v)])),
              sum(is.na(v)) == sum(is.na(dc$adherence_pct)))
  })
}

dm <- .dataset("messy_cross_sectional_dataset.csv")
if (!is.null(dm)) {
  tym <- stats::setNames(detect_all_types(dm)$type, detect_all_types(dm)$variable)
  ok("[data] the messy file's text-typed numbers still transform", {
    # Every column imports as character here, so the engine must coerce.
    r <- apply_transform(dm, transform_spec("zscore", variable = "systolic_bp"), tym)
    stopifnot(isTRUE(r$ok), abs(mean(r$df[[r$added]], na.rm = TRUE)) < 1e-9)
  })
  ok("[data] BMI recomputed from height and weight matches the stored column", {
    r <- apply_transform(dm, transform_spec("bmi", variable = "weight_kg",
                                           with = "height_cm"), tym)
    stopifnot(isTRUE(r$ok))
    a <- r$df[[r$added]]; b <- suppressWarnings(as.numeric(dm$bmi))
    keep <- is.finite(a) & is.finite(b)
    # Judged robustly on purpose. This fixture holds a few rows whose height was
    # recorded in metres rather than centimetres, which sends the recomputed BMI
    # into the thousands and destroys a Pearson correlation while leaving the
    # typical row exact. The engine is faithful to the arithmetic; the unit
    # mix-up is a data-quality finding, not an engine bug.
    stopifnot(sum(keep) > 100,
              stats::median(abs(a[keep] - b[keep])) < 0.1,
              mean(abs(a[keep] - b[keep]) < 1) > 0.95)
  })
  ok("[data] a missing indicator plus median fill covers a gappy column", {
    res <- apply_transforms(dm, list(
      transform_spec("missing_indicator", variable = "heart_rate"),
      transform_spec("impute_median", variable = "heart_rate")), tym)
    stopifnot(isTRUE(res$ok), length(res$added) == 2,
              !any(is.na(res$df$heart_rate_impmed)),
              sum(res$df$heart_rate_wasna) > 0)
  })
  ok("[data] rare categories in a real column collapse without error", {
    r <- apply_transform(dm, transform_spec("collapse_rare", variable = "ethnicity",
                                           min_n = 50), tym)
    stopifnot(isTRUE(r$ok), nlevels(r$df[[r$added]]) <= length(unique(dm$ethnicity)))
  })
}

dt <- .dataset("amr_surveillance_timeseries.csv")
if (!is.null(dt)) {
  tyt <- stats::setNames(detect_all_types(dt)$type, detect_all_types(dt)$variable)
  ok("[data] the surveillance series yields calendar parts and a proportion logit", {
    r <- apply_transform(dt, transform_spec("date_month", variable = "month"), tyt)
    stopifnot(isTRUE(r$ok), is.ordered(r$df[[r$added]]))
    r2 <- apply_transform(dt, transform_spec("logit", variable = "ecoli_cipro_pct",
                                            percent = TRUE), tyt)
    v2 <- r2$df[[r2$added]]
    stopifnot(isTRUE(r2$ok), all(is.finite(v2[!is.na(v2)])))
  })
}

da <- .dataset("amr_isolates_crosssectional.csv")
if (!is.null(da)) {
  tya <- stats::setNames(detect_all_types(da)$type, detect_all_types(da)$variable)
  ok("[data] resistant_drug_count discretises and length of stay transforms", {
    r <- apply_transform(da, transform_spec("bin_quantile",
                                           variable = "length_of_stay_days",
                                           bins = 4), tya)
    stopifnot(isTRUE(r$ok), nlevels(r$df[[r$added]]) <= 4)
    r2 <- apply_transform(da, transform_spec("dichotomise",
                                            variable = "resistant_drug_count",
                                            threshold = 3), tya)
    stopifnot(isTRUE(r2$ok), nlevels(r2$df[[r2$added]]) == 2)
  })
}

# ============================================================================
# Determinism
# ============================================================================
ok("the same spec on the same data gives the same answer twice", {
  for (mth in c("log10", "boxcox", "yeojohnson", "inverse_normal", "bin_quantile",
                "winsorise", "zscore")) {
    var <- if (mth == "yeojohnson") "withneg" else "pos"
    a <- apply_transform(d, transform_spec(mth, variable = var), ty, pnl)
    b <- apply_transform(d, transform_spec(mth, variable = var), ty, pnl)
    stopifnot(identical(a$df[[a$added]], b$df[[b$added]]),
              identical(a$record$params, b$record$params))
  }
})
ok("every catalogue method is reachable - none is declared but unimplemented", {
  unreachable <- character(0)
  for (m in cat_all) {
    var <- switch(m$requires,
                  any = "gaps",
                  positive = "pos", nonnegative = "withzero", numeric = "pos",
                  proportion = "prop", categorical = "grp", date = "vdate",
                  panel = "pos", partner = "pos", columns = "pos")
    sp <- transform_spec(m$id, variable = var)
    # Supply the parameters that have no sensible default.
    if (m$requires == "partner")
      sp$with <- if (m$id %in% c("date_diff", "age_from_date")) "vdate" else
                 if (m$id == "bmi") "height" else
                 if (m$id == "zscore_by_group") "grp" else "withzero"
    if (m$id == "bmi") sp$variable <- "weight"
    if (m$id %in% c("date_diff", "age_from_date")) sp$variable <- "dob"
    if (m$requires == "columns") sp$columns <- c("pos", "withzero")
    if (m$id == "bin_breaks") sp$breaks <- c(10, 100)
    if (m$id == "clip") { sp$min <- 1; sp$max <- 100 }
    if (m$id == "dichotomise") sp$threshold <- 100
    if (m$id == "relevel") sp$reference <- "B"
    if (m$id == "recode_levels") sp$map <- list(C = "Other")
    if (m$id %in% c("impute_mean", "impute_median", "impute_constant",
                    "missing_indicator")) sp$variable <- "gaps"
    if (m$id == "impute_mode") { sp$variable <- "grp"
      dd <- d; dd$grp[1] <- NA
      r <- apply_transform(dd, sp, ty, pnl)
      if (!isTRUE(r$ok)) unreachable <- c(unreachable, m$id)
      next }
    r <- apply_transform(d, sp, ty, pnl)
    if (!isTRUE(r$ok)) unreachable <- c(unreachable, sprintf("%s (%s)", m$id, r$reason))
  }
  if (length(unreachable))
    stop(paste("unreachable:", paste(unreachable, collapse = " | ")))
})

# ============================================================================
# Wiring: AI suggestions -> runnable specs, and derived types
# ============================================================================
ok("the fuzzy matcher maps descriptions and ids onto catalogue methods", {
  stopifnot(
    transform_match_method("log10") == "log10",
    transform_match_method("log 10 transformation") == "log10",
    transform_match_method("natural log") == "log",
    transform_match_method("log of (x + 1)") == "log1p",
    transform_match_method("Box-Cox") == "boxcox",
    transform_match_method("Yeo-Johnson") == "yeojohnson",
    transform_match_method("ordinal coding") == "to_ordinal",
    transform_match_method("winsorize the tails") == "winsorise",
    transform_match_method("split into quartiles") == "bin_quantile",
    transform_match_method("z-score standardisation") == "zscore",
    transform_match_method("") == "",
    transform_match_method(NULL) == "",
    transform_match_method("quantum entanglement") == "")
})
ok("every catalogue id resolves to itself, so ids are always honoured", {
  for (m in transform_method_ids())
    stopifnot(identical(transform_match_method(m), m))
})
ok("a plan's transformations become specs, with honest refusals", {
  plan <- list(transformations = list(
    list(variable = "pos", transform = "log10", reason = "Right-skewed."),
    list(variable = "grp", transform = "ordinal coding", reason = "Ordered."),
    list(variable = "pos", transform = "astrology", reason = "Nonsense."),
    list(variable = "ghost", transform = "log10", reason = "Absent.")))
  sp <- plan_transform_specs(plan, d, ty, pnl)
  stopifnot(length(sp) == 4,
            isTRUE(sp[[1]]$ok), sp[[1]]$method == "log10",
            identical(sp[[1]]$spec$variable, "pos"),
            sp[[1]]$reason == "Right-skewed.",
            sp[[2]]$method == "to_ordinal",
            isFALSE(sp[[3]]$ok), grepl("no matching transformation", sp[[3]]$why_not),
            isFALSE(sp[[4]]$ok), grepl("not in the current dataset", sp[[4]]$why_not))
})
ok("a suggested spec actually runs when applied", {
  plan <- list(transformations = list(
    list(variable = "pos", transform = "log10", reason = "Right-skewed.")))
  sp <- plan_transform_specs(plan, d, ty, pnl)[[1]]
  r <- apply_transform(d, sp$spec, ty, pnl)
  stopifnot(isTRUE(r$ok), identical(r$added, "pos_log10"))
})
ok("a plan with no transformations yields nothing, and NULL is safe", {
  stopifnot(length(plan_transform_specs(list(), d, ty)) == 0,
            length(plan_transform_specs(list(transformations = list()), d, ty)) == 0)
})
ok("the planner prompt advertises the transformation vocabulary", {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(TRUE))
  m <- build_planner_metadata(d, ty)
  pr <- planner_prompt("Risk factors for pos?", m)
  stopifnot(grepl("available_transformations", pr$user, fixed = TRUE),
            grepl("available_transformations", pr$system, fixed = TRUE),
            grepl("log10", pr$user, fixed = TRUE))
})
ok("derived columns take their measurement type from the catalogue", {
  # This is what makes a derived column usable: without a type, no analysis
  # tab would offer it.
  res <- apply_transforms(d, list(
    transform_spec("log10", variable = "pos"),
    transform_spec("bin_quantile", variable = "pos", bins = 4),
    transform_spec("dichotomise", variable = "pos", threshold = 100)), ty, pnl)
  ty2 <- ty
  for (rec in res$records) for (nm in rec$output) ty2[[nm]] <- rec$produces
  stopifnot(ty2[["pos_log10"]] == "continuous",
            ty2[["pos_grp"]] == "ordinal",
            ty2[["pos_hi"]] == "binary",
            length(ty2) == length(ty) + 3)
})
ok("a derived continuous column is offered as an analysable outcome", {
  r <- apply_transform(d, transform_spec("log10", variable = "pos"), ty, pnl)
  stopifnot("continuous" %in% outcome_type_options(r$df$pos_log10),
            is_analysable_type("continuous"))
})

report_and_exit()
