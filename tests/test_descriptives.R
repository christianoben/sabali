# tests/test_descriptives.R -------------------------------------------------
# Statistical correctness of the descriptive helpers (Gap 3) and missing-data
# accounting (Gap 6). Numeric summaries are checked against base R; skewness,
# excess kurtosis and the Wilson CI are checked against independently
# hand-computed constants so the tests are not circular.
#
# Run from the project root:  Rscript tests/test_descriptives.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

val <- function(d, s) d$value[d$statistic == s]                 # pull one statistic
near <- function(a, b, tol = 1e-3) is.finite(a) && is.finite(b) && abs(a - b) <= tol

cat("\n== describe_numeric() vs base R ==\n")
x  <- c(2, 4, 4, 4, 5, 5, 7, 9)
d  <- describe_numeric(x)
ok("n valid", stopifnot(val(d, "n valid") == length(x)))
ok("mean   vs base::mean",     stopifnot(val(d, "mean")   == round(mean(x), 4)))
ok("SD     vs stats::sd",      stopifnot(val(d, "SD")     == round(sd(x), 4)))
ok("median vs stats::median",  stopifnot(val(d, "median") == round(median(x), 4)))
ok("Q1     vs stats::quantile",stopifnot(val(d, "Q1")     == round(quantile(x, .25, names = FALSE), 4)))
ok("Q3     vs stats::quantile",stopifnot(val(d, "Q3")     == round(quantile(x, .75, names = FALSE), 4)))
ok("IQR    = Q3 - Q1",         stopifnot(near(val(d, "IQR"), val(d, "Q3") - val(d, "Q1"))))
ok("min / max / range",        stopifnot(val(d, "min") == min(x), val(d, "max") == max(x),
                                          val(d, "range") == max(x) - min(x)))

cat("\n== skewness / kurtosis on known vectors ==\n")
# Symmetric vector -> zero skew.
ok("skewness(symmetric) ~ 0",      stopifnot(near(skewness(c(1,2,3,4,5)), 0)))
# c(1,1,1,5): mean 2, sum(dev^3)=24, n=4, sd=2 -> (24/4)/2^3 = 0.75
ok("skewness(c(1,1,1,5)) ~ 0.75",  stopifnot(near(skewness(c(1,1,1,5)), 0.75)))
# c(1..5): sum(dev^4)=34, n=5, sd^4=6.25 -> (34/5)/6.25 - 3 = -1.912
ok("excess kurtosis(1:5) ~ -1.912",stopifnot(near(kurtosis_excess(c(1,2,3,4,5)), -1.912)))
# Guards for tiny samples.
ok("skewness(n<3) is NA",          stopifnot(is.na(skewness(c(1, 2)))))
ok("kurtosis(n<4) is NA",          stopifnot(is.na(kurtosis_excess(c(1, 2, 3)))))

cat("\n== wilson_ci() ==\n")
# 5/10 at 95%: hand-computed Wilson interval ~ (0.2366, 0.7634).
ci <- wilson_ci(5, 10)
ok("wilson_ci(5,10) lower ~ 0.2366", stopifnot(near(ci[1], 0.2366)))
ok("wilson_ci(5,10) upper ~ 0.7634", stopifnot(near(ci[2], 0.7634)))
ok("wilson_ci(0,0) -> NA, NA",       stopifnot(all(is.na(wilson_ci(0, 0)))))

cat("\n== missing-data accounting (Gap 6) ==\n")
xm <- c(1, 2, NA, 4, NA)
dm <- describe_numeric(xm)
ok("missing count in synthetic vector", stopifnot(val(dm, "missing") == sum(is.na(xm))))
ok("n valid excludes missing",          stopifnot(val(dm, "n valid") == sum(!is.na(xm))))

# On the real sample: cd4_count has exactly 37 missing values.
dat <- read_any_format("data/lvct_cohort_repeated.csv", "lvct_cohort_repeated.csv")
cd4 <- suppressWarnings(as.numeric(dat$cd4_count))
dr  <- describe_numeric(cd4)
ok("cd4_count missing == sum(is.na) == 37",
   stopifnot(val(dr, "missing") == sum(is.na(cd4)), val(dr, "missing") == 37))

report_and_exit()
