# Deterministic property-style tests for type detection and coercion.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== detect_type property checks ==\n")
set.seed(20260724)
allowed <- MEASUREMENT_TYPES
safe_text <- c("alpha", "beta", "gamma", "", NA_character_, "Δ")
for (i in seq_len(200)) {
  n <- sample(0:60, 1)
  kind <- sample(c("numeric", "integer", "text", "factor", "date", "missing"), 1)
  x <- switch(kind,
    numeric = if (n) rnorm(n) else numeric(),
    integer = if (n) sample(0:6, n, replace = TRUE) else integer(),
    text = if (n) sample(safe_text, n, replace = TRUE) else character(),
    factor = if (n) factor(sample(c("a", "b", "c"), n, replace = TRUE)) else factor(),
    date = if (n) as.Date("2020-01-01") + sample(0:1000, n, replace = TRUE) else as.Date(character()),
    missing = rep(NA_real_, n))
  ok(sprintf("random vector %03d has a supported type result", i), {
    got <- detect_type(x)
    stopifnot(length(got) == 1L, got %in% allowed)
    d <- detect_all_types(data.frame(v = x))
    stopifnot(nrow(d) == 1L, identical(d$variable, "v"), d$type %in% allowed)
  })
}
ok("coercion remains total for representative target types", {
  x <- c("1", "2", NA, "3")
  out <- lapply(allowed, function(tp) tryCatch(coerce_to_type(x, tp), error = identity))
  stopifnot(!any(vapply(out, inherits, logical(1), "error")))
})

report_and_exit()
