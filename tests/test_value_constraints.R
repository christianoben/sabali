# tests/test_value_constraints.R --------------------------------------------
# The "valid values" cleaning layer (fct_clean.R) and its wiring into
# mod_data. Rules declared on the Data step (valid levels for categorical,
# min/max for numeric) coerce out-of-scope values to NA, centrally, so a
# confirmed type/override takes effect in the data EVERY downstream tab uses.
#
# Run:  Rscript tests/test_value_constraints.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== apply_value_constraints ==\n")

ok("levels rule sends non-kept CHARACTER values to NA", {
  df <- data.frame(g = c("A", "B", "Junk", "A", "", NA), stringsAsFactors = FALSE)
  out <- apply_value_constraints(df, list(g = list(kind = "levels", keep = c("A", "B"))))
  stopifnot(identical(out$g, c("A", "B", NA, "A", NA, NA)))
})

ok("levels rule on a FACTOR drops unused levels and preserves order", {
  df <- data.frame(g = factor(c("lo", "mid", "hi", "hi"),
                              levels = c("lo", "mid", "hi"), ordered = TRUE))
  out <- apply_value_constraints(df, list(g = list(kind = "levels", keep = c("lo", "hi"))))
  stopifnot(is.ordered(out$g),
            identical(levels(out$g), c("lo", "hi")),
            identical(as.character(out$g), c("lo", NA, "hi", "hi")))
})

ok("range rule sends out-of-range values to NA and can enforce integers", {
  df <- data.frame(n = c(-1, 0, 3.5, 4, 99))
  out <- apply_value_constraints(df, list(
    n = list(kind = "range", min = 0, max = 10, integer = TRUE)))
  # -1 (below), 3.5 (non-integer), 99 (above) -> NA; 0 and 4 kept
  stopifnot(identical(out$n, c(NA, 0, NA, 4, NA)))
})

ok("open-ended range (NA bound) only clips the specified side", {
  df <- data.frame(n = c(-5, 0, 5, 50))
  out <- apply_value_constraints(df, list(
    n = list(kind = "range", min = 0, max = NA_real_, integer = FALSE)))
  stopifnot(identical(out$n, c(NA, 0, 5, 50)))
})

ok("no constraints / missing variable is a safe no-op", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  stopifnot(identical(apply_value_constraints(df, list()), df),
            identical(apply_value_constraints(df, list(zzz = list(kind = "levels", keep = "x"))), df))
})

cat("\n== default_value_constraint & helpers ==\n")

ok("defaults: levels for categorical, range for numeric, NULL for id/text/date", {
  stopifnot(
    identical(default_value_constraint(c("a", "b", "a"), "binary")$kind, "levels"),
    identical(default_value_constraint(c(1, 2, 3), "continuous")$kind, "range"),
    isTRUE(default_value_constraint(c(1, 2, 3), "count")$integer),
    is.null(default_value_constraint(1:5, "identifier")),
    is.null(default_value_constraint(letters, "text")))
})

ok("default level set excludes blanks", {
  d <- default_value_constraint(c("A", "B", "", "  ", NA), "binary")
  stopifnot(setequal(d$keep, c("A", "B")))
})

ok("constraint_is_active detects a tightened rule only", {
  x <- c("A", "B", "C")
  loose <- list(kind = "levels", keep = c("A", "B", "C"))
  tight <- list(kind = "levels", keep = c("A", "B"))
  stopifnot(isFALSE(constraint_is_active(loose, x, "nominal")),
            isTRUE(constraint_is_active(tight, x, "nominal")))
})

ok("constraint_summary is human readable", {
  s1 <- constraint_summary("colonization", list(kind = "levels", keep = c("Yes", "No")))
  s2 <- constraint_summary("age", list(kind = "range", min = 0, max = 120))
  stopifnot(grepl("keep 2 level", s1), grepl("0 to 120", s2))
})

cat("\n== dq_contract_to_constraints (tab-0 contract -> enforcement) ==\n")

ok("maps categorical allowed levels and numeric ranges; skips datetime ranges", {
  df <- data.frame(g = c("A", "B", "C"), n = c(1, 2, 3),
                   d = as.Date(c("2020-01-01", "2020-06-01", "2020-12-01")),
                   stringsAsFactors = FALSE)
  ct <- dq_contract(
    expected_types = c(g = "binary", n = "continuous", d = "datetime"),
    allowed_levels = list(g = c("A", "B")),
    ranges = list(n = list(min = "0", max = "2"),
                  d = list(min = "2020-01-01", max = "2020-12-31")))
  cons <- dq_contract_to_constraints(ct, df, c(g = "binary", n = "continuous", d = "datetime"))
  stopifnot(identical(cons$g$kind, "levels"), setequal(cons$g$keep, c("A", "B")),
            identical(cons$n$kind, "range"), cons$n$max == 2,
            is.null(cons$d))                         # datetime range left to the checks
})

ok("no-op for an empty / NULL contract", {
  stopifnot(length(dq_contract_to_constraints(NULL, data.frame(a = 1))) == 0,
            length(dq_contract_to_constraints(dq_contract(), data.frame(a = 1))) == 0)
})

cat("\n== end-to-end: enforcement reaches the analysis data (ds$data), not the base ==\n")

ok("an enforce rule coerces out-of-scope values to NA in data_enforced only", {
  d <- data.frame(
    record = 1:6,
    colonization = c("Colonized", "Not colonized", "Indeterminate",
                     "Colonized", "Not colonized", "typo"),
    stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv"); utils::write.csv(d, path, row.names = FALSE, na = "")
  rules <- list(colonization = list(kind = "levels",
                                    keep = c("Colonized", "Not colonized")))
  got <- new.env()
  shiny::testServer(mod_data_server, args = list(enforce = shiny::reactive(rules)), {
    session$setInputs(file = list(datapath = path, name = basename(path)))
    session$setInputs(data_kind = "cross_sectional")
    session$flushReact()
    got$base <- data_filtered()$colonization       # pre-enforcement (tab-0 reads this)
    got$enf  <- data_enforced()$colonization        # what the analysis tabs receive
  })
  stopifnot(
    "Indeterminate" %in% got$base, "typo" %in% got$base,   # base keeps everything
    sum(is.na(got$enf)) == 2,                               # both out-of-scope -> NA
    setequal(got$enf[!is.na(got$enf)], c("Colonized", "Not colonized")),
    "binary" %in% outcome_type_options(got$enf))           # now binary-eligible
})

cat("\n== tab-0 Data contract emits enforcement rules when tightened ==\n")

ok("restricting a categorical level in the contract yields a levels rule", {
  df <- data.frame(g = c("A", "B", "C", "A"), n = c(1, 2, 3, 4),
                   stringsAsFactors = FALSE)
  tp <- c(g = "nominal", n = "continuous")
  got <- new.env()
  shiny::testServer(mod_datacheck_server,
    args = list(data = shiny::reactive(df), types = shiny::reactive(tp)), {
      session$setInputs(contract_levels_1 = "A, B")   # drop level C for g (col 1)
      session$flushReact()
      got$enf <- session$returned$enforcement()
    })
  stopifnot(!is.null(got$enf$g), identical(got$enf$g$kind, "levels"),
            setequal(got$enf$g$keep, c("A", "B")))
})

report_and_exit()
