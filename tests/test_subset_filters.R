# tests/test_subset_filters.R -----------------------------------------------
# The optional subset filters on the "1. Data" tab: pick a variable, then keep a
# numeric/date RANGE or a set of LEVELS. A row survives only if it passes every
# active filter; an inactive filter (no variable, full range, or all levels) is
# a no-op and must not even drop rows missing that variable. The filtered data
# is what every downstream tab receives as `data`.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

# A small fixture written to disk so the module's fileInput/read path is
# exercised exactly as in the running app.
make_fixture <- function() {
  df <- data.frame(
    id  = sprintf("P%02d", 1:12),
    age = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, NA, 70),
    arm = factor(rep(c("control", "drug_a", "drug_b"), 4)),
    visits = c(0:10, NA),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  list(df = df, path = path, name = basename(path))
}

# Drive mod_data_server with a loaded file, letting the type editor default.
with_module <- function(expr_fun) {
  fx <- make_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    # detected() drives the type_* inputs; leave them NULL so types() falls back
    # to the auto-detected type for every column.
    expr_fun(environment())
  })
}

cat("\n== Subset filters ==\n")

ok("no filters selected -> all rows analysed", {
  with_module(function(env) with(env, {
    stopifnot(nrow(data_filtered()) == nrow(raw()))
    stopifnot(identical(nrow(data_filtered()), 12L))
  }))
})

ok("numeric range keeps rows in [lo, hi] and drops NA in that column", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "age", filter_val_1 = c(30, 55))
    session$elapse(450)
    got <- data_filtered()
    stopifnot(all(got$age >= 30 & got$age <= 55))
    stopifnot(!any(is.na(got$age)))
    stopifnot(nrow(got) == sum(raw()$age >= 30 & raw()$age <= 55, na.rm = TRUE))
  }))
})

ok("full numeric range is a no-op and keeps NA rows", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "age", filter_val_1 = c(20, 70))
    session$elapse(450)
    stopifnot(nrow(data_filtered()) == nrow(raw()))       # 12, NA age retained
  }))
})

ok("level picker keeps only chosen categories", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "arm", filter_val_1 = c("control", "drug_b"))
    session$elapse(450)
    got <- data_filtered()
    stopifnot(all(as.character(got$arm) %in% c("control", "drug_b")))
    stopifnot(nrow(got) == 8L)
  }))
})

ok("empty categorical selection keeps all rows", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "arm", filter_val_1 = character(0))
    session$elapse(450)
    stopifnot(nrow(data_filtered()) == nrow(raw()))
  }))
})

ok("three filters combine with AND", {
  with_module(function(env) with(env, {
    session$setInputs(
      filter_var_1 = "age",    filter_val_1 = c(30, 65),
      filter_var_2 = "arm",    filter_val_2 = c("control", "drug_a"),
      filter_var_3 = "visits", filter_val_3 = c(2, 9))
    session$elapse(450)
    got <- data_filtered()
    ref <- raw()
    keep <- !is.na(ref$age) & ref$age >= 30 & ref$age <= 65 &
      as.character(ref$arm) %in% c("control", "drug_a") &
      !is.na(ref$visits) & ref$visits >= 2 & ref$visits <= 9
    stopifnot(nrow(got) == sum(keep))
    stopifnot(all(got$id %in% ref$id[keep]))
  }))
})

ok("a variable chosen but its control not yet rendered is inactive", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "age")               # filter_val_1 still NULL
    session$elapse(450)
    stopifnot(nrow(data_filtered()) == nrow(raw()))
  }))
})

ok("module returns filtered data as `data` and full data as `data_full`", {
  with_module(function(env) with(env, {
    session$setInputs(filter_var_1 = "arm", filter_val_1 = "control")
    session$elapse(450)
    out <- session$returned
    stopifnot(nrow(out$data()) == 4L)          # filtered subset
    stopifnot(nrow(out$data_full()) == 12L)    # unfiltered
  }))
})

ok("a very high-cardinality categorical shows a note, not a giant picker", {
  df <- data.frame(code = sprintf("C%04d", 1:600),
                   grp = factor(rep(c("x", "y", "z"), length.out = 600)),
                   y = rnorm(600), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = path, name = basename(path)))
    session$setInputs(filter_var_1 = "code")             # 600 distinct values
    html <- html_text(output$filter_ctrl_1)
    stopifnot(grepl("too many to filter", html))
    # a small categorical still renders a real picker (no note)
    session$setInputs(filter_var_1 = "grp")
    stopifnot(!grepl("too many to filter", html_text(output$filter_ctrl_1)))
  })
})

report_and_exit()
