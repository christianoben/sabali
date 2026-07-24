# tests/test_method_gating.R ------------------------------------------------
# The Charts / Tests / Regression tabs must offer only methods and variables
# appropriate to the declared dataset KIND - so a non-statistician cannot, for
# example, run a t-test or histogram on a censored survival time.
#
#   * survival     -> follow-up time / event / start / end columns are reserved
#                     (analysed only by Kaplan-Meier / Cox); a warning banner shows.
#   * repeated     -> subject ID reserved; a pseudoreplication warning shows.
#   * time series  -> an autocorrelation warning shows.
#   * cross-section -> nothing reserved, no warning (all methods valid).
#
# Run:  Rscript tests/test_method_gating.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({ library(shiny); library(ggplot2) })

# ===========================================================================
cat("\n== reserved_analysis_vars() ==\n")
ok("cross-sectional reserves nothing",
   stopifnot(length(reserved_analysis_vars(list(kind = "cross_sectional"))) == 0))
ok("survival (duration) reserves id + follow-up + event", {
  r <- reserved_analysis_vars(list(kind = "cross_sectional", id = "pid",
    survival = TRUE, surv_mode = "duration", surv_time = "fu", surv_event = "status",
    event = "status"))
  stopifnot(setequal(r, c("pid", "fu", "status")))
})
ok("survival (dates) reserves id + start + end + event", {
  r <- reserved_analysis_vars(list(kind = "repeated", id = "pid", survival = TRUE,
    surv_mode = "dates", surv_start = "t0", surv_end = "t1", surv_event = "ev", event = "ev"))
  stopifnot(setequal(r, c("pid", "t0", "t1", "ev")))
})
ok("repeated non-survival reserves the subject ID only",
   stopifnot(setequal(reserved_analysis_vars(list(kind = "repeated", id = "pid")), "pid")))

# ===========================================================================
cat("\n== analysis_kind_note() guidance ==\n")
ok("cross-sectional -> no banner", stopifnot(is.null(analysis_kind_note(list(kind = "cross_sectional")))))
ok("survival -> time-to-event guidance",
   stopifnot(grepl("time-to-event", analysis_kind_note(list(kind = "cross_sectional", survival = TRUE)))))
ok("repeated -> pseudoreplication guidance",
   stopifnot(grepl("repeated measures|pseudoreplication",
                   analysis_kind_note(list(kind = "repeated", id = "p", time = "t")))))
ok("time series -> autocorrelation guidance",
   stopifnot(grepl("time series|autocorrelated", analysis_kind_note(list(kind = "time_series", time = "t")))))
ok("repeated + survival -> both warnings", {
  n <- analysis_kind_note(list(kind = "repeated", id = "p", time = "t", survival = TRUE))
  stopifnot(grepl("time-to-event", n), grepl("repeated measures", n))
})

# ---- shared fixtures -------------------------------------------------------
set.seed(1); n <- 120
d <- data.frame(pid = 1:n, fu_days = round(rexp(n, 1/300)),
                status = factor(sample(c("censored", "died"), n, TRUE),
                                levels = c("censored", "died")),
                arm = factor(sample(c("A", "B"), n, TRUE)),
                age = rnorm(n, 45, 10), sex = factor(sample(c("F", "M"), n, TRUE)))
types <- c(pid = "continuous", fu_days = "continuous", status = "binary",
           arm = "nominal", age = "continuous", sex = "binary")
surv_panel <- list(kind = "cross_sectional", id = "pid", survival = TRUE,
                   surv_mode = "duration", surv_time = "fu_days",
                   surv_event = "status", event = "status")
cs_panel <- list(kind = "cross_sectional")

banner_shown <- function(server, panel) {
  shown <- NULL
  shiny::testServer(server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("age", "fu_days")), panel = reactive(panel),
                add_report = function(x) NULL), {
      session$flushReact()
      shown <<- !is.null(output$kind_note)
    })
  isTRUE(shown)
}

# ===========================================================================
cat("\n== Charts tab: survival columns hidden, banner shown ==\n")
ok("survival columns excluded from the chart variable pool; covariates kept", {
  pool <- NULL
  shiny::testServer(mod_charts_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("age", "fu_days")), panel = reactive(surv_panel),
                add_report = function(x) NULL), {
      session$flushReact(); pool <<- analysable()
    })
  stopifnot(!any(c("fu_days", "status", "pid") %in% pool),
            all(c("age", "arm", "sex") %in% pool))
})
ok("charts warning banner shows for a survival dataset", stopifnot(banner_shown(mod_charts_server, surv_panel)))
ok("charts: NO banner for a plain cross-sectional dataset", stopifnot(!banner_shown(mod_charts_server, cs_panel)))

# ===========================================================================
cat("\n== Tests tab: survival columns hidden from every role pool ==\n")
ok("num/cat/binary pools drop the reserved survival + ID columns", {
  nv <- cv <- bv <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("age", "fu_days")), panel = reactive(surv_panel),
                add_report = function(x) NULL), {
      session$flushReact(); nv <<- num_vars(); cv <<- cat_vars(); bv <<- binary_vars()
    })
  reserved <- c("fu_days", "status", "pid")
  stopifnot(!any(reserved %in% c(nv, cv, bv)), "age" %in% nv, "arm" %in% cv)
})
ok("tests warning banner shows for a survival dataset", stopifnot(banner_shown(mod_tests_server, surv_panel)))

# ===========================================================================
cat("\n== Regression tab: censored time cannot be an outcome or predictor ==\n")
ok("reserved columns excluded from outcome and predictor pools", {
  ov <- pv <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("age", "fu_days")), panel = reactive(surv_panel),
                add_report = function(x) NULL), {
      session$setInputs(outcome = "age"); session$flushReact()
      ov <<- outcome_vars(); pv <<- predictor_vars()
    })
  stopifnot(!any(c("fu_days", "status", "pid") %in% ov),
            !any(c("fu_days", "status", "pid") %in% pv),
            "age" %in% ov)
})
ok("regression warning banner shows for a survival dataset", stopifnot(banner_shown(mod_models_server, surv_panel)))
ok("regression: NO banner for a plain cross-sectional dataset", stopifnot(!banner_shown(mod_models_server, cs_panel)))

# ===========================================================================
cat("\n== Repeated data: subject ID reserved, pseudoreplication banner ==\n")
rep_panel <- list(kind = "repeated", id = "pid", time = "arm")
ok("tests pools drop the subject ID for repeated data", {
  nv <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(d), types = reactive(types),
                outcomes = reactive(c("age")), panel = reactive(rep_panel),
                add_report = function(x) NULL), {
      session$flushReact(); nv <<- num_vars()
    })
  stopifnot(!("pid" %in% nv), "age" %in% nv)
})
ok("all three tabs warn on repeated data", {
  stopifnot(banner_shown(mod_charts_server, rep_panel),
            banner_shown(mod_tests_server, rep_panel),
            banner_shown(mod_models_server, rep_panel))
})

# ===========================================================================
cat("\n== Composite gating: recommended methods by layout kind ==\n")
ok("analysis_layout_kind reads the declared layout", {
  stopifnot(identical(analysis_layout_kind(list(kind = "repeated")), "repeated"),
            identical(analysis_layout_kind(list(kind = "cross_sectional")), "cross_sectional"),
            identical(analysis_layout_kind(NULL), "cross_sectional"))
})
ok("recommended_test_families drops independence families for repeated/time series", {
  stopifnot(setequal(recommended_test_families("repeated"),
                     c("location", "categorical", "correlation", "cochran_q")),
            length(recommended_test_families("time_series")) == 0,
            is.null(recommended_test_families("cross_sectional")))
})
ok("recommended location/correlation designs for repeated are within-subject", {
  stopifnot(setequal(recommended_location_designs("repeated"), c("paired_repeated", "mixed")),
            identical(recommended_corr_designs("repeated"), "repeated"),
            identical(recommended_cat_designs("repeated"), "repeated_binary"))
})
ok("cross-sectional location designs are independence-based (no paired/mixed)", {
  cs <- recommended_location_designs("cross_sectional")
  stopifnot(setequal(cs, c("one_sample", "two_independent", "multi_independent",
                           "factorial", "ancova")),
            !("paired_repeated" %in% cs), !("mixed" %in% cs))
})
ok("feasible_location_designs offers only designs the variables can support", {
  # only a numeric outcome -> just one_sample
  d1 <- data.frame(y = rnorm(30)); ty1 <- c(y = "continuous")
  stopifnot(setequal(feasible_location_designs(ty1, d1), "one_sample"))
  # numeric + a 2-level group -> one_sample + two_independent only
  d2 <- data.frame(y = rnorm(30), g = factor(rep(c("A", "B"), 15)))
  f2 <- feasible_location_designs(c(y = "continuous", g = "binary"), d2)
  stopifnot("two_independent" %in% f2, "one_sample" %in% f2,
            !("multi_independent" %in% f2), !("factorial" %in% f2), !("ancova" %in% f2))
  # two numerics + a 3-level factor -> multi_independent and ancova, but not factorial (one factor)
  d3 <- data.frame(y = rnorm(30), x = rnorm(30), g = factor(rep(c("A", "B", "C"), 10)))
  f3 <- feasible_location_designs(c(y = "continuous", x = "continuous", g = "nominal"), d3)
  stopifnot("multi_independent" %in% f3, "ancova" %in% f3,
            !("factorial" %in% f3), !("two_independent" %in% f3))
})
ok("independent regression recommended only for cross-sectional", {
  stopifnot(regression_recommended_for("cross_sectional"),
            !regression_recommended_for("repeated"),
            !regression_recommended_for("time_series"))
})

# repeated fixture with a continuous outcome + categoricals + binary
set.seed(3); ns <- 40
rd <- data.frame(id = rep(1:ns, each = 3), visit = rep(c("v1","v2","v3"), ns),
                 arm = rep(sample(c("A","B"), ns, TRUE), each = 3),
                 y = rnorm(ns * 3), b = factor(sample(c("no","yes"), ns * 3, TRUE)))
rtypes <- c(id = "identifier", visit = "nominal", arm = "binary",
            y = "continuous", b = "binary")
rpanel <- list(kind = "repeated", id = "id", time = "visit")

vals_in <- function(html, v) grepl(sprintf('value="%s"', v), html, fixed = TRUE)
# NB: parameter is `expand`, not `show_all` - the latter is the module's own
# reactive and would be shadowed inside the testServer block.
tests_ui_html <- function(out_name, expand = FALSE, set_family = NULL,
                          data = rd, types = rtypes, panel = rpanel) {
  h <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(data), types = reactive(types),
                outcomes = reactive(character(0)), panel = reactive(panel),
                add_report = function(x) NULL), {
      if (!is.null(set_family)) session$setInputs(family = set_family)
      if (isTRUE(expand)) session$setInputs(show_all = TRUE)
      session$flushReact()
      o <- output[[out_name]]
      h <<- if (is.list(o)) paste(o$html, collapse = " ") else as.character(o)
    })
  h
}

# cross-sectional fixture (one row per subject) for the study-design gate
cs_d <- data.frame(y = rnorm(60), g = factor(sample(c("A", "B"), 60, TRUE)))
cs_types <- c(y = "continuous", g = "binary")
cs_panel <- list(kind = "cross_sectional")

cat("\n== Tests tab: repeated hides independence families/designs by default ==\n")
ok("family radio excludes proportion & point-biserial for repeated data", {
  h <- tests_ui_html("family_ui")
  stopifnot(!vals_in(h, "proportion"), !vals_in(h, "point_biserial"))
})
ok("'Include un-recommended analyses' reveals the independence families", {
  h <- tests_ui_html("family_ui", expand = TRUE)
  stopifnot(vals_in(h, "proportion"))
})
ok("location designs are within-subject only by default (no two_independent)", {
  h <- tests_ui_html("controls", set_family = "location")
  stopifnot(vals_in(h, "paired_repeated"), !vals_in(h, "two_independent"),
            !vals_in(h, "one_sample"))
})
ok("un-recommended toggle restores the independent designs", {
  h <- tests_ui_html("controls", expand = TRUE, set_family = "location")
  stopifnot(vals_in(h, "two_independent"), vals_in(h, "one_sample"))
})

cat("\n== Tests tab: cross-sectional hides within-subject designs by default ==\n")
ok("cross-sectional location designs exclude paired/repeated and mixed", {
  h <- tests_ui_html("controls", set_family = "location",
                     data = cs_d, types = cs_types, panel = cs_panel)
  stopifnot(vals_in(h, "two_independent"), vals_in(h, "one_sample"),
            !vals_in(h, "paired_repeated"), !vals_in(h, "mixed"))
})
ok("override restores within-subject designs for cross-sectional", {
  h <- tests_ui_html("controls", expand = TRUE, set_family = "location",
                     data = cs_d, types = cs_types, panel = cs_panel)
  stopifnot(vals_in(h, "paired_repeated"), vals_in(h, "mixed"))
})

cat("\n== Regression tab: gated off for repeated data, escape hatch works ==\n")
ok("no regression outcomes offered for repeated data by default", {
  ov <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(rd), types = reactive(rtypes),
                outcomes = reactive(c("y")), panel = reactive(rpanel),
                add_report = function(x) NULL), {
      session$flushReact(); ov <<- outcome_vars()
    })
  stopifnot(length(ov) == 0)
})
ok("un-recommended toggle re-enables regression for repeated data", {
  ov <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(rd), types = reactive(rtypes),
                outcomes = reactive(c("y")), panel = reactive(rpanel),
                add_report = function(x) NULL), {
      session$setInputs(show_all = TRUE); session$flushReact(); ov <<- outcome_vars()
    })
  stopifnot("y" %in% ov)
})
ok("cross-sectional regression is unrestricted", {
  ov <- NULL
  shiny::testServer(mod_models_server,
    args = list(data = reactive(rd), types = reactive(rtypes),
                outcomes = reactive(c("y")), panel = reactive(list(kind = "cross_sectional")),
                add_report = function(x) NULL), {
      session$flushReact(); ov <<- outcome_vars()
    })
  stopifnot("y" %in% ov)
})

report_and_exit()
