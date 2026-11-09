# tests/test_survival.R ------------------------------------------------------
# Phase 4 time-to-event tests for Kaplan-Meier and Cox regression.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages({
  library(shiny)
  library(survival)
})

set.seed(74)
n <- 180
d <- data.frame(
  time = rexp(n, rate = 0.08),
  event = factor(rbinom(n, 1, 0.58), levels = c(0, 1),
                 labels = c("Censored", "Event")),
  group = factor(sample(c("Control", "Intervention"), n, replace = TRUE)),
  x = rnorm(n),
  age = rnorm(n, 50, 8)
)
status <- as.integer(d$event == "Event")

cat("\n== Kaplan-Meier analysis ==\n")
ok("unstratified Kaplan-Meier returns survival estimates", {
  r <- run_guided_survival(d, list(method = "kaplan_meier", time = "time",
                                   event = "event", strata = ""))
  stopifnot(grepl("Kaplan-Meier", r$label), nrow(r$coefficients) == 1,
            inherits(r$diagnostic_plot, "ggplot"),
            r$fit_stats$Value[r$fit_stats$Metric == "events"] == sum(status))
})
ok("grouped Kaplan-Meier matches survival::survfit()", {
  r <- run_guided_survival(d, list(method = "kaplan_meier", time = "time",
                                   event = "event", strata = "group"))
  ref <- survival::survfit(survival::Surv(time, status) ~ group, data = d)
  ref_tab <- survival_summary_table(ref)
  stopifnot(nrow(r$coefficients) == 2,
            all.equal(r$coefficients$`Median survival`,
                      ref_tab$`Median survival`, tolerance = 1e-8),
            "Log-rank p-value" %in% r$fit_stats$Metric)
})
ok("Kaplan-Meier rejects data with no observed events", {
  bad <- d; bad$event <- factor("Censored", levels = c("Censored", "Event"))
  inherits(try(run_guided_survival(
    bad, list(method = "kaplan_meier", time = "time", event = "event", strata = "")),
    silent = TRUE), "try-error")
})

cat("\n== Cox proportional-hazards regression ==\n")
ok("Cox hazard ratios match survival::coxph()", {
  r <- run_guided_survival(d, list(method = "cox", time = "time", event = "event",
                                   predictors = c("x", "group")))
  ref <- survival::coxph(survival::Surv(time, status) ~ x + group,
                         data = d, ties = "efron")
  stopifnot(grepl("Cox", r$label),
            all.equal(r$coefficients$`Hazard ratio`,
                      unname(exp(stats::coef(ref))), tolerance = 1e-8),
            inherits(r$diagnostic_plot, "ggplot"),
            any(grepl("proportional-hazards", vapply(r$assumptions,
                                                      function(x) x$note, character(1)),
                      fixed = TRUE)))
})
ok("Cox requires at least one predictor", {
  inherits(try(run_guided_survival(
    d, list(method = "cox", time = "time", event = "event", predictors = character(0))),
    silent = TRUE), "try-error")
})

cat("\n== One-row survival from start + end dates ==\n")
set.seed(21); nd <- 140
sd <- as.Date("2020-01-01") + sample(0:90, nd, TRUE)
dd <- data.frame(start_date = sd, end_date = sd + round(rexp(nd, 1 / 250)),
                 status = factor(sample(c("censored", "died"), nd, TRUE),
                                 levels = c("censored", "died")),
                 arm = factor(sample(c("A", "B"), nd, TRUE)), age = rnorm(nd, 50, 9))
ok("follow-up helper computes day differences", {
  stopifnot(survival_followup_from_dates(as.Date("2020-01-01"),
                                         as.Date("2020-02-01")) == 31)
})
ok("Cox from dates matches coxph on the computed follow-up", {
  r <- run_guided_survival(dd, list(method = "cox", start = "start_date",
                                    end = "end_date", event = "status",
                                    predictors = c("arm", "age")))
  fu <- as.numeric(dd$end_date - dd$start_date); st <- as.integer(dd$status == "died")
  ref <- survival::coxph(survival::Surv(fu, st) ~ arm + age, data = dd, ties = "efron")
  stopifnot(isTRUE(all.equal(r$coefficients$`Hazard ratio`,
                             unname(exp(stats::coef(ref))), tolerance = 1e-6)))
})
ok("Kaplan-Meier from dates runs with a grouping", {
  r <- run_guided_survival(dd, list(method = "kaplan_meier", start = "start_date",
                                    end = "end_date", event = "status", strata = "arm"))
  stopifnot(grepl("Kaplan-Meier", r$label), nrow(r$coefficients) == 2)
})

cat("\n== Counting-process (start-stop, time-varying) Cox ==\n")
set.seed(8)
cp <- do.call(rbind, lapply(1:60, function(i) {
  k <- sample(1:3, 1); t <- cumsum(c(0, sort(sample(1:400, k))))
  data.frame(id = i, t0 = t[-length(t)], t1 = t[-1], x = rnorm(k),
             trt = factor(sample(c("A", "B"), 1)), status = 0)
}))
.last <- tapply(seq_len(nrow(cp)), cp$id, max)
cp$status[sample(.last, length(.last) / 2)] <- 1L
cp$event <- factor(ifelse(cp$status == 1, "yes", "no"), levels = c("no", "yes"))
ok("start-stop Cox matches coxph(Surv(t0,t1,status) ~ . + cluster(id))", {
  r <- run_guided_survival(cp, list(method = "cox", id = "id", start = "t0",
                                    end = "t1", event = "event",
                                    predictors = c("x", "trt")))
  d2 <- cp; d2$.__id <- factor(d2$id)
  ref <- survival::coxph(survival::Surv(t0, t1, status) ~ x + trt + cluster(.__id),
                         data = d2, ties = "efron")
  stopifnot(grepl("start-stop", r$label, ignore.case = TRUE),
            isTRUE(all.equal(r$coefficients$`Hazard ratio`,
                             unname(exp(stats::coef(ref))), tolerance = 1e-6)),
            isTRUE(all.equal(r$coefficients$`Std. Error`,
                             unname(summary(ref)$coefficients[, "robust se"]),
                             tolerance = 1e-6)))
})
ok("Kaplan-Meier is refused for start-stop data", {
  inherits(try(run_guided_survival(cp, list(method = "kaplan_meier", id = "id",
    start = "t0", end = "t1", event = "event")), silent = TRUE), "try-error")
})

cat("\n== Survival availability + family gating come from the declaration ==\n")
ok("one-row survival opens Advanced models (survival only, not clustered)", {
  info <- advanced_data_structure(dd, c(start_date = "datetime", end_date = "datetime",
    status = "binary", arm = "nominal", age = "continuous"),
    list(kind = "cross_sectional", survival = TRUE, surv_start = "start_date",
         surv_end = "end_date", surv_event = "status"))
  stopifnot(isTRUE(info$available), isTRUE(info$survival), !isTRUE(info$clustered))
})
ok("repeated + survival is both clustered AND survival (start-stop)", {
  info <- advanced_data_structure(cp, c(id = "identifier", t0 = "continuous",
    t1 = "continuous", event = "binary", x = "continuous", trt = "binary"),
    list(kind = "repeated", id = "id", time = "t1", survival = TRUE,
         surv_start = "t0", surv_end = "t1", surv_event = "event"))
  stopifnot(isTRUE(info$available), isTRUE(info$survival), isTRUE(info$clustered))
})

cat("\n== Phase 4 survival tab wiring ==\n")
ok("survival method controls send the internal method values", {
  method_html <- NULL
  shiny::testServer(mod_phase4_server,
    args = list(data = reactive(d), types = reactive(c(
      time = "continuous", event = "binary", group = "nominal",
      x = "continuous", age = "continuous")),
      add_report = function(x) NULL), {
      session$setInputs(analysis = "survival")
      session$flushReact()
      method_html <<- output$survival_method_ui$html
    })
  stopifnot(grepl('value="kaplan_meier"', method_html, fixed = TRUE),
            grepl('value="cox"', method_html, fixed = TRUE),
            grepl("Kaplan-Meier survival analysis", method_html, fixed = TRUE),
            grepl("Cox proportional-hazards regression", method_html, fixed = TRUE))
})
ok("Advanced tab renders Kaplan-Meier results and adds report items", {
  bag <- list(); rendered <- NULL
  # Time/event/strata now come from the Data-tab declaration (panel).
  surv_panel <- reactive(list(kind = "cross_sectional", survival = TRUE,
    surv_mode = "duration", surv_time = "time", surv_event = "event",
    event = "event"))
  shiny::testServer(mod_phase4_server,
    args = list(data = reactive(d), types = reactive(c(
      time = "continuous", event = "binary", group = "nominal",
      x = "continuous", age = "continuous")),
      panel = surv_panel,
      add_report = function(x) bag[[length(bag) + 1]] <<- x), {
      session$setInputs(analysis = "survival", survival_method = "kaplan_meier",
                        survival_strata = "group")
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      rendered <<- output$coefficients
      session$setInputs(include_diag = TRUE); session$flushReact()
      session$setInputs(add = 1); session$flushReact()
    })
  stopifnot(is.character(rendered), grepl("Median", rendered, fixed = TRUE),
            length(bag) >= 3)
})
ok("Advanced family radio offers only survival for cross-sectional survival data", {
  html <- NULL
  surv_panel <- reactive(list(kind = "cross_sectional", survival = TRUE,
    surv_mode = "duration", surv_time = "time", surv_event = "event", event = "event"))
  shiny::testServer(mod_phase4_server,
    args = list(data = reactive(d), types = reactive(c(
      time = "continuous", event = "binary", group = "nominal",
      x = "continuous", age = "continuous")),
      panel = surv_panel, add_report = function(x) NULL), {
      session$flushReact()
      html <<- as.character(output$analysis_ui$html)
    })
  stopifnot(grepl('value="survival"', html, fixed = TRUE),
            !grepl('value="clustered"', html, fixed = TRUE))
})

report_and_exit()
