# tests/test_data_readiness.R -----------------------------------------------
# End-to-end golden checks for the 22-point data-readiness workflow:
# contract -> raw/typed checks -> action findings -> scorecard -> analysis
# readiness -> review decisions -> report/UI wiring.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== A. Scorecard and action contract ==\n")
ok("scorecard has 22 named points", {
  stopifnot(nrow(DQ_SCORECARD) == 22L,
            length(unique(DQ_SCORECARD$id)) == 22L,
            all(DQ_SCORECARD$context %in% c("always", "contract", "analysis")))
})
ok("clean data returns explicit scorecard coverage", {
  d <- data.frame(id = 1:30, y = rnorm(30), g = factor(rep(c("A", "B"), 15)))
  r <- dq_run_checks(d, c(id = "identifier", y = "continuous", g = "binary"))
  stopifnot(nrow(r$scorecard) == 22L,
            all(c("issue", "impact", "recommendation", "affected_n", "affected_pct",
                  "finding_id", "status") %in% names(r$findings)),
            all(r$findings$severity == "pass" |
                (nzchar(r$findings$issue) & nzchar(r$findings$impact) &
                   nzchar(r$findings$recommendation))))
})
ok("action report includes the scorecard and recommendations", {
  d <- data.frame(age = c(10, 20, 999), stringsAsFactors = FALSE)
  r <- dq_run_checks(d, c(age = "continuous"),
                     contract = dq_contract(ranges = list(age = list(min = 0, max = 120))))
  txt <- dq_report_text(r, "dirty.csv", "fixed timestamp")
  stopifnot(grepl("22-POINT SCORECARD", txt, fixed = TRUE),
            grepl("ACTION FINDINGS", txt, fixed = TRUE),
            grepl("Recommendation:", txt, fixed = TRUE),
            grepl("fixed timestamp", txt, fixed = TRUE))
})

cat("\n== B. Raw import and data-contract checks ==\n")
raw <- data.frame(
  id = c(1:19, 19),
  age = c(as.character(20:38), "not recorded"),
  sex = c(rep("Female", 10), rep("female ", 9), "Other"),
  stringsAsFactors = FALSE
)
typed <- raw
typed$age <- suppressWarnings(as.numeric(typed$age))
typed$id <- as.integer(typed$id)
contract <- dq_contract(
  expected_types = c(id = "identifier", age = "continuous", sex = "binary"),
  required = c("id", "missing_field"),
  ranges = list(age = list(min = 0, max = 120)),
  allowed_levels = list(sex = c("Female", "Male")),
  identifiers = "id",
  unique_keys = list("id"),
  subgroup_vars = "sex"
)
r_contract <- dq_run_checks(typed,
  c(id = "identifier", age = "continuous", sex = "binary"),
  contract = contract, raw_df = raw)
ok("raw coercion loss is a blocking finding", {
  stopifnot(any(r_contract$findings$check == "raw coercion loss" &
                r_contract$findings$severity == "error"))
})
ok("contract catches required fields, ranges, levels, and duplicate keys", {
  stopifnot(any(r_contract$findings$check == "required fields"),
            any(r_contract$findings$check == "expected ranges"),
            any(r_contract$findings$check == "allowed levels"),
            any(r_contract$findings$check == "duplicate key"))
})
ok("category findings do not print raw category values", {
  txt <- dq_report_text(r_contract, "sensitive.csv", "t")
  stopifnot(!grepl("Female", txt, fixed = TRUE),
            !grepl("Other", txt, fixed = TRUE),
            grepl("category", txt, ignore.case = TRUE))
})

cat("\n== C. Missingness, variation, dates, and identifiers ==\n")
d <- data.frame(
  id = c(1:39, 39),
  grp = factor(c(rep("A", 20), rep("B", 20))),
  value = c(rep(1, 38), 2, NA),
  start = as.Date("2020-01-01") + 0:39,
  end = as.Date("2020-01-01") + c(0:38, -1),
  stringsAsFactors = FALSE
)
d$value[1:10] <- NA
d$grp[1:4] <- NA
r <- dq_run_checks(d,
  c(id = "identifier", grp = "nominal", value = "continuous",
    start = "datetime", end = "datetime"),
  contract = dq_contract(
    identifiers = "id", unique_keys = list("id"), subgroup_vars = "grp",
    date_order = list(c("start", "end"))))
ok("missingness patterns and subgroup gaps are reported", {
  stopifnot(any(r$findings$check == "missingness patterns"),
            any(r$findings$check == "missingness by subgroup"))
})
ok("near-zero variance, identifier, and date-order checks fire", {
  stopifnot(any(r$findings$check == "near-zero variance"),
            any(r$findings$check == "duplicate key"),
            any(r$findings$check == "date order" & r$findings$severity == "error"))
})

cat("\n== D. Analysis-readiness checks ==\n")
analysis_df <- data.frame(
  outcome = factor(c(rep("no", 36), rep("yes", 4))),
  x = c(-20:-1, 1:20),
  x_copy = c(-20:-1, 1:20),
  skew = c(rep(1, 39), 100),
  stringsAsFactors = FALSE
)
r_analysis <- dq_run_checks(analysis_df,
  c(outcome = "binary", x = "continuous", x_copy = "continuous", skew = "continuous"),
  analysis = list(outcome = "outcome", predictors = c("x", "x_copy", "skew"),
                  family = "logistic"))
ok("correlation, imbalance, skew, and separation are detected", {
  stopifnot(any(r_analysis$findings$check == "highly correlated predictors"),
            any(r_analysis$findings$check == "class imbalance"),
            any(r_analysis$findings$check == "skewed distributions"),
            any(r_analysis$findings$check == "logistic separation" &
                r_analysis$findings$severity == "error"))
})
ok("model-specific checks remain unevaluated without context", {
  r <- dq_run_checks(analysis_df, c(outcome = "binary", x = "continuous"))
  sc <- r$scorecard
  stopifnot(all(sc$status[sc$context == "analysis"] == "not evaluated"))
})
ok("modeling suitability identifies IDs and excessive missingness", {
  d2 <- data.frame(y = factor(c("n", "y", "n", "y")), id = 1:4,
                   txt = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  rr <- dq_run_checks(d2, c(y = "binary", id = "identifier", txt = "text"),
                      analysis = list(outcome = "y", predictors = c("id", "txt"), family = "logistic"))
  stopifnot(any(rr$findings$check == "modeling suitability"))
})

cat("\n== E. Human review, report, and UI wiring ==\n")
ok("review decisions can suppress a resolved issue without deleting it", {
  rr <- r_analysis
  issue <- rr$findings[rr$findings$severity == "warning", , drop = FALSE]
  stopifnot(nrow(issue) > 0)
  review <- data.frame(finding_id = issue$finding_id[[1]], status = "accepted",
                       reviewer = "data owner", review_note = "Documented in protocol",
                       reviewed_at = "2026-01-01", stringsAsFactors = FALSE)
  after <- dq_apply_reviews(rr, review)
  hit <- after$findings$finding_id == issue$finding_id[[1]]
  stopifnot(after$findings$status[hit] == "accepted",
            after$summary$open_warnings <= rr$summary$open_warnings,
            any(after$findings$finding_id == issue$finding_id[[1]]))
})
ok("Data Check UI exposes contract, readiness, review, and scorecard controls", {
  html <- paste(as.character(mod_datacheck_ui("qc")), collapse = " ")
  stopifnot(grepl("Data contract", html, fixed = TRUE),
            grepl("Analysis readiness", html, fixed = TRUE),
            grepl("Review &amp; audit", html, fixed = TRUE) || grepl("Review & audit", html, fixed = TRUE),
            grepl("22-point coverage scorecard", html, fixed = TRUE))
})
ok("Data Check server renders contract and analysis context end to end", {
  got <- NULL
  shiny::testServer(mod_datacheck_server,
    args = list(data = shiny::reactive(analysis_df),
                raw_data = shiny::reactive(analysis_df),
                types = shiny::reactive(c(outcome = "binary", x = "continuous",
                                          x_copy = "continuous", skew = "continuous")),
                outcomes = shiny::reactive("outcome"), panel = shiny::reactive(NULL)), {
      session$flushReact()
      stopifnot(!is.null(output$contract_editor), !is.null(output$readiness_editor))
      session$setInputs(readiness_outcome = "outcome",
                        readiness_predictors = c("x", "x_copy"),
                        readiness_family = "logistic")
      session$flushReact()
      got <<- output$scorecard_table
      stopifnot(!is.null(output$qc_table), !is.null(output$analysis_summary), !is.null(got))
    })
})
report_and_exit()
