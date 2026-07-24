# tests/test_datacheck.R ----------------------------------------------------
# Tab 0 "Data check": the data-quality engine (fct_dataqc.R) and the module
# wiring (mod_datacheck.R). Every check is exercised both ways - it must FIRE
# on a crafted trigger and stay SILENT on clean data - so a future change that
# breaks or over-fires a check is caught.
#
# Run:  Rscript tests/test_datacheck.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# Does the result contain a finding for `check` (optionally at `severity`)?
fired <- function(res, check, severity = NULL) {
  f <- res$findings
  rows <- f[f$check == check & f$severity != "pass", , drop = FALSE]
  if (!is.null(severity)) rows <- rows[rows$severity == severity, , drop = FALSE]
  nrow(rows) > 0
}
passed <- function(res, check) {
  any(res$findings$check == check & res$findings$severity == "pass")
}

# A clean, well-formed dataset (enough rows for the outlier check).
set.seed(1)
clean <- data.frame(
  pid   = 1:40,
  age   = round(rnorm(40, 45, 8)),
  sex   = factor(rep(c("Female", "Male"), 20)),
  visits = rpois(40, 2),
  score = round(rnorm(40, 10, 2), 1),
  stringsAsFactors = FALSE
)
clean_types <- c(pid = "identifier", age = "continuous", sex = "binary",
                 visits = "count", score = "continuous")

cat("\n== A. Clean data raises no errors or warnings [GUARD] ==\n")
rc <- dq_run_checks(clean, clean_types, panel = list(id = "pid"))
ok("clean data: zero errors", stopifnot(rc$summary$errors == 0))
ok("clean data: zero warnings", stopifnot(rc$summary$warnings == 0))
ok("clean data: checks were actually run", stopifnot(rc$n_checks >= 15))
ok("clean data: key checks explicitly pass",
   stopifnot(passed(rc, "duplicate rows"), passed(rc, "constant columns"),
             passed(rc, "column names unique")))

cat("\n== B. Structural checks fire on triggers ==\n")
ok("duplicate column names -> error", {
  d <- data.frame(a = 1:5, a = 6:10, check.names = FALSE)
  stopifnot(fired(dq_run_checks(d), "column names unique", "error"))
})
ok("padded column name -> warning", {
  d <- data.frame(x = 1:5); names(d) <- "x "
  stopifnot(fired(dq_run_checks(d), "column names valid", "warning"))
})
ok("duplicate rows -> warning", {
  d <- rbind(clean, clean[1, ])
  stopifnot(fired(dq_run_checks(d, clean_types), "duplicate rows", "warning"))
})
ok("entirely-missing row -> warning", {
  d <- clean; d[3, ] <- NA
  stopifnot(fired(dq_run_checks(d, clean_types), "empty rows", "warning"))
})
ok("identical columns -> redundant note", {
  d <- clean; d$score2 <- d$score
  stopifnot(fired(dq_run_checks(d, clean_types), "redundant columns"))
})

cat("\n== C. Missingness checks fire at the right severity ==\n")
ok("fully empty column -> error", {
  d <- clean; d$blank <- NA
  stopifnot(fired(dq_run_checks(d, clean_types), "empty columns", "error"))
})
ok(">50% missing column -> error", {
  d <- clean; d$score[1:22] <- NA
  stopifnot(fired(dq_run_checks(d, clean_types), "column missingness", "error"))
})
ok("~30% missing column -> warning (not error)", {
  d <- clean; d$score[1:13] <- NA
  r <- dq_run_checks(d, clean_types)
  stopifnot(fired(r, "column missingness", "warning"),
            !fired(r, "column missingness", "error"))
})

cat("\n== D. Value-content checks ==\n")
ok("constant column -> warning", {
  d <- clean; d$grp <- "only"
  stopifnot(fired(dq_run_checks(d, c(clean_types, grp = "nominal")), "constant columns"))
})
ok("disguised-missing sentinels -> warning", {
  d <- clean; d$code <- c(rep("A", 38), "N/A", "-")
  stopifnot(fired(dq_run_checks(d, c(clean_types, code = "nominal")), "disguised missing"))
})
ok("numbers stored as text -> warning", {
  d <- clean; d$num_text <- as.character(1:40)
  stopifnot(fired(dq_run_checks(d, c(clean_types, num_text = "nominal")),
                  "numbers stored as text"))
})
ok("case/whitespace-inconsistent categories -> warning", {
  d <- clean; d$grp <- rep(c("Yes", "yes ", "No", "no"), 10)
  stopifnot(fired(dq_run_checks(d, c(clean_types, grp = "nominal")),
                  "inconsistent categories"))
})
ok("high-cardinality category flagged as possible ID", {
  d <- data.frame(label = as.character(1:60), y = rnorm(60), stringsAsFactors = FALSE)
  stopifnot(fired(dq_run_checks(d, c(label = "nominal", y = "continuous")),
                  "high-cardinality category"))
})
ok("rare categories -> note", {
  d <- clean; d$grp <- c("common", rep("common", 36), "r1", "r2", "r3")
  stopifnot(fired(dq_run_checks(d, c(clean_types, grp = "nominal")), "rare categories"))
})
ok("binary typed column with 3 levels -> warning", {
  d <- clean; d$sex <- factor(c(rep(c("F", "M"), 19), "U", "U"))
  stopifnot(fired(dq_run_checks(d, clean_types), "binary has two levels"))
})

cat("\n== E. Numeric / date checks ==\n")
ok("infinite value -> error", {
  d <- clean; d$score[1] <- Inf
  stopifnot(fired(dq_run_checks(d, clean_types), "infinite values", "error"))
})
ok("negative count -> error", {
  d <- clean; d$visits[1] <- -2
  stopifnot(fired(dq_run_checks(d, clean_types), "count is non-negative", "error"))
})
ok("non-integer count -> warning", {
  d <- clean; d$visits <- as.numeric(d$visits); d$visits[1] <- 2.5
  stopifnot(fired(dq_run_checks(d, clean_types), "count is integer", "warning"))
})
ok("gross outlier -> note", {
  d <- clean; d$age[1] <- 9999
  stopifnot(fired(dq_run_checks(d, clean_types), "outliers"))
})
ok("future date -> warning", {
  d <- data.frame(dt = as.Date("2025-01-01") + c(0:38, 4000), y = rnorm(40))
  stopifnot(fired(dq_run_checks(d, c(dt = "datetime", y = "continuous")),
                  "implausible dates", "warning"))
})
ok("pre-1900 date -> note", {
  d <- data.frame(dt = c(as.Date("1850-01-01"), as.Date("2020-01-01") + 0:38),
                  y = rnorm(40))
  stopifnot(fired(dq_run_checks(d, c(dt = "datetime", y = "continuous")),
                  "implausible dates", "note"))
})

cat("\n== F. Panel / ID structure ==\n")
panel_df <- data.frame(
  id = rep(1:10, each = 3), t = rep(1:3, 10), y = rnorm(30))
ok("duplicated (id, time) -> error", {
  d <- panel_df; d$t[2] <- 1   # id 1 now has t = 1,1,3
  r <- dq_run_checks(d, c(id = "identifier", t = "ordinal", y = "continuous"),
                     panel = list(id = "id", time = "t"))
  stopifnot(fired(r, "unique id-time", "error"))
})
ok("clean panel has no id-time duplication", {
  r <- dq_run_checks(panel_df, c(id = "identifier", t = "ordinal", y = "continuous"),
                     panel = list(id = "id", time = "t"))
  stopifnot(!fired(r, "unique id-time"))
})
ok("single-observation subjects -> note", {
  d <- rbind(panel_df, data.frame(id = 99, t = 1, y = 0))
  r <- dq_run_checks(d, c(id = "identifier", t = "ordinal", y = "continuous"),
                     panel = list(id = "id", time = "t"))
  stopifnot(fired(r, "single-observation subjects", "note"))
})

cat("\n== G. Robustness / edge cases ==\n")
ok("NULL dataset -> single error, no crash",
   stopifnot(dq_run_checks(NULL)$summary$errors == 1))
ok("zero-row dataset -> error, no crash",
   stopifnot(dq_run_checks(clean[0, ], clean_types)$summary$errors >= 1))
ok("all-NA column does not abort other checks", {
  d <- clean; d$blank <- NA; d$grp <- "const"
  r <- dq_run_checks(d, c(clean_types, blank = "nominal", grp = "nominal"))
  stopifnot(fired(r, "empty columns"), fired(r, "constant columns"))
})

cat("\n== H. Text report (.txt) ==\n")
rep_txt <- dq_report_text(dq_run_checks(clean, clean_types),
                          dataset_name = "clean.csv",
                          generated_at = "2026-07-23 16:00:00")
ok("report is a single string", stopifnot(is.character(rep_txt), length(rep_txt) == 1))
ok("report header shows dataset + fixed timestamp",
   stopifnot(grepl("clean.csv", rep_txt, fixed = TRUE),
             grepl("2026-07-23 16:00:00", rep_txt, fixed = TRUE)))
ok("report states dimensions and a result line",
   stopifnot(grepl("40 rows x 5 columns", rep_txt, fixed = TRUE),
             grepl("error(s),", rep_txt, fixed = TRUE)))
ok("dirty report lists a specific finding message", {
  d <- clean; d$visits[1] <- -1
  txt <- dq_report_text(dq_run_checks(d, clean_types), "d.csv", "t")
  stopifnot(grepl("negative value", txt))
})

cat("\n== I. Module wiring (mod_datacheck_server) ==\n")
ok("qc summary + findings render; report content non-empty", {
  captured <- NULL
  shiny::testServer(mod_datacheck_server,
    args = list(data = reactive(clean), types = reactive(clean_types),
                panel = reactive(list(id = "pid")),
                data_name = reactive("clean.csv")), {
      session$flushReact()
      captured <<- qc()
      out_tbl <- output$qc_table   # forces the DT render
      stopifnot(!is.null(out_tbl))
    })
  stopifnot(captured$summary$n_rows == 40, captured$summary$errors == 0)
})
ok("subset builder keeps only the selected columns", {
  got <- NULL
  shiny::testServer(mod_datacheck_server,
    args = list(data = reactive(clean), types = reactive(clean_types),
                panel = reactive(NULL), data_name = reactive("clean.csv")), {
      session$setInputs(cols = c("pid", "age"))
      session$flushReact()
      got <<- subset_df()
    })
  stopifnot(identical(names(got), c("pid", "age")), nrow(got) == 40)
})
ok("subset default is every column, and downloads as valid CSV", {
  d1 <- NULL
  shiny::testServer(mod_datacheck_server,
    args = list(data = reactive(clean), types = reactive(clean_types),
                panel = reactive(NULL), data_name = reactive("clean.csv")), {
      session$flushReact()
      d1 <<- subset_df()
    })
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(d1, tmp, row.names = FALSE, na = "")
  back <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  stopifnot(ncol(d1) == ncol(clean), nrow(back) == nrow(clean),
            identical(names(back), names(clean)))
})

report_and_exit()
