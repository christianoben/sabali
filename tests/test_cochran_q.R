# tests/test_cochran_q.R ----------------------------------------------------
# Coverage for repeated binary outcomes: Cochran's Q and its guided route.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

near <- function(a, b, tol = 1e-8) is.finite(a) && is.finite(b) && abs(a - b) <= tol

set.seed(2026)
n_subjects <- 24
conditions <- c("baseline", "week4", "week8")
binary_matrix <- cbind(
  baseline = rbinom(n_subjects, 1, 0.25),
  week4 = rbinom(n_subjects, 1, 0.50),
  week8 = rbinom(n_subjects, 1, 0.75)
)
long <- data.frame(
  id = rep(seq_len(n_subjects), times = length(conditions)),
  time = rep(conditions, each = n_subjects),
  response = as.vector(binary_matrix),
  stringsAsFactors = FALSE
)

cat("\n== run_cochran_q() ==\n")
r <- run_cochran_q(long$response, long$time, long$id, success = "1")
ref <- suppressWarnings(stats::friedman.test(binary_matrix))
ok("Cochran's Q statistic and p-value match the binary Friedman reference", {
  stopifnot(grepl("Cochran's Q", r$primary$label), is.null(r$alternative),
            near(r$primary$p_value, ref$p.value),
            near(as.numeric(r$primary$table$value[r$primary$table$statistic == "Q"]),
                 unname(ref$statistic), 1e-3))
})
ok("complete subject and condition counts are reported", {
  stopifnot(r$primary$table$value[r$primary$table$statistic == "complete subjects"] == n_subjects,
            r$primary$table$value[r$primary$table$statistic == "conditions"] == 3,
            identical(r$posthoc$label, "Pairwise McNemar tests"),
            nrow(r$posthoc$table) == 3,
            all(r$posthoc$table[["p-adjusted"]] >= r$posthoc$table[["p-value"]] |
                is.na(r$posthoc$table[["p-adjusted"]])))
})
ok("incomplete subject blocks are excluded safely", {
  incomplete <- long[!(long$id == 1 & long$time == "week8"), , drop = FALSE]
  ri <- run_cochran_q(incomplete$response, incomplete$time, incomplete$id,
                      success = "1")
  ref_i <- suppressWarnings(stats::friedman.test(binary_matrix[-1, , drop = FALSE]))
  stopifnot(near(ri$primary$p_value, ref_i$p.value),
            ri$primary$table$value[ri$primary$table$statistic == "complete subjects"] == n_subjects - 1)
})
ok("invalid repeated-binary structures fail clearly", {
  two_conditions <- long[long$time != "week8", , drop = FALSE]
  duplicate <- rbind(long, long[1, , drop = FALSE])
  err_two <- tryCatch(run_cochran_q(two_conditions$response,
                                    two_conditions$time,
                                    two_conditions$id),
                      error = function(e) conditionMessage(e))
  err_dup <- tryCatch(run_cochran_q(duplicate$response,
                                    duplicate$time,
                                    duplicate$id),
                      error = function(e) conditionMessage(e))
  stopifnot(grepl("at least three", err_two), grepl("one outcome per subject", err_dup))
})

cat("\n== Cochran's Q routing ==\n")
ok("Cochran's Q is opt-in in family availability", {
  tp <- c(response = "binary", id = "identifier", time = "datetime")
  stopifnot(!("cochran_q" %in% available_test_families(tp)),
            "cochran_q" %in% available_test_families(tp, include_cochran_q = TRUE))
})
ok("guided router dispatches Cochran's Q", {
  spec <- list(family = "cochran_q", outcome = "response",
               within = "time", subject = "id", success = "1")
  routed <- run_guided_test(long, spec)
  direct <- run_cochran_q(long$response, long$time, long$id, success = "1")
  stopifnot(near(routed$primary$p_value, direct$primary$p_value),
            grepl("Cochran's Q", routed$primary$label))
})
ok("the same longitudinal route uses McNemar for two conditions", {
  two_time <- long[long$time != "week8", , drop = FALSE]
  routed <- run_guided_test(two_time,
    list(family = "cochran_q", outcome = "response", within = "time",
         subject = "id", success = "1"))
  stopifnot(grepl("McNemar", routed$primary$label), is.null(routed$alternative))
})
ok("categorical family exposes the repeated-binary Cochran route", {
  tp <- c(id = "identifier", time = "nominal", response = "binary")
  rendered <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = shiny::reactive(long), types = shiny::reactive(tp),
                outcomes = shiny::reactive("response"),
                panel = shiny::reactive(list(id = "id", time = "time", event = NULL)),
                add_report = function(x) NULL), {
      session$setInputs(family = "categorical", cat_design = "repeated_binary",
                        outcome = "response", cochran_within = "time",
                        subject = "id", cochran_success = "1")
      session$flushReact()
      session$setInputs(run = 1)
      session$flushReact()
      rendered <<- output$primary_tbl
    })
  stopifnot(is.character(rendered), grepl("Cochran", rendered))
})
ok("categorical family shows the longitudinal design before outcome selection", {
  tp <- c(id = "identifier", time = "nominal", response = "binary")
  controls <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = shiny::reactive(long), types = shiny::reactive(tp),
                outcomes = shiny::reactive(character(0)),
                panel = shiny::reactive(list(id = "id", time = "time", event = NULL)),
                add_report = function(x) NULL), {
      session$setInputs(family = "categorical")
      session$flushReact()
      controls <<- output$controls$html
    })
  stopifnot(is.character(controls),
            grepl("Repeated binary outcome across", controls),
            grepl("Select a binary candidate outcome", controls))
})
ok("Cochran's Q requires a binary outcome and three conditions", {
  nonbinary <- long; nonbinary$response[1] <- 2
  spec <- list(family = "cochran_q", outcome = "response",
               within = "time", subject = "id")
  err <- tryCatch(run_guided_test(nonbinary, spec),
                  error = function(e) conditionMessage(e))
  stopifnot(grepl("binary outcome", err))
})

report_and_exit()
