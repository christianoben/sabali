# tests/test_rmcorr.R -------------------------------------------------------
# Coverage for repeated-measures correlation in the longitudinal correlation
# workflow, including reference-model matching and Tests-tab wiring.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

near <- function(a, b, tol = 1e-8) is.finite(a) && is.finite(b) && abs(a - b) <= tol

set.seed(2040)
n_subjects <- 20
n_repeats <- 4
id <- rep(seq_len(n_subjects), each = n_repeats)
time <- rep(seq_len(n_repeats), times = n_subjects)
x <- rnorm(length(id))
y <- 0.75 * x + rep(rnorm(n_subjects, sd = 1.2), each = n_repeats) +
  rnorm(length(id), sd = 0.35)

reference_rmcor <- function(x, y, id) {
  d <- data.frame(x = x, y = y, subject = factor(id))
  fit <- stats::lm(y ~ subject + x, data = d,
                   contrasts = list(subject = "contr.sum"))
  dr <- stats::drop1(fit, . ~ ., test = "F")
  row <- match("x", rownames(dr))
  ss_x <- unname(dr[["Sum of Sq"]][row])
  rss <- unname(dr[["RSS"]][1])
  slope <- unname(stats::coef(fit)[["x"]])
  list(r = sign(slope) * sqrt(ss_x / (ss_x + rss)),
       p = unname(dr[["Pr(>F)"]][row]), df = unname(fit$df.residual))
}

cat("\n== run_repeated_correlation() ==\n")
ref <- reference_rmcor(x, y, id)
rr <- run_repeated_correlation(x, y, id)
ok("rmcorr coefficient matches the subject-fixed-effects reference", {
  stopifnot(grepl("rmcorr", rr$primary$label, fixed = TRUE),
            near(rr$primary$p_value, ref$p),
            near(as.numeric(rr$primary$table$value[rr$primary$table$statistic == "r_rm"]),
                 round(ref$r, 3), 1e-3),
            rr$primary$table$value[rr$primary$table$statistic == "df"] == ref$df)
})
ok("rmcorr reports no misleading ordinary-correlation alternative", {
  stopifnot(is.null(rr$alternative), rr$recommended == "primary",
            !is.null(rr$assumptions$repeated_subjects))
})
ok("rmcorr rejects subjects without repeated complete pairs", {
  keep <- !(id == 1 & time != 1)
  err <- tryCatch(run_repeated_correlation(x[keep], y[keep], id[keep]),
                  error = function(e) conditionMessage(e))
  stopifnot(grepl("at least two complete paired observations", err))
})

cat("\n== repeated correlation routing ==\n")
df <- data.frame(id = id, time = time, x = x, y = y)
ok("guided router dispatches the repeated correlation route", {
  routed <- run_guided_test(df, list(family = "correlation", x = "x", y = "y",
                                     repeated = TRUE, subject = "id"))
  stopifnot(grepl("rmcorr", routed$primary$label, fixed = TRUE),
            near(routed$primary$p_value, rr$primary$p_value))
})
ok("repeated correlation validates its subject role", {
  err <- tryCatch(run_guided_test(df, list(family = "correlation", x = "x",
                                           y = "y", repeated = TRUE,
                                           subject = "x")),
                  error = function(e) conditionMessage(e))
  stopifnot(grepl("distinct|subject", err, ignore.case = TRUE))
})

cat("\n== Tests-tab wiring ==\n")
ok("longitudinal correlation controls run rmcorr", {
  tp <- c(id = "identifier", time = "nominal", x = "continuous", y = "continuous")
  rendered <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = shiny::reactive(df), types = shiny::reactive(tp),
                outcomes = shiny::reactive(c("x", "y")),
                panel = shiny::reactive(list(id = "id", time = "time", event = NULL)),
                add_report = function(x) NULL), {
      session$setInputs(family = "correlation", corr_design = "repeated",
                        x = "x", y = "y", subject = "id")
      session$flushReact()
      session$setInputs(run = 1)
      session$flushReact()
      rendered <<- output$primary_tbl
    })
  stopifnot(is.character(rendered), grepl("rmcorr", rendered, fixed = TRUE))
})

report_and_exit()
