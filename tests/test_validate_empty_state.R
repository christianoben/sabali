# Regression coverage for rendering the Tests tab before a dataset is loaded.
local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

cat("\n== empty Tests-tab state ==\n")
ok("Tests controls render a friendly prompt with no dataset selected", {
  html <- ""
  shiny::testServer(mod_tests_server,
    args = list(data = shiny::reactive(NULL),
                types = shiny::reactive(NULL),
                outcomes = shiny::reactive(character(0)),
                add_report = function(x) NULL), {
      session$flushReact()
      html <<- html_text(output$controls)
    })
  stopifnot(grepl("Load a dataset", html, fixed = TRUE),
            !grepl("argument is of length zero", html, fixed = TRUE))
})

ok("Tests controls still render a family after data is loaded", {
  d <- data.frame(y = 1:6, group = factor(c("a", "b", "a", "b", "a", "b")))
  tp <- c(y = "continuous", group = "binary")
  html <- ""
  shiny::testServer(mod_tests_server,
    args = list(data = shiny::reactive(d), types = shiny::reactive(tp),
                outcomes = shiny::reactive("y"),
                add_report = function(x) NULL), {
      session$setInputs(family = "location")
      session$flushReact()
      html <<- html_text(output$controls)
    })
  stopifnot(grepl("Numeric outcome", html, fixed = TRUE))
})

report_and_exit()
