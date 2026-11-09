# install_dependencies.R ----------------------------------------------------
# Run once on the server (or your machine) before launching the app:
#   Rscript install_dependencies.R

required <- c(
  "shiny",      # web app framework
  "ggplot2",    # charts
  "DT",         # interactive data preview table
  "rio",        # read any file format (wraps foreign, haven, readxl, ...)
  "foreign",    # SPSS/Stata/SAS import (used by rio; requested explicitly)
  "haven",      # modern SPSS/Stata/SAS import + value labels
  "readxl",     # Excel
  "officer",    # build the Word document
  "flextable",  # nicely formatted tables in the Word document
  "MASS",       # robust regression, negative-binomial GLM, GLMM support
  "quantreg",   # quantile regression
  "glmnet",     # LASSO / ridge (L1 / L2) penalised regression + cross-validation
  "pls",        # partial least squares regression
  "lmtest",     # Breusch-Pagan regression diagnostic
  "geepack",    # generalized estimating equations
  "lme4",        # linear and generalized linear mixed models
  "survival"     # Kaplan-Meier and Cox proportional-hazards models
)

optional <- c(
  "openxlsx",  # writes the Excel fixture used by the import validation suite
  "ggdist",     # true raincloud plots (falls back to violin+jitter if absent)
  "GGally",     # scatterplot matrix (falls back to a message if absent)
  "nortest",    # Anderson-Darling normality for very large n (Phase 2);
                # if absent, large-n samples rely on the CLT note instead
  "httr2",      # AI interpretation panel's optional LLM backend (offline works
                # without it); needed only to call an external model endpoint
  "commonmark", # renders the AI interpretation Markdown if shiny::markdown() is
                # unavailable on an older Shiny
  "renv",       # reproducible dependency restore from renv.lock
  "shinytest2", # browser smoke tests (requires a supported browser runtime)
  "covr",       # measured coverage gate
  "lintr",      # static lint gate
  "styler",     # style verification gate
  "urca",       # exact unit-root tests + Zivot-Andrews break test (time series)
  "rugarch",    # GARCH volatility model (time series)
  "mice",       # multiple imputation for missing data (Regression tab)
  "lavaan",     # observed-variable path analysis / mediation (Advanced models)
  "survey",     # design-based estimation for weighted / stratified / clustered
                # samples (the Data tab's complex-sample design declaration)
  "shinymanager", # login layer when serving over a network (see DEPLOYMENT.md)
  "getPass"     # hidden password entry for setup_auth.R (optional convenience)
)

install_if_missing <- function(pkgs) {
  to_get <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(to_get)) {
    message("Installing: ", paste(to_get, collapse = ", "))
    install.packages(to_get, repos = "https://cloud.r-project.org")
  } else {
    message("All present: ", paste(pkgs, collapse = ", "))
  }
}

install_if_missing(required)
install_if_missing(optional)
message("Done. Launch with:  shiny::runApp('.')")
