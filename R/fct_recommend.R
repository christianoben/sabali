# fct_recommend.R -----------------------------------------------------------
# The guided decision rules for descriptives and charts (Sections 3-4).
# Given the variables the user selected and their measurement types, return the
# recommended chart types (first = default) plus a plain-language rationale.

#' Recommend chart types from the selected variables' measurement types.
#'
#' @param types Character vector of measurement types for the selected variables.
#' @param skewed Optional logical: is the (single) numeric variable skewed?
#' @return list(default, options, rationale)
recommend_charts <- function(types, skewed = FALSE) {
  n <- length(types)
  num <- sum(is_numeric_type(types))
  cat <- sum(is_categorical_type(types))
  has_date <- any(types == "datetime")

  # ---- one variable -------------------------------------------------------
  if (n == 1) {
    t <- types[1]
    if (is_numeric_type(t)) {
      opts <- c("histogram", "density", "boxplot", "violin", "raincloud",
                "qq", "ecdf")
      rat <- if (skewed)
        "One numeric variable that looks skewed - a histogram or boxplot shows the shape better than a summary bar."
      else
        "One numeric variable - a histogram shows its distribution; a Q-Q plot checks normality."
      return(list(default = "histogram", options = opts, rationale = rat))
    }
    if (is_categorical_type(t)) {
      return(list(default = "bar",
                  options = c("bar", "pareto", "pie"),
                  rationale = "One categorical variable - a bar chart of counts is clearest (pie charts are harder to compare)."))
    }
    if (has_date) {
      return(list(default = "line",
                  options = c("line"),
                  rationale = "A date variable - a timeline of counts over time."))
    }
  }

  # ---- two variables ------------------------------------------------------
  if (n == 2) {
    if (num == 2) {
      return(list(default = "scatter",
                  options = c("scatter", "density2d"),
                  rationale = "Two numeric variables - a scatter plot shows their relationship (add a trend line to judge linearity)."))
    }
    if (num == 1 && cat == 1) {
      opts <- c("box_by_group", "violin_by_group", "raincloud_by_group",
                "mean_ci_bar", "jitter")
      rat <- if (skewed)
        "A numeric outcome across groups, and it is skewed - grouped boxplots or violins are safer than a bar of means."
      else
        "A numeric outcome across groups - grouped boxplots compare the distributions directly."
      return(list(default = "box_by_group", options = opts, rationale = rat))
    }
    if (cat == 2) {
      return(list(default = "grouped_bar",
                  options = c("grouped_bar", "stacked_bar", "mosaic", "heatmap"),
                  rationale = "Two categorical variables - a grouped bar chart compares the cross-tab; a stacked bar shows composition."))
    }
    if (num == 1 && has_date) {
      return(list(default = "line",
                  options = c("line", "run_chart"),
                  rationale = "A numeric measure over time - a line chart reveals the trend."))
    }
  }

  # ---- three or more ------------------------------------------------------
  if (n >= 3) {
    if (num >= 3 && cat == 0) {
      return(list(default = "corr_heatmap",
                  options = c("corr_heatmap", "scatter_matrix"),
                  rationale = "Several numeric variables - a correlation heatmap summarises all pairwise relationships at once."))
    }
    return(list(default = "box_by_group",
                options = c("box_by_group", "grouped_bar", "scatter"),
                rationale = "Multiple variables - pick two to focus on, or facet a bivariate chart by a third categorical variable."))
  }

  list(default = NA_character_, options = character(0),
       rationale = "Select one or more analysable variables to see chart suggestions.")
}

# Friendly labels for chart keys (for dropdowns and report captions).
CHART_LABELS <- c(
  histogram = "Histogram", density = "Density plot", boxplot = "Boxplot",
  violin = "Violin plot", raincloud = "Raincloud plot", qq = "Q-Q (normality) plot",
  ecdf = "Cumulative distribution (ECDF)", bar = "Bar chart", pareto = "Pareto chart",
  pie = "Pie chart", scatter = "Scatter plot", density2d = "2D density plot",
  box_by_group = "Boxplot by group", violin_by_group = "Violin by group",
  raincloud_by_group = "Raincloud by group", mean_ci_bar = "Mean with 95% CI",
  jitter = "Jitter / strip plot", grouped_bar = "Grouped bar chart",
  stacked_bar = "Stacked bar chart", mosaic = "Mosaic plot", heatmap = "Contingency heatmap",
  line = "Line chart", run_chart = "Run chart", corr_heatmap = "Correlation heatmap",
  scatter_matrix = "Scatterplot matrix",
  ts_acf = "Autocorrelation (ACF)", ts_pacf = "Partial autocorrelation (PACF)",
  ts_decomposition = "Seasonal decomposition", ts_seasonal = "Seasonal subseries plot",
  ts_spectrum = "Spectral density (periodogram)"
)
