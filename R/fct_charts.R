# fct_charts.R --------------------------------------------------------------
# The chart engine: a single aesthetic-customisation pipeline that turns a
# chart "spec" (a plain list of choices) into a ggplot2 object. This is the
# headline asset of Phase 1 - every chart in the app is produced here, so
# styling is consistent and fully controllable from the UI.

library(ggplot2)

# --- Curated, colour-blind-friendly palettes -------------------------------
# A named palette of NULL means "use viridis" (handled specially below).
# The default palette is QUALITATIVE (distinct hues), led by the brand teal, so
# adjacent categories are easy to tell apart. Single-series charts use the first
# colour (the brand teal) via base_col().
CHART_PALETTES <- list(
  "Default"     = c("#018786", "#E76F51", "#E9C46A", "#457B9D", "#8AB17D", "#9B5DE5"),
  "Okabe-Ito"   = c("#0072B2", "#E69F00", "#009E73", "#CC79A7",
                    "#56B4E9", "#D55E00", "#F0E442", "#000000"),
  "Viridis"     = NULL,
  "Muted"       = c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3", "#937860"),
  "Greyscale"   = c("#222222", "#555555", "#888888", "#AAAAAA", "#CCCCCC")
)

CHART_THEMES <- c("Minimal" = "minimal", "Classic" = "classic",
                  "Light" = "light", "Grey" = "grey", "Void" = "void")

# Default spec: every field the pipeline understands, with safe defaults.
default_chart_spec <- function() {
  list(
    type = "histogram", x = NULL, y = NULL, group = NULL, facet = NULL,
    title = "", subtitle = "", caption = "", xlab = NULL, ylab = NULL,
    palette = "Default", theme_base = "minimal", base_size = 18,
    font_family = "", legend_pos = "right", flip = FALSE, show_grid = TRUE,
    bins = 30, alpha = 0.85, point_size = 2, add_trend = TRUE,
    add_ref_line = TRUE, stacked_fill = FALSE, data_labels = TRUE,
    ts_time = NULL, ts_frequency = NULL
  )
}

# Data-label styling, shared by every chart type that can carry value labels.
.LABEL_SIZE <- 3.1
# Format one value for an on-chart label: integers stay whole, other numbers
# get up to two decimals (kept tidy so labels do not crowd the plot).
.label_fmt <- function(v) {
  vapply(v, function(x) {
    if (!is.finite(x)) return("")
    if (x == round(x) && abs(x) < 1e7) return(formatC(x, format = "d", big.mark = ","))
    formatC(x, format = "f", digits = 2, big.mark = ",")
  }, character(1))
}

# Apply a discrete colour/fill scale from the chosen palette.
.apply_palette <- function(p, name, aesthetics = c("fill", "colour")) {
  if (identical(name, "Viridis") || is.null(CHART_PALETTES[[name]])) {
    if ("fill" %in% aesthetics)   p <- p + scale_fill_viridis_d(option = "D")
    if ("colour" %in% aesthetics) p <- p + scale_colour_viridis_d(option = "D")
    return(p)
  }
  cols <- CHART_PALETTES[[name]]
  # Use the palette's own distinct colours in order (recycled if there are more
  # groups than swatches). This keeps adjacent categories easy to tell apart,
  # unlike ramping the palette into 24 near-identical shades.
  vals <- rep_len(cols, max(length(cols), 24L))
  if ("fill" %in% aesthetics)
    p <- p + scale_fill_manual(values = vals, na.value = "grey70")
  if ("colour" %in% aesthetics)
    p <- p + scale_colour_manual(values = vals, na.value = "grey70")
  p
}

# Build the theme from the spec.
.build_theme <- function(spec) {
  base <- switch(spec$theme_base,
                 minimal = theme_minimal, classic = theme_classic,
                 light = theme_light, grey = theme_grey, void = theme_void,
                 theme_minimal)
  fam <- if (nzchar(spec$font_family)) spec$font_family else NULL
  th <- base(base_size = spec$base_size, base_family = fam %||% "")
  th <- th + theme(
    legend.position = spec$legend_pos,
    plot.title    = element_text(face = "bold"),
    plot.caption  = element_text(colour = "grey40")
  )
  if (!spec$show_grid) {
    th <- th + theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank())
  }
  th
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# A friendly full-panel message (used when a chart can't be drawn as asked).
.message_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 4.2, lineheight = 1.1) +
    theme_void()
}

# Apply labels, palette, theme, orientation - shared final step.
.finish <- function(p, spec, palette_aes = c("fill", "colour")) {
  # Faceting splits the plotted data into panels, so the facet variable must be
  # present in the layer data. Charts that summarise their data into totals
  # (plain bar/pareto without a group, pie, contingency/correlation heatmaps,
  # mean-CI) drop it, so faceting them would crash. Fail safely with guidance
  # instead of surfacing a raw ggplot error.
  if (!is.null(spec$facet) && nzchar(spec$facet)) {
    pdata <- tryCatch(p$data, error = function(e) NULL)
    if (!is.data.frame(pdata) || !(spec$facet %in% names(pdata))) {
      return(.message_plot(paste0(
        "Faceting by '", spec$facet, "' isn't available for this chart,\n",
        "because it summarises the data into totals.\n\n",
        "To split into panels, either add a Colour / group by variable,\n",
        "choose a per-observation chart (histogram, boxplot, scatter),\n",
        "or set 'Facet by' back to (none).")))
    }
  }

  p <- p + labs(
    title    = if (nzchar(spec$title))    spec$title    else NULL,
    subtitle = if (nzchar(spec$subtitle)) spec$subtitle else NULL,
    caption  = if (nzchar(spec$caption))  spec$caption  else NULL,
    x = spec$xlab %||% spec$x, y = spec$ylab %||% spec$y
  )
  p <- .apply_palette(p, spec$palette, palette_aes)
  p <- p + .build_theme(spec)
  if (isTRUE(spec$flip)) p <- p + coord_flip()
  if (!is.null(spec$facet) && nzchar(spec$facet))
    p <- p + facet_wrap(stats::as.formula(paste("~", "`", spec$facet, "`", sep = "")))
  p
}

#' Build a ggplot2 chart from a data frame and a spec.
build_chart <- function(df, spec) {
  spec <- utils::modifyList(default_chart_spec(), spec)
  x <- spec$x; y <- spec$y; g <- spec$group

  p <- switch(
    spec$type,

    histogram = if (has_group(g))
      ggplot(df, aes(x = .data[[x]], fill = .data[[g]])) +
        geom_histogram(bins = spec$bins, colour = "white", alpha = spec$alpha,
                       position = "identity")
      else ggplot(df, aes(x = .data[[x]])) +
        geom_histogram(bins = spec$bins, fill = base_col(spec), colour = "white",
                       alpha = spec$alpha),

    density = if (has_group(g))
      ggplot(df, aes(x = .data[[x]], fill = .data[[g]])) +
        geom_density(alpha = spec$alpha)
      else ggplot(df, aes(x = .data[[x]])) +
        geom_density(fill = base_col(spec), alpha = spec$alpha),

    boxplot = if (has_group(g))
      ggplot(df, aes(x = .data[[g]], y = .data[[x]], fill = .data[[g]])) +
        geom_boxplot(alpha = spec$alpha)
      else ggplot(df, aes(y = .data[[x]])) +
        geom_boxplot(fill = base_col(spec), alpha = spec$alpha, width = 0.4),

    violin = if (has_group(g))
      ggplot(df, aes(x = .data[[g]], y = .data[[x]], fill = .data[[g]])) +
        geom_violin(alpha = spec$alpha) +
        geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA)
      else ggplot(df, aes(x = factor(1), y = .data[[x]])) +
        geom_violin(fill = base_col(spec), alpha = spec$alpha) +
        geom_boxplot(width = 0.1, outlier.shape = NA),

    raincloud = .raincloud(df, x, group = if (has_group(g)) g else NULL, spec),

    qq = if (has_group(g))
      ggplot(df, aes(sample = .data[[x]], colour = .data[[g]])) +
        stat_qq() + stat_qq_line()
      else ggplot(df, aes(sample = .data[[x]])) +
        stat_qq(colour = base_col(spec)) + stat_qq_line(colour = "grey30"),

    ecdf = if (has_group(g))
      ggplot(df, aes(x = .data[[x]], colour = .data[[g]])) +
        stat_ecdf(geom = "step", linewidth = 0.9)
      else ggplot(df, aes(x = .data[[x]])) +
        stat_ecdf(geom = "step", colour = base_col(spec), linewidth = 0.9),

    bar = .bar_count(df, x, spec, pareto = FALSE, group = g),
    pareto = .bar_count(df, x, spec, pareto = TRUE, group = g),

    pie = .pie(df, x, spec),

    scatter = {
      pp <- ggplot(df, aes(x = .data[[x]], y = .data[[y]]))
      if (!is.null(g) && nzchar(g)) pp <- pp + aes(colour = .data[[g]])
      pp <- pp + geom_point(size = spec$point_size, alpha = spec$alpha)
      if (isTRUE(spec$add_trend)) pp <- pp + geom_smooth(method = "lm", se = TRUE, formula = y ~ x)
      pp
    },

    density2d = ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(alpha = 0.3, size = spec$point_size) +
      geom_density_2d(colour = base_col(spec)),

    box_by_group = ggplot(df, aes(x = .data[[g]], y = .data[[x]], fill = .data[[g]])) +
      geom_boxplot(alpha = spec$alpha),

    violin_by_group = ggplot(df, aes(x = .data[[g]], y = .data[[x]], fill = .data[[g]])) +
      geom_violin(alpha = spec$alpha) +
      geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA),

    raincloud_by_group = .raincloud(df, x, group = g, spec),

    mean_ci_bar = .mean_ci(df, x, g, spec),

    jitter = ggplot(df, aes(x = .data[[g]], y = .data[[x]], colour = .data[[g]])) +
      geom_jitter(width = 0.2, height = 0, alpha = spec$alpha, size = spec$point_size),

    grouped_bar = .grouped_bar(df, x, g, spec,
                               stack_pos = if (isTRUE(spec$stacked_fill)) "fill" else "dodge"),

    stacked_bar = .grouped_bar(df, x, g, spec,
                               stack_pos = if (isTRUE(spec$stacked_fill)) "fill" else "stack"),

    mosaic = ggplot(df, aes(x = .data[[x]], fill = .data[[g]])) +
      geom_bar(position = "fill", alpha = spec$alpha) +
      labs(y = "proportion"),

    heatmap = .contingency_heat(df, x, g, spec),

    line = .line_chart(df, x, y, g, spec),
    run_chart = .run_chart(df, x, y, spec),

    corr_heatmap = .corr_heatmap(df, spec),
    scatter_matrix = .scatter_matrix(df, spec),

    ts_acf = .ts_chart(df, spec, "acf"),
    ts_pacf = .ts_chart(df, spec, "pacf"),
    ts_decomposition = .ts_chart(df, spec, "decomposition"),
    ts_seasonal = .ts_chart(df, spec, "seasonal"),
    ts_spectrum = .ts_chart(df, spec, "spectrum"),

    ggplot(df) + annotate("text", x = 1, y = 1,
                          label = paste("Chart type not available:", spec$type))
  )

  # heatmap and corr_heatmap map a *continuous* value to fill and define their
  # own gradient scale, so no discrete palette scale must be applied to them.
  palette_aes <- if (spec$type %in% c("heatmap", "corr_heatmap"))
    character(0)
  else if (spec$type %in% c("scatter", "jitter", "ecdf", "density2d",
                            "line", "run_chart", "qq"))
    "colour"
  else
    "fill"
  .finish(p, spec, palette_aes)
}

# --- helpers ---------------------------------------------------------------
base_col <- function(spec) {
  cols <- CHART_PALETTES[[spec$palette]]
  if (is.null(cols)) "#440154" else cols[1]
}

# TRUE when a usable colour / group-by variable has been chosen.
has_group <- function(g) !is.null(g) && length(g) == 1 && nzchar(g)

.bar_count <- function(df, x, spec, pareto = FALSE, group = NULL) {
  # A colour / group-by variable turns the count bar into a grouped bar.
  if (has_group(group)) {
    fill_mode <- isTRUE(spec$stacked_fill)
    p <- ggplot(df, aes(x = .data[[x]], fill = .data[[group]])) +
      geom_bar(position = if (fill_mode) "fill" else "dodge", alpha = spec$alpha)
    if (isTRUE(spec$data_labels))
      p <- p + geom_text(stat = "count", aes(label = after_stat(count)),
                         position = if (fill_mode) position_fill(vjust = 0.5)
                                    else position_dodge(width = 0.9),
                         vjust = if (fill_mode) 0.5 else -0.3,
                         size = .LABEL_SIZE)
    return(p)
  }
  d <- as.data.frame(table(df[[x]]), stringsAsFactors = FALSE)
  names(d) <- c(x, "count")
  if (pareto) d <- d[order(-d$count), , drop = FALSE]
  d[[x]] <- factor(d[[x]], levels = d[[x]])
  p <- ggplot(d, aes(x = .data[[x]], y = count, fill = .data[[x]])) +
    geom_col(alpha = spec$alpha) + guides(fill = "none")
  if (isTRUE(spec$data_labels))
    p <- p + geom_text(aes(label = count), vjust = -0.35, size = .LABEL_SIZE)
  p
}

.pie <- function(df, x, spec) {
  d <- as.data.frame(table(df[[x]]), stringsAsFactors = FALSE)
  names(d) <- c(x, "count")
  total <- sum(d$count)
  # Compute the percentage BEFORE building the plot, so the column is part of
  # the plot's captured data and the label aesthetic can find it.
  d$pct <- if (total > 0) 100 * d$count / total else 0
  p <- ggplot(d, aes(x = "", y = count, fill = .data[[x]])) +
    geom_col(width = 1, alpha = spec$alpha) + coord_polar(theta = "y") +
    theme_void(base_size = spec$base_size)
  if (isTRUE(spec$data_labels) && total > 0)
    p <- p + geom_text(aes(label = sprintf("%s\n%.0f%%", count, pct)),
                       position = position_stack(vjust = 0.5),
                       size = .LABEL_SIZE, colour = "white", fontface = "bold")
  p
}

.mean_ci <- function(df, x, g, spec) {
  d <- do.call(rbind, lapply(split(df[[x]], df[[g]]), function(v) {
    v <- v[!is.na(v)]; n <- length(v)
    m <- mean(v); se <- if (n > 1) stats::sd(v) / sqrt(n) else NA
    data.frame(mean = m,
               lo = m - stats::qt(0.975, max(n - 1, 1)) * se,
               hi = m + stats::qt(0.975, max(n - 1, 1)) * se)
  }))
  d$group <- rownames(d)
  p <- ggplot(d, aes(x = group, y = mean, fill = group)) +
    geom_col(alpha = spec$alpha) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
    guides(fill = "none") + labs(y = paste("mean", x, "(95% CI)"))
  if (isTRUE(spec$data_labels))
    p <- p + geom_text(aes(y = hi, label = .label_fmt(mean)),
                       vjust = -0.5, size = .LABEL_SIZE)
  p
}

.raincloud <- function(df, x, group = NULL, spec) {
  has_gg <- requireNamespace("ggdist", quietly = TRUE)
  if (is.null(group)) {
    p <- ggplot(df, aes(x = factor(1), y = .data[[x]]))
    if (has_gg) {
      p + ggdist::stat_halfeye(fill = base_col(spec), alpha = spec$alpha,
                               adjust = 0.6, width = 0.5, justification = -0.2) +
        geom_boxplot(width = 0.12, outlier.shape = NA) +
        geom_jitter(width = 0.06, alpha = 0.4, size = spec$point_size)
    } else {
      p + geom_violin(fill = base_col(spec), alpha = spec$alpha) +
        geom_boxplot(width = 0.1, outlier.shape = NA) +
        geom_jitter(width = 0.08, alpha = 0.4)
    }
  } else {
    p <- ggplot(df, aes(x = .data[[group]], y = .data[[x]], fill = .data[[group]]))
    if (has_gg) {
      p + ggdist::stat_halfeye(alpha = spec$alpha, adjust = 0.6, width = 0.5,
                               justification = -0.2) +
        geom_boxplot(width = 0.12, outlier.shape = NA) +
        geom_jitter(width = 0.06, alpha = 0.4, size = spec$point_size)
    } else {
      p + geom_violin(alpha = spec$alpha) +
        geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
        geom_jitter(width = 0.08, alpha = 0.4)
    }
  }
}

.grouped_bar <- function(df, x, g, spec, stack_pos = "dodge") {
  p <- ggplot(df, aes(x = .data[[x]], fill = .data[[g]])) +
    geom_bar(position = stack_pos, alpha = spec$alpha)
  if (isTRUE(spec$data_labels)) {
    pos <- switch(stack_pos,
                  dodge = position_dodge(width = 0.9),
                  fill = position_fill(vjust = 0.5),
                  position_stack(vjust = 0.5))
    p <- p + geom_text(stat = "count", aes(label = after_stat(count)),
                       position = pos,
                       vjust = if (identical(stack_pos, "dodge")) -0.3 else 0.5,
                       size = .LABEL_SIZE)
  }
  p
}

.contingency_heat <- function(df, x, g, spec) {
  d <- as.data.frame(table(df[[x]], df[[g]]), stringsAsFactors = FALSE)
  names(d) <- c(x, g, "count")
  ggplot(d, aes(x = .data[[x]], y = .data[[g]], fill = count)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = count), size = 3.5) +
    scale_fill_gradient(low = "#E7F3F2", high = base_col(spec))
}

.line_chart <- function(df, x, y, g, spec) {
  d <- df[order(df[[x]]), , drop = FALSE]
  p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]]))
  if (!is.null(g) && nzchar(g)) p <- p + aes(colour = .data[[g]], group = .data[[g]])
  p <- p + geom_line(linewidth = 0.8) +
    geom_point(size = spec$point_size, alpha = spec$alpha)
  if (isTRUE(spec$data_labels))
    p <- p + geom_text(aes(label = .label_fmt(.data[[y]])),
                       vjust = -0.7, size = .LABEL_SIZE, show.legend = FALSE)
  p
}

.run_chart <- function(df, x, y, spec) {
  d <- df[order(df[[x]]), , drop = FALSE]
  med <- stats::median(d[[y]], na.rm = TRUE)
  p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
    geom_line(colour = base_col(spec)) + geom_point(size = spec$point_size)
  if (isTRUE(spec$add_ref_line))
    p <- p + geom_hline(yintercept = med, linetype = "dashed", colour = "grey40")
  if (isTRUE(spec$data_labels))
    p <- p + geom_text(aes(label = .label_fmt(.data[[y]])),
                       vjust = -0.7, size = .LABEL_SIZE)
  p
}

.corr_heatmap <- function(df, spec) {
  num <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  if (ncol(num) < 2)
    return(ggplot() + annotate("text", x = 1, y = 1,
                               label = "Need at least two numeric variables."))
  m <- round(stats::cor(num, use = "pairwise.complete.obs"), 2)
  d <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
  names(d) <- c("v1", "v2", "r")
  ggplot(d, aes(x = v1, y = v2, fill = r)) +
    geom_tile(colour = "white") + geom_text(aes(label = r), size = 3) +
    scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#018786",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

.scatter_matrix <- function(df, spec) {
  num <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  if (requireNamespace("GGally", quietly = TRUE) && ncol(num) >= 2) {
    return(GGally::ggpairs(num))
  }
  ggplot() + annotate("text", x = 1, y = 1,
                      label = "Scatterplot matrix needs the GGally package.")
}

#' Summarise what a chart depicts as one or more small tables (counts,
#' group-wise summary statistics, or an association). This is the AGGREGATE the
#' AI interpreter and report write-up read - raw patient rows are never sent.
#' Returns a named list of data frames (possibly empty). Never throws.
chart_summary_tables <- function(df, spec) {
  x <- spec$x; y <- spec$y; g <- spec$group; t <- spec$type
  out <- list()
  is_num <- function(v) is.numeric(v) || inherits(v, "Date") || inherits(v, "POSIXct")
  num_summary <- function(v) {
    v <- suppressWarnings(as.numeric(v)); v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    data.frame(n = length(v), mean = mean(v),
               sd = if (length(v) > 1) stats::sd(v) else NA_real_,
               median = stats::median(v),
               Q1 = stats::quantile(v, 0.25, names = FALSE),
               Q3 = stats::quantile(v, 0.75, names = FALSE),
               min = min(v), max = max(v), check.names = FALSE)
  }
  tryCatch({
    if (t %in% c("bar", "pareto", "pie") && !is.null(x) && x %in% names(df)) {
      tab <- as.data.frame(table(df[[x]]), stringsAsFactors = FALSE)
      names(tab) <- c(x, "count")
      if (sum(tab$count) > 0)
        tab$percent <- round(100 * tab$count / sum(tab$count), 1)
      out[[paste0("Counts of ", x)]] <- tab
    } else if (t %in% c("grouped_bar", "stacked_bar", "mosaic", "heatmap") &&
               !is.null(x) && !is.null(g) && all(c(x, g) %in% names(df))) {
      ct <- as.data.frame.matrix(table(df[[x]], df[[g]]))
      out[[paste0(x, " by ", g)]] <-
        cbind(data.frame(Level = rownames(ct), check.names = FALSE), ct)
    } else if (t %in% c("scatter", "density2d") && !is.null(x) && !is.null(y) &&
               all(c(x, y) %in% names(df))) {
      xv <- suppressWarnings(as.numeric(df[[x]]))
      yv <- suppressWarnings(as.numeric(df[[y]]))
      keep <- is.finite(xv) & is.finite(yv)
      r <- if (sum(keep) > 2) stats::cor(xv[keep], yv[keep]) else NA_real_
      out[[sprintf("Association of %s and %s", x, y)]] <- data.frame(
        statistic = c("n", "Pearson r"),
        value = c(sum(keep), round(r, 3)), check.names = FALSE)
    } else if (!is.null(x) && x %in% names(df) && is_num(df[[x]])) {
      if (!is.null(g) && g %in% names(df)) {
        parts <- split(df[[x]], df[[g]])
        rows <- do.call(rbind, lapply(names(parts), function(k) {
          s <- num_summary(parts[[k]]); if (is.null(s)) NULL else cbind(Group = k, s)
        }))
        if (!is.null(rows)) out[[paste0("Summary of ", x, " by ", g)]] <- rows
      } else {
        s <- num_summary(df[[x]])
        if (!is.null(s)) out[[paste0("Summary of ", x)]] <- s
      }
    }
    # Add a summary of the y variable for charts where it is a separate numeric
    # series (line/run/panel), unless it was already the focus above.
    if (!is.null(y) && y %in% names(df) && is_num(df[[y]]) &&
        !(t %in% c("scatter", "density2d"))) {
      s <- num_summary(df[[y]])
      if (!is.null(s)) out[[paste0("Summary of ", y)]] <- s
    }
  }, error = function(e) NULL)

  lapply(out, function(d) {
    for (nm in names(d)) if (is.numeric(d[[nm]])) d[[nm]] <- round(d[[nm]], 3)
    d
  })
}
