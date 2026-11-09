# mod_tests.R ---------------------------------------------------------------
# Phase 2: guided hypothesis testing. The user picks a question type and the
# roles; the app previews the recommended test and why, then runs BOTH the
# primary and the alternative, shows effect sizes, CIs, assumption verdicts,
# posthoc comparisons where applicable, and a plain-language interpretation;
# it can add the result to the Word report.

# Friendly labels for the test families (used to build the family radio buttons).
FAMILY_LABELS <- c(
  location    = "Compare groups / factorial designs (means / locations)",
  categorical = "Association between two categories",
  cochran_q  = "Repeated binary outcomes (Cochran's Q)",
  correlation = "Correlation between two numbers",
  point_biserial = "Point-biserial correlation (binary vs numeric)",
  proportion  = "One proportion vs a reference",
  diagnostic  = "Diagnostic / screening test accuracy (vs a reference standard)",
  effects     = "Effect size for a binary outcome (risk difference, RR, OR, NNT)",
  trend       = "Trend across an ordered exposure (dose-response)"
)

# Flatten a single test result into a tidy Field/Value table (for display+report)
result_to_df <- function(res) {
  tb <- res$table; tb$value <- as.character(tb$value)
  names(tb) <- c("Field", "Value")
  rows <- rbind(
    data.frame(Field = "Test", Value = res$label, stringsAsFactors = FALSE),
    tb,
    data.frame(Field = "p-value", Value = p_fmt(res$p_value)))
  if (!is.null(res$terms_table) && nrow(res$terms_table)) {
    terms <- res$terms_table
    term_rows <- data.frame(
      Field = paste0("Term: ", terms$term),
      Value = vapply(seq_len(nrow(terms)), function(i) {
        vals <- terms[i, setdiff(names(terms), "term"), drop = FALSE]
        paste(sprintf("%s = %s", names(vals),
                     vapply(vals, as.character, character(1))),
              collapse = "; ")
      }, character(1)),
      stringsAsFactors = FALSE
    )
    rows <- rbind(rows, term_rows)
  }
  if (!is.null(res$variance_table) && nrow(res$variance_table)) {
    vt <- res$variance_table
    var_rows <- data.frame(
      Field = paste0("Variance: ", vt$Component),
      Value = vapply(seq_len(nrow(vt)), function(i) {
        vals <- vt[i, setdiff(names(vt), "Component"), drop = FALSE]
        paste(sprintf("%s = %s", names(vals),
                      vapply(vals, function(v) formatC(v, digits = 4, format = "g"),
                             character(1))),
              collapse = "; ")
      }, character(1)),
      stringsAsFactors = FALSE)
    rows <- rbind(rows, var_rows)
  }
  if (length(res$icc))
    rows <- rbind(rows, data.frame(
      Field = names(res$icc),
      Value = formatC(as.numeric(res$icc), digits = 3, format = "f"),
      stringsAsFactors = FALSE))
  if (length(res$effect))
    rows <- rbind(rows, data.frame(
      Field = names(res$effect),
      Value = as.character(unlist(res$effect)), stringsAsFactors = FALSE))
  rows <- rbind(rows, data.frame(Field = "Interpretation",
                                 Value = res$interpretation))
  rows$Value <- as.character(rows$Value)
  rows
}

mod_tests_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Hypothesis testing"),
      shiny::uiOutput(ns("ai_gate_ui")),
      shiny::uiOutput(ns("unrecommended_ui")),
      shiny::uiOutput(ns("family_ui")),
      shiny::tags$hr(),
      shiny::uiOutput(ns("controls")),
      shiny::tags$hr(),
      shiny::actionButton(ns("run"), "Run test", icon = shiny::icon("play"),
                          class = "btn-primary"),
      shiny::actionButton(ns("add"), "Add to report", icon = shiny::icon("plus"))
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput(ns("kind_note")),
      shiny::div(class = "alert alert-info", shiny::htmlOutput(ns("preview"))),
      shiny::uiOutput(ns("recommend_badge")),
      shiny::fluidRow(
        shiny::column(6, shiny::h5(shiny::textOutput(ns("primary_title"))),
                      shiny::tableOutput(ns("primary_tbl"))),
        shiny::column(6, shiny::h5(shiny::textOutput(ns("alt_title"))),
                      shiny::tableOutput(ns("alt_tbl")))
      ),
      shiny::h5(shiny::textOutput(ns("extra_title"))),
      shiny::tableOutput(ns("extra_tbl")),
      shiny::h5("Posthoc comparisons"),
      shiny::uiOutput(ns("posthoc_info")),
      shiny::tableOutput(ns("posthoc_tbl")),
      shiny::h5("Assumption checks"),
      shiny::uiOutput(ns("assumptions")),
      shiny::h5("Recommendation"),
      shiny::textOutput(ns("rationale")),
      mod_interpret_ui(ns("interpret"))
    )
  )
}

mod_tests_server <- function(id, data, types, outcomes = NULL, panel = NULL,
                             plan_rec = NULL, add_report, audit = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # AI-plan gate. When a validated plan exists it narrows the offered test
    # families to those the AI recommended (on top of the layout gate); the
    # override checkbox restores the full feasible menu. The checkbox is shown
    # only when a plan is present.
    plan_now <- shiny::reactive({
      if (is.null(plan_rec)) return(NULL)
      tryCatch(plan_rec(), error = function(e) NULL)
    })
    ai_gate_on <- shiny::reactive(
      !is.null(plan_now()) && !isTRUE(input$include_unrecommended))
    output$ai_gate_ui <- shiny::renderUI({
      if (is.null(plan_now())) return(NULL)
      shiny::div(class = "alert alert-info", style = "padding:8px 10px;",
        shiny::icon("magic"), " Guided by your AI analysis plan.",
        shiny::checkboxInput(ns("include_unrecommended"),
          "Include un-recommended analyses (override the AI plan)",
          value = FALSE))
    })

    panel_spec <- shiny::reactive({
      if (is.null(panel)) return(list(id = NULL, time = NULL, event = NULL))
      panel()
    })
    outcome_candidates <- shiny::reactive({
      if (is.null(outcomes)) return(default_outcome_candidates(data(), types()))
      outcomes()
    })
    analysis_types <- shiny::reactive({
      df <- data(); tp <- types()
      selected <- intersect(outcome_candidates(), names(df))
      for (v in selected) {
        opts <- outcome_type_options(df[[v]])
        if (length(opts)) tp[[v]] <- preferred_outcome_type(df[[v]], tp[[v]])
      }
      tp
    })

    # Columns reserved by the Data-tab declaration (subject ID; and, for a
    # time-to-event study, the censored follow-up time and event columns) must
    # not be offered as ordinary test variables - censored time analysed with a
    # t-test/correlation is invalid; it belongs to the survival methods.
    reserved <- shiny::reactive(reserved_analysis_vars(panel_spec()))
    test_kind <- shiny::reactive(analysis_layout_kind(panel_spec()))
    # Only offer the "un-recommended" escape hatch when the data kind actually
    # restricts the menu (repeated / time series). Cross-sectional shows all.
    output$unrecommended_ui <- shiny::renderUI({
      if (identical(test_kind(), "cross_sectional")) return(NULL)
      shiny::checkboxInput(ns("show_all"),
        "Include un-recommended analyses (assume independent observations)",
        value = FALSE)
    })
    show_all <- shiny::reactive(isTRUE(input$show_all))
    num_vars <- shiny::reactive(setdiff(names(analysis_types())[vapply(analysis_types(), is_numeric_type, logical(1))], reserved()))
    cat_vars <- shiny::reactive(setdiff(names(analysis_types())[vapply(analysis_types(), is_categorical_type, logical(1))], reserved()))
    binary_vars <- shiny::reactive(setdiff(names(analysis_types())[analysis_types() == "binary"], reserved()))
    output$kind_note <- shiny::renderUI({
      note <- analysis_kind_note(panel_spec())
      if (is.null(note)) return(NULL)
      shiny::div(class = "alert alert-warning", shiny::HTML(note))
    })
    location_outcomes <- shiny::reactive({
      df <- data()
      setdiff(analysis_outcome_vars(df, types(), outcome_candidates(), c("continuous", "count")), reserved())
    })
    proportion_outcomes <- shiny::reactive({
      df <- data()
      setdiff(analysis_outcome_vars(df, types(), outcome_candidates(), "binary"), reserved())
    })
    cochran_outcomes <- shiny::reactive({
      df <- data()
      analysis_outcome_vars(df, analysis_types(), outcome_candidates(), "binary")
    })
    # Used only to decide whether the longitudinal design choice should be
    # visible. The actual outcome dropdown remains restricted to the explicit
    # Data-tab candidate selection above.
    cochran_available_outcomes <- shiny::reactive({
      df <- data()
      analysis_outcome_vars(df, analysis_types(), names(df), "binary")
    })
    factor_vars <- shiny::reactive({
      valid_factor_vars(analysis_types(), data(),
                        exclude = unique(c(input$outcome %||% character(0), reserved())))
    })
    # Types with the reserved (survival / ID) columns removed, so the family
    # radio itself does not offer, say, a correlation whose only extra numeric
    # is a reserved follow-up-time column.
    open_types <- shiny::reactive({
      tp <- analysis_types(); tp[setdiff(names(tp), reserved())]
    })
    within_vars <- shiny::reactive({
      ps <- panel_spec()
      setdiff(unique(c(intersect(ps$time %||% character(0), names(data())), cat_vars())),
              input$outcome %||% character(0))
    })
    # Subject-ID candidates exclude the chosen outcome (and free text). An ID may
    # be read as numeric, so we do NOT restrict to categorical here.
    id_vars  <- shiny::reactive({
      ids <- valid_subject_vars(analysis_types(), outcome = input$outcome)
      ps <- panel_spec()
      unique(c(intersect(ps$id %||% character(0), ids), ids))
    })

    # --- only offer families the data can actually support -----------------
    output$family_ui <- shiny::renderUI({
      fams <- available_test_families(open_types(), include_proportion = TRUE,
                                      include_point_biserial = TRUE,
                                      include_cochran_q =
                                        panel_has_subject_time(panel_spec()) &&
                                        length(cochran_available_outcomes()) > 0)
      # Idiot-proof default: for repeated / time-series data, drop the families
      # that assume independent observations unless the user opts in.
      if (!show_all()) {
        rec <- recommended_test_families(test_kind())
        if (!is.null(rec)) fams <- intersect(fams, rec)
      }
      # AI-plan gate (stacks on top of the layout gate above).
      fams <- gate_choices(fams, plan_test_families(plan_now()), ai_gate_on())
      shiny::validate(shiny::need(length(fams) > 0,
        if (identical(test_kind(), "time_series"))
          "No standard hypothesis test is recommended for a single time series. Tick 'Include un-recommended analyses' to override."
        else if (identical(test_kind(), "repeated"))
          "No within-subject test is available for the current variables. Tick 'Include un-recommended analyses' to see independence-based tests."
        else "No hypothesis tests are available for the current variable types."))
      shiny::radioButtons(ns("family"), "What do you want to test?",
        choices = stats::setNames(fams, FAMILY_LABELS[fams]))
    })

    # Grouping-variable choices depend on the chosen design (level count).
    output$group_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "location"), input$design)
      ch <- valid_grouping_vars(analysis_types(), data(), input$design)
      shiny::selectInput(ns("group"),
                         "Grouping variable", choices = ch)
    })

    output$mixed_group_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "location"),
                 identical(input$design, "mixed"))
      ch <- valid_factor_vars(analysis_types(), data(),
                              exclude = c(input$outcome %||% character(0),
                                          input$mixed_within %||% character(0)))
      shiny::selectInput(ns("mixed_group"), "Between-subjects group", choices = ch)
    })

    output$factor_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "location"),
                 identical(input$design, "factorial"))
      shiny::selectizeInput(ns("factors"), "Factor variables (select 2 or 3)",
                            choices = factor_vars(), multiple = TRUE,
                            options = list(maxItems = 3,
                                           placeholder = "Select two or three factors"))
    })

    output$ancova_covariate_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "location"),
                 identical(input$design, "ancova"))
      ch <- setdiff(num_vars(), input$outcome %||% character(0))
      shiny::selectizeInput(ns("covariates"), "Continuous covariate(s) to adjust for",
                            choices = ch, multiple = TRUE,
                            options = list(placeholder = "Select one or more covariates"))
    })

    # Subject-ID choices depend on the chosen outcome. Rendered on its own so
    # that changing the outcome does NOT rebuild (and reset) the whole controls
    # panel - which would snap the outcome dropdown back to the first variable.
    output$subject_ui <- shiny::renderUI({
      ch <- id_vars(); ps <- panel_spec()
      shiny::selectInput(ns("subject"), "Subject ID", choices = ch,
                         selected = if (!is.null(ps$id) && ps$id %in% ch) ps$id else NULL)
    })

    output$mixed_subject_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "location"),
                 identical(input$design, "mixed"))
      ch <- id_vars(); ps <- panel_spec()
      shiny::selectInput(ns("mixed_subject"), "Subject ID", choices = ch,
                         selected = if (!is.null(ps$id) && ps$id %in% ch) ps$id else NULL)
    })

    output$success_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "proportion"), input$outcome)
      lev <- levels(droplevels(as.factor(data()[[input$outcome]][!is.na(data()[[input$outcome]])])))
      shiny::selectInput(ns("success"), "Success level", choices = lev,
                         selected = if (length(lev)) lev[[length(lev)]] else NULL)
    })

    output$cochran_success_ui <- shiny::renderUI({
      shiny::req(
        identical(input$family, "cochran_q") ||
          (identical(input$family, "categorical") &&
             identical(input$cat_design, "repeated_binary")),
        input$outcome)
      lev <- levels(droplevels(as.factor(data()[[input$outcome]][
        !is.na(data()[[input$outcome]])])))
      shiny::selectInput(ns("cochran_success"), "Success level", choices = lev,
                         selected = if (length(lev)) lev[[length(lev)]] else NULL)
    })

    # --- family-specific controls -----------------------------------------
    output$controls <- shiny::renderUI({
      # The Tests tab is visible before a file is uploaded.  `input$family` is
      # therefore NULL on the first render; comparing NULL with `==` produces
      # logical(0), which makes `if` fail with "argument is of length zero".
      family <- input$family %||% ""
      if (!nzchar(family)) {
        return(shiny::helpText("Load a dataset on the Data tab to choose a hypothesis test."))
      }
      if (identical(family, "location")) {
        all_designs <- c(
          "Compare to a value (one sample)" = "one_sample",
          "Two independent groups" = "two_independent",
          "More than two independent groups" = "multi_independent",
          "Same subjects over 2+ conditions (paired / repeated)" = "paired_repeated",
          "Two or three crossed factors (factorial ANOVA)" = "factorial",
          "Between × within mixed-model ANOVA" = "mixed",
          "Compare groups adjusting for a covariate (ANCOVA)" = "ancova")
        # Design menu = three composed gates, base to top:
        #  1) DATA FEASIBILITY (idiot-proof, always): only designs the current
        #     variables can support.
        #  2) STUDY DESIGN (Data-tab layout), unless the layout override.
        #  3) AI PLAN, unless its own override. gate_choices never empties.
        feas <- feasible_location_designs(analysis_types(), data(), panel_spec(), reserved())
        design_choices <- all_designs[all_designs %in% feas]
        if (!show_all()) {
          rec_designs <- recommended_location_designs(test_kind())
          if (!is.null(rec_designs))
            design_choices <- design_choices[unname(design_choices) %in% rec_designs]
        }
        keep <- gate_choices(unname(design_choices), plan_test_designs(plan_now()), ai_gate_on())
        design_choices <- design_choices[unname(design_choices) %in% keep]
        if (!length(design_choices))         # never leave the menu empty
          design_choices <- all_designs[all_designs %in% "one_sample"]
        shiny::tagList(
          shiny::selectInput(ns("outcome"), "Numeric outcome", choices = location_outcomes()),
          shiny::radioButtons(ns("design"), "Design", choices = design_choices),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'one_sample'", ns("design")),
            shiny::numericInput(ns("ref_value"), "Reference value", value = 0)),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'two_independent' || input['%s'] == 'multi_independent' || input['%s'] == 'ancova'",
                    ns("design"), ns("design"), ns("design")),
            shiny::uiOutput(ns("group_ui"))),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'ancova'", ns("design")),
            shiny::uiOutput(ns("ancova_covariate_ui"))),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'factorial'", ns("design")),
            shiny::uiOutput(ns("factor_ui"))),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'paired_repeated'", ns("design")),
            shiny::selectInput(ns("within"), "Condition / time variable",
                               choices = within_vars(),
                               selected = panel_spec()$time %||% NULL),
            shiny::uiOutput(ns("subject_ui"))),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'mixed'", ns("design")),
            shiny::uiOutput(ns("mixed_group_ui")),
            shiny::selectInput(ns("mixed_within"), "Within-subjects condition / time",
                               choices = within_vars(),
                               selected = panel_spec()$time %||% NULL),
            shiny::uiOutput(ns("mixed_subject_ui")))
        )
      } else if (identical(family, "categorical")) {
        cat_design_choices <- c(
          "Association between two categorical variables" = "association",
          # Agreement is a different question from association: two measurements
          # can be strongly associated yet disagree systematically, and can have
          # identical margins yet disagree on every case. Concordance questions
          # - two raters, two sites, test vs reference - are this one.
          "Agreement between two measurements of the same cases (Cohen's kappa)" =
            "agreement"
        )
        if (panel_has_subject_time(panel_spec()) &&
            length(cochran_available_outcomes()) > 0)
          cat_design_choices <- c(
            cat_design_choices,
            "Repeated binary outcome across conditions/time (McNemar or Cochran's Q)" =
              "repeated_binary"
          )
        rec_cat <- if (!show_all()) recommended_cat_designs(test_kind()) else NULL
        if (!is.null(rec_cat) && any(cat_design_choices %in% rec_cat))
          cat_design_choices <- cat_design_choices[cat_design_choices %in% rec_cat]
        keep_cat <- gate_choices(unname(cat_design_choices),
                                 plan_test_designs(plan_now()), ai_gate_on())
        cat_design_choices <- cat_design_choices[unname(cat_design_choices) %in% keep_cat]
        selected_cat_design <- if (!is.null(input$cat_design) &&
                                   input$cat_design %in% unname(cat_design_choices))
          input$cat_design else "association"
        shiny::tagList(
          shiny::radioButtons(ns("cat_design"), "Categorical design",
                              choices = cat_design_choices,
                              selected = selected_cat_design),
          if (identical(selected_cat_design, "agreement")) shiny::tagList(
            shiny::selectizeInput(ns("agree_raters"),
              "Columns holding the repeated measurements",
              choices = cat_vars(), multiple = TRUE,
              selected = shiny::isolate(input$agree_raters),
              options = list(placeholder = "Pick two columns, or three or more for a panel")),
            shiny::helpText(
              "Two columns give Cohen's kappa - agreement between that specific ",
              "pair. Three or more give Fleiss' kappa, which measures how far a ",
              "panel agrees as a whole and is not the average of the pairwise ",
              "Cohen's kappas."),
            shiny::selectInput(ns("kappa_weights"), "Category weighting",
                               choices = AGREEMENT_WEIGHTS,
                               selected = shiny::isolate(input$kappa_weights) %||%
                                 "unweighted"),
            shiny::helpText(
              "Use weights only when the categories are ORDERED, so that a ",
              "near miss counts as partial agreement. With two categories every ",
              "scheme gives the same answer.")),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'association'", ns("cat_design")),
            shiny::selectInput(ns("x"), "Variable 1", choices = cat_vars()),
            shiny::selectInput(ns("y"), "Variable 2", choices = cat_vars()),
            shiny::checkboxInput(ns("paired"), "Paired (same subjects, 2x2)", FALSE)
          ),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'repeated_binary'", ns("cat_design")),
            shiny::selectInput(ns("outcome"), "Binary outcome",
                               choices = cochran_outcomes()),
            shiny::selectInput(ns("cochran_within"), "Condition / time variable",
                               choices = within_vars(),
                               selected = panel_spec()$time %||% NULL),
            shiny::uiOutput(ns("subject_ui")),
            shiny::uiOutput(ns("cochran_success_ui")),
            if (length(cochran_outcomes()) == 0)
              shiny::helpText("Select a binary candidate outcome in 1 · Data first. The outcome list here intentionally stays empty until you do so.")
            else
              shiny::helpText("Requires at least three repeated conditions/time points for Cochran's Q. With exactly two conditions, the paired binary route is McNemar's test.")
          )
        )
      } else if (identical(family, "cochran_q")) {
        shiny::tagList(
          shiny::selectInput(ns("outcome"), "Binary outcome", choices = cochran_outcomes()),
          shiny::selectInput(ns("cochran_within"), "Condition / time variable",
                             choices = within_vars(),
                             selected = panel_spec()$time %||% NULL),
          shiny::uiOutput(ns("subject_ui")),
          shiny::uiOutput(ns("cochran_success_ui")),
          if (length(cochran_outcomes()) == 0)
            shiny::helpText("Select a binary candidate outcome in 1 · Data first. The outcome list here intentionally stays empty until you do so.")
          else
            shiny::helpText("Requires at least three repeated conditions/time points and one observation per subject-condition pair. Incomplete subject blocks are excluded.")
        )
      } else if (identical(family, "effects")) {
        shiny::tagList(
          shiny::helpText(shiny::strong("How much, not just whether."), " ",
            "A chi-square test says the groups differ; these measures say by ",
            "how much, on the three scales that answer different questions."),
          shiny::selectInput(ns("eff_outcome"), "Binary outcome",
                             choices = cat_vars(),
                             selected = shiny::isolate(input$eff_outcome)),
          shiny::selectInput(ns("eff_group"), "Group / exposure (two levels)",
                             choices = cat_vars(),
                             selected = shiny::isolate(input$eff_group)),
          shiny::uiOutput(ns("eff_levels_ui")))
      } else if (identical(family, "trend")) {
        shiny::tagList(
          shiny::helpText(shiny::strong("For an ORDERED exposure"), " - a dose, ",
            "a wealth quintile, an age band, a duration. Asks whether risk ",
            "moves consistently with the level, which a chi-square test cannot."),
          shiny::selectInput(ns("tr_outcome"), "Binary outcome",
                             choices = cat_vars(),
                             selected = shiny::isolate(input$tr_outcome)),
          shiny::selectInput(ns("tr_exposure"), "Ordered exposure (3+ levels)",
                             choices = c(cat_vars(), num_vars()),
                             selected = shiny::isolate(input$tr_exposure)),
          shiny::uiOutput(ns("tr_positive_ui")))
      } else if (identical(family, "diagnostic")) {
        pool <- cat_vars()
        shiny::tagList(
          shiny::helpText(shiny::strong("Directional, unlike agreement."), " ",
            "One variable is the reference standard the other is judged against, ",
            "so which is which matters."),
          shiny::selectInput(ns("dx_test"), "Test being evaluated",
                             choices = pool,
                             selected = shiny::isolate(input$dx_test)),
          shiny::selectInput(ns("dx_ref"), "Reference standard",
                             choices = pool,
                             selected = shiny::isolate(input$dx_ref)),
          shiny::uiOutput(ns("dx_positive_ui")),
          shiny::helpText(
            "Predictive values are also recomputed across a range of ",
            "prevalences, because PPV and NPV depend on how common the ",
            "condition is where the test is used - they do not transfer from ",
            "one setting to another."))
      } else if (identical(family, "proportion")) {
        shiny::tagList(
          shiny::selectInput(ns("outcome"), "Binary outcome", choices = proportion_outcomes()),
          shiny::uiOutput(ns("success_ui")),
          shiny::numericInput(ns("ref_prop"), "Reference proportion", value = 0.5,
                              min = 0.001, max = 0.999, step = 0.01))
      } else if (identical(family, "point_biserial")) {
        shiny::tagList(
          shiny::selectInput(ns("pb_binary"), "Binary variable", choices = binary_vars()),
          shiny::selectInput(ns("pb_numeric"), "Numeric variable", choices = num_vars()))
      } else {
        shiny::tagList(
          {
            corr_design_choices <- c("Ordinary correlation" = "standard")
            if (panel_has_subject_time(panel_spec()))
              corr_design_choices <- c(
                corr_design_choices,
                "Repeated-measures correlation (rmcorr)" = "repeated"
              )
            rec_corr <- if (!show_all()) recommended_corr_designs(test_kind()) else NULL
            if (!is.null(rec_corr) && any(corr_design_choices %in% rec_corr))
              corr_design_choices <- corr_design_choices[corr_design_choices %in% rec_corr]
            keep_corr <- gate_choices(unname(corr_design_choices),
                                      plan_test_designs(plan_now()), ai_gate_on())
            corr_design_choices <- corr_design_choices[unname(corr_design_choices) %in% keep_corr]
            selected_corr_design <- if (!is.null(input$corr_design) &&
                                        input$corr_design %in% unname(corr_design_choices))
              input$corr_design else "standard"
            shiny::radioButtons(ns("corr_design"), "Correlation design",
                                choices = corr_design_choices,
                                selected = selected_corr_design)
          },
          shiny::selectInput(ns("x"), "Numeric variable X", choices = num_vars()),
          shiny::selectInput(ns("y"), "Numeric variable Y", choices = num_vars()),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'repeated'", ns("corr_design")),
            shiny::uiOutput(ns("subject_ui")),
            shiny::helpText("Uses the subject ID to estimate the common within-subject association. Each subject needs at least two complete X/Y observations.")
          )
        )
      }
    })

    # Keep the second role distinct from the first (y != x) in the two
    # two-variable families, so a variable can never be compared with itself.
    shiny::observeEvent(input$x, {
      shiny::req(input$family %in% c("categorical", "correlation"))
      pool <- if (identical(input$family, "categorical")) cat_vars() else num_vars()
      sel <- if (!is.null(input$y) && !identical(input$y, input$x)) input$y else NULL
      shiny::updateSelectInput(session, "y",
        choices = setdiff(pool, input$x), selected = sel)
    }, ignoreNULL = TRUE)

    output$eff_levels_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "effects"), input$eff_outcome,
                 input$eff_group)
      df <- data()
      olv <- levels(droplevels(as.factor(df[[input$eff_outcome]])))
      glv <- levels(droplevels(as.factor(df[[input$eff_group]])))
      msg <- function(what, lv) shiny::div(class = "alert alert-warning",
        style = "padding:8px 10px;",
        sprintf("The %s has %d observed values (%s); exactly two are needed.",
                what, length(lv), paste(utils::head(lv, 5), collapse = ", ")))
      if (length(olv) != 2L) return(msg("outcome", olv))
      if (length(glv) != 2L) return(msg("group", glv))
      shiny::tagList(
        shiny::selectInput(ns("eff_positive"), "Which outcome value is the EVENT?",
                           choices = olv,
                           selected = shiny::isolate(input$eff_positive) %||% olv[[2]]),
        shiny::selectInput(ns("eff_exposed"), "Which group is EXPOSED / intervention?",
                           choices = glv,
                           selected = shiny::isolate(input$eff_exposed) %||% glv[[2]]),
        shiny::helpText("The other group becomes the reference the effects are measured against."))
    })

    output$tr_positive_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "trend"), input$tr_outcome)
      olv <- levels(droplevels(as.factor(data()[[input$tr_outcome]])))
      if (length(olv) != 2L)
        return(shiny::div(class = "alert alert-warning", style = "padding:8px 10px;",
          sprintf("A trend test needs a binary outcome; '%s' has %d observed values.",
                  input$tr_outcome, length(olv))))
      shiny::tagList(
        shiny::selectInput(ns("tr_positive"), "Which outcome value is the EVENT?",
                           choices = olv,
                           selected = shiny::isolate(input$tr_positive) %||% olv[[2]]),
        shiny::helpText("Levels are scored 1, 2, 3, ... in their existing order. If the categories are unequally spaced (age bands, say), the trend is still tested but 'linear' means linear in those ranks."))
    })

    output$dx_positive_ui <- shiny::renderUI({
      shiny::req(identical(input$family, "diagnostic"), input$dx_test, input$dx_ref)
      df <- data()
      lv <- union(levels(as.factor(df[[input$dx_test]])),
                  levels(as.factor(df[[input$dx_ref]])))
      if (length(lv) != 2L)
        return(shiny::div(class = "alert alert-warning", style = "padding:8px 10px;",
          sprintf("The test and reference together show %d values (%s). A diagnostic evaluation needs exactly two - recode to positive/negative first.",
                  length(lv), paste(utils::head(lv, 5), collapse = ", "))))
      shiny::tagList(
        shiny::selectInput(ns("dx_positive"), "Which value means POSITIVE?",
                           choices = lv,
                           selected = shiny::isolate(input$dx_positive) %||% lv[[2]]),
        shiny::helpText("Getting this backwards swaps sensitivity and specificity, so it is asked rather than guessed."))
    })

    # --- resolve a full spec -----------------------------------------------
    spec <- shiny::reactive({
      shiny::req(input$family)
      if (input$family == "location") {
        shiny::req(input$outcome, input$design)
        design <- input$design
        if (design == "paired_repeated") {
          shiny::req(input$within)
          nl <- n_group_levels(data()[[input$within]])
          design <- if (nl <= 2) "two_paired" else "repeated"
        }
        group <- if (identical(design, "mixed")) input$mixed_group else input$group
        within <- if (identical(design, "mixed")) input$mixed_within else input$within
        subject <- if (identical(design, "mixed")) input$mixed_subject else input$subject
        list(family = "location", outcome = input$outcome, design = design,
             group = group, within = within, subject = subject,
             ref_value = input$ref_value, factors = input$factors,
             covariates = input$covariates)
      } else if (input$family == "proportion") {
        shiny::req(input$outcome, input$ref_prop)
        list(family = "proportion", outcome = input$outcome,
             success = input$success, ref_prop = input$ref_prop)
      } else if (input$family == "cochran_q") {
        shiny::req(input$outcome, input$cochran_within, input$subject)
        list(family = "cochran_q", outcome = input$outcome,
             within = input$cochran_within, subject = input$subject,
             success = input$cochran_success)
      } else if (input$family == "effects") {
        shiny::req(input$eff_outcome, input$eff_group, input$eff_positive,
                   input$eff_exposed)
        list(family = "effects", outcome = input$eff_outcome,
             group = input$eff_group, positive = input$eff_positive,
             exposed = input$eff_exposed)
      } else if (input$family == "trend") {
        shiny::req(input$tr_outcome, input$tr_exposure, input$tr_positive)
        list(family = "trend", outcome = input$tr_outcome,
             exposure = input$tr_exposure, positive = input$tr_positive)
      } else if (input$family == "diagnostic") {
        shiny::req(input$dx_test, input$dx_ref, input$dx_positive)
        list(family = "diagnostic", test = input$dx_test,
             reference = input$dx_ref, positive = input$dx_positive)
      } else if (input$family == "point_biserial") {
        shiny::req(input$pb_binary, input$pb_numeric)
        list(family = "point_biserial", x = input$pb_binary, y = input$pb_numeric)
      } else if (input$family == "categorical") {
        cat_design <- input$cat_design %||% "association"
        if (identical(cat_design, "agreement")) {
          raters <- input$agree_raters %||% character(0)
          if (length(raters) >= 3L)
            return(list(family = "fleiss", raters = raters))
          if (length(raters) == 2L)
            return(list(family = "agreement", x = raters[[1]], y = raters[[2]],
                        weights = input$kappa_weights %||% "unweighted"))
          shiny::req(input$x, input$y)
          return(list(family = "agreement", x = input$x, y = input$y,
                      weights = input$kappa_weights %||% "unweighted"))
        }
        if (identical(cat_design, "repeated_binary")) {
          shiny::req(input$outcome, input$cochran_within, input$subject)
          list(family = "cochran_q", outcome = input$outcome,
               within = input$cochran_within, subject = input$subject,
               success = input$cochran_success)
        } else {
          shiny::req(input$x, input$y)
          list(family = "categorical", x = input$x, y = input$y,
               paired = input$paired)
        }
      } else {
        shiny::req(input$x, input$y)
        list(family = "correlation", x = input$x, y = input$y,
             repeated = identical(input$corr_design, "repeated"),
             subject = input$subject)
      }
    })

    output$preview <- shiny::renderUI({
      sp <- spec()
      msg <- if (sp$family == "location") {
        s <- suggest_location_test(sp$design, n_group_levels(
          if (!is.null(sp$group)) data()[[sp$group]] else NULL))
        followup <- switch(sp$design,
          multi_independent = "After a significant omnibus result: Tukey HSD for equal variances, Games-Howell for unequal variances, or Dunn's test for the rank-based route.",
          repeated = "After a significant omnibus result: Holm-adjusted paired comparisons will identify which conditions differ.",
          factorial = "The full term table will report main effects and interactions; the aligned-rank-transform ANOVA is the nonparametric alternative.",
          mixed = "The fixed-effect term table will report between, within, and interaction effects with a subject random intercept.",
          ancova = "The group effect is tested after adjusting for the covariate(s); a homogeneity-of-slopes check is reported.",
          "No posthoc pairwise comparisons are needed for this design.")
        if (identical(sp$design, "ancova"))
          sprintf("<b>Plan:</b> %s (primary), with <b>%s</b> as the nonparametric alternative.<br>%s<br><b>Follow-up:</b> %s",
                  s$primary, s$alt, s$why, followup)
        else
          sprintf("<b>Plan:</b> %s (primary) with <b>%s</b> as the nonparametric alternative.<br>%s<br><b>Follow-up:</b> %s",
                  s$primary, s$alt, s$why, followup)
      } else if (sp$family == "proportion") {
        "<b>Plan:</b> exact binomial test against the selected reference proportion, with a normal-approximation proportion test as a sensitivity analysis."
      } else if (sp$family == "fleiss") {
        paste0("<b>Plan:</b> Fleiss' kappa for how far the panel of ",
               length(sp$raters), " raters agrees beyond chance, with a ",
               "per-category breakdown showing which categories are applied ",
               "inconsistently. The interval is bootstrapped over subjects, ",
               "because the classic Fleiss standard error is derived under ",
               "kappa = 0 and is too narrow otherwise.")
      } else if (sp$family == "agreement") {
        paste0("<b>Plan:</b> Cohen's kappa for how far the two measurements ",
               "agree case by case beyond chance, with a confidence interval, ",
               "and McNemar's test alongside it for whether one records the ",
               "category more often overall. Agreement and association are ",
               "different questions - kappa answers the first.")
      } else if (sp$family == "effects") {
        paste0("<b>Plan:</b> Risk in each group, then the risk difference ",
               "(Newcombe interval), risk ratio, odds ratio and number needed ",
               "to treat, with the chi-square or Fisher p-value. The odds ratio ",
               "and risk ratio are both shown because they diverge when the ",
               "outcome is common.")
      } else if (sp$family == "trend") {
        paste0("<b>Plan:</b> Cochran-Armitage test for a monotonic trend across ",
               "the ordered levels, with the chi-square test alongside it so you ",
               "can see what the ordering bought.")
      } else if (sp$family == "diagnostic") {
        paste0("<b>Plan:</b> Sensitivity, specificity, predictive values and ",
               "likelihood ratios with Wilson confidence intervals, plus the ",
               "predictive values recalculated across a range of prevalences. ",
               "Sensitivity and specificity describe the test; PPV and NPV ",
               "describe the test in a population.")
      } else if (sp$family == "cochran_q") {
        "<b>Plan:</b> Cochran's Q test for differences in a binary outcome across three or more matched conditions/time points. If significant, follow up with pairwise McNemar tests using multiplicity adjustment."
      } else if (sp$family == "point_biserial") {
        "<b>Plan:</b> point-biserial correlation between the binary and numeric variables (rank-biserial from the Mann-Whitney U as the nonparametric alternative)."
      } else if (sp$family == "categorical") {
        "<b>Plan:</b> chi-square test of association, with Fisher's exact test as the small-sample alternative (McNemar if paired 2x2)."
      } else {
        if (isTRUE(sp$repeated))
          "<b>Plan:</b> repeated-measures correlation (rmcorr), estimating the common within-subject association while accounting for subject-level differences."
        else
          "<b>Plan:</b> Pearson correlation if both variables are ~normal, otherwise Spearman; Kendall's tau also reported."
      }
      shiny::HTML(msg)
    })

    # Held in a reactiveVal so a stale test result is dropped when the data
    # changes (e.g. a level removed via the tab-0 Data contract), rather than
    # lingering with values the data no longer contains.
    fit_store <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, {
      shiny::req(spec())
      fit_store(tryCatch(run_guided_test(prepare_test_data(data(), spec()), spec()),
                         error = function(e) list(error = conditionMessage(e))))
    })
    shiny::observeEvent(data(), fit_store(NULL), ignoreInit = TRUE)
    result <- shiny::reactive(fit_store())

    is_ok <- shiny::reactive(!is.null(result()) && is.null(result()$error))

    output$recommend_badge <- shiny::renderUI({
      shiny::req(result())
      if (!is_ok()) return(shiny::div(class = "alert alert-danger",
                                      paste("Could not run test:", result()$error)))
      rec <- result()$recommended
      lbl <- if (rec == "primary") result()$primary$label else result()$alternative$label
      shiny::div(class = "alert alert-success",
                 shiny::HTML(sprintf("<b>Recommended test:</b> %s", lbl)))
    })

    output$primary_title <- shiny::renderText({
      shiny::req(is_ok()); paste0(result()$primary$label,
        if (result()$recommended == "primary") "  (recommended)" else "") })
    output$alt_title <- shiny::renderText({
      shiny::req(is_ok())
      if (is.null(result()$alternative)) return("")
      paste0(result()$alternative$label,
        if (result()$recommended == "alternative") "  (recommended)" else "") })

    output$primary_tbl <- shiny::renderTable({
      shiny::req(is_ok()); result_to_df(result()$primary) }, colnames = FALSE)
    output$alt_tbl <- shiny::renderTable({
      shiny::req(is_ok()); if (is.null(result()$alternative)) return(NULL)
      result_to_df(result()$alternative) }, colnames = FALSE)

    output$extra_title <- shiny::renderText({
      shiny::req(is_ok())
      if (is.null(result()$extra)) "" else result()$extra$label })
    output$extra_tbl <- shiny::renderTable({
      shiny::req(is_ok()); if (is.null(result()$extra)) return(NULL)
      result_to_df(result()$extra) }, colnames = FALSE)

    output$posthoc_info <- shiny::renderUI({
      shiny::req(is_ok())
      ph <- result()$posthoc
      if (is.null(ph))
        return(shiny::tags$em("No posthoc comparisons are required for this design."))
      cls <- if (isTRUE(ph$omnibus_significant))
        "alert alert-success" else "alert alert-warning"
      shiny::div(class = cls, shiny::HTML(ph$note))
    })

    output$posthoc_tbl <- shiny::renderTable({
      shiny::req(is_ok())
      ph <- result()$posthoc
      if (is.null(ph) || is.null(ph$table) || nrow(ph$table) == 0) return(NULL)
      ph$table
    }, digits = 4)

    output$assumptions <- shiny::renderUI({
      shiny::req(is_ok())
      assumption_ui(result()$assumptions,
                    empty = "No formal assumption checks for this test.")
    })

    output$rationale <- shiny::renderText({
      shiny::req(is_ok()); result()$rationale })

    interp_payload <- shiny::reactive({
      if (!is_ok()) return(list())
      res <- result()
      tables <- list()
      tables[[res$primary$label]] <- result_to_df(res$primary)
      if (!is.null(res$alternative))
        tables[[res$alternative$label]] <- result_to_df(res$alternative)
      if (!is.null(res$posthoc) && !is.null(res$posthoc$table) &&
          nrow(res$posthoc$table))
        tables[["Posthoc comparisons"]] <- res$posthoc$table
      rec_label <- if (identical(res$recommended, "primary")) res$primary$label
                   else if (!is.null(res$alternative)) res$alternative$label
                   else res$primary$label
      list(tables = tables,
           context = list(title = rec_label, analysis = "Hypothesis test",
                          method = res$recommended,
                          app_interpretation = res$rationale))
    })
    interp <- mod_interpret_server("interpret", interp_payload, audit = audit)

    shiny::observeEvent(input$add, {
      shiny::req(is_ok())
      res <- result()
      cap <- sprintf("Hypothesis test: %s",
        if (res$recommended == "primary") res$primary$label else res$alternative$label)
      p <- interp_payload()
      ai <- if (length(p$tables %||% list()))
        list(tables = p$tables, context = p$context,
             interpretation = interp_reuse_text(interp$interpretation(), p$context))
      else NULL
      add_report(list(kind = "table", caption = cap,
                      data = result_to_df(res$primary), ai = ai))
      if (!is.null(res$alternative))
        add_report(list(kind = "table",
                        caption = paste("Alternative:", res$alternative$label),
                        data = result_to_df(res$alternative)))
      if (!is.null(res$extra))
        add_report(list(kind = "table",
                        caption = paste("Sensitivity analysis:", res$extra$label),
                        data = result_to_df(res$extra)))
      # Post-hoc pairwise comparisons are not pasted as their own table in a
      # publication-style report - a wall of pairwise rows is rarely reported in
      # full. They still travel in the AI payload above (via interp_payload), so
      # the prose write-up can call out the comparisons that matter.
      shiny::showNotification("Test results added to report.",
                              type = "message", duration = 3)
    })
  })
}
