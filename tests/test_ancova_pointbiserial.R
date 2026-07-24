# tests/test_ancova_pointbiserial.R -----------------------------------------
# Two Tests-tab additions:
#   * ANCOVA  - compare group means adjusting for continuous covariate(s).
#   * Point-biserial correlation - binary vs continuous association.
# Both are routed exactly as the Tests tab does (run_guided_test) and checked
# against reference base-R, plus feasibility gating and module wiring.
#
# Run:  Rscript tests/test_ancova_pointbiserial.R

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))
near <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol
route <- function(df, spec) tryCatch(run_guided_test(df, spec),
                                     error = function(e) conditionMessage(e))
val <- function(res, stat) as.numeric(res$primary$table$value[res$primary$table$statistic == stat])

# ===========================================================================
cat("\n== ANCOVA ==\n")
# ===========================================================================
set.seed(5); n <- 160
anc <- data.frame(grp = factor(sample(c("A", "B", "C"), n, TRUE)), cov = rnorm(n))
anc$y <- 2 + 1.5 * anc$cov + as.numeric(anc$grp) + rnorm(n)

r_anc <- route(anc, list(family = "location", design = "ancova",
                         outcome = "y", group = "grp", covariates = "cov"))
full <- lm(y ~ cov + grp, anc); red <- lm(y ~ cov, anc); an <- anova(red, full)

ok("ANCOVA routes and is labelled", {
  stopifnot(is.list(r_anc), !is.null(r_anc$primary),
            grepl("ANCOVA", r_anc$primary$label))
})
ok("ANCOVA group F matches anova(reduced, full)",
   stopifnot(near(val(r_anc, "F"), round(an$F[2], 3), tol = 1e-3)))
ok("ANCOVA group p matches anova(reduced, full)",
   stopifnot(near(r_anc$primary$p_value, an$`Pr(>F)`[2])))
ok("ANCOVA partial eta-squared matches SS ratio", {
  ss_g <- an$`Sum of Sq`[2]; ss_r <- an$RSS[2]
  stopifnot(near(unlist(r_anc$primary$effect), round(ss_g / (ss_g + ss_r), 3), tol = 1e-3))
})
ok("ANCOVA df1 = groups - 1", stopifnot(val(r_anc, "df1") == nlevels(anc$grp) - 1))
ok("ANCOVA reports a homogeneity-of-slopes check", {
  notes <- vapply(r_anc$assumptions, function(z) z$note, character(1))
  stopifnot(any(grepl("slopes", notes, ignore.case = TRUE)))
})
q_fit <- lm(rank(y) ~ rank(cov), data = anc)
q_group <- lm(residuals(q_fit) ~ grp, data = anc)
q_ref <- anova(q_group)
ok("ANCOVA reports Quade rank ANCOVA as the alternative", {
  stopifnot(grepl("Quade", r_anc$alternative$label, ignore.case = TRUE),
            r_anc$alternative$table$value[r_anc$alternative$table$statistic == "F"] ==
              round(q_ref$`F value`[1], 3))
})
ok("Quade ANCOVA p-value matches ranked-residual reference", {
  stopifnot(near(r_anc$alternative$p_value, q_ref$`Pr(>F)`[1]))
})

ok("ANCOVA with two covariates matches the two-covariate reference", {
  anc$cov2 <- rnorm(n)
  r2 <- route(anc, list(family = "location", design = "ancova", outcome = "y",
                        group = "grp", covariates = c("cov", "cov2")))
  full2 <- lm(y ~ cov + cov2 + grp, anc); red2 <- lm(y ~ cov + cov2, anc)
  stopifnot(near(r2$primary$p_value, anova(red2, full2)$`Pr(>F)`[2]))
})
ok("ANCOVA with a 2-level group is allowed (adjusted 2-group comparison)", {
  anc$g2 <- factor(ifelse(anc$grp == "A", "x", "y"))
  r <- route(anc, list(family = "location", design = "ancova", outcome = "y",
                       group = "g2", covariates = "cov"))
  stopifnot(is.list(r), !is.null(r$primary))
})

cat("\n-- ANCOVA guards --\n")
ok("ANCOVA with no covariate is refused with a clear message",
   stopifnot(is.character(route(anc, list(family = "location", design = "ancova",
     outcome = "y", group = "grp", covariates = character(0))))))
ok("ANCOVA covariate equal to the outcome is refused",
   stopifnot(is.character(route(anc, list(family = "location", design = "ancova",
     outcome = "y", group = "grp", covariates = "y")))))
ok("valid_grouping_vars('ancova') allows 2-level factors", {
  tp <- c(grp = "nominal", g2 = "binary", cov = "continuous", y = "continuous")
  gv <- valid_grouping_vars(tp, anc, "ancova")
  stopifnot("g2" %in% gv, "grp" %in% gv)
})

# ===========================================================================
cat("\n== Point-biserial correlation ==\n")
# ===========================================================================
set.seed(9); m <- 140
pb <- data.frame(bin = factor(sample(c("no", "yes"), m, TRUE)), val = rnorm(m))
pb$val <- pb$val + ifelse(pb$bin == "yes", 0.7, 0)
r_pb <- route(pb, list(family = "point_biserial", x = "bin", y = "val"))
x01 <- as.integer(pb$bin == "yes"); ref <- suppressWarnings(cor.test(x01, pb$val))

ok("point-biserial routes and is labelled", {
  stopifnot(is.list(r_pb), !is.null(r_pb$primary),
            grepl("point-biserial", r_pb$primary$label, ignore.case = TRUE))
})
ok("r_pb equals Pearson r on 0/1 coding",
   stopifnot(near(val(r_pb, "r_pb"), round(unname(ref$estimate), 3), tol = 1e-3)))
ok("p-value equals cor.test on 0/1 coding",
   stopifnot(near(r_pb$primary$p_value, ref$p.value)))
ok("p-value equals the pooled two-sample t-test",
   stopifnot(near(r_pb$primary$p_value,
                  t.test(val ~ bin, data = pb, var.equal = TRUE)$p.value)))
ok("nonparametric alternative is the rank-biserial (Mann-Whitney)",
   stopifnot(grepl("rank-biserial", r_pb$alternative$label, ignore.case = TRUE)))
ok("large-n normal data recommends the parametric primary",
   stopifnot(r_pb$recommended == "primary"))

cat("\n-- point-biserial guards & gating --\n")
ok("x == y is refused",
   stopifnot(is.character(route(pb, list(family = "point_biserial", x = "bin", y = "bin")))))
ok("a non-binary x is refused", {
  pb$three <- factor(sample(c("a", "b", "c"), m, TRUE))
  stopifnot(is.character(route(pb, list(family = "point_biserial", x = "three", y = "val"))))
})
ok("family is opt-in: offered only when requested, needs binary + numeric", {
  tp <- c(bin = "binary", val = "continuous")
  stopifnot("point_biserial" %in% available_test_families(tp, include_point_biserial = TRUE),
            !"point_biserial" %in% available_test_families(tp),
            !"point_biserial" %in% available_test_families(c(a = "binary", b = "nominal"),
                                                           include_point_biserial = TRUE))
})

# ===========================================================================
cat("\n== Tests-tab wiring ==\n")
# ===========================================================================
tp <- c(grp = "nominal", cov = "continuous", bin = "binary", y = "continuous")
wire <- data.frame(grp = factor(sample(c("A","B","C"), 120, TRUE)), cov = rnorm(120),
                   bin = factor(sample(c("no","yes"), 120, TRUE)))
wire$y <- 1 + wire$cov + as.numeric(wire$grp) + rnorm(120)

ok("Tests tab routes an ANCOVA end-to-end", {
  title <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(wire), types = reactive(tp),
                outcomes = reactive(c("y")), panel = reactive(list())), {
      session$setInputs(family = "location", outcome = "y", design = "ancova",
                        group = "grp", covariates = "cov")
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      title <<- output$primary_title
    })
  stopifnot(is.character(title), grepl("ANCOVA", title))
})
ok("Tests tab routes a point-biserial correlation end-to-end", {
  title <- NULL; okflag <- NULL
  shiny::testServer(mod_tests_server,
    args = list(data = reactive(wire), types = reactive(tp),
                outcomes = reactive(c("y")), panel = reactive(list())), {
      session$setInputs(family = "point_biserial", pb_binary = "bin", pb_numeric = "y")
      session$flushReact(); session$setInputs(run = 1); session$flushReact()
      title <<- output$primary_title; okflag <<- is_ok()
    })
  stopifnot(isTRUE(okflag), grepl("point-biserial", title, ignore.case = TRUE))
})

report_and_exit()
