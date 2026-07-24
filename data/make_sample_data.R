# make_sample_data.R --------------------------------------------------------
# Optional: build a clean-typed .rda version of the sample cohort from the CSV,
# so you can also smoke-test the RDA import path (with proper Date / ordered
# factor types already set).
#
# Run from this folder:  Rscript make_sample_data.R

csv <- "lvct_cohort_repeated.csv"
if (!file.exists(csv))
  stop("Run make_sample_data.py first to create ", csv)

lvct_cohort <- read.csv(csv, stringsAsFactors = FALSE)

# Set proper R types (these override what auto-detection would guess).
lvct_cohort$patient_id   <- as.character(lvct_cohort$patient_id)        # identifier
lvct_cohort$visit        <- factor(lvct_cohort$visit,
                                   levels = c("Baseline", "6 months", "12 months"),
                                   ordered = TRUE)                        # ordinal
lvct_cohort$visit_date   <- as.Date(lvct_cohort$visit_date)              # datetime
lvct_cohort$who_stage    <- factor(lvct_cohort$who_stage, ordered = TRUE) # ordinal
lvct_cohort$sex          <- factor(lvct_cohort$sex)                       # binary
lvct_cohort$on_art       <- factor(lvct_cohort$on_art)                    # binary
lvct_cohort$county       <- factor(lvct_cohort$county)                    # nominal
lvct_cohort$regimen      <- factor(lvct_cohort$regimen)                   # nominal

save(lvct_cohort, file = "lvct_cohort_repeated.rda")
cat(sprintf("Wrote lvct_cohort_repeated.rda: %d rows, %d cols, %d patients.\n",
            nrow(lvct_cohort), ncol(lvct_cohort),
            length(unique(lvct_cohort$patient_id))))
