# tests/test_redcap_dictionary.R ---------------------------------------------
# Reading a REDCap data dictionary and using it to type and label the data.
#
# The problem it solves is measured, not assumed: on the PPS Phase II patient
# export, 61 radio/dropdown fields are typed as numeric by shape alone, and 14 of
# those cannot be separated from genuine counts by any shape-based rule -
# `ant_one` (25 antibiotic codes) and `dur6` (19 durations in days) are identical
# in form. The dictionary states the answer instead of inferring it.
#
# The contract pinned here:
#   * the export is parsed as it comes (BOM, comma-bearing labels, trailing
#     columns that vary between REDCap versions);
#   * a checkbox field's expanded `x___k` columns are recognised;
#   * a column the dictionary does not describe is left entirely alone;
#   * a code present in the data but missing from the dictionary is KEPT, never
#     silently turned into NA;
#   * a stated type is not flagged as needing the analyst's attention.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# --- a dictionary written the way REDCap writes them ----------------------
dict_csv <- function(rows, bom = TRUE, header = NULL) {
  hdr <- header %||% paste(
    '"Variable / Field Name","Form Name","Section Header","Field Type"',
    '"Field Label","Choices, Calculations, OR Slider Labels","Field Note"',
    '"Text Validation Type OR Show Slider Number"', sep = ",")
  p <- tempfile(fileext = ".csv")
  con <- file(p, open = "wb")
  writeLines(paste0(if (bom) "﻿" else "", hdr), con, useBytes = TRUE)
  writeLines(rows, con, useBytes = TRUE)
  close(con)
  p
}

rows <- c(
  'participant_id,form1,,text,"Participant ID",,,',
  'ward_group,form1,,radio,"Ward group","1, Medicine|2, Surgery|3, Paediatrics",,',
  'sex,form1,,radio,"Sex","1, Female|2, Male",,',
  'consented,form1,,yesno,"Consent given",,,',
  'devices,form1,,checkbox,"Devices in situ","1, Urinary|2, Central|6, Other",,',
  'age_years,form1,,text,"Age in years",,,integer',
  'weight_kg,form1,,text,"Weight (kg)",,,number',
  'admit_date,form1,,text,"Date of admission",,,date_dmy',
  'bmi_calc,form1,,calc,"BMI","round(([weight_kg]/1),2)",,',
  'section_head,form1,,descriptive,"Section heading",,,',
  'other_txt,form1,,text,"Other, please specify",,,',
  'reason,form1,,dropdown,"Reason","1, Treatment, curative|2, Prophylaxis",,'
)
dpath <- dict_csv(rows)

cat("\n== reading the dictionary ==\n")

ok("the export is read despite a BOM on the first header cell", {
  d <- read_redcap_dictionary(dpath)
  stopifnot(nrow(d) == 12L, "participant_id" %in% d$field,
            identical(d$field[[1]], "participant_id"))
})

ok("REDCap field types map to app measurement types", {
  d <- read_redcap_dictionary(dpath)
  tp <- stats::setNames(d$type, d$field)
  stopifnot(identical(unname(tp[["ward_group"]]), "nominal"),
            identical(unname(tp[["sex"]]), "binary"),      # only 2 choices
            identical(unname(tp[["reason"]]), "binary"),   # only 2 choices
            identical(unname(tp[["consented"]]), "binary"),
            identical(unname(tp[["devices"]]), "binary"),
            identical(unname(tp[["age_years"]]), "count"),
            identical(unname(tp[["weight_kg"]]), "continuous"),
            identical(unname(tp[["admit_date"]]), "datetime"),
            identical(unname(tp[["bmi_calc"]]), "continuous"))
})

ok("a plain text field makes no claim, and descriptive has no column", {
  d <- read_redcap_dictionary(dpath)
  tp <- stats::setNames(d$type, d$field)
  stopifnot(is.na(tp[["participant_id"]]), is.na(tp[["other_txt"]]),
            is.na(tp[["section_head"]]))
})

ok("the date validation supplies the day/month order", {
  d <- read_redcap_dictionary(dpath)
  stopifnot(identical(d$date_format[d$field == "admit_date"], "dmy"),
            d$date_format[d$field == "admit_date"] %in% DATE_FORMAT_CHOICES)
})

ok("choices are parsed, splitting only on the first comma", {
  ch <- redcap_parse_choices("1, Treatment, curative|2, Prophylaxis")
  stopifnot(identical(ch, c("1" = "Treatment, curative", "2" = "Prophylaxis")))
})

ok("yesno gets its implicit labels; a calc formula is not read as choices", {
  d <- read_redcap_dictionary(dpath)
  stopifnot(identical(d$choices[[which(d$field == "consented")]],
                      c("0" = "No", "1" = "Yes")),
            is.null(d$choices[[which(d$field == "bmi_calc")]]),
            is.null(redcap_parse_choices("round(([weight_kg]/1),2)")))
})

ok("a malformed choice entry is skipped, not fatal to the field", {
  # Three ARCH 1.0 hospital fields really do list an option with no comma
  # ("speculum_exam_Not_under_anaesthesia"). Discarding the other eight labels
  # over it would lose more than it protects, so the bad entry is skipped.
  ch <- redcap_parse_choices("1, Suturing|nocomma_option|2, Wound care")
  stopifnot(identical(ch, c("1" = "Suturing", "2" = "Wound care")))
  # But a field where NOTHING parses is still rejected, which is what keeps a
  # calc formula from being read as choices.
  stopifnot(is.null(redcap_parse_choices("nocomma_only")),
            is.null(redcap_parse_choices("1, A|1, B")))   # duplicate codes
})

ok("a non-dictionary CSV is refused with a clear message", {
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1, b = 2), bad, row.names = FALSE)
  stopifnot(grepl("does not look like a REDCap data dictionary",
                  friendly_error(read_redcap_dictionary(bad))),
            grepl("could not be found",
                  friendly_error(read_redcap_dictionary(tempfile()))))
})

# --- data matching the dictionary -----------------------------------------
df <- data.frame(
  participant_id = sprintf("P%03d", 1:6),
  ward_group     = c(1, 2, 3, 1, 2, 9),          # 9 is NOT in the dictionary
  sex            = c(1, 2, 1, 2, 1, 2),
  consented      = c(1, 1, 0, 1, 0, 1),
  devices___1    = c(0, 1, 0, 1, 1, 0),
  devices___6    = c(1, 0, 0, 0, 1, 1),
  age_years      = c(34, 2, 71, 55, 8, 40),
  weight_kg      = c(60.5, 12.2, 70, 80.1, 25, 66),
  admit_date     = c("03/04/2023", "11/12/2023", "01/01/2023",
                     "28/02/2023", "15/06/2023", "09/09/2023"),
  reason         = c(1, 2, 1, 1, 2, 1),
  other_txt      = c("a", "b", "", "d", "e", "f"),
  redcap_event_name = rep("visit_1", 6),         # REDCap's own, not in the dict
  form1_complete = rep(2, 6),                    # REDCap's own, not in the dict
  stringsAsFactors = FALSE
)
dict <- read_redcap_dictionary(dpath)

cat("\n== matching columns to fields ==\n")

ok("expanded checkbox columns are matched back to their field", {
  m <- form_match_columns(df, dict)
  stopifnot(identical(m$field[m$column == "devices___1"], "devices"),
            identical(m$choice_code[m$column == "devices___6"], "6"))
})

ok("REDCap's own structure columns stay unmatched", {
  m <- form_match_columns(df, dict)
  unmatched <- m$column[is.na(m$field)]
  stopifnot(setequal(unmatched, c("redcap_event_name", "form1_complete")))
})

ok("a checkbox stem is matched to the longest field name, not a prefix", {
  d2 <- rbind(dict, dict[dict$field == "devices", ])
  d2$field[nrow(d2)] <- "dev"
  d2$multi[nrow(d2)] <- TRUE
  m <- form_match_columns(data.frame(dev_ices___1 = 1, devices___1 = 1), d2)
  stopifnot(identical(m$field[m$column == "devices___1"], "devices"))
})

cat("\n== applying the dictionary to the data ==\n")
a <- apply_form_metadata(df, dict)

ok("coded fields become factors labelled with their real names", {
  stopifnot(is.factor(a$data$ward_group),
            all(c("Medicine", "Surgery", "Paediatrics") %in%
                  levels(a$data$ward_group)),
            identical(levels(a$data$sex), c("Female", "Male")),
            identical(levels(a$data$consented), c("No", "Yes")))
})

ok("a checkbox column reads as checked/unchecked", {
  stopifnot(is.factor(a$data$devices___1),
            setequal(levels(a$data$devices___1), c("Unchecked", "Checked")),
            identical(unname(a$types[["devices___1"]]), "binary"))
})

ok("a code in the data but not in the dictionary is kept, not dropped", {
  # Silently turning it into NA would lose a value the export really contains.
  stopifnot("9" %in% levels(a$data$ward_group),
            !any(is.na(a$data$ward_group)),
            !is.null(a$unknown_codes),
            identical(a$unknown_codes$value, "9"),
            identical(a$unknown_codes$column, "ward_group"))
})

ok("validated dates are parsed with the stated order, not a guessed one", {
  # 03/04/2023 is day-first per date_dmy: 3 April, never 4 March.
  stopifnot(inherits(a$data$admit_date, "Date"),
            identical(format(a$data$admit_date[1], "%Y-%m-%d"), "2023-04-03"),
            identical(unname(a$date_formats[["admit_date"]]), "dmy"))
})

ok("validated numerics become numeric with the right measurement type", {
  stopifnot(is.numeric(a$data$age_years), is.numeric(a$data$weight_kg),
            identical(unname(a$types[["age_years"]]), "count"),
            identical(unname(a$types[["weight_kg"]]), "continuous"))
})

ok("columns the dictionary does not describe are untouched", {
  stopifnot(identical(a$data$redcap_event_name, df$redcap_event_name),
            identical(a$data$other_txt, df$other_txt),
            identical(a$data$participant_id, df$participant_id),
            !any(c("redcap_event_name", "form1_complete", "other_txt",
                   "participant_id") %in% names(a$types)))
})

ok("the field label travels with the column for the planner and report", {
  stopifnot(identical(attr(a$data$ward_group, "label"), "Ward group"),
            identical(attr(a$data$weight_kg, "label"), "Weight (kg)"))
})

ok("an empty or absent dictionary is a no-op, not an error", {
  z <- apply_form_metadata(df, NULL)
  stopifnot(identical(z$data, df), identical(z$types, character(0)),
            setequal(z$unmatched, names(df)))
})

ok("the summary reports what happened, including unlisted values", {
  msg <- form_apply_summary(a, df)
  stopifnot(grepl("matched", msg), grepl("relabelled", msg),
            grepl("not listed in the dictionary", msg))
})

cat("\n== the effect on detection and on the types screen ==\n")

ok("a stated type overrides the shape-based guess", {
  # ward_group is 1/2/3/9: whole numbers, few distinct - detected as a count.
  before <- detect_all_types(df)
  stopifnot(identical(before$type[before$variable == "ward_group"], "count"))
  after <- detect_all_types(a$data)
  ty <- stats::setNames(after$type, after$variable)
  ty[names(a$types)] <- a$types
  stopifnot(identical(unname(ty[["ward_group"]]), "nominal"),
            identical(unname(ty[["sex"]]), "binary"))
})

ok("a dictionary-sourced type is not flagged as needing attention", {
  # It is stated, not inferred, so there is nothing for the analyst to decide.
  d <- detect_all_types(df)
  d$from_dictionary <- d$variable %in% c("ward_group", "sex")
  flag <- types_needing_attention(d)
  stopifnot(!any(flag[d$variable %in% c("ward_group", "sex")]),
            # and the rule is unchanged where no dictionary applies
            identical(types_needing_attention(d[, setdiff(names(d), "from_dictionary")])[
                        d$variable == "ward_group"], TRUE))
})

# --- against the real PPS export ------------------------------------------
# Opt in with SABALI_PPS_DIR pointing at the folder holding the export and its
# dictionary. Skipped otherwise, so the suite carries no participant data.
pps <- Sys.getenv("SABALI_PPS_DIR", "")
data_csv <- if (nzchar(pps))
  Sys.glob(file.path(pps, "*Patient data*DATA_2*.csv")) else character(0)
dict_real <- if (nzchar(pps))
  Sys.glob(file.path(pps, "*Patient*DataDictionary*.csv")) else character(0)
if (length(data_csv) && length(dict_real)) {
  cat("\n== real PPS Phase II patient export ==\n")
  ok("every mistyped categorical field is corrected by the dictionary", {
    rdf <- read_any_format(data_csv[[1]])
    rd  <- read_redcap_dictionary(dict_real[[1]])
    ra  <- apply_form_metadata(rdf, rd)
    before <- detect_all_types(rdf)
    ty0 <- stats::setNames(before$type, before$variable)
    after <- detect_all_types(ra$data)
    ty1 <- stats::setNames(after$type, after$variable)
    ty1[names(ra$types)] <- ra$types
    cats <- rd$field[rd$source_type %in% c("radio", "dropdown")]
    n0 <- sum(ty0[cats] %in% c("count", "continuous"), na.rm = TRUE)
    n1 <- sum(ty1[cats] %in% c("count", "continuous"), na.rm = TRUE)
    cat(sprintf("      categorical fields typed as numeric: %d -> %d\n", n0, n1))
    stopifnot(n0 >= 60L, n1 == 0L)
  })
} else {
  cat("\n  (set SABALI_PPS_DIR to also run the real-export check)\n")
}

report_and_exit()
