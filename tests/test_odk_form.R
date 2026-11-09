# tests/test_odk_form.R ------------------------------------------------------
# Reading an ODK / XLSForm form definition and using it to type and label data.
#
# HONEST SCOPE NOTE. The REDCap reader was built and measured against nine real
# dictionaries and their exports. There is no ODK form anywhere in the WSU study
# folders - 1,653 workbooks were scanned for a survey+choices sheet pair and none
# has one, there is no ODK XML, and no ODK-shaped CSV export; that work is all
# REDCap, plus one Epicollect5 file. So this reader is written to the XLSForm
# specification and verified against fixtures written from it, NOT against a real
# form. That is a weaker standard than the rest of this suite, and the fixtures
# below are deliberately awkward - translated label columns, nested groups,
# underscore and space spellings of begin/end, an external choice list, a
# duplicate choice code - so that the spec is exercised rather than a happy path.
# Point it at a real form before relying on it.
#
# What is pinned:
#   * XLSForm question types map to app measurement types, and a type that
#     carries no measurement claim (text, calculate, geopoint) makes none;
#   * group and repeat nesting produces the path an export uses in its column
#     names, and a bare field name still matches a path-prefixed column;
#   * BOTH select_multiple export conventions are handled - expanded to one 0/1
#     column per choice, and the single space-separated column ODK Central
#     writes by default, which is NOT silently treated as a category;
#   * everything shares the apply path with the REDCap dictionary.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

if (!requireNamespace("writexl", quietly = TRUE) &&
    !requireNamespace("openxlsx", quietly = TRUE)) {
  cat("\n  SKIP  writing an .xlsx fixture needs writexl or openxlsx\n")
  report_and_exit()
}

write_xlsform <- function(survey, choices = NULL, survey_sheet = "survey") {
  p <- tempfile(fileext = ".xlsx")
  sheets <- stats::setNames(list(survey), survey_sheet)
  if (!is.null(choices)) sheets[["choices"]] <- choices
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, p)
  } else {
    wb <- openxlsx::createWorkbook()
    for (nm in names(sheets)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, sheets[[nm]])
    }
    openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  }
  p
}

# A form with the shapes that actually cause trouble: a translated label column,
# nested groups, both begin/end spellings, an external choice list, and a
# select_multiple.
survey <- data.frame(
  type = c("start", "deviceid",
           "begin group", "select_one yes_no", "integer", "decimal",
           "begin_repeat", "select_one ward", "date", "end_repeat",
           "end group",
           "select_multiple symptoms", "text", "calculate", "geopoint",
           "acknowledge", "note", "select_one_from_file households.csv"),
  name = c("start_time", "device",
           "household", "consented", "members", "water_litres",
           "person", "ward", "visit_date", "person",
           "household",
           "symptoms", "other_detail", "score", "location",
           "agreed", "intro", "hh_ref"),
  `label::English` = c("Start", "Device",
           "Household", "Consent given", "Household members", "Water (litres)",
           "Person", "Ward", "Date of visit", "Person",
           "Household",
           "Symptoms reported", "Anything else", "Score", "Location",
           "Acknowledged", "Read this", "Household reference"),
  check.names = FALSE, stringsAsFactors = FALSE
)
choices <- data.frame(
  list_name = c("yes_no", "yes_no",
                "ward", "ward", "ward", "ward",
                "symptoms", "symptoms", "symptoms"),
  name = c("yes", "no", "med", "surg", "paeds", "med",
           "fever", "cough", "rash"),
  `label::English` = c("Yes", "No", "Medicine", "Surgery", "Paediatrics",
                       "Medicine (duplicate)", "Fever", "Cough", "Rash"),
  check.names = FALSE, stringsAsFactors = FALSE
)
fpath <- write_xlsform(survey, choices)

cat("\n== reading the form definition ==\n")

ok("the survey sheet is read and structure rows are not questions", {
  f <- read_odk_form(fpath)
  stopifnot(!any(c("household", "person", "intro") %in%
                   f$field[f$source_type %in% ODK_STRUCTURE_TYPES]),
            !any(grepl("^(begin|end) ", f$source_type)),
            "consented" %in% f$field, "visit_date" %in% f$field)
})

ok("XLSForm types map to app measurement types", {
  f <- read_odk_form(fpath)
  tp <- stats::setNames(f$type, f$field)
  stopifnot(identical(unname(tp[["consented"]]), "binary"),   # 2-choice list
            identical(unname(tp[["ward"]]), "nominal"),
            identical(unname(tp[["members"]]), "count"),
            identical(unname(tp[["water_litres"]]), "continuous"),
            identical(unname(tp[["visit_date"]]), "datetime"),
            identical(unname(tp[["start_time"]]), "datetime"),
            identical(unname(tp[["device"]]), "identifier"),
            identical(unname(tp[["agreed"]]), "binary"),
            identical(unname(tp[["hh_ref"]]), "nominal"))  # external list
})

ok("types carrying no measurement claim make none", {
  f <- read_odk_form(fpath)
  tp <- stats::setNames(f$type, f$field)
  stopifnot(is.na(tp[["other_detail"]]), is.na(tp[["score"]]),
            is.na(tp[["location"]]), is.na(tp[["symptoms"]]))
})

ok("ODK dates are read as ISO, not left to be guessed", {
  f <- read_odk_form(fpath)
  stopifnot(identical(f$date_format[f$field == "visit_date"], "ymd"),
            f$date_format[f$field == "visit_date"] %in% DATE_FORMAT_CHOICES)
})

ok("a translated label column is used when there is no plain 'label'", {
  f <- read_odk_form(fpath)
  stopifnot(identical(f$label[f$field == "water_litres"], "Water (litres)"),
            identical(f$label[f$field == "consented"], "Consent given"))
})

ok("group and repeat nesting produces the export's path", {
  f <- read_odk_form(fpath)
  grp <- stats::setNames(f$form, f$field)
  stopifnot(identical(unname(grp[["consented"]]), "household"),
            identical(unname(grp[["ward"]]), "household/person"),
            identical(unname(grp[["symptoms"]]), ""))
})

ok("both begin/end spellings close their group", {
  # `begin_repeat` / `end_repeat` and `begin group` / `end group` both occur.
  f <- read_odk_form(fpath)
  stopifnot(identical(f$form[f$field == "visit_date"], "household/person"),
            identical(f$form[f$field == "other_detail"], ""))
})

ok("a duplicate choice code keeps the first rather than losing the list", {
  f <- read_odk_form(fpath)
  ward <- f$choices[[which(f$field == "ward")]]
  stopifnot(length(ward) == 3L, identical(unname(ward[["med"]]), "Medicine"))
})

ok("a select_multiple is flagged as multi and claims no single type", {
  f <- read_odk_form(fpath)
  stopifnot(isTRUE(f$multi[f$field == "symptoms"]),
            is.na(f$type[f$field == "symptoms"]),
            !any(f$multi[f$field != "symptoms"]))
})

ok("the expansion separator and source are declared for the shared apply path", {
  f <- read_odk_form(fpath)
  stopifnot(identical(attr(f, "expand_sep"), "/"),
            grepl("ODK", attr(f, "source")),
            # same columns as the REDCap spec, so apply_form_metadata() is shared
            all(c("field", "form", "source_type", "label", "type",
                  "date_format", "multi", "choices") %in% names(f)))
})

cat("\n== the guards ==\n")

ok("a workbook with no survey sheet is refused clearly", {
  bad <- write_xlsform(data.frame(a = 1, b = 2), survey_sheet = "sheet1")
  stopifnot(grepl("needs a 'survey' sheet", friendly_error(read_odk_form(bad))),
            grepl("could not be found", friendly_error(read_odk_form(tempfile()))))
})

ok("a survey sheet without type or name is refused clearly", {
  bad <- write_xlsform(data.frame(question = "x", stringsAsFactors = FALSE))
  stopifnot(grepl("needs both a 'type' and a 'name' column",
                  friendly_error(read_odk_form(bad))))
})

ok("a form with only structure rows is refused rather than returning nothing", {
  bad <- write_xlsform(data.frame(type = c("begin group", "end group"),
                                  name = c("g", "g"), stringsAsFactors = FALSE))
  stopifnot(grepl("lists no questions", friendly_error(read_odk_form(bad))))
})

ok("odk_parse_type splits the keyword from the list name", {
  stopifnot(identical(odk_parse_type("select_one yes_no")$list_name, "yes_no"),
            identical(odk_parse_type("select_multiple  symptoms")$list_name,
                      "symptoms"),
            isTRUE(odk_parse_type("select_multiple s")$multi),
            !isTRUE(odk_parse_type("select_one s")$multi),
            isTRUE(odk_parse_type("select_one_from_file hh.csv")$external),
            identical(odk_parse_type("begin_group")$kind, "begin group"),
            identical(odk_parse_type("integer")$kind, "integer"),
            identical(odk_parse_type("")$kind, ""))
})

cat("\n== applying an ODK form to an export ==\n")

# An export written the way ODK Central writes one: full group paths, and the
# select_multiple SPLIT into one 0/1 column per choice.
odk_df <- data.frame(
  `start_time`               = c("2024-04-03T08:15", "2024-04-04T09:00"),
  `household/consented`      = c("yes", "no"),
  `household/members`        = c(4, 6),
  `household/water_litres`   = c(20.5, 33),
  `household/person/ward`    = c("med", "surg"),
  `household/person/visit_date` = c("2024-04-03", "2024-04-05"),
  `symptoms/fever`           = c(1, 0),
  `symptoms/cough`           = c(0, 1),
  `other_detail`             = c("note a", "note b"),
  `meta/instanceID`          = c("uuid:1", "uuid:2"),
  check.names = FALSE, stringsAsFactors = FALSE
)
spec <- read_odk_form(fpath)
a <- apply_form_metadata(odk_df, spec)

ok("a path-prefixed column matches its bare field name", {
  m <- form_match_columns(odk_df, spec)
  stopifnot(identical(m$field[m$column == "household/consented"], "consented"),
            identical(m$field[m$column == "household/person/ward"], "ward"),
            identical(m$field[m$column == "start_time"], "start_time"))
})

ok("coded answers become factors with their real labels", {
  stopifnot(is.factor(a$data[["household/consented"]]),
            setequal(levels(a$data[["household/consented"]]), c("Yes", "No")),
            is.factor(a$data[["household/person/ward"]]),
            all(c("Medicine", "Surgery") %in%
                  levels(a$data[["household/person/ward"]])))
})

ok("a split select_multiple column reads as checked/unchecked", {
  stopifnot(is.factor(a$data[["symptoms/fever"]]),
            setequal(levels(a$data[["symptoms/fever"]]),
                     c("Unchecked", "Checked")),
            identical(unname(a$types[["symptoms/fever"]]), "binary"),
            identical(unname(a$types[["symptoms/cough"]]), "binary"))
})

ok("numeric and date types are converted, with ODK's ISO order", {
  stopifnot(is.numeric(a$data[["household/members"]]),
            identical(unname(a$types[["household/members"]]), "count"),
            identical(unname(a$types[["household/water_litres"]]), "continuous"),
            inherits(a$data[["household/person/visit_date"]], "Date"),
            identical(format(a$data[["household/person/visit_date"]][1],
                             "%Y-%m-%d"), "2024-04-03"))
})

ok("columns the form does not describe are left alone", {
  stopifnot(identical(a$data[["meta/instanceID"]], odk_df[["meta/instanceID"]]),
            identical(a$data[["other_detail"]], odk_df[["other_detail"]]),
            "meta/instanceID" %in% a$unmatched)
})

ok("labels travel with the columns", {
  stopifnot(identical(attr(a$data[["household/water_litres"]], "label"),
                      "Water (litres)"))
})

cat("\n== the unsplit select_multiple hazard ==\n")

ok("an unsplit select_multiple is reported, not read as a category", {
  # ODK Central's default writes "fever cough" into one column. Treating that as
  # a factor would invent a level per observed COMBINATION, which is wrong.
  flat <- data.frame(
    `household/consented` = c("yes", "no"),
    symptoms = c("fever cough", "rash"),
    check.names = FALSE, stringsAsFactors = FALSE)
  b <- apply_form_metadata(flat, spec)
  stopifnot(!is.null(b$multi_note),
            grepl("space-separated", b$multi_note),
            grepl("symptoms", b$multi_note),
            # and it must NOT have been turned into a factor of combinations
            !is.factor(b$data$symptoms),
            !"symptoms" %in% names(b$types))
  stopifnot(grepl("space-separated", form_apply_summary(b, flat)))
})

cat("\n== format detection ==\n")

ok("an XLSForm and a REDCap dictionary are each detected from the file", {
  f <- read_form_metadata(fpath, basename(fpath))
  stopifnot(grepl("ODK", attr(f, "source")))
  rc <- tempfile(fileext = ".csv")
  writeLines(c('"Variable / Field Name","Form Name","Section Header","Field Type","Field Label","Choices, Calculations, OR Slider Labels","Field Note","Text Validation Type OR Show Slider Number"',
               'sex,f1,,radio,"Sex","1, Female|2, Male",,'), rc)
  d <- read_form_metadata(rc, basename(rc))
  stopifnot(grepl("REDCap", attr(d, "source")),
            identical(d$type[d$field == "sex"], "binary"))
})

ok("an unrecognised file names both formats in the error", {
  junk <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1, b = 2), junk, row.names = FALSE)
  msg <- friendly_error(read_form_metadata(junk, basename(junk)))
  stopifnot(grepl("REDCap", msg), grepl("XLSForm", msg))
})

report_and_exit()
