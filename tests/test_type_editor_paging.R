# tests/test_type_editor_paging.R --------------------------------------------
# The confirm-types screen on wide data.
#
# The screen used to render one row - two dropdowns plus an observer - for every
# column in the file. On the ARCH 2.0 household extracts (1,743 columns) that is
# 3,486 widgets, ~3.6 MB of HTML and 1,743 observers before the analyst can click
# anything, and a further 1,743 date-preview outputs of which all but 60 could
# never appear. It is now paged, searchable and filterable.
#
# The filtering and paging rules live in R/fct_types.R rather than inside the
# module so they can be checked here directly. What matters:
#   * row indices are positions in detected(), so input ids survive paging and
#     searching and types()/base_levels() keep reading them unchanged;
#   * every filtered row appears on exactly one page;
#   * the "needs a look" filter surfaces DECISIONS, not columns nothing can be
#     done about - see the note on coverage at the end.

local({
  args <- commandArgs(FALSE)
  fa <- sub("^--file=", "", args[grepl("^--file=", args)])
  here <- if (length(fa) == 1) dirname(normalizePath(fa)) else "tests"
  source(file.path(here, "helper.R"))
})

suppressPackageStartupMessages(library(shiny))

# A detected-types frame covering every case the filters distinguish.
det <- data.frame(
  variable    = c("ward_group", "age_group", "visits", "ant_one", "bmi",
                  "participant_id", "empty_col", "const_col", "sex",
                  "hh.date(1)"),
  type        = c("count", "count", "count", "count", "continuous",
                  "identifier", "text", "count", "binary", "datetime"),
  distinct    = c(4L, 5L, 25L, 25L, 300L, 500L, 0L, 1L, 2L, 40L),
  missing_pct = c(0, 0.4, 0, 46, 2, 0, 100, 0, 0, 1),
  stringsAsFactors = FALSE
)

cat("\n== which columns are a decision, and which are just dead ==\n")

ok("all-missing and constant columns are classed unusable", {
  u <- types_unusable(det)
  stopifnot(identical(det$variable[u], c("empty_col", "const_col")))
})

ok("small integer code sets are flagged as needing a decision", {
  a <- types_needing_attention(det)
  stopifnot(all(c("ward_group", "age_group") %in% det$variable[a]))
})

ok("an identifier is flagged, a continuous variable is not", {
  a <- types_needing_attention(det)
  stopifnot("participant_id" %in% det$variable[a],
            !"bmi" %in% det$variable[a])
})

ok("high-cardinality counts are not flagged", {
  # 25 distinct whole numbers is as consistent with a duration in days as with a
  # code list; flagging them would surface every count column. See the coverage
  # note at the foot of this file.
  a <- types_needing_attention(det)
  stopifnot(!any(c("visits", "ant_one") %in% det$variable[a]))
})

ok("dead columns are NOT also flagged as needing a decision", {
  # Including them is what made the filter useless: on adult_merge.dta 623 empty
  # columns accounted for most of 1,058 hits and none could be acted on.
  a <- types_needing_attention(det)
  stopifnot(!any(c("empty_col", "const_col") %in% det$variable[a]))
  stopifnot(!any(types_unusable(det) & a))
})

ok("an empty detected frame yields empty logicals, not an error", {
  e <- det[0, ]
  stopifnot(identical(types_needing_attention(e), logical(0)),
            identical(types_unusable(e), logical(0)),
            identical(type_editor_rows(e), integer(0)))
})

cat("\n== the search box and the filter ==\n")

ok("search is a literal substring, case-insensitive", {
  stopifnot(identical(det$variable[type_editor_rows(det, "GROUP")],
                      c("ward_group", "age_group")),
            identical(det$variable[type_editor_rows(det, "_id")],
                      "participant_id"))
})

ok("a search containing regex metacharacters searches, not explodes", {
  # An analyst typing a real column name like "hh.date(1)" must not hit an
  # invalid-regex error, and "." must not match any character.
  stopifnot(identical(det$variable[type_editor_rows(det, "hh.date(1)")],
                      "hh.date(1)"),
            length(type_editor_rows(det, "hh_date(1)")) == 0L,
            length(type_editor_rows(det, "[")) == 0L)
})

ok("the filters select the right subsets, and combine with search", {
  stopifnot(setequal(det$variable[type_editor_rows(det, "", "attention")],
                     c("ward_group", "age_group", "participant_id")),
            setequal(det$variable[type_editor_rows(det, "", "unusable")],
                     c("empty_col", "const_col")),
            identical(det$variable[type_editor_rows(det, "ward", "attention")],
                      "ward_group"),
            length(type_editor_rows(det, "", "all")) == nrow(det))
})

ok("an unknown filter value falls back to showing everything", {
  stopifnot(length(type_editor_rows(det, "", "nonsense")) == nrow(det))
})

cat("\n== paging arithmetic ==\n")

ok("page size and page count are consistent", {
  stopifnot(TYPE_EDITOR_PAGE_SIZE >= 10L,
            type_editor_n_pages(1:100, 25L) == 4L,
            type_editor_n_pages(1:101, 25L) == 5L,
            type_editor_n_pages(integer(0)) == 1L)
})

ok("a page returns exactly its slice of the filtered rows", {
  rows <- seq(2L, 200L, by = 2L)            # 100 rows, non-contiguous indices
  stopifnot(identical(type_editor_page_rows(rows, 1L, 25L), rows[1:25]),
            identical(type_editor_page_rows(rows, 4L, 25L), rows[76:100]))
})

ok("the last page is short rather than padded", {
  rows <- 1:57
  last <- type_editor_page_rows(rows, 3L, 25L)
  stopifnot(identical(last, 51:57), length(last) == 7L)
})

ok("a page number out of range is clamped, never blank", {
  rows <- 1:57
  stopifnot(identical(type_editor_page_rows(rows, 99L, 25L), 51:57),
            identical(type_editor_page_rows(rows, 0L, 25L), 1:25),
            identical(type_editor_page_rows(rows, -5L, 25L), 1:25),
            identical(type_editor_page_rows(integer(0), 1L), integer(0)))
})

ok("every filtered row appears on exactly one page", {
  for (n in c(1L, 24L, 25L, 26L, 99L, 1743L)) {
    rows <- which(seq_len(n * 2L) %% 2L == 0L)
    seen <- unlist(lapply(seq_len(type_editor_n_pages(rows, 25L)),
                          function(p) type_editor_page_rows(rows, p, 25L)))
    stopifnot(identical(sort(seen), sort(rows)), !anyDuplicated(seen))
  }
})

ok("row indices are positions in detected(), so input ids stay stable", {
  # The id for a row is built from this index. If filtering renumbered the rows,
  # a search would silently rebind every dropdown to a different column.
  hits <- type_editor_rows(det, "", "attention")
  stopifnot(is.integer(hits),
            identical(det$variable[hits],
                      c("ward_group", "age_group", "participant_id")),
            identical(hits, which(types_needing_attention(det))))
})

cat("\n== the flag's stated reason ==\n")

ok("each flagged column gets an accurate reason", {
  r <- function(i) type_attention_reason(det$type[i], det$distinct[i],
                                         det$missing_pct[i])
  stopifnot(grepl("coded category", r(1)),
            grepl("looks like an ID", r(6)),
            grepl("all values missing", r(7)),
            grepl("only one observed value", r(8)),
            identical(r(5), ""))          # bmi: nothing to say
})

cat("\n== a correction survives paging away and back ==\n")

# This is the behaviour most likely to break silently. Shiny keeps an input's
# value when its UI is swapped out, so types() still reads a correction made on
# a page the analyst has left. But re-rendering the row is what would undo it:
# a selectInput rendered with the DETECTED type as `selected` pushes that value
# back to the client, discarding the correction without a trace. So the test that
# matters is on the rendered HTML - the re-rendered row must mark the analyst's
# choice as selected, not the detected one.
#
# testServer has no browser, so it cannot reproduce the client-side overwrite
# itself; asserting on the markup checks the mechanism that prevents it.
# `id` is the bare input id; the closing quote is added here so the pattern
# cannot match a longer id (type_3 must not match type_30).
selected_type <- function(html, id) {
  pat <- sprintf('<select id="[^"]*%s"', id)
  # sub() returns its input untouched when nothing matches, which would silently
  # hand back the FIRST select on the page instead of reporting "not rendered".
  if (!grepl(pat, html)) return(NA_character_)
  seg <- sub(paste0(".*", pat), "", html)
  seg <- sub("</select>.*", "", seg)
  m <- regmatches(seg, regexpr('value="[^"]*" selected', seg))
  if (!length(m)) return(NA_character_)
  sub('value="([^"]*)" selected', "\\1", m)
}

wide_fixture <- function(n_cols = 60L, n_rows = 40L) {
  set.seed(11)
  df <- as.data.frame(lapply(seq_len(n_cols), function(j)
    sample(seq_len(3L + (j %% 5L)), n_rows, replace = TRUE)))
  names(df) <- sprintf("v%02d", seq_len(n_cols))
  p <- tempfile(fileext = ".csv")
  utils::write.csv(df, p, row.names = FALSE)
  list(path = p, name = basename(p), n = n_cols)
}

cat("\n== a stale choice must not outlive the detection it belonged to ==\n")

ok("re-detecting the data discards a choice made against the old detection", {
  # Found by running the app: uploading a REDCap dictionary after the data
  # re-detects every type, but the row inputs retained from the pre-dictionary
  # render won, so `ward_group` still read "count" - and types() read that same
  # stale input, so the column would have been ANALYSED as a count despite the
  # dictionary saying it is categorical. Row inputs are therefore keyed by a
  # detection generation as well as a row index.
  fx <- wide_fixture()
  fx2 <- wide_fixture(n_cols = 30L)          # a different file
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(type_search = "", type_filter = "all")
    session$setInputs(`type_1_3` = "nominal")
    stopifnot(identical(unname(types()[["v03"]]), "nominal"))

    # A new file re-detects: the generation advances, so the old input is not
    # read back against the new detection.
    session$setInputs(file = list(datapath = fx2$path, name = fx2$name))
    stopifnot(identical(unname(types()[["v03"]]), detected()$type[[3]]),
              !identical(unname(types()[["v03"]]), "nominal") ||
                identical(detected()$type[[3]], "nominal"))
  })
})

ok("paging shows successive slices and keeps a correction", {
  fx <- wide_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(type_search = "", type_filter = "all")

    stopifnot(identical(type_page_rows(), 1:25))
    detected_type <- detected()$type[[3]]

    # Correct column 3 on page 1, then leave the page.
    session$setInputs(`type_1_3` = "nominal")   # generation 1, row 3
    stopifnot(!identical(detected_type, "nominal"),
              identical(unname(types()[["v03"]]), "nominal"))

    session$setInputs(type_next = 1)
    stopifnot(identical(type_page_rows(), 26:50),
              # still remembered while off-screen
              identical(unname(types()[["v03"]]), "nominal"))
    # row 3 is not on this page at all
    stopifnot(is.na(selected_type(html_text(output$type_editor), "type_1_3")))

    session$setInputs(type_prev = 1)
    stopifnot(identical(type_page_rows(), 1:25),
              identical(unname(types()[["v03"]]), "nominal"))
    # and the re-rendered widget agrees, so the client cannot reset it
    stopifnot(identical(selected_type(html_text(output$type_editor), "type_1_3"),
                        "nominal"))
  })
})

ok("the last page is not padded and Next stops there", {
  fx <- wide_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(type_search = "", type_filter = "all")
    for (i in 1:5) session$setInputs(type_next = i)   # past the end
    stopifnot(identical(type_page_rows(), 51:60),
              length(type_page_rows()) == 10L)
    for (i in 1:9) session$setInputs(type_prev = i)   # past the start
    stopifnot(identical(type_page_rows(), 1:25))
  })
})

ok("searching narrows the rows and resets to the first page", {
  fx <- wide_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(type_search = "", type_filter = "all")
    session$setInputs(type_next = 1)
    stopifnot(identical(type_page_rows(), 26:50))
    session$setInputs(type_search = "v5")            # v50..v59
    stopifnot(identical(detected()$variable[type_page_rows()],
                        sprintf("v5%d", 0:9)))
  })
})

ok("an untouched column keeps its detected type through paging", {
  fx <- wide_fixture()
  shiny::testServer(mod_data_server, {
    session$setInputs(file = list(datapath = fx$path, name = fx$name))
    session$setInputs(type_search = "", type_filter = "all")
    before <- types()
    session$setInputs(type_next = 1)
    session$setInputs(type_prev = 1)
    stopifnot(identical(types(), before),
              # never rendered, so no input exists - the reader falls back
              identical(unname(types()[["v60"]]),
                        detected()$type[[60]]))
  })
})

# --- coverage, measured against a real data dictionary ---------------------
# The PPS Phase II patient export ships its own REDCap dictionary, which gives a
# ground truth for how well the flag does. Of 61 radio/dropdown fields the
# detector typed as numeric: 36 are surfaced by "Needs a look", 11 more are
# constant and appear under "Empty or constant", and 14 are code lists with more
# than 10 distinct values that no shape-based rule can separate from a genuine
# count - `ant_one` (25 antibiotic codes) and `dur6` (19 durations in days) are
# indistinguishable. A "dense run of codes" rule was tried and measured: it
# matched 1 of the 14, so it was not adopted. Closing that last gap needs the
# dictionary itself, not a better heuristic.

report_and_exit()
