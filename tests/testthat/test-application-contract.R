test_that("declared application ids are complete and unique", {
  for (ids in list(c(1, 1, 2, NA), c("a", "a", "b", NA),
                   factor(c("a", "a", "b", NA)))) {
    expect_error(loan_tbl(data.frame(id = ids), application_id = "id"),
                 "3 offending rows (1 missing, 2 with duplicated values). Examples:",
                 fixed = TRUE)
  }
  expect_error(loan_tbl(data.frame(id = c("a", "a")), application_id = "id"),
               "Examples: a.", fixed = TRUE)
  expect_error(loan_tbl(data.frame(id = c(1, NA, NA)), application_id = "id"),
               "2 offending rows (2 missing, 0 with duplicated values). Examples: NA.",
               fixed = TRUE)
  data <- data.frame(id = 1:3, loan = c(1, 1, NA), client = c("a", "a", "a"))
  expect_s3_class(loan_tbl(data, application_id = "id", loan_id = "loan",
                           client_id = "client"), "loan_tbl")
  expect_s3_class(suppressMessages(loan_tbl(data.frame(id = c(1, 1, NA)),
                                             unlisted_role = "supplementary")), "loan_tbl")
})

test_that("group statistics distinguish absent, raw and mapped statuses", {
  data <- data.frame(group = c("a", "a", "a", "a", "b", "b"),
                     outcome = c(0, 1, NA, NA, NA, NA),
                     status = c("A", "A", "R", "C", "C", "C"))
  outcome <- binary_outcome("outcome", bad = 1, good = 0)
  map <- value_map("status", approved = "A", rejected = "R", cancelled = "C")
  columns <- c("applications", "outcomes", "application_statuses", "outcome_statuses")
  expect_no_warning(no_status <- group_stats(data, "group", outcome = outcome,
                                              stats = c("woe", "fisher_p_val"),
                                              table_cols_shown = columns))
  expect_setequal(names(no_status), c("the_var", "count_good", "count_bad",
                                      "issued_loans_total", "bad_rate", "woe", "fisher_p_val"))
  expect_equal(no_status$issued_loans_total, c(2, 0))
  expect_equal(no_status$bad_rate, c(0.5, NA))
  expect_equal(as.character(no_status$the_var), c("a", "b"))
  expect_null(attr(no_status, "app_status_dict")$values)

  expect_no_warning(expect_message(
    raw <- group_stats(data, "group", outcome = outcome, application_status = "status"),
    "value_map()", fixed = TRUE
  ))
  expect_true(all(c("count_A", "count_R", "count_C", "applications_total") %in% names(raw)))
  expect_false(any(c("approval_rate", "decisioned_total", "count_approved",
                      "count_rejected", "count_cancelled") %in% names(raw)))
  expect_equal(raw$applications_total, c(4, 2))
  expect_equal(raw$count_C, c(1, 2))
  expect_null(attr(raw, "app_status_dict")$status_map)
  expect_null(attr(raw, "app_status_dict")$accept_label)

  mapped <- group_stats(data, "group", outcome = outcome, application_status = map,
                         table_cols_shown = columns)
  expect_equal(mapped$approval_rate, c(2 / 3, NA))
  expect_equal(mapped$decisioned_total, c(3, 0))
  expect_equal(mapped$count_cancelled, c(1, 2))
  expect_equal(mapped$bad_rate, no_status$bad_rate)
  expect_equal(mapped$fisher_p_val, no_status$fisher_p_val)

  for (status in list(NULL, "status", map)) {
    roles <- list(data = data, outcomes = outcome, unlisted_role = "supplementary")
    if(!is.null(status)) roles$application_status <- status
    declared <- suppressMessages(do.call(loan_tbl, roles))
    frame <- suppressMessages(group_stats(data, "group", outcome = outcome,
                                          application_status = status, stats = character()))
    from_roles <- suppressMessages(group_stats(declared, "group", stats = character()))
    vector <- suppressMessages(group_stats(data$group, check_outcome(data$outcome, outcome),
                                            application_status = if(!is.null(status)) data$status,
                                            status_map = if(is_value_map(status)) map,
                                            stats = character()))
    for (column in names(frame)) {
      expect_identical(frame[[column]], from_roles[[column]])
      expect_identical(frame[[column]], vector[[column]])
    }
    expect_s3_class(plot(frame), "ggplot")
    expect_no_error(ggplot2::ggplot_build(plot(frame)))
  }
  expect_error(plot(no_status, plots_to_make = "approval_rate"),
               "No available plots for the selected stats", fixed = TRUE)

  expect_warning(inferred <- contingency_table(data, "group", outcome = outcome,
                                                application_status = "status"),
                 "No `value_map()` supplied", fixed = TRUE)
  expect_true("decisioned_total" %in% names(inferred))
})

test_that("unmapped status works without missing outcomes or inferred meaning", {
  outcome <- factor(c("good", "bad"), levels = c("good", "bad"))
  expect_no_warning(expect_message(
    raw <- group_stats(c("a", "b"), outcome, c("approved", "rejected"),
                        stats = character()),
    "value_map()", fixed = TRUE
  ))
  # These names are raw labels only; no decision or rate is inferred.
  expect_equal(raw$count_approved, c(1, 0))
  expect_equal(raw$count_rejected, c(0, 1))
  expect_false(any(c("approval_rate", "decisioned_total") %in% names(raw)))
  expect_null(attr(raw, "app_status_dict")$class_table)
})
