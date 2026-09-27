test_that("value maps reject ambiguity and require coverage", {
  expect_error(value_map(approved = "A", rejected = "A"),
               "raw values are mapped to more than one canonical label: A", fixed = TRUE)
  expect_error(apply_value_map(c("A", "B"), value_map(approved = "A")),
               "These values were not mapped: B", fixed = TRUE)
  expect_warning(
    mapped <- apply_value_map(c("A", "B", NA),
                              value_map(approved = "A", .default = "rejected")),
    'absorbed unlisted value(s): B', fixed = TRUE
  )
  expect_identical(as.character(mapped), c("approved", "rejected", NA_character_))
})

test_that("binary declarations require bad and allow implicit good silently", {
  expect_error(binary_outcome("result"), "needs `bad`", fixed = TRUE)
  expect_no_warning(mapped <- check_outcome(c("D", "P", "other", NA),
                                             binary_outcome(bad = "D")))
  expect_identical(mapped, factor(c("bad", "good", "good", NA),
                                  levels = c("good", "bad")))
  expect_no_warning(mapped <- check_outcome(c(0, 1, NA),
                                             binary_outcome(bad = 1, good = 0)))
  expect_identical(mapped, factor(c("good", "bad", NA), levels = c("good", "bad")))
})

test_that("defaults and mapped labels must belong to the role vocabulary", {
  expect_error(loan_tbl(
    data.frame(status = "X"),
    application_status = value_map("status", approved = "X", .default = "maybe"),
    unlisted_role = "supplementary"
  ), "Unknown application_status label(s): maybe", fixed = TRUE)
  expect_error(check_vocabulary(value_map(maybe = "X", .default = "maybe"),
                                "application_status"),
               "Unknown application_status label(s): maybe", fixed = TRUE)
  expect_error(apply_value_map("X", value_map(good = "X", .default = "maybe"),
                              role = "outcome"),
               "Unknown outcome label(s): maybe", fixed = TRUE)
  expect_warning(mapped <- apply_value_map(
    c("X", "Y", NA), value_map(approved = "X", .default = "rejected"),
    role = "application_status"
  ), "absorbed unlisted value(s): Y", fixed = TRUE)
  expect_identical(as.character(mapped), c("approved", "rejected", NA_character_))
  expect_true(check_vocabulary(value_map(custom = "X", .default = "other"),
                               "supplementary"))
})

test_that("raw status names cannot contradict canonical count names", {
  outcome <- factor(c(NA, "good"), levels = c("good", "bad"))
  for(label in loan_vocabulary("application_status")) {
    other <- setdiff(loan_vocabulary("application_status"), label)[1L]
    map <- do.call(value_map, setNames(list("A", label), c(label, other)))
    expect_error(contingency_table(c("g1", "g2"), outcome,
                                   application_status = c(label, "A"),
                                   status_map = map),
                 paste0("Conflicting count column(s): count_", label), fixed = TRUE)
  }
  expect_warning(expect_error(contingency_table(
    c("g1", "g2"), outcome, application_status = c("approved", "A"),
    status_map = value_map(approved = "A", .default = "rejected")
  ), "Conflicting count column(s): count_approved", fixed = TRUE),
  "absorbed unlisted value(s): approved", fixed = TRUE)
  expect_error(contingency_table(
    c("g1", "g2"), outcome,
    application_status = factor(c("R", "A"), levels = c("R", "A", "approved")),
    status_map = value_map(approved = "A", rejected = c("R", "approved"))
  ), "Conflicting count column(s): count_approved", fixed = TRUE)
})

test_that("raw statuses may map to the canonical labels they spell", {
  ct <- contingency_table(
    c("g1", "g2", "g3", "g4"),
    factor(c(NA, "good", NA, NA), levels = c("good", "bad")),
    application_status = c("rejected", "approved", "cancelled", NA),
    status_map = value_map(approved = "approved", rejected = "rejected",
                           cancelled = "cancelled")
  )
  expect_s3_class(ct, "loan_cont_table")
  expect_false(anyDuplicated(names(ct)) > 0L)
  expect_equal(ct$count_approved, c(0, 1, 0, 0))
  expect_equal(ct$count_rejected, c(1, 0, 0, 0))
  expect_equal(ct$count_cancelled, c(0, 0, 1, 0))
  expect_equal(ct$decisioned_total, c(1, 1, 0, 0))
})
