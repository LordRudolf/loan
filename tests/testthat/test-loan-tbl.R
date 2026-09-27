test_that("unknown and removed roles are rejected", {
  for (role in c("typo", "loan_status", "loan_closed_at")) {
    args <- list(data = data.frame(value = 1:3))
    args[[role]] <- "value"
    expect_error(do.call(loan_tbl, args), paste0("Unknown role(s): ", role), fixed = TRUE)
  }
})

test_that("a column cannot be both outcome and predictor", {
  expect_error(loan_tbl(data.frame(y = c(0, 1)),
                        outcomes = binary_outcome("y", bad = 1), predictors = "y"),
               'Column(s) declared as `predictors` already have another role: y.', fixed = TRUE)
})

test_that("single-outcome analyses use the primary or explicitly named declaration", {
  data <- data.frame(group = rep(c("a", "b"), each = 4),
                     first = rep(c(0, 0, 0, 1), 2),
                     second = rep(c(0, 1, 1, 1), 2))
  declarations <- list(binary_outcome("first", bad = 1), binary_outcome("second", bad = 1))
  declared <- loan_tbl(data, outcomes = declarations, primary_outcome = "first",
                       predictors = "group")
  primary <- group_stats(declared, "group")
  other <- group_stats(declared, "group", outcome = "second")
  expect_equal(primary$bad_rate, c(0.25, 0.25))
  expect_equal(other$bad_rate, c(0.75, 0.75))
  expect_identical(primary, group_stats(data, "group", outcome = declarations[[1]]))
  expect_identical(other, group_stats(data, "group", outcome = declarations[[2]]))
  ambiguous <- loan_tbl(data, outcomes = declarations, predictors = "group")
  expect_error(group_stats(ambiguous, "group"), "but no `primary_outcome`", fixed = TRUE)
})

test_that("unlisted_role assigns columns and validates its mode", {
  data <- data.frame(value = 1:6, day = as.Date("2024-01-01") + 1:6)
  supplementary <- suppressMessages(loan_tbl(data, unlisted_role = "supplementary"))
  expect_length(loan_roles(supplementary)$predictors, 0)
  expect_setequal(loan_roles(supplementary)$supplementary, names(data))
  expect_warning(predictors <- suppressMessages(loan_tbl(data, unlisted_role = "predictors")),
                 "day (Date)", fixed = TRUE)
  expect_setequal(loan_roles(predictors)$predictors, names(data))
  auto <- suppressMessages(loan_tbl(data, unlisted_role = "auto"))
  expect_identical(loan_roles(auto)$predictors, "value")
  expect_identical(loan_roles(auto)$supplementary, "day")
  expect_error(loan_tbl(data, unlisted_role = "invalid"),
               '`unlisted_role` must be "auto", "predictors" or "supplementary".', fixed = TRUE)
})

test_that("auto screens recoded copies but keeps nested nominal predictors", {
  data <- data.frame(application_id = 1:24, client_age = rep(21:26, 4),
                     nominal = rep(c("a", "b", "c"), 8),
                     outcome = rep(c(0, 1), 12),
                     province = rep(c("north", "south"), each = 12),
                     city = rep(letters[1:8], each = 3))
  data$id_text <- as.character(data$application_id)
  data$age_months <- data$client_age * 12
  data$nominal_code <- match(data$nominal, c("a", "b", "c"))
  data$outcome_copy <- data$outcome == 1
  declared <- suppressMessages(loan_tbl(
    data, application_id = "application_id", outcomes = binary_outcome("outcome", bad = 1),
    predictors = c("client_age", "nominal", "province"), unlisted_role = "auto"
  ))
  expect_setequal(loan_roles(declared)$supplementary,
                  c("id_text", "age_months", "nominal_code", "outcome_copy"))
  expect_setequal(loan_roles(declared)$predictors,
                  c("client_age", "nominal", "province", "city"))
})

test_that("sparse association agrees with dense Tschuprow's T", {
  counts <- table(portfolio$gender, portfolio$marriage_status)
  counts <- counts[rowSums(counts) > 0, colSums(counts) > 0, drop = FALSE]
  chi_squared <- unname(stats::chisq.test(counts, correct = FALSE)$statistic)
  dense <- sqrt(chi_squared / (sum(counts) * sqrt(prod(dim(counts) - 1))))
  expect_equal(association(portfolio$gender, portfolio$marriage_status), dense)
})

test_that("declared Date predictors are kept with a warning", {
  data <- data.frame(day = as.Date("2024-01-01") + 1:3)
  expect_warning(declared <- loan_tbl(data, predictors = "day"), "day (Date)", fixed = TRUE)
  expect_identical(loan_roles(declared)$predictors, "day")
  expect_identical(declared$day, data$day)
})

test_that("dplyr filter preserves roles", {
  filtered <- dplyr::filter(portfolio_roles, client_age >= 30)
  expect_gt(nrow(filtered), 0)
  expect_lt(nrow(filtered), nrow(portfolio_roles))
  expect_s3_class(filtered, "loan_tbl")
  expect_identical(loan_roles(filtered), loan_roles(portfolio_roles))
})

test_that("column renames update every role and declaration", {
  data <- data.frame(id = 1:4, date = as.Date('2024-01-01') + 1:4,
                     status = c('yes', 'yes', 'no', 'no'),
                     event = c(0, 1, NA, NA), feature = 1:4, extra = letters[1:4])
  lt <- loan_tbl(data, application_id = 'id', application_created_at = 'date',
                 application_status = value_map('status', approved = 'yes', rejected = 'no'),
                 outcomes = binary_outcome('event', bad = 1, good = 0),
                 predictors = 'feature', supplementary = 'extra')
  renamed <- dplyr::rename(lt, new_id = id, new_date = date, new_status = status,
                           new_event = event, new_feature = feature, new_extra = extra)
  roles <- loan_roles(renamed)
  expect_identical(roles$application_id, 'new_id')
  expect_identical(roles$application_created_at, 'new_date')
  expect_identical(roles$application_status$column, 'new_status')
  expect_identical(roles$outcomes$event$column, 'new_event')
  expect_identical(roles$predictors, 'new_feature')
  expect_identical(roles$supplementary, 'new_extra')
  expect_s3_class(group_stats(renamed, 'new_feature', stats = character()),
                  'loan_group_stats')
  selected <- dplyr::select(lt, feature_name = feature, event_name = event)
  expect_identical(loan_roles(selected)$predictors, 'feature_name')
  expect_identical(loan_roles(selected)$outcomes$event$column, 'event_name')
  expect_null(loan_roles(selected)$application_status)
})

test_that("column removal drops roles without choosing a new primary outcome", {
  data <- data.frame(id = 1:4, first = c(0, 1, 0, 1),
                     second = c(1, 0, 1, 0), feature = letters[1:4])
  lt <- loan_tbl(data, application_id = 'id',
                 outcomes = list(first = binary_outcome('first', bad = 1),
                                 second = binary_outcome('second', bad = 1)),
                 primary_outcome = 'first', predictors = 'feature')
  without_primary <- dplyr::select(lt, -first)
  expect_named(loan_roles(without_primary)$outcomes, 'second')
  expect_length(loan_roles(without_primary)$primary_outcome, 0)
  expect_error(group_stats(without_primary, 'feature'), 'No `primary_outcome` remains', fixed = TRUE)
  expect_s3_class(group_stats(without_primary, 'feature', outcome = 'second'),
                  'loan_group_stats')
  without_id <- lt[, -match('id', names(lt))]
  expect_s3_class(without_id, 'loan_tbl')
  expect_null(loan_roles(without_id)$application_id)
  expect_identical(loan_roles(lt[1:2, ])$application_id, 'id')
  empty <- lt[, FALSE]
  expect_s3_class(empty, 'loan_tbl')
  expect_equal(ncol(empty), 0)
  expect_null(loan_roles(empty)$application_id)
  expect_length(loan_roles(empty)$outcomes, 0)
})

test_that("predictor provenance records declarations and follows column changes", {
  data <- data.frame(id = 1:4, declared = c(2, 3, 2, 3),
                     automatic = c("a", "a", "b", "b"),
                     day = as.Date("2024-01-01") + 1:4)
  lt <- suppressMessages(loan_tbl(data, application_id = "id",
                                  predictors = "declared", unlisted_role = "auto"))
  expect_identical(predictor_provenance(lt),
                   c(declared = "declared", automatic = "auto"))
  expect_output(print(lt), "predictors \\(2: 1 declared, 1 auto\\): declared, automatic")
  expect_identical(predictor_provenance(dplyr::filter(lt, id > 1)),
                   predictor_provenance(lt))
  renamed <- dplyr::rename(lt, feature = declared, found = automatic)
  expect_identical(predictor_provenance(renamed),
                   c(feature = "declared", found = "auto"))
  expect_identical(predictor_provenance(dplyr::select(lt, found = automatic)),
                   c(found = "auto"))
  expect_identical(predictor_provenance(lt[, c("id", "declared")]),
                   c(declared = "declared"))
  expect_identical(predictor_provenance(lt[, FALSE]), character())
  expect_identical(predictor_provenance(suppressMessages(
    loan_tbl(data, unlisted_role = "supplementary"))), character())
  expect_identical(predictor_provenance(suppressMessages(
    loan_tbl(data[c("id", "declared", "automatic")], application_id = "id",
             unlisted_role = "predictors"))),
    c(declared = "auto", automatic = "auto"))
  expect_error(predictor_provenance(data),
               "`x` must be a `loan_tbl`; got `data.frame`.", fixed = TRUE)
})
