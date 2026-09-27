test_that("nominal missing groups retain every application in all input forms", {
  for (x in list(c('a', 'b', NA, NA), factor(c('a', 'b', NA, NA)),
                 c(TRUE, FALSE, NA, NA), rep(NA_character_, 4),
                 factor(rep(NA_character_, 4)), rep(NA, 4))) {
    data <- data.frame(x = x, y = c(0, 1, NA, NA), status = c('A', 'A', 'R', 'C'))
    outcome <- binary_outcome('y', bad = 1, good = 0)
    status <- value_map('status', approved = 'A', rejected = 'R', cancelled = 'C')
    roles <- loan_tbl(data, outcomes = outcome, application_status = status,
                      predictors = 'x')
    vector <- contingency_table(x, check_outcome(data$y, outcome), data$status,
                                 status_map = status)
    frame <- contingency_table(data, 'x', outcome = outcome, application_status = status)
    declared <- contingency_table(roles, 'x')
    expect_identical(vector, frame)
    expect_identical(frame, declared)
    expect_equal(sum(frame$applications_total), nrow(data))
    expect_equal(frame$applications_total[frame$the_var == 'value_NA'], sum(is.na(x)))
    expect_equal(frame$count_rejected[frame$the_var == 'value_NA'], 1)
    expect_equal(frame$count_cancelled[frame$the_var == 'value_NA'], 1)
    stats <- suppressWarnings(group_stats(roles, 'x', stats = character()))
    expect_identical(stats$the_var, frame$the_var)
    expect_identical(stats$applications_total, frame$applications_total)
  }
})

test_that("reserved raw values get unique names without changing input", {
  values <- c('value_NA', 'value_NA_1', 'value_other', 'value_other_1', NA, 'a')
  outcome <- factor(rep(c('good', 'bad'), 3), levels = c('good', 'bad'))
  for (x in list(values, factor(values, levels = rev(stats::na.omit(values))))) {
    original <- x
    ## NAs present, so the genuine `value_NA` collides and is renamed; nothing is
    ## merged into `value_other`, so the genuine `value_other` is left alone
    expect_message(ct <- contingency_table(x, outcome),
                    '`value_NA` -> `value_NA_2`', fixed = TRUE)
    expect_setequal(ct$the_var, c('value_NA_2', 'value_NA_1', 'value_other',
                                 'value_other_1', 'value_NA', 'a'))
    ## group_infrequent leaves missing values as NA (the table makes them
    ## value_NA), so it has no value_NA group to collide with: nothing renamed
    expect_message(grouped <- group_infrequent(x, verbose = FALSE), NA)
    expect_equal(sum(is.na(grouped)), 1)
    expect_setequal(stats::na.omit(as.character(grouped)),
                    c('value_NA', 'value_NA_1', 'value_other', 'value_other_1', 'a'))
    ## merging does happen with unique_val = 2 -> the genuine value_other collides
    expect_message(group_infrequent(x, unique_val = 2, verbose = FALSE),
                    '`value_other` -> `value_other_2`', fixed = TRUE)
    expect_equal(ct$issued_loans_total, rep(1, 6))
    expect_identical(x, original)
    expect_message(stats <- suppressWarnings(group_stats(x, outcome, stats = character())),
                    'Renamed reserved variable value(s)', fixed = TRUE)
    expect_setequal(stats$the_var, ct$the_var)
  }
})

test_that("missing groups stay separate from infrequent groups", {
  x <- c(rep('a', 5), rep('b', 3), 'c', NA)
  outcome <- factor(rep(c('good', 'bad'), 5), levels = c('good', 'bad'))
  ct <- contingency_table(x, outcome, classes_limit = 2)
  expect_equal(ct$issued_loans_total[ct$the_var == 'value_NA'], 1)
  expect_equal(sum(ct$issued_loans_total), length(x))
  grouped <- group_infrequent(x, unique_val = 1, verbose = FALSE)
  expect_equal(sum(is.na(grouped)), 1)
  expect_equal(sum(grouped == 'value_other', na.rm = TRUE), 4)
})

test_that("numeric missing bins are not renamed as raw nominal values", {
  x <- c(1:100, NA_real_)
  outcome <- factor(rep('good', length(x)), levels = c('good', 'bad'))
  expect_message(ct <- contingency_table(x, outcome), NA)
  expect_equal(ct$issued_loans_total[ct$the_var == 'value_NA'], 1)
  expect_false('value_NA_1' %in% ct$the_var)
})

test_that("re-grouping already-grouped input keeps the package's own groups", {
  y <- factor(c('good', 'bad', 'good', 'bad', 'good'), levels = c('good', 'bad'))
  grouped <- group_infrequent(c('F', 'M', NA, 'F', 'M'))
  expect_identical(levels(grouped), c('F', 'M'))
  expect_message(ct <- contingency_table(grouped, y), NA)
  ## and a table's own value_NA group fed back in is kept as is
  expect_message(again_ct <- contingency_table(factor(as.character(ct$the_var)), factor(rep('good', 3), levels = c('good', 'bad'))), NA)
  expect_false('value_NA_1' %in% as.character(again_ct$the_var))
  expect_setequal(as.character(ct$the_var), c('F', 'M', 'value_NA'))
  expect_false('value_NA_1' %in% as.character(ct$the_var))
  expect_message(again <- group_infrequent(grouped), NA)
  expect_identical(levels(again), levels(grouped))
})

test_that("the vector form keeps a nominal variable's missing values (utm_medium)", {
  y <- check_outcome(portfolio$fpd15, outcome_map)
  ct <- suppressWarnings(contingency_table(portfolio$utm_medium, y))
  expect_true("value_NA" %in% ct$the_var)
  expect_equal(ct$issued_loans_total[ct$the_var == "value_NA"],
               sum(is.na(portfolio$utm_medium) & !is.na(y)))
  expect_equal(sum(ct$issued_loans_total), sum(!is.na(y)))
})
