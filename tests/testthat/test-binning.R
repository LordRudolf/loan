test_that("template binning reproduces counts on the same rows", {
  original <- contingency_table(portfolio, "client_age", outcome = outcome_map,
                                application_status = status_map)
  reused <- contingency_table(portfolio, "client_age", outcome = outcome_map,
                              application_status = status_map, template_matrix = original)
  expect_identical(reused$the_var, original$the_var)
  counts <- names(original)[grepl("^count_|_total$", names(original))]
  for (column in counts) expect_identical(reused[[column]], original[[column]])
})

test_that("psi of a sample against itself is zero", {
  for (variable in c("client_age", "gender")) {
    result <- psi(portfolio, variable, outcome = outcome_map,
                  application_status = status_map,
                  time_split_base = rep(TRUE, nrow(portfolio)),
                  time_split_comparison = rep(TRUE, nrow(portfolio)))
    expect_equal(as.numeric(result), 0)
  }
})

test_that("group_infrequent preserves factor order and leaves missing values to the table", {
  x <- factor(c("b", "a", "b", NA), levels = c("b", "a"))
  expect_no_warning(grouped <- group_infrequent(x))
  expect_identical(levels(grouped), c("b", "a"))
  expect_identical(as.character(grouped), c("b", "a", "b", NA))
})

test_that("group_stats retains missing gender as its own row", {
  data <- data.frame(gender = factor(c("F", "M", NA, NA)),
                     result = factor(c("good", "bad", "good", "bad"),
                                     levels = c("good", "bad")))
  expect_no_warning(result <- group_stats(data, "gender", outcome = "result"))
  expect_true("value_NA" %in% result$the_var)
  expect_equal(result$issued_loans_total[result$the_var == "value_NA"], 2)
})

test_that("default group_stats bins agree without double binning", {
  vector <- group_stats(portfolio$client_age, portfolio_outcome,
                        portfolio$application_status, status_map = status_map)
  frame <- group_stats(portfolio, "client_age", outcome = outcome_map,
                       application_status = status_map)
  expect_identical(vector$the_var, frame$the_var)
  expect_identical(vector$issued_loans_total, frame$issued_loans_total)
  expect_false("value_other" %in% vector$the_var)
})
