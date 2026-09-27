test_that("all data-frame methods use the same missing-column error", {
  data <- data.frame(age = 1:6, result = factor(rep(c("good", "bad"), 3),
                                              levels = c("good", "bad")))
  message <- paste0('`variable`: the column "typo" was not found in the data. ',
                    'Available columns: age, result.')
  expect_error(resolve_roles(data, variable = "typo"), message, fixed = TRUE)
  for (fun in list(contingency_table, group_stats, psi, plot_univariate_smooth)) {
    expect_error(fun(data, "typo", outcome = "result"), message, fixed = TRUE)
  }
})

test_that("Date variables use check_variable errors in both forms", {
  data <- data.frame(day = as.Date("2024-01-01") + 1:6,
                     result = factor(rep(c("good", "bad"), 3),
                                     levels = c("good", "bad")))
  message <- '`variable` must be a numeric, logical, character or factor vector -- got Date.'
  expect_error(check_variable(data$day), message, fixed = TRUE)
  for (fun in list(contingency_table, group_stats, psi)) {
    expect_error(fun(data$day, outcome = data$result), message, fixed = TRUE)
    expect_error(fun(data, "day", outcome = "result"), message, fixed = TRUE)
  }
  message <- '`variable` must be a numeric vector -- got Date.'
  expect_error(check_variable(data$day, types = "numeric"), message, fixed = TRUE)
  expect_error(plot_univariate_smooth(data$day, outcome = data$result), message, fixed = TRUE)
  expect_error(plot_univariate_smooth(data, "day", outcome = "result"), message, fixed = TRUE)
})

test_that("wrong-length companion vectors use check_same_length errors", {
  x <- seq_len(20)
  y <- factor(rep(c("good", "bad"), 10), levels = c("good", "bad"))
  for (role in c("outcome", "application_status")) {
    message <- paste0('`', role, '` must have one value per element of `variable` (20) -- got length 2.')
    args <- list(variable = x)
    args[[role]] <- c("good", "bad")
    expect_error(do.call(check_same_length, args), message, fixed = TRUE)
    for (fun in list(contingency_table, group_stats, psi, plot_univariate_smooth)) {
      args <- list(x = x, outcome = y)
      args[[role]] <- c("good", "bad")
      if (identical(fun, psi)) {
        args$time_split_base <- rep(TRUE, 20)
        args$time_split_comparison <- rep(TRUE, 20)
      }
      expect_error(do.call(fun, args), message, fixed = TRUE)
    }
  }
})

test_that("variable and outcome validators enforce supported types", {
  expect_identical(check_variable(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA))
  expect_error(check_variable("text", types = "numeric"),
               '`variable` must be a numeric vector -- got character.', fixed = TRUE)
  expect_error(check_outcome(c(1.5, 2.5, 3.5), types = "binary"),
               '`outcome` must be a binary outcome -- got a numeric outcome.', fixed = TRUE)
})
