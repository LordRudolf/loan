test_that("smooth plot groups resolve names, vectors and data expressions", {
  data <- data.frame(age = seq_len(12), gender = rep(c("M", "F"), 6),
                     result = factor(rep(c("good", "bad"), 6),
                                     levels = c("good", "bad")))
  cutoff <- "M"

  by_name <- plot_univariate_smooth(data, "age", outcome = "result",
                                    grouping_var = "gender")
  by_vector <- plot_univariate_smooth(data, "age", outcome = "result",
                                      grouping_var = data$gender)
  by_factor <- plot_univariate_smooth(data, "age", outcome = "result",
                                      grouping_var = factor(data$gender))
  by_symbol <- plot_univariate_smooth(data, "age", outcome = "result",
                                      grouping_var = gender)
  by_expression <- plot_univariate_smooth(data, "age", outcome = "result",
                                          grouping_var = gender == cutoff)
  by_numeric <- plot_univariate_smooth(data$age, data$result,
                                       grouping_var = data$gender)
  by_numeric_logical <- plot_univariate_smooth(data$age, data$result,
                                               grouping_var = data$gender == cutoff)

  expect_s3_class(by_expression, "ggplot")
  expect_identical(as.character(by_name$data$the_group), data$gender)
  expect_identical(by_vector$data$the_group, by_name$data$the_group)
  expect_identical(by_factor$data$the_group, by_name$data$the_group)
  expect_identical(by_symbol$data$the_group, by_name$data$the_group)
  expect_identical(by_numeric$data$the_group, by_name$data$the_group)
  expect_identical(as.character(by_expression$data$the_group),
                   as.character(data$gender == cutoff))
  expect_length(by_expression$data$the_group, nrow(data))
  expect_identical(by_numeric_logical$data$the_group, by_expression$data$the_group)
})

test_that("smooth plot grouping uses standard argument errors", {
  data <- data.frame(age = seq_len(12), gender = rep(c("M", "F"), 6),
                     result = factor(rep(c("good", "bad"), 6),
                                     levels = c("good", "bad")))
  expect_error(plot_univariate_smooth(data, "age", outcome = "result",
                                       grouping_var = "gendr"),
               '`grouping_var`: the column "gendr" was not found in the data.',
               fixed = TRUE)
  expect_error(plot_univariate_smooth(data, "age", outcome = "result",
                                       grouping_var = data$gender[1:2]),
               '`grouping_var` must be a single column name, or a vector of length 12 -- got character of length 2.',
               fixed = TRUE)
  expect_error(plot_univariate_smooth(data$age, data$result,
                                       grouping_var = data$gender[1:2]),
               '`grouping_var` must be a single column name, or a vector of length 12 -- got character of length 2.',
               fixed = TRUE)
  expect_error(plot_univariate_smooth(data, "age", outcome = "result",
                                       grouping_var = seq_len(12)),
               '`grouping_var` must be a character, factor or logical vector -- got integer.',
               fixed = TRUE)
})

test_that("smooth plot keeps missing grouping values separate", {
  data <- data.frame(age = seq_len(6), group = c("value_NA", "A", NA, "A", NA, "value_NA"),
                     result = factor(rep(c("good", "bad"), 3),
                                     levels = c("good", "bad")))
  expect_message(p <- plot_univariate_smooth(data, "age", outcome = "result",
                                              grouping_var = "group"),
                 '`value_NA` -> `value_NA_1`', fixed = TRUE)
  expect_identical(as.character(p$data$the_group),
                   c("value_NA_1", "A", "value_NA", "A", "value_NA", "value_NA_1"))
})
