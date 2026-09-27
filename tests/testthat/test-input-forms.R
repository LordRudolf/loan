test_that("group_stats rates agree across all three input forms", {
  vector <- group_stats(portfolio$client_age, portfolio_outcome,
                        portfolio$application_status, status_map = status_map)
  frame <- group_stats(portfolio, "client_age", outcome = outcome_map,
                       application_status = status_map)
  declared <- group_stats(portfolio_roles, "client_age")
  for (column in c("the_var", "approval_rate", "bad_rate")) {
    expect_identical(vector[[column]], frame[[column]])
    expect_identical(declared[[column]], frame[[column]])
  }
})

test_that("psi agrees across all three input forms", {
  base <- seq_len(nrow(portfolio)) <= 10000
  vector <- psi(portfolio$client_age, portfolio_outcome, base, !base,
                application_status = portfolio$application_status,
                status_map = status_map)
  frame <- psi(portfolio, "client_age", outcome = outcome_map,
               application_status = status_map,
               time_split_base = base, time_split_comparison = !base)
  declared <- psi(portfolio_roles, "client_age",
                  time_split_base = base, time_split_comparison = !base)
  expect_s3_class(vector, "loan_psi")
  expect_equal(as.numeric(vector), as.numeric(frame))
  expect_equal(as.numeric(declared), as.numeric(frame))
})

test_that("contingency_table agrees for column names and vectors", {
  frame <- contingency_table(portfolio, "gender", outcome = outcome_map,
                             application_status = status_map)
  vector <- contingency_table(portfolio$gender, portfolio_outcome,
                              portfolio$application_status, status_map = status_map)
  expect_identical(frame, vector)
})

test_that("named and reordered arguments dispatch on x", {
  age <- portfolio$client_age[!is.na(portfolio_outcome)]
  y <- portfolio_outcome[!is.na(portfolio_outcome)]
  expect_identical(contingency_table(outcome = y, x = age),
                   contingency_table(age, y))
})
