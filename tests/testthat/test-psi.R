test_that('both zero-count rules use aligned proportions and nonnegative contributions', {
  base <- structure(data.frame(the_var = c('a', 'b'), issued_loans_total = c(2, 1)),
                    class = c('loan_cont_table', 'data.frame'))
  comparison <- structure(data.frame(the_var = c('a', 'c'), issued_loans_total = c(1, 4)),
                          class = c('loan_cont_table', 'data.frame'))

  for(rule in c('add_half', 'floor')) {
    result <- psi_from_tables(base, comparison, zero_counts = rule, floor_value = 0.01)
    parts <- attr(result, 'PSI_table')
    p_base <- if(rule == 'add_half') (c(2, 1, 0) + 0.5) / 4.5 else pmax(c(2, 1, 0) / 3, 0.01)
    p_comp <- if(rule == 'add_half') (c(1, 0, 4) + 0.5) / 6.5 else pmax(c(1, 0, 4) / 5, 0.01)
    expected <- (p_comp - p_base) * log(p_comp / p_base)

    expect_identical(parts$the_var, c('a', 'b', 'c'))
    expect_equal(parts$count_base, c(2, 1, 0))
    expect_equal(parts$count_comparison, c(1, 0, 4))
    expect_equal(parts$PSI, expected)
    expect_equal(as.numeric(result), sum(expected))
    expect_true(all(is.finite(parts$PSI) & parts$PSI >= 0))
    expect_identical(attr(result, 'zero_counts'), rule)
    expect_identical(attr(result, 'floor_value'), 0.01)
    expect_equal(as.numeric(psi_from_tables(base, base, zero_counts = rule)), 0)
  }
})

test_that('psi detects a comparison-only nominal category', {
  x <- c('a', 'b', 'a', 'new', 'new', 'b')
  outcome <- factor(rep('good', length(x)), levels = c('good', 'bad'))
  for(rule in c('add_half', 'floor')) {
    result <- psi(x, outcome, 1:3, 4:6, zero_counts = rule)
    parts <- attr(result, 'PSI_table')
    expect_gt(as.numeric(result), 0)
    expect_true('new' %in% parts$the_var)
    expect_true(all(is.finite(parts$PSI) & parts$PSI >= 0))
    expect_equal(as.numeric(result), sum(parts$PSI))
  }
})

test_that('nominal template keeps an already-grouped missing label', {
  outcome <- factor(c('good', 'bad', 'good'), levels = c('good', 'bad'))
  base <- contingency_table(c('a', NA, 'a'), outcome)
  comparison <- contingency_table(c('new', NA, 'new'), outcome,
                                  template_matrix = base)
  expect_setequal(as.character(comparison$the_var), c('a', 'value_NA', 'new'))
  expect_false('value_NA_1' %in% as.character(comparison$the_var))
  expect_equal(comparison$issued_loans_total[comparison$the_var == 'a'], 0)
  expect_equal(comparison$issued_loans_total[comparison$the_var == 'value_NA'], 1)
})

test_that('psi_from_tables rejects incompatible tables and invalid zero rules', {
  base <- structure(data.frame(the_var = 'a', issued_loans_total = 1),
                    class = c('loan_cont_table', 'data.frame'))
  expect_error(psi_from_tables(base, data.frame(the_var = 'a')),
               '`cont_table` must be a `loan_cont_table`', fixed = TRUE)
  expect_error(psi_from_tables(base, base, measure = 'missing'), '`measure` column', fixed = TRUE)
  duplicate <- base[c(1, 1), ]
  expect_error(psi_from_tables(duplicate, base), 'unique, non-missing groups', fixed = TRUE)
  expect_error(psi_from_tables(base, base, zero_counts = 'floor', floor_value = 0),
               '`floor_value` must', fixed = TRUE)
})

test_that('a group empty in both samples does not change the PSI', {
  table_of <- function(groups, counts) {
    structure(data.frame(the_var = groups, issued_loans_total = counts),
              class = c('loan_cont_table', 'data.frame'))
  }
  for (rule in c('add_half', 'floor')) {
    without <- psi_from_tables(table_of(c('a', 'b', 'c'), c(2, 1, 0)),
                               table_of(c('a', 'b', 'c'), c(10, 0, 40)), zero_counts = rule)
    with_empty <- psi_from_tables(table_of(c('a', 'b', 'c', 'd'), c(2, 1, 0, 0)),
                                  table_of(c('a', 'b', 'c', 'd'), c(10, 0, 40, 0)), zero_counts = rule)
    expect_equal(as.numeric(with_empty), as.numeric(without))
    expect_false('d' %in% attr(with_empty, 'PSI_table')$the_var)
  }
})
