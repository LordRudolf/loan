test_that("public API is explicit and S3 methods are registered", {
  public <- getNamespaceExports("loan")
  expected <- c(
    "%>%", "loan_tbl", "is_loan_tbl", "predictor_provenance", "value_map",
    "is_value_map", "binary_outcome", "contingency_table", "group_stats",
    "add_woe", "add_fisher_p", "psi", "psi_from_tables",
    "plot_univariate_smooth", "dynamic_stats", "evaluate_features",
    "fit_model", "cv_folds", "auto_recipe", "step_woebin",
    "plot_paired_u_test", "plot_variable_importance", "plot_profit_curve"
  )
  expect_setequal(public, expected)
  expect_false(exists("plot_density", envir = asNamespace("loan"), inherits = FALSE))
  expect_false(is.null(getS3method("contingency_table", "logical", optional = TRUE)))
})

test_that("recipes methods dispatch when recipes is installed", {
  skip_if_not_installed("recipes")
  expect_false(is.null(getS3method("prep", "step_woebin", optional = TRUE,
                                  envir = asNamespace("recipes"))))
  expect_false(is.null(getS3method("bake", "step_woebin", optional = TRUE,
                                  envir = asNamespace("recipes"))))
})
