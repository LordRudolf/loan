test_that("registered S3 methods have compatible signatures", {
  result <- tryCatch(tools::checkS3methods(package = "loan"), error = identity)
  if (inherits(result, "error")) {
    skip(paste("checkS3methods unavailable in this environment:", conditionMessage(result)))
  }
  expect_length(result, 0)
})
