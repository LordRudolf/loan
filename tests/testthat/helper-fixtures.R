portfolio <- local({
  data("fintech", package = "loan")
  as.data.frame(head(fintech, 20000))
})
status_map <- value_map("application_status", approved = "LOAN_ISSUED",
                        rejected = "REJECTED", cancelled = "CANCELLED")
outcome_map <- binary_outcome("fpd15", bad = 1, good = 0)
portfolio_outcome <- check_outcome(portfolio$fpd15, outcome_map)
portfolio_roles <- suppressMessages(loan_tbl(
  portfolio, outcomes = outcome_map, application_status = status_map,
  unlisted_role = "supplementary"
))
