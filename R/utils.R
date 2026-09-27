#' loan: loan portfolio analysis and credit risk management
#'
#' Tools for analysing a lender's application funnel and loan book: every row
#' is one closed application -- approved, rejected or cancelled -- so approval
#' rates, outcomes and reject inference can be studied together.
#'
#' @section The data contract:
#' * [loan_tbl()] declares which column plays which role: ids, timestamps, the
#'   application status, one or several outcomes (one of them primary),
#'   predictors, and supplementary columns excluded from automatic predictor
#'   selection but available when named explicitly. Unlisted columns are
#'   assigned automatically; [predictor_provenance()] identifies their origin.
#' * [value_map()] maps an institution's raw status values onto the canonical
#'   `approved` / `rejected` / `cancelled`; [binary_outcome()] says which raw
#'   outcome values are `bad`.
#'
#' @section Analysis functions:
#' [contingency_table()], [group_stats()], [psi()] and
#' [plot_univariate_smooth()] each accept a `loan_tbl` whose roles fill in the
#' arguments, a data frame with column names, or vectors when the inputs have
#' a natural vector form. The same values and options use the same calculation.
#' Measures on contingency tables: [add_woe()], [add_fisher_p()].
#'
#' @section Status:
#' The data contract and the analysis functions above are the stable core.
#' `dynamic_stats()` and the modelling layer (`evaluate_features()`,
#' `fit_model()`, `cv_folds()`, `auto_recipe()`, `step_woebin()`) are
#' experimental and being redesigned.
#'
#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

## Column names used inside dplyr / ggplot2 / tidyr calls (non-standard evaluation).
## Not `X` in fit_model(): that one is a genuine global-variable bug (AGENTS.md).
utils::globalVariables(c(
  'Freq', 'the_var', 'value', 'variable', 'the_group', '.',
  'AUC', 'cv_fold', 'variable_set', 'variable_importance', 'spec_labels',
  'acceptance_rate', 'profit_increase_per_application'
))

auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}
