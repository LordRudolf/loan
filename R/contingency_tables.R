#' @export
produce_contingency_table <- function(x, ...) {
  UseMethod('produce_contingency_table')
}

#' @export
produce_contingency_table.factor <- function(the_var, target, application_status = NULL, template_mat = NULL, ...) {

  cont_table <- table(the_var) %>% as.data.frame()

  cont_loan <- table(the_var, target) %>% as.data.frame.matrix()
  colnames(cont_loan) <- paste0('count_', colnames(cont_loan))
  cont_loan$issued_loans_total <- rowSums(cont_loan)
  cont_loan$the_var <- rownames(cont_loan)

  if(!is.null(application_status)) {
    #TO DO: do not calculate issuance rate (and the the contingency table below) if all the applications have been approved
    cont_app <- table(the_var, application_status) %>% as.data.frame.matrix()
    colnames(cont_app) <- paste0('count_', colnames(cont_app))
    cont_app$applications_total <- rowSums(cont_app)
    cont_app$the_var <- rownames(cont_app)

    cont_table <- merge(subset(cont_table, select = 1),  cont_app, by = 'the_var', all.x = TRUE, sort = FALSE)
    cont_table <- merge(cont_table,  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
  } else {
    cont_table <- merge(subset(cont_table, select = 1),  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
  }

  ## Make a new class so some get_[stats] functions could be used for multiple different classes
  cont_table <- structure(
    cont_table,
    class = c('loan_cont_table', 'data.frame')
  )

  return(cont_table)
}

#' @export
produce_contingency_table.character <- produce_contingency_table.factor

#' @export
produce_contingency_table.numeric <- function(the_var, target, application_status = NULL, template_mat = NULL, ...) {
  if(!is.null(template_mat)) {
    stopifnot(class(template_mat)[2] == 'loan_cont_table')

    discretized_intervals <- template_mat$the_var
    the_var_num <- discretize_values(the_var, breaks = 5, discretized_intervals = discretized_intervals)
  }
  the_var_num <- discretize_values(the_var, breaks = 5)
  produce_contingency_table(the_var = the_var_num, target = target, application_status = application_status, ...)
}
