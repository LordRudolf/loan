
#' Population stability index from two contingency tables
#'
#' The low-level form of [psi()], for tables you have already built.
#'
#' @param cont_table_prev,cont_table The base and comparison `loan_cont_table`s,
#'   with the same groups -- build the second with
#'   `template_matrix = cont_table_prev`.
#' @inheritParams psi
#' @return A `loan_psi`.
#' @export
psi_from_tables <- function(cont_table_prev, cont_table, measure = 'issued_loans_total', var_name = 'variable') {
  ##TO DO: check whether current_obs and prev_obs contain the sames groups/intervals
  ##TO DO: prevent having number of factor levels more than 16

  t1 <- cont_table_prev[, c('the_var', measure)]
  t2 <- cont_table[, c('the_var', measure)]

  tt <- merge(cont_table[, c('the_var', measure)], cont_table_prev[, c('the_var', measure)], by = 'the_var', all = TRUE)
  tt$p1 <- tt[, 2] / sum(tt[, 2])
  tt$p2 <- tt[, 3] / sum(tt[, 3])
  tt$rate_diff <- tt$p1 - tt$p2
  tt$ratio <- log(tt[, 2] / tt[, 3])
  
  tt <- tt[, c(1, 4:7)]
  
  ## some simple way on how to deal with no-events groups
  if(any(is.infinite(tt$ratio))) {
    the_inf <- is.infinite(tt$ratio) & !is.na(is.infinite(tt$ratio)) & !is.nan(is.infinite(tt$ratio))
    tt$ratio[the_inf] <- max(abs(tt$ratio[!the_inf])) * sign(tt$ratio[the_inf])
  }

  tt$PSI <- tt$rate_diff * tt$ratio

  psi_values <- tt$PSI
  psi_total <- sum(psi_values[!is.na(psi_values) & !is.infinite(psi_values) & !is.nan(psi_values)])
  
  PSI_table <- structure(
    psi_total,
    class = c('loan_psi', 'numeric'),
    PSI_table = tt,
    var_name = var_name,
    measure = measure
  )
  
  return(PSI_table)
}


check_psi_args <- function(variable, time_split) {

  if(!any(is.logical(time_split), is.integer(time_split))) {
    stop('The time_split base or time_split_comparison variables must be either logical or integer vectors')
  }
  
  l <- length(variable)
  
  if(is.logical(time_split)) {
    if(length(time_split) != l) {
      stop('The time_split, if logical, must match with the length of the variable')
    }
  }
  
  if(is.integer(time_split)) {
    if(any(time_split) < 1) {
      stop('The time split,if integer, cannot hold negative numbers')
    }
    if(max(time_split) > l) {
      stop('The time split, if integer, cannot have larger elements than the length of the variable.')
    }
  }
}


#' Population stability index of a variable
#'
#' Compares a variable's distribution between a base and a comparison sample
#' -- typically two time periods: the sum over groups of
#' `(p_comparison - p_base) * log(p_comparison / p_base)`. The variable is
#' grouped on the base sample and the comparison sample reuses those groups, so
#' both are binned identically. Accepts the same three input forms as
#' [contingency_table()].
#'
#' @inheritParams contingency_table
#' @param ... In the data-frame form, passed on to the vector form: the time
#'   splits, `measure`, `breaks`.
#' @return A `loan_psi`: the PSI value, with the per-group table as its
#'   `PSI_table` attribute.
#' @examples
#' data(fintech)
#' base <- as.Date(fintech$app_created_at) <  as.Date("2024-04-01")
#' comp <- as.Date(fintech$app_created_at) >= as.Date("2024-07-01")
#' psi(fintech, "gender", outcome = binary_outcome("fpd15", bad = 1),
#'     time_split_base = base, time_split_comparison = comp)
#' @export
psi <- function(x, ...) {
  if(!is.data.frame(x)) check_variable(x)
  UseMethod('psi')
}

#' @rdname psi
#' @export
psi.data.frame <- function(x, variable, outcome = NULL, application_status = NULL, ...) {

  r <- resolve_roles(x,
                     variable           = variable,
                     outcome            = outcome,
                     application_status = application_status,
                     .required = c('variable', 'outcome'))

  var_label <- if(is.character(variable) && length(variable) == 1L) variable else 'variable'

  psi(r$variable,
      check_outcome(r$outcome, attr(r, 'maps')$outcome, types = c('binary', 'multiclass')),
      ...,
      var_name           = var_label,
      application_status = r$application_status,
      status_map         = attr(r, 'maps')$application_status)
}


#' @rdname psi
#' @param time_split_base,time_split_comparison Rows of the base and the
#'   comparison sample: logical vectors with one element per row, or integer
#'   row indices.
#' @param measure The count compared between the samples:
#'   `"issued_loans_total"` (default: applications with a known outcome),
#'   `"applications_total"` (all applications; needs `application_status`), or
#'   any other count column of the [contingency_table()], e.g.
#'   `"count_rejected"`.
#' @param var_name Label stored in the result; the data-frame form sets it to
#'   the column name.
#' @param requires_verification Check the inputs first (default `TRUE`).
#' @export
psi.character <-
  psi.factor <-
  psi.logical <-
  psi.numeric <- function(x, outcome, time_split_base, time_split_comparison,
                          measure = 'issued_loans_total', 
                          var_name = 'variable',
                          application_status = NULL, 
                          ..., 
                          requires_verification = TRUE) {
  
  if(requires_verification) {
    check_psi_args(x, time_split_base)
    check_psi_args(x, time_split_comparison)
    check_same_length(x, outcome = outcome, application_status = application_status)
    
    if(is.null(application_status) && measure == 'applications_total') {
      stop('You need to provide the application_status to apply the PSI on application counts')
    }
  }
  
  ## canonicalise once here, not in each of the two contingency tables below
  outcome <- check_outcome(outcome, types = c('binary', 'multiclass'))

  the_base_var <- x[time_split_base]
  if(is.numeric(the_base_var)) the_base_var <- bin_variable(the_base_var, ...)
  
  cont_table_prev <- contingency_table(the_base_var, 
                                               outcome[time_split_base], 
                                               application_status = application_status[time_split_base],
                                               ...)
  
  cont_table <- contingency_table(x[time_split_comparison], 
                                          outcome[time_split_comparison], 
                                          application_status = application_status[time_split_comparison], 
                                          template_matrix = cont_table_prev, 
                                          ...)

  psi_from_tables(cont_table_prev, cont_table,
                      measure = measure,
                      var_name = var_name)
}

#' @export
print.loan_psi <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}
