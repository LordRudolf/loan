#' Per-group approval rate, bad rate and statistics of a variable
#'
#' Builds a [contingency_table()] for one variable and adds, per group, the
#' approval rate -- `approved / (approved + rejected)`; cancelled applications
#' never received a risk decision and are excluded -- the bad rate of the
#' binary outcome, and optional statistics. Accepts the same three input forms
#' as [contingency_table()]: vectors, a data frame, or a [loan_tbl()].
#'
#' @inheritParams contingency_table
#' @param outcome A **binary** outcome, given as for [contingency_table()].
#' @param stats Statistics to add per group: any of `"woe"` ([add_woe()]) and
#'   `"fisher_p_val"` ([add_fisher_p()]).
#' @param table_cols_shown Column groups to keep in the result: `"applications"`
#'   (status totals and approval rate), `"outcomes"` (outcome total and bad
#'   rate), `"application_statuses"` (the raw `count_<status>` columns),
#'   `"outcome_statuses"` (`count_good` / `count_bad`).
#' @param template_matrix Not implemented yet.
#' @param score Not used yet.
#' @param ... Passed to the binning, e.g. `breaks` (number of quantile bins,
#'   default 10) or `unique_val` (maximum groups of a nominal variable).
#' @return A `loan_group_stats` (a `loan_cont_table`), with a
#'   [plot()][plot.loan_group_stats] method.
#' @examples
#' data(fintech)
#' status <- value_map("application_status", approved = "LOAN_ISSUED",
#'                     rejected = "REJECTED", cancelled = "CANCELLED")
#' gs <- group_stats(fintech, "client_age",
#'                   outcome = binary_outcome("fpd15", bad = 1),
#'                   application_status = status,
#'                   stats = c("woe", "fisher_p_val"))
#' gs
#' plot(gs)
#' @export

group_stats <- function(x, ...) {
  if(!is.data.frame(x)) check_variable(x)
  UseMethod('group_stats')
}

#' @rdname group_stats
#' @export
group_stats.numeric <- function(x, outcome, application_status = NULL, status_map = NULL, ...) {

  check_same_length(x, outcome = outcome, application_status = application_status)

  ## funnel into the data-frame method, which bins: one workhorse for every input
  ## form (binning here as well double-binned, merging a bin into 'value_other')
  f <- frame_with_status(x, application_status, status_map)
  group_stats(f$frame, 'variable', outcome = outcome, application_status = f$status, ...)
}

#' @rdname group_stats
#' @export
group_stats.character <- group_stats.factor <- group_stats.logical <- group_stats.numeric

  
#' @rdname group_stats
#' @export
group_stats.data.frame <- function(x, variable,
                                       outcome = NULL, application_status = NULL,
                                       stats = c('fisher_p_val'), table_cols_shown = c('applications', 'outcomes'),
                            ..., template_matrix = NULL, score = NULL) {
  
  r <- resolve_roles(x,
                     variable           = variable,
                     outcome            = outcome,
                     application_status = application_status,
                     .required = c('variable', 'outcome'))

  variable_values <- r$variable
  outcome         <- check_outcome(r$outcome, attr(r, 'maps')$outcome, types = 'binary')
  status_map      <- attr(r, 'maps')$application_status

  ## label used in the output attributes
  var_label <- if(is.character(variable) && length(variable) == 1L) variable else 'variable'

  if(is.null(r$application_status)) {
    ## No status column: applications with no outcome could not have been approved.
    ## Synthesised values are already canonical, so the map is the identity one and
    ## no inference warning is needed.
    if(anyNA(outcome)) warning('The outcome has missing values and no `application_status` was given: treating those applications as rejected.')
    application_status <- ifelse(is.na(outcome), 'rejected', 'approved')
    status_map <- value_map(approved = 'approved', rejected = 'rejected')
  } else {
    application_status <- r$application_status
  }

  ## creating initial contingency tables ----------------------------------
  if(is.null(template_matrix)) {

    ##TO DO: check that the variable names are not named after the markers
    the_var <- bin_variable(variable_values, ...)
    cont_table <- contingency_table(the_var, outcome, application_status,
                                    status_map = status_map)

  } else {
    
    stop('Functionality hasnt been developed (yet)')
    ##TO DO: the template_matrix is list that uses template form (the row and column values for the many iterations (e.g., different time periods))
    #in that case the_var shall already be coerced into the factor variable with not too many factor levels
  }
  
  ####################### acquiring good/bad and/or acceptance rate labels
  bad_label <- attributes(cont_table)$target_var_dict$bad_label
  accept_label <- attributes(cont_table)$app_status_dict$accept_label
  
  
  #######################
  ## calculate stats here
  
  ## Approval rate -- denominator is applications that actually received a risk
  ## decision (approved + rejected); `cancelled` is excluded by design.
  if(!is.null(cont_table$decisioned_total)) {
    cont_table$approval_rate <- cont_table[[accept_label]] / cont_table$decisioned_total
    cont_table$approval_rate[is.nan(cont_table$approval_rate)] <- NA
  }
  
  ## Bad rate
  cont_table$bad_rate <- cont_table[[bad_label]] / cont_table$issued_loans_total
  cont_table$bad_rate[is.nan(cont_table$bad_rate)] <- NA
  
  ## WOE
  if('woe' %in% stats) {
    cont_table <- add_woe(cont_table)
  }
  
  ## Fisher test
  if('fisher_p_val' %in% stats) {
    cont_table <- add_fisher_p(cont_table, ...)
  }
  
  ## Average score
  # if('avg_score' %in% stats) {
  #   cont_table$avg_score <- NA
  #   for(i in 1:nrow(cont_table)) {
  #     cont_table$avg_score[[i]] <- mean(score[the_var == cont_table$the_var[[i]]], na.rm = TRUE)
  #   }
  # }
  
  #######################
  ## additional table visualizations
  if(!any(table_cols_shown == 'application_statuses' )) {
    apps_status_vals <- paste0('count_', as.character(unique(application_status)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% apps_status_vals]
  }
  
  if(!any(table_cols_shown == 'applications' )) {
    apps_status_vals <- paste0('count_', as.character(unique(application_status)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(apps_status_vals, 'approval_rate', 'applications_total', 'decisioned_total')]
  }
  
  if(!any(table_cols_shown == 'outcome_statuses' )) {
    outcome_status_vals <- paste0('count_', as.character(unique(outcome)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(outcome_status_vals)]
  }
  
  if(!any(table_cols_shown == 'outcomes' )) {
    outcome_status_vals <- paste0('count_', as.character(unique(outcome)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(outcome_status_vals, 'issued_loans_total', 'bad_rate', '')]
  }
  
  cont_table <- structure(
    cont_table,
    class = c('loan_group_stats', 'loan_cont_table', 'data.frame'),
    cont_table_info = list(
      varname = var_label,
      table_cols_shown = table_cols_shown,
      stats = stats
    )
  )
  
  return(cont_table)
}

#' Plot group statistics
#'
#' One panel per statistic of a [group_stats()] result, across the groups of
#' the variable.
#'
#' @param x A `loan_group_stats`.
#' @param plots_to_make Panels to draw: `"all"` (every rate and statistic in the
#'   table), or a selection such as `c("bad_rate", "woe")`.
#' @param ... Unused.
#' @return A ggplot.
#' @export
plot.loan_group_stats <- function(x, plots_to_make = 'all', ...) {

  cont_info <- attributes(x)$cont_table_info
  
  if(any(plots_to_make == 'all')) {
    plots_to_make <- character()
    
    if('applications' %in% cont_info$table_cols_shown) {
      plots_to_make <- c(plots_to_make, 'approval_rate')
    }
    if('outcomes' %in% cont_info$table_cols_shown) {
      plots_to_make <- c(plots_to_make, 'bad_rate')
    }
    
    for(i in 1:length(cont_info$stats)) {
      plots_to_make <- c(plots_to_make, cont_info$stats[[i]])
    }
  }
  plots_to_make <- unique(plots_to_make)
  
  if(length(plots_to_make) < 1) stop('No available plots for the selected stats') 
  
  long_data <- tidyr::pivot_longer(x, cols = intersect(plots_to_make, colnames(x)), names_to = 'variable')
  
  g <- ggplot2::ggplot(long_data, ggplot2::aes(x = the_var, y = value, group = variable, color = variable)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~variable,  scales="free", ncol = 1) +
    ggplot2::theme_minimal()
  
  
  return(g)
}


