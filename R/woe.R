get_variable_dynamic_stats <- function(df, stats = c('woe', 'IV', 'PSI'), horizont = 'Q',
                                       target = NULL, application_status = NULL, ...) {

}


#' Summarize some statistics
#'@description
#'Extract predefined unified variables from the *loan_df* class object
#'
#' @param df The data.frame object
#' @param varname Feature to explore
#' @return table that shows relationship between the feature and the outcome variables
#' 
#' @export
#' 
#' @examples 
#' lalalaaa
#' 
#' ##TO DO: in documentation, add information that the user can add 'application_statuses' and/or 'outcome_statuses' as table_cols_shown_arguments
get_group_stats <- function(df, varname, stats = c('woe', 'fisher_p_val'), table_cols_shown = c('applications', 'outcomes'),
                            binary_outcome = NULL, application_status = NULL, 
                            ..., template_mat = NULL, score = NULL) {
  variables_availability_check(df, required_vectors = c(
    'binary_outcome'), ..., optional_vectors = 'application_status')

  if(!exists('application_status')) {
    application_status <- ifelse(is.na(outcome), 'REJECTED', 'ACCEPTED')
  }

  ## creating initial contingency tables ----------------------------------
  if(is.null(template_mat)) {

    if(!any(varname %in% colnames(df))) stop(paste0('The variable "', varname, '" has not been found in the data set'))
    ##TO DO: check that the variable names are not named after the markers
    the_var <- make_cont_table_var(df[[varname]], ...)
    
    cont_table <- produce_contingency_table(the_var, binary_outcome, application_status)

  } else {

    stop('Functionality hasnt been developed (yet)')
    ##TO DO: the template_mat is list that uses template form (the row and column values for the many iterations (e.g., different time periods))
    #in that case the_var shall already be coerced into the factor variable with not too many factor levels
  }

  #######################
  ## calculate stats here
  
  ## Issuance rate
  if(!is.null(cont_table$applications_total)) {
    cont_table$issuance_rate <- cont_table$count_ACCEPTED / cont_table$applications_total
    cont_table$issuance_rate[is.nan(cont_table$issuance_rate)] <- NA
  }

  ## Bad rate
  cont_table$bad_rate <- cont_table$count_BAD / cont_table$issued_loans_total
  cont_table$bad_rate[is.nan(cont_table$bad_rate)] <- NA

  ## WOE
  if('woe' %in% stats) {
    cont_table <- get_WOE(cont_table)
  }
  if('fisher_p_val' %in% stats) {
    cont_table <- get_fisher_p_val(cont_table)
  }
  
  if('avg_score' %in% stats) {
    cont_table$avg_score <- NA
    for(i in 1:nrow(cont_table)) {
      cont_table$avg_score[[i]] <- mean(score[the_var == cont_table$the_var[[i]]], na.rm = TRUE)
    }
  }
  
  ## additional table visualizations
  if(!any(table_cols_shown == 'application_statuses' )) {
    apps_status_vals <- paste0('count_', as.character(unique(application_status)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% apps_status_vals]
  }
  
  if(!any(table_cols_shown == 'applications' )) {
    apps_status_vals <- paste0('count_', as.character(unique(application_status)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(apps_status_vals, 'issuance_rate', 'applications_total')]
  }
  
  if(!any(table_cols_shown == 'outcome_statuses' )) {
    outcome_status_vals <- paste0('count_', as.character(unique(binary_outcome)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(outcome_status_vals)]
  }
  
  if(!any(table_cols_shown == 'outcomes' )) {
    outcome_status_vals <- paste0('count_', as.character(unique(binary_outcome)))
    
    cont_table <- cont_table[, !colnames(cont_table) %in% c(outcome_status_vals, 'issued_loans_total', 'bad_rate', '')]
  }

  return(cont_table)
}

#get_group_stats(df, 'client_age')


get_fisher_p_val <- function(x, ...) {
  UseMethod('get_fisher_p_val')
}

#' @param ... addition argument you mau use for the p-value adjustment function.
get_fisher_p_val.loan_cont_table <- function(cont_table, p.adjust_func = p.adjust, ...) {
  tt <- cont_table[, c('count_BAD', 'count_GOOD')]
  p_val <- rep(NA, nrow(tt))
  for(i in 1:length(p_val)) {
    the_new_table <- rbind(tt[i, ],
                           colSums(tt[-i,]))
    p_val[[i]] <- fisher.test(the_new_table)$p.value
  }
  p_val[is.na(cont_table$bad_rate)] <- NA

  if(!is.null(p.adjust)) {
    non_nas <- !is.na(p_val)
    p_val[non_nas] <- p.adjust_func(p_val[non_nas], ...)
  }

  cont_table$fisher_p_val <- p_val

  return(cont_table)
}

get_WOE <- function(x, ...) {
  UseMethod('get_WOE')
}

get_WOE.loan_cont_table <- function(cont_table, template_WOE = NULL) {
  total_bads <- sum(cont_table$count_BAD)
  total_goods <- sum(cont_table$count_GOOD)

  cont_table$woe <- log((cont_table$count_BAD / total_bads) /
                         (cont_table$count_GOOD / total_goods))
  cont_table$woe[is.nan(cont_table$woe) | is.infinite(cont_table$woe)] <- 0 #TO DO: add tolerance value to prevent division by 0 instead

  return(cont_table)
}
#get_WOE(x)

## TO DO: get_WOE if var and target are vectors, and get_WOE when df is loand_df class


