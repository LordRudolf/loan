

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

get_group_stats <- function(x, ...) {
  UseMethod('get_group_stats')
}

get_group_stats.numeric <- function(the_var, binary_outcome, application_status = NULL, ..., make_var = TRUE) {
  
  stopifnot(is.vector(the_var))
  stopifnot(is.vector(binary_outcome))
  
  if(make_var) {
    the_var <- make_cont_table_var(the_var, ...)
  }
  
  temp_df <- data.frame(
    variable = the_var,
    binary_outcome = binary_outcome
  )
  
  if(!is.null(application_status)) temp_df$application_status <- application_status
  
  if (is.null(application_status)) {
    get_group_stats(temp_df, 'variable', binary_outcome = 'binary_outcome', ...)
  } else if (!is.null(application_status)) {
    get_group_stats(temp_df, 'variable', binary_outcome = 'binary_outcome', application_status = 'application_status', ...)
  } else {
    stop('Some impossible scenario')
  }
}

get_group_stats.character <- get_group_stats.factor <- get_group_stats.numeric

  
get_group_stats.data.frame <- function(df, varname, 
                                       binary_outcome, application_status = NULL, 
                                       stats = c('fisher_p_val'), table_cols_shown = c('applications', 'outcomes'),
                            ..., template_mat = NULL, score = NULL) {
  
  if(!binary_outcome %in% colnames(df)) stop(paste0('The binary_outcome variable named "', binary_outcome, '" has not been found in the data set'))
  binary_outcome <- df[[binary_outcome]]
  
  if((unique(binary_outcome) %>% na.omit() %>% length() != 2 )) {
    stop('The binary_outcome variable must have excactly two possible vlues.')
  }
  
  if(is.null(application_status)) {
    if(anyNA(binary_outcome)) warning('There been missing values in the binary_outcome variable. Treating these as rejected applications...')
    application_status <- ifelse(is.na(binary_outcome), 'REJECTED', 'ACCEPTED')
  } else {
    if(!application_status %in% colnames(df)) stop(paste0('The application_status variable named "', application_status, '" has not been found in the data set'))
    application_status <- df[[application_status]]
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
  
  ####################### acquiring good/bad and/or acceptance rate labels
  bad_label <- attributes(cont_table)$target_var_dict$bad_label
  accept_label <- attributes(cont_table)$app_status_dict$accept_label
  
  
  #######################
  ## calculate stats here
  
  ## Issuance rate
  if(!is.null(cont_table$applications_total)) {
    cont_table$issuance_rate <- cont_table[[accept_label]] / cont_table$applications_total
    cont_table$issuance_rate[is.nan(cont_table$issuance_rate)] <- NA
  }
  
  ## Bad rate
  cont_table$bad_rate <- cont_table[[bad_label]] / cont_table$issued_loans_total
  cont_table$bad_rate[is.nan(cont_table$bad_rate)] <- NA
  
  ## WOE
  if('woe' %in% stats) {
    cont_table <- get_WOE(cont_table)
  }
  
  ## Fisher test
  if('fisher_p_val' %in% stats) {
    cont_table <- get_fisher_p_val(cont_table, ...)
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
  
  cont_table <- structure(
    cont_table,
    class = c('loan_group_stats', 'loan_cont_table', 'data.frame'),
    cont_table_info = list(
      varname = varname,
      table_cols_shown = table_cols_shown,
      stats = stats
    )
  )
  
  return(cont_table)
}

#get_group_stats(df, 'client_age')



## TO DO: get_WOE if var and target are vectors, and get_WOE when df is loan_df class

plot.loan_group_stats <- function(cont_table, plots_to_make = 'all') {
  
  cont_info <- attributes(cont_table)$cont_table_info
  
  if(any(plots_to_make == 'all')) {
    plots_to_make <- character()
    
    if('applications' %in% cont_info$table_cols_shown) {
      plots_to_make <- c(plots_to_make, 'issuance_rate')
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
  
  long_data <- tidyr::pivot_longer(cont_table, cols = c('issuance_rate', 'bad_rate', 'woe', 'fisher_p_val'), names_to = 'variable')
  
  g <- ggplot2::ggplot(long_data, ggplot2::aes(x = the_var, y = value, group = variable, color = variable)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~variable,  scales="free", ncol = 1) +
    ggplot2::theme_minimal()
  
  
  return(g)
}


