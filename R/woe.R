get_variable_dynamic_stats <- function(df, stats = c('woe', 'IV', 'PSI'), horizont = 'Q',
                                       target = NULL, application_status = NULL, ...) {

}

#' @param varname the column name which we want to calculate WOE for
get_group_stats <- function(df, varname, stats = c('woe', 'fisher_p_val'),
                            target = NULL, application_status = NULL, 
                            ..., template_mat = NULL) {
  
  func_vars <- variables_availability_check(df, required_vectors = c(
    'target'), ..., optional_vectors = 'application_status')
  
  target <- func_vars$target
  if(is.null(func_vars[['application_status']])) {
    application_status <- ifelse(is.na(target), 'REJECTED', 'ISSUED')
  } else {
    application_status <- func_vars$application_status
  }

  ## creating initial contingency tables ----------------------------------
  if(is.null(template_mat)) {

    if(!any(varname %in% colnames(df))) stop(paste0('The variable "', varname, '" has not been found in the data set'))
    ##TO DO: check that the variable names are not named after the markers
    the_var <- make_cont_table_var(df[[varname]], ...)
    
    cont_table <- produce_contingency_table(the_var, target, application_status)

  } else {

    stop('Functionality hasnt been developed (yet)')
    ##TO DO: the template_mat is list that uses template form (the row and column values for the many iterations (e.g., different time periods))
    #in that case the_var shall already be coerced into the factor variable with not too many factor levels
  }


  #######################
  ## calculate stats here
  ## Issuance rate
  if(!is.null(cont_table$applications_total)) {
    cont_table$issuance_rate <- cont_table$issued_loans_total / cont_table$applications_total
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


