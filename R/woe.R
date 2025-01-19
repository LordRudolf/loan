get_variable_dynamic_stats <- function(df, stats = c('woe', 'IV', 'PSI'), horizont = 'Q',
                                       target = NULL, application_status = NULL, ...) {

}


get_WOE <- function(x, ...) {
  UseMethod('get_WOE')
}

get_WOE.loan_cont_table <- function(cont_table, template_WOE = NULL) {
  bad_label <- attributes(cont_table)$target_var_dict$bad_label
  
  total_bads <- sum(cont_table[[bad_label]])
  total_goods <- sum(cont_table$issued_loans_total) - total_bads

  cont_table$woe <- log((cont_table$count_BAD / total_bads) /
                         (cont_table$count_GOOD / total_goods))
  cont_table$woe[is.nan(cont_table$woe) | is.infinite(cont_table$woe)] <- 0 #TO DO: add tolerance value to prevent division by 0 instead

  return(cont_table)
}
#get_WOE(x)