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
  
  if(!is.null(p.adjust_func)) {
    non_nas <- !is.na(p_val)
    p_val[non_nas] <- p.adjust_func(p_val[non_nas])
  }
  
  cont_table$fisher_p_val <- p_val
  
  return(cont_table)
}