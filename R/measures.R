#' Weight of evidence per group
#'
#' Adds a `woe` column to a binary-outcome contingency table: the log of each
#' group's share of all bads over its share of all goods. Positive values mark
#' groups riskier than average. Groups without goods or bads get 0.
#'
#' @param x A `loan_cont_table` with a binary outcome, from
#'   [contingency_table()] or [group_stats()].
#' @param template_WOE Not used yet.
#' @param ... Unused.
#' @return `x` with a `woe` column.
#' @export
add_woe <- function(x, ...) {
  UseMethod('add_woe')
}

## Outcome count columns of a binary contingency table, as named by
## contingency_table() -- measures never assume the column names themselves.
binary_count_labels <- function(x) {
  dict <- attributes(x)$target_var_dict
  if(is.null(dict$bad_label)) {
    stop('`outcome` must be a binary outcome -- got a multiclass outcome.', call. = FALSE)
  }
  c(bad = dict$bad_label, good = dict$good_label)
}

#' @rdname add_woe
#' @export
add_woe.loan_cont_table <- function(x, template_WOE = NULL, ...) {
  labels <- binary_count_labels(x)
  bads  <- x[[labels[['bad']]]]
  goods <- x[[labels[['good']]]]

  x$woe <- log((bads / sum(bads)) / (goods / sum(goods)))
  x$woe[is.nan(x$woe) | is.infinite(x$woe)] <- 0 #TO DO: add tolerance value to prevent division by 0 instead
  
  return(x)
}



#' Fisher exact test per group
#'
#' Adds a `fisher_p_val` column to a binary-outcome contingency table: each
#' group's bad / good split tested against all other groups combined, with
#' Fisher's exact test, adjusted for multiple testing.
#'
#' @inheritParams add_woe
#' @param p.adjust_func Adjustment applied to the p-values: default
#'   [stats::p.adjust()] (Holm); `NULL` for none.
#' @return `x` with a `fisher_p_val` column.
#' @export
add_fisher_p <- function(x, ...) {
  UseMethod('add_fisher_p')
}

#' @rdname add_fisher_p
#' @export
add_fisher_p.loan_cont_table <- function(x, p.adjust_func = p.adjust, ...) {
  tt <- x[, binary_count_labels(x)]
  p_val <- rep(NA, nrow(tt))
  for(i in 1:length(p_val)) {
    the_new_table <- rbind(tt[i, ],
                           colSums(tt[-i,]))
    p_val[[i]] <- fisher.test(the_new_table)$p.value
  }
  p_val[is.na(x$bad_rate)] <- NA
  
  if(!is.null(p.adjust_func)) {
    non_nas <- !is.na(p_val)
    p_val[non_nas] <- p.adjust_func(p_val[non_nas])
  }
  
  x$fisher_p_val <- p_val
  
  return(x)
}
