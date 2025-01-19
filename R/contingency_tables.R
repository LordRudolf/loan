#' @export
produce_contingency_table <- function(x, ...) {
  UseMethod('produce_contingency_table')
}

#' #' @export
#' produce_contingency_table.loan_df <- function(df, varname, ...) {
#'   
#'   if(!any(varname %in% colnames(df))) stop(paste0('The variable "', varname, '" has not been found in the data set'))
#'   the_var <- make_cont_table_var(df[[varname]], ...)
#'   
#'   produce_contingency_table(the_var, binary_outcome, application_status)
#' }

#' @export
produce_contingency_table.factor <- function(the_var, target, application_status = NULL, 
                                             bad_label = NULL, accept_label = NULL, 
                                             template_mat = NULL, 
                                             classes_limit = Inf,
                                             infrequent_group_name = '_OTHER_',
                                             ...) {
  stopifnot(classes_limit >= 2)
  
  cont_table <- table(the_var) %>% as.data.frame()
  
  if(nrow(cont_table) > classes_limit) {
    freq_vector <- cont_table %>% arrange(desc(Freq)) %>% pull(Freq)
    freq_threshold <- freq_vector[[classes_limit]]
    
    the_var <- as.character(the_var)
    the_var[the_var %in% cont_table$the_var[cont_table$Freq <= freq_threshold]] <- infrequent_group_name
    the_var <- as.factor(the_var)
    cont_table <- table(the_var) %>% as.data.frame()
    
    infreq_row_order <- which(cont_table$the_var == infrequent_group_name)
    cont_table <- cont_table[-infreq_row_order, ] %>% rbind(cont_table[infreq_row_order,])
  }
  
  cont_loan <- table(the_var, target) %>% as.data.frame.matrix()
  colnames(cont_loan) <- paste0('count_', colnames(cont_loan))
  cont_loan$issued_loans_total <- rowSums(cont_loan)
  cont_loan$the_var <- rownames(cont_loan)
  
  target_vals <- unique(target)
  if(is.null(bad_label)) {
    bad_label <- determine_bad_label(target)
  }

  if(!is.null(application_status)) {
    #TO DO: do not calculate issuance rate (and the the contingency table below) if all the applications have been approved
    cont_app <- table(the_var, application_status) %>% as.data.frame.matrix()
    colnames(cont_app) <- paste0('count_', colnames(cont_app))
    cont_app$applications_total <- rowSums(cont_app)
    cont_app$the_var <- rownames(cont_app)

    cont_table <- merge(subset(cont_table, select = 1),  cont_app, by = 'the_var', all.x = TRUE, sort = FALSE)
    cont_table <- merge(cont_table,  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
    
    if(is.null(accept_label)) {
      accept_label <- determine_accept_label(target, application_status)
    }
    app_status_attr <- list(
      values = unique(application_status),
      table = table(application_status, exclude = NULL),
      accept_label = accept_label
    )
  } else {
    cont_table <- merge(subset(cont_table, select = 1),  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
    if(any(is.na(target_vals))) warning('Missing values in the target variable. They are not shown int contingency tables.')
    app_status_attr <- list(
      values = NULL,
      table = NULL,
      accept_label = NULL
    )
  }

  ## Make a new class so some get_[stats] functions could be used for multiple different classes
  cont_table <- structure(
    cont_table,
    class = c('loan_cont_table', 'data.frame'),
    target_var_dict = list(
      values = unique(target),
      table = table(target, exclude = NULL),
      bad_label = bad_label
    ),
    app_status_dict = app_status_attr
  )

  return(cont_table)
}

#' @export
produce_contingency_table.character <- function(the_var, target, ...) {
  the_var <- as.factor(the_var)
  produce_contingency_table(the_var, target, ...)
}

#' @export
produce_contingency_table.numeric <- function(the_var, target, application_status = NULL, template_mat = NULL, breaks = 5, ...) {
  if(!is.null(template_mat)) {
    stopifnot(class(template_mat)[2] == 'loan_cont_table')

    discretized_intervals <- template_mat$the_var
    the_var_num <- discretize_values(the_var, breaks = breaks, discretized_intervals = discretized_intervals)
  }
  the_var_num <- discretize_values(the_var, breaks = breaks)
  produce_contingency_table(the_var = the_var_num, target = target, application_status = application_status, ...)
}


make_cont_table_var <- function(the_var, breaks = 10, unique_val = 10, ...) {
  if(is.numeric(the_var)) {
    the_var <- discretize_values(the_var, breaks = breaks)
  } else if(is.character(the_var) | is.factor(the_var)) {
    the_var <- group_infrequent(the_var, unique_val = unique_val, verbose = TRUE) #TO DO: the grouping shall used only accepted applications instead of all
  } else {
    stop('The variable is not either numeric or character')
  }
  
  return(the_var)
}

determine_bad_label <- function(target) {
  tt <- table(target) %>% as.data.frame()
  bad_label <- tt$target[which.min(tt$Freq)] %>% as.character()
  
  bad_label <- paste0('count_', bad_label)
  
  return(bad_label)
}

determine_accept_label <- function(target, application_status) {
  tt <- table(target, application_status, exclude = NULL) %>%
    prop.table(margin = 2) %>%
    as.data.frame.matrix()
  
  # 1 determine by checking where target variable is not missing
  if(any(rownames(tt) == 'NA.')) {
    tt2 <- tt[rownames(tt) == 'NA.', ]
    
    accept_label <- colnames(tt2)[which.min(unlist(tt2))] %>% as.character()
  } else {
    # 2 Other scenarious too complicated.
    stop('Could not determine the accept label.')
  }
  
  accept_label <- paste0('count_', accept_label)
  
  return(accept_label)
}
