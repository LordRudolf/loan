
calculate_PSI_table <- function(cont_table_prev, cont_table, param_name = 'issued_loans_total', var_name = 'variable') {
  ##TO DO: check whether current_obs and prev_obs contain the sames groups/intervals
  ##TO DO: prevent having number of factor levels more than 16

  t1 <- cont_table_prev[, c('the_var', param_name)]
  t2 <- cont_table[, c('the_var', param_name)]

  tt <- merge(cont_table[, c('the_var', param_name)], cont_table_prev[, c('the_var', param_name)], by = 'the_var', all = TRUE)
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

  psi <- tt$PSI
  PSI <- sum(psi[!is.na(psi) & !is.infinite(psi) & !is.nan(psi)])
  
  PSI_table <- structure(
    PSI,
    class = c('loan_PSI_table', 'numeric'),
    PSI_table = tt,
    var_name = var_name,
    param_name = param_name
  )
  
  return(PSI_table)
}


verify_parametrs <- function(the_var, time_split) {
  
  if(!any(is.character(the_var), is.factor(the_var), is.numeric(the_var))) {
    stop('The PSI variable must be character, factor or numeric class')
  }
  
  if(!any(is.logical(time_split), is.integer(time_split))) {
    stop('The time_split base or time_split_comparison variables must be either logical or integer vectors')
  }
  
  l <- length(the_var)
  
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


calculate_PSI <- function(x, ...) {
  UseMethod('calculate_PSI')
}

calculate_PSI.data.frame <- function(df, var_name, target_name, application_status_name = NULL, ...) {
  the_var <- df[[var_name]]
  target <- df[[target_name]]
  if(!is.null(application_status_name)) {
    application_status <- df[[application_status_name]]
  } else {
    application_status <- NULL
  }
  
  calculate_PSI(the_var, target, ..., var_name = var_name, application_status = application_status)
}


calculate_PSI.character <-
  calculate_PSI.factor <-
  calculate_PSI.numeric <- function(the_var, target, time_split_base, time_split_comparison,
                          param_name = 'issued_loans_total', 
                          var_name = 'variable',
                          application_status = NULL, 
                          ..., 
                          requires_verification = TRUE) {
  
  if(requires_verification) {
    verify_parametrs(the_var, time_split_base)
    verify_parametrs(the_var, time_split_comparison)
    stopifnot(length(the_var) == length(target))
    if(!is.null(application_status)) {
      stopifnot(length(application_status) == length(the_var))
    }
    
    if(is.null(application_status) && param_name == 'applications_total') {
      stop('You need to provide the application_status to apply the PSI on application counts')
    }
  }
  
  the_base_var <- the_var[time_split_base]
  if(is.numeric(the_base_var)) the_base_var <- make_cont_table_var(the_base_var, ...)
  
  cont_table_prev <- produce_contingency_table(the_base_var, 
                                               target[time_split_base], 
                                               application_status = application_status[time_split_base],
                                               ...)
  
  cont_table <- produce_contingency_table(the_var[time_split_comparison], 
                                          target[time_split_comparison], 
                                          application_status = application_status[time_split_comparison], 
                                          template_mat = cont_table_prev, 
                                          ...)

  calculate_PSI_table(cont_table_prev, cont_table,
                      param_name = param_name,
                      var_name = var_name)
}

print.loan_PSI_table <- function(PSI) {
  print(unclass(unclass(PSI)))
}
