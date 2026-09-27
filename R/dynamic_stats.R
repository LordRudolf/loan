#' Dynamic portfolio statistics
#'
#' Experimental: being redesigned.
#' @keywords internal
dynamic_stats <- function(x, ...) {
  UseMethod('dynamic_stats')
}


dynamic_stats.data.frame <- function(x, variables = NULL, application_created_at = NULL, time_splits,
                                         stats_funcs = list(PSI = psi),
                                         compare_against = 'base_period',
                                         base_period = NULL,
                                         outcome = NULL,
                                         application_status = NULL,
                                         ...) {

  ## on a loan_tbl, `variables` defaults to the declared predictors
  if(is.null(variables)) variables <- loan_roles(x)$predictors

  r <- resolve_roles(x,
                     variables              = variables,
                     application_created_at = application_created_at,
                     outcome                = outcome,
                     application_status     = application_status,
                     .required = c('variables', 'application_created_at'),
                     .multi    = 'variables')

  variable_list          <- r$variables
  application_created_at <- r$application_created_at
  outcome                <- check_outcome(r$outcome, attr(r, 'maps')$outcome,
                                          types = c('binary', 'multiclass'))
  application_status     <- r$application_status

  dynamic_stats(variable_list,
                    application_created_at = application_created_at, 
                    time_splits = time_splits,
                    compare_against = compare_against,
                    base_period = base_period,
                    stats_funcs = stats_funcs,
                    outcome = outcome,
                    application_status = application_status,
                    status_map = attr(r, 'maps')$application_status,
                    ...)
}

dynamic_stats.list <- function(x,
                                   application_created_at,
                                   time_splits = 'month',
                                   compare_against = 'base_period',
                                   base_period = NULL,
                                   stats_funcs = list(PSI = psi),
                                   outcome = NULL, application_status = NULL,
                                   ... ){

  variable_list <- x
  ## canonicalise once, so the many psi() calls below do not each re-infer it
  if(!is.null(outcome)) outcome <- check_outcome(outcome, types = c('binary', 'multiclass'))
  if(is.null(names(stats_funcs)) || any(names(stats_funcs) == '')) {
    stop('`stats_funcs` must be a named list of functions, e.g. `list(PSI = psi)`; ',
         'the names label the results.', call. = FALSE)
  }

  ## Detecting time splits
  application_created_at <- as.Date(application_created_at)
  
  if(length(time_splits) == length(application_created_at)) {
    #situation where the end splits been already provided by the user
    end_splits <- split(seq_along(time_splits), as.factor(time_splits))
    
  } else if(is.list(time_splits)) {
    #situation where a user may provide a starting and end date
    end_splits <- list()
    for(i in 1:length(time_splits)) {
      stopifnot('Date' %in% class(time_splits[[i]]))
      stopifnot(length(time_splits[[i]]) == 2)
      ss <- time_splits[[i]]
      temp <- which(application_created_at >= ss[[1]] & application_created_at <= ss[[2]])
      aliases <- paste0('from_', min(application_created_at[temp]), '_to_', max(application_created_at[temp]))
      end_splits[[aliases]] <- temp
    } 
    
  } else if (is.character(time_splits) & length(time_splits)) {
    # lubridate syntax
    temp <- lubridate::floor_date(application_created_at, time_splits)
    end_splits <- split(seq_along(temp), as.factor(temp))
  }
  
  ## Detecting base period
  stopifnot(compare_against %in% c('base_period', 'prev_period'))
  
  if(compare_against == 'base_period') {
    if(is.null(base_period)) {
      # Do nothing. Assume that the first element in the end_splits is the base 
      
    } else {
      if(is.integer(base_period)) {
        base_index <- base_period
      } else if (is.logical(base_period) && length(base_period) == length(application_created_at)) {
        base_index <- which(base_period)
      } else if ('Date' %in% class(base_period) && length(base_period) == 2 ) {
        base_index <- which(application_created_at >= base_period[[1]] & application_created_at <= base_period[[2]])
      } else {
        stop('Unrecognised base_period parameter')
      }
      
      to_be_removed <- c()
      for(i in 1:length(end_splits)) {
        temp <- sum(end_splits[[i]] %in% base_index) / length(end_splits[[i]])
        if(temp > 0.5) to_be_removed <- c(to_be_removed, i)
      }
      if(length(to_be_removed) > 0) {
        warning('There been signficant portion of timesplit rows duplicating with the base period rows. These are getting removed from the dataset.')
        end_splits <- end_splits[!(1:length(end_splits)) %in% to_be_removed]
      }
      
      aliases <- paste0('from_', min(application_created_at[base_index]), '_to_', max(application_created_at[base_index]))
      temp_list <- list()
      temp_list[[aliases]] <- base_index
      end_splits <- c(
        temp_list,
        end_splits
      )
    }
  }
  
  
  ## Doing the loops
  
  stats_array <- array(
    data = NA,
    dim = c(
      length(variable_list),
      length(end_splits),
      length(stats_funcs)
    ),
    dimnames = list(
      variable = names(variable_list),
      time_splits = names(end_splits),
      function_name = names(stats_funcs)
    )
  )
  
  for(l in 1:dim(stats_array)[[3]]) {
    message('Gathering ', names(stats_funcs)[[l]], ' statistics.')
    
    for(i in 1:dim(stats_array)[[1]]) {
      variable <- variable_list[[i]]

      if(compare_against == 'base_period') {
        res_vector <- time_split_loop_base_period(variable, end_splits, stats_funcs[[l]], outcome = outcome, application_status = application_status, ...)
      } else if (compare_against == 'prev_period') {
        res_vector <- time_split_loop_prev_period(variable, end_splits, stats_funcs[[l]], outcome = outcome, application_status = application_status, ...)
      }
      
      stats_array[i, ,l] <- res_vector
    }
  }
  
  return(stats_array)
}

time_split_loop_base_period <- function(variable, end_splits, func, outcome, application_status, ...) {
  
  res_vector <- rep(NA, length(end_splits))
  
  base_index <- end_splits[[1]]
  for(i in 2:length(res_vector)) {
    res_vector[[i]] <- func(variable, outcome = outcome, application_status = application_status, time_split_base = base_index, time_split_comparison = end_splits[[i]], ...)
  }
  
  return(res_vector)
}

time_split_loop_prev_period <- function(variable, end_splits, func, outcome, application_status, ...) {

  if(is.numeric(variable)) {
    variable <- bin_variable(variable, ...)
  }
  
  res_vector <- rep(NA, length(end_splits))
  
  base_index <- end_splits[[1]]
  
  for(i in 2:length(res_vector)) {
    new_indx <- end_splits[[i]]
    res_vector[[i]] <- func(variable, outcome = outcome, application_status = application_status, time_split_base = base_index, time_split_comparison = new_indx, ...)
    base_index <- new_indx
  }
  
  return(res_vector)
}
