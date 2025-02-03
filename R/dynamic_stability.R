get_dynamic_stats <- function(x, ...) {
  UseMethod('get_dynamic_stats')
}


get_dynamic_stats.data.frame <- function(df, variables, application_created_at, time_splits, 
                                         stats_funcs = c(calculate_PSI),
                                         compare_against = 'base_period',
                                         base_period = NULL, 
                                         target = NULL,
                                         application_status = NULL, 
                                         ...) {
  
  stopifnot(is.character(variables))
  stopifnot(is.character(application_created_at) && length(application_created_at) == 1)
  
  variable_list <- list()
  for(var in variables) {
    if(is.null(df[[var]])) stop(paste0('Variable ', var, ' has not been found in the dataset provided'))
    variable_list[[var]] <- df[[var]]
  }
  
  application_created_at <- df[[application_created_at]]
  
  if(!is.null(target)) {
    stopifnot(is.character(target) && length(target) == 1)
    target <- df[[target]]
  }
  if(!is.null(application_status)) {
    stopifnot(is.character(application_status) && length(application_status) == 1)
    application_status <- df[[application_status]]
  }
  
  get_dynamic_stats(variable_list = variable_list, 
                    application_created_at = application_created_at, 
                    time_splits = time_splits,
                    compare_against = compare_against,
                    base_period = base_period,
                    stats_funcs = stats_funcs,
                    target = target,
                    application_status = application_status,
                    ...)
}

get_dynamic_stats.list <- function(variable_list, 
                                   application_created_at,
                                   time_splits = 'month',
                                   compare_against = 'base_period',
                                   base_period = NULL,
                                   stats_funcs = list(PSI = calculate_PSI),
                                   target = NULL, application_status = NULL,
                                   ... ){
  
  
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
      length(length(stats_funcs))
    ),
    dimnames = list(
      variable = names(variable_list),
      time_splits = names(end_splits),
      function_name = names(stats_funcs)
    )
  )
  
  for(l in 1:dim(stats_array)[[3]]) {
    print(paste0('Gathering ', names(stats_funcs)[[l]], ' statistics.'))
    
    for(i in 1:dim(stats_array)[[1]]) {
      the_var <- variable_list[[i]]
      
      if(compare_against == 'base_period') {
        res_vector <- time_split_loop_base_period(the_var, end_splits, stats_funcs[[l]], target = target, application_status = application_status, ...)
      } else if (compare_against == 'prev_period') {
        res_vector <- time_split_loop_prev_period(the_var, end_splits, stats_funcs[[l]], target = target, application_status = application_status, ...)
      }
      
      stats_array[i, ,l] <- res_vector
    }
  }
  
  return(stats_array)
}

time_split_loop_base_period <- function(the_var, end_splits, func, target, application_status, ...) {
  
  res_vector <- rep(NA, length(end_splits))
  
  base_index <- end_splits[[1]]
  for(i in 2:length(res_vector)) {
    res_vector[[i]] <- func(the_var, target = target, application_status = application_status, time_split_base = base_index, time_split_comparison = end_splits[[i]], ...)
  }
  
  return(res_vector)
}

time_split_loop_prev_period <- function(the_var, end_splits, func, target, application_status, ...) {
  
  if(is.numeric(the_var)) {
    the_var <- make_cont_table_var(the_var, ...)
  }
  
  res_vector <- rep(NA, length(end_splits))
  
  base_index <- end_splits[[1]]
  
  for(i in 2:length(res_vector)) {
    new_indx <- end_splits[[i]]
    res_vector[[i]] <- func(the_var, target = target, application_status = application_status, time_split_base = base_index, time_split_comparison = new_indx, ...)
    base_index <- new_indx
  }
  
  return(res_vector)
}
