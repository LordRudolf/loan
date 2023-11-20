dynamic_summary <- function(df, varname, application_created_at = NULL, stats = c('PSI'), interval = 'week',
                            split_by_custom_intervals = FALSE, ...) {
  
  
  application_created_at <- variables_availability_check(df, required_vectors = c('application_created_at'), ...)[[1]]
  
  ## interval here
  if(is.character(interval) && length(interval) == 1) {
    time_splits <- lubridate::ceiling_date(application_created_at, unit = interval)
    if(split_by_custom_intervals) warning(paste0('No customer intervals been provided. Splitting by ', interval))
  } else if (length(interval) == nrow(df)) {
    if(length(interval) > nrow(df)/6) { #the number of interval buckets shall not exceed one sixth of the total
      #number of rows - a simple heuristic not worth spending more time on it now.
      stop('Too many custom interval periods.')
    }
    if(split_by_custom_intervals) {
      #`application_created_at <- interval` has been defined lines above; therefore, no operation in this line
    } else {
      time_splits <- interval
      temp_max <- stats::aggregate(application_created_at ~ time_splits,
                                   data.frame(
                                     application_created_at = application_created_at,
                                     time_splits = time_splits
                                   ), max)
      temp_min <- stats::aggregate(application_created_at ~ time_splits,
                                   data.frame(
                                     application_created_at = application_created_at,
                                     time_splits = time_splits
                                   ), max)
      if(!all(rank(temp_min$application_created_at) <= rank(temp_max$application_created_at))) {
        stop('The applications created at are not at the same order as the customer interval')
      }
    }
    
  } else if (length(interval > 1 && length(interval) != nrow(df)) |
             length(interval) && !is.character(interval)) {
    stop('Wrong `interval` argument. It must be a character or a vector of length equal to the number of rows in the dataset.')
  } else {
    stop('Something else wrong... Make a bug report, please!')
  }
  ##TO DO: auto interval detection based on the given application_created_at variable
  
  ## actual variables used for creating tables afterwards here
  the_var <- df[[varname]]
  ##TO DO: throw error if variable hasn't been found
  
  unique_periods <- unique(time_splits)
  
  dynamic_table <- as.data.frame(matrix(NA, length(unique_periods), length(stats)))
  rownames(dynamic_table) <- unique_periods
  colnames(dynamic_table) <- stats
  
  ## Block of calculating request stats ----------------------------------
  if('PSI' %in% stats) {
    dynamic_table[['PSI']] <- calculate_PSI_vector(the_var, df$target, df$application_status, time_splits)
  }
  
  return(dynamic_table)
}

#dynamic_summary(df, 'client_age', application_created_at = df$app_created_at)
