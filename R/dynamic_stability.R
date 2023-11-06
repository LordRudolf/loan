dynamic_summary <- function(df, varname, stats = c('PSI'), interval = 'week') {

  #attributes_df <- generate_attribute_list(df)

  if(is.character(interval) && length(interval) == 1) {
    ##TO DO: check the interval name against the lubridate::ceiling_date `unit` names
    time_splits <- lubridate::ceiling_date(df$application_created_at, unit = interval)

  } else if (length(interval)) {
    ##TO DO: custom time splits
    ##TO DO: more variable checks and explanations if the format is wrong
  }
  ##TO DO: mechanisms for `all_before` and other compare and current_obs periods
  ## Refer to the function calculate_PSI_vector.character and dynamic_summary
  the_var <- df[[varname]]

  if(is.character(interval) && length(interval) == 1) {
    ##TO DO: check the interval name against the lubridate::ceiling_date `unit` names
    time_splits <- lubridate::ceiling_date(df$application_created_at, unit = interval)

  } else if (length(interval)) {
    stop()
    ##TO DO: custom time splits
    ##TO DO: more variable checks and explanations if the format is wrong
  }

  unique_periods <- unique(time_splits)

  dynamic_table <- as.data.frame(matrix(NA, length(unique_periods), length(stats)))
  rownames(dynamic_table) <- unique_periods
  colnames(dynamic_table) <- stats

  if('PSI' %in% stats) {
    dynamic_table[['PSI']] <- calculate_PSI_vector(the_var, df$target, df$application_status, time_splits)
  }

  return(dynamic_table)
}

#dynamic_summary(df, 'client_age')
