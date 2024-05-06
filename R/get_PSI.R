#' @param interval The interval on how you want to split the time period. You may provide a character string specifying
#' a time unit to be to be rounded to. It uses the `lubridate::ceiling_date()` (see `?lubridate::round_date` for more information). Or, you may specify
#' your own splits by providing Dates vector with length equal to the number of rows in the dataset.
#' @param compare_against a character string specifying the period you want to use the basis for the PSI calculation. Valid values are:
#' * `prev_period`: goes through each interval starting from the second one where compares with the interval just one before.
#' * `all_before`: takes the most recent interval and compares it against each of the interval before.
#' Another option is providing a logical vector with length equal to the number of rows in the dataset. TRUE values will
#' indicate the base period you will want to compare against. If any rows from the intervals generated from the parameter `interval`
#' overlaps with `compare_against` rows then entire interval will get discarded.
#' @param current_obs optional logical vector where TRUE indicating the observations you want to calculate PSI for. If both, the argument `current_obs`
#' and a vector of `compare_against` provided, the function will ignore the argument `interval`. ##TO DO
#' @param variables Name of the variables or a vector of the variable names you want to calculate the PSI for. Leave it NULL or NA if you want to calculate for all the
#'  predictors and outcome variables
get_PSI <- function(x, ...) {
  UseMethod('get_PSI')
}
get_PSI.loan_df <- function(df,
                            interval = 'month',
                            compare_against = 'prev_period',
                            current_obs = NA,
                            variables = NULL,
                            ...
                            ) {

  if(is.character(interval) && length(interval) == 1) {
    ##TO DO: check the interval name against the lubridate::ceiling_date `unit` names
    time_splits <- lubridate::ceiling_date(df$application_created_at, unit = interval)

  } else if (length(interval)) {
    ##TO DO: custom time splits
    ##TO DO: more variable checks and explanations if the format is wrong
  }
  ##TO DO: mechanisms for `all_before` and other compare and current_obs periods
  ## Refer to the function calculate_PSI_vector.character and dynamic_summary


  if(is.null(variables)) {
    variables <- c(attributes(df)$predictors) ##TO DO: add score to the variables
  }

  unique_periods <- unique(time_splits)
  PSI_table <- as.data.frame(matrix(NA, length(variables), length(unique_periods)))
  rownames(PSI_table) <- variables
  colnames(PSI_table) <- unique_periods

  for(i in 1:length(variables)) {
    PSI_table[i, ] <- calculate_PSI_vector(df[, variables[[i]]], attributes_df$target, attributes_df$application_status, time_splits)
  }

  return(PSI_table)
}
##TO DO: PSI function which requires simply vector inputs


calculate_PSI_vector <- function(x, ...) {
  UseMethod('calculate_PSI_vector')
}


calculate_PSI_vector.loan_df <- function(df, the_var, interval = 'month', compare_against = 'prev_period', ...) {
  the_var <- df[[the_var]]
  #attributes_df <- generate_attribute_list(df)

  if(is.character(interval) && length(interval) == 1) {
    ##TO DO: check the interval name against the lubridate::ceiling_date `unit` names
    time_splits <- lubridate::ceiling_date(df$application_created_at, unit = interval)
  } else if (length(interval)) {
    ##TO DO: custom time splits
    ##TO DO: more variable checks and explanations if the format is wrong
  }
  ##TO DO: mechanisms for `all_before` and other compare and current_obs periods
  ##Refer to the function get_PSI.loan_df

  v <- calculate_PSI_vector(the_var, df$target, attributes_df$application_status, time_splits)

  return(v)
}

calculate_PSI_vector.numeric <- function(the_var, target, application_status, time_splits) {
  unique_periods <- unique(time_splits)
  v <- double(length = length(unique_periods))
  v[[1]] <- NA

  for(j in 1:length(unique_periods)) {

    selected_cases <- time_splits == unique_periods[[j]]

    if(j == 1) {
      cont_table <- produce_contingency_table(the_var[selected_cases], target[selected_cases], application_status[selected_cases])
    } else {
      cont_table <- produce_contingency_table(the_var[selected_cases], target[selected_cases], application_status[selected_cases], template_mat = cont_table_prev)
      v[[j]] <- calculate_PSI(cont_table,  cont_table_prev)
    }

    cont_table_prev <- cont_table
  }

  return(v)
}
calculate_PSI_vector.character <- calculate_PSI_vector.numeric


calculate_PSI <- function(cont_table, cont_table_prev, param = 'issued_loans_total') {
  ##TO DO: check whether current_obs and prev_obs contain the sames groups/intervals
  ##TO DO: prevent having number of factor levels more than 16

  t1 <- cont_table_prev[, c('the_var', param)]
  t2 <- cont_table[, c('the_var', param)]

  tt <- merge(cont_table[, c('the_var', param)], cont_table_prev[, c('the_var', param)], by = 'the_var', all = TRUE)
  tt$p1 <- tt[, 2] / sum(tt[, 2])
  tt$p2 <- tt[, 3] / sum(tt[, 3])
  tt$rate_diff <- tt$p1 - tt$p2
  tt$ratio <- log(tt[, 2] / tt[, 3])

  if(any(is.infinite(tt$ratio))) {
    the_inf <- is.infinite(tt$ratio) & !is.na(is.infinite(tt$ratio)) & !is.nan(is.infinite(tt$ratio))
    tt$ratio[the_inf] <- max(abs(tt$ratio[!the_inf])) * sign(tt$ratio[the_inf])
  }

  tt$PSI <- tt$rate_diff * tt$ratio

  PSI <- sum(tt$PSI[!(is.na(tt$PSI) | is.nan(tt$PSI))])

  return(PSI)
}
