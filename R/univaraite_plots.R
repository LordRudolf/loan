plot_univariate_smooth <- function(df, varname, grouping_var = NULL, type = 'bad_rate',
                                   include_missing = FALSE, add_histogram = FALSE,
                                   x_log_scale = FALSE,
                            ...) {

  ## Type is BAD RATE
  if(type == 'bad_rate') {
    variables_availability_check(df, required_vectors = c(
      'outcome'), ...)
    outcome_type <- detect_outcome_type(outcome)
    if(outcome_type == 'binary_in_numeric_form') {
      outcome <- ifelse(outcome == 1, 'event', 'not_event')
      base_factor <- 'event'
      rate_name <- 'event rate'
    } else if (outcome_type == 'binary') {
      if(all(c('GOOD', 'BAD') %in% unique(outcome))) {
        base_factor <- 'BAD'
        rate_name <- 'bad rate'
      } else {
        base_factor <- unique(na.omit(outcome))[[1]]
        rate_name <- paste0('rate of occorunce of ', base_factor)
      }
    } else {
      stop('At the moment the function `plot_univaraite_smooth supports only binary outcome variables.`')
    }

    dependent_var <- ifelse(outcome == base_factor, 1, 0)
    
    
    ##Type is ACCEPTANCE RATE
  } else if (type == 'acceptance_rate') {
    variables_availability_check(df, required_vectors = c(
      'application_status'), ...)
    if(!all(c('ACCEPTED') %in% application_status)) stop('The application status column must include factor named `ACCEPTED`')
    
    dependent_var <- ifelse(application_status == 'ACCEPTED', 1,
                           ifelse(application_status %in% c('CANCELLED', 'IN_PROCESS'), NA, 0))
    
    rate_name <- 'acceptance rate'
  }
  
  the_var <- df[[varname]]
  
  if(is.character(the_var) | is.factor(the_var)) {
    stop('The variable is not numeric.')
  }
    
  ## Detecting what the grouping variable is
  ## Allowing both - the expression input, vector input or name
  expr <- substitute(grouping_var)
  if(!is.null(expr)) {
    
    if(is.call(expr)) {
      the_group_outcomes <- eval(expr, df)
      grouping_var_name <- deparse(expr)
    } else {
      grouping_var <- eval(grouping_var)
      if(length(grouping_var) == 1) {
        if(!(as.character(grouping_var) %in% colnames(df))) stop(paste0('There does not exist such variable named ', grouping_var))
        the_group_outcomes <- df[[grouping_var]]
        grouping_var_name <- grouping_var
      } else {
        the_group_outcomes <- grouping_var
        grouping_var_name <- 'The group'
      }
    }
    
    if(!(is.character(the_group_outcomes) | is.factor(the_group_outcomes))) stop('The expression of grouping variable must output a character of factor type vector.')
    if(length(the_group_outcomes) != length(dependent_var)) stop('The grouping variable length must match with the outcome variable length.')
    
    if(anyNA(the_group_outcomes)) {
      the_group_outcomes <- as.character(the_group_outcomes)
      the_group_outcomes[is.na(the_group_outcomes)] <- '.MISSING_VALUES'
      the_group_outcomes <- as.factor(the_group_outcomes)
    }
  }
  
  if(is.null(grouping_var)) {
    #grouping variable not provided
    ggdata <- tibble::tibble(
      dependent_var = dependent_var, 
      variable = the_var
    )
    g <- ggplot2::ggplot(data = ggdata, ggplot2::aes(x = variable, y = dependent_var))
    
  } else {
    #grouping variable provided
    ggdata <- tibble::tibble(
      dependent_var = dependent_var, 
      variable = the_var,
      the_group = the_group_outcomes
    )
    
    g <- ggplot2::ggplot(data = ggdata, 
                         ggplot2::aes(x = variable, y = dependent_var, colour = the_group, group = the_group)) +
      ggplot2::labs(fill = grouping_var_name)
  }
  if(x_log_scale) g_outcome + ggplot2::scale_x_log10()
    
  average_rate <- mean(ggdata$dependent_var, na.rm = TRUE)
  avg_rate_name <- paste0('Average ', rate_name)
  
  g_outcome <- g +
    ggplot2::geom_smooth(
      method = 'gam',
      method.args = list(family = 'binomial')
    ) + 
    ggplot2::geom_hline(yintercept = average_rate) +
    ggplot2::annotate('text', y = average_rate, x = min(the_var, na.rm = TRUE),  label = avg_rate_name, vjust = -1) +
    ggplot2::labs(x = varname, y = rate_name) +
    ggplot2::theme_minimal()
  
  
  
  ###########################
  ## Information about the missing values
  if(include_missing && anyNA(the_var)) {
    
    res <- ggdata %>%
      mutate(type = ifelse(!is.na(the_var), 'available', 'values_missing')) %>%
      group_by(type) %>%
      summarize(
        number_of_cases = n(),
        proportion_missing = n() / nrow(ggdata),
        average_rate = mean(dependent_var, na.rm = TRUE),
        number_of_known_rate_observations = sum(!is.na(dependent_var)),
        proportion_the_rate_unknown = mean(is.na(dependent_var))
      )
    
    the_message <- paste0(
      'Proportion of the variable missing:  ', round(res$proportion_missing[[2]], 4), '\n',
      avg_rate_name, ' of the missing values: ', round(res$average_rate[[2]], 4), '\n'
    )
    
    g_outcome <- g_outcome +
      ggplot2::labs(tag = the_message) +
      ggplot2::theme(
        plot.tag.location = 'plot'
      )
    
  }
  
  if(add_histogram) {
    if(is.null(grouping_var)) {
      g_hist <- ggplot2::ggplot(data = ggdata, ggplot2::aes(x = variable)) 
    } else {
      g_hist <- ggplot2::ggplot(data = ggdata, ggplot2::aes(x = variable, colour = the_group, group = the_group, fill = the_group)) 
    }
    
    g_hist <- g_hist +
      ggplot2::geom_histogram(alpha = 0.5, position = ggplot2::position_dodge(0.2)) +
      ggplot2::theme_minimal()
    
    if(x_log_scale) g_hist <- g_hist + ggplot2::scale_x_log10()
    
    g_outcome <- gridExtra::grid.arrange(g_hist, g_outcome, nrow = 2, heights=c(1, 4))
  }
  
    
  return(g_outcome)
}


plot_density <- function(df, varname, show_unknown = FALSE, ...) {
  
}
