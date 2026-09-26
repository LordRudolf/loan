#' Smoothed bad rate or approval rate across a numeric variable
#'
#' Plots a smooth (binomial GAM) of the bad rate -- or of the approval rate --
#' against a numeric variable, with the overall average as a reference line.
#' Accepts the same input forms as [contingency_table()]; the variable must be
#' numeric.
#'
#' @inheritParams contingency_table
#' @param variable Used when `x` is a data frame: the **numeric** variable to
#'   plot, as a single column name or a vector of values with one element per row.
#' @param outcome A **binary** outcome, given as for [contingency_table()];
#'   needed for `type = "bad_rate"`.
#' @param application_status As for [contingency_table()]; for
#'   `type = "approval_rate"` it must carry a [value_map()], so that
#'   cancelled applications can be excluded.
#' @param type `"bad_rate"` or `"approval_rate"`.
#' @param grouping_var Optional: one curve per group. A column name, a vector,
#'   or an expression evaluated in the data (e.g. `gender == "MALE"`).
#' @param include_missing Annotate the share of missing values and their rate.
#' @param add_histogram Add a histogram of the variable above the curve.
#' @param x_log_scale Log-scale the x axis.
#' @param ... Unused.
#' @return A ggplot; a gtable when `add_histogram = TRUE`.
#' @examples
#' data(fintech)
#' plot_univariate_smooth(fintech, "client_age",
#'                        outcome = binary_outcome("fpd15", bad = 1))
#' @export
plot_univariate_smooth <- function(x, ...) {
  if(!is.data.frame(x)) check_variable(x, types = 'numeric')
  UseMethod('plot_univariate_smooth')
}

#' @rdname plot_univariate_smooth
#' @export
plot_univariate_smooth.numeric <- function(x, outcome = NULL, application_status = NULL,
                                           status_map = NULL, ...) {
  check_same_length(x, outcome = outcome, application_status = application_status)
  f <- frame_with_status(x, application_status, status_map)
  plot_univariate_smooth(f$frame, 'variable', outcome = outcome,
                         application_status = f$status, ...)
}

#' @rdname plot_univariate_smooth
#' @export
plot_univariate_smooth.data.frame <- function(x, variable, grouping_var = NULL, type = 'bad_rate',
                                   outcome = NULL, application_status = NULL,
                                   include_missing = FALSE, add_histogram = FALSE,
                                   x_log_scale = FALSE,
                            ...) {

  r <- resolve_roles(x,
                     variable           = variable,
                     outcome            = outcome,
                     application_status = application_status,
                     .required = if(type == 'bad_rate') c('variable', 'outcome')
                                 else c('variable', 'application_status'))

  ## Type is BAD RATE
  if(type == 'bad_rate') {
    outcome <- check_outcome(r$outcome, attr(r, 'maps')$outcome, types = 'binary')
    dependent_var <- as.numeric(outcome == 'bad')
    rate_name <- 'bad rate'
    
    
    ##Type is APPROVAL RATE
  } else if (type == 'approval_rate') {
    ## canonical status classes; `cancelled` is excluded (NA), not counted as a decline
    status_map <- attr(r, 'maps')$application_status
    if(is.null(status_map)) {
      stop('For type = "approval_rate", give `application_status` as a `value_map()` ',
           'so that approved / rejected / cancelled are unambiguous.', call. = FALSE)
    }
    status_class <- as.character(apply_value_map(r$application_status, status_map,
                                                role = 'application_status'))

    dependent_var <- ifelse(status_class == 'approved', 1,
                            ifelse(status_class == 'rejected', 0, NA))

    rate_name <- 'approval rate'
  } else {
    stop('`type` must be "bad_rate" or "approval_rate".', call. = FALSE)
  }

  the_var <- check_variable(r$variable, types = 'numeric')
  var_label <- if(is.character(variable) && length(variable) == 1L) variable else 'variable'
    
  ## Detecting what the grouping variable is
  ## Allowing both - the expression input, vector input or name
  expr <- substitute(grouping_var)
  if(!is.null(expr)) {
    
    if(is.call(expr)) {
      the_group_outcomes <- eval(expr, x, parent.frame())
      grouping_var_name <- deparse(expr)
    } else {
      grouping_var <- eval(grouping_var)
      if(length(grouping_var) == 1) {
        if(!(as.character(grouping_var) %in% colnames(x))) stop(paste0('There does not exist such variable named ', grouping_var))
        the_group_outcomes <- x[[grouping_var]]
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
    
  average_rate <- mean(ggdata$dependent_var, na.rm = TRUE)
  avg_rate_name <- paste0('Average ', rate_name)
  
  g_outcome <- g +
    ggplot2::geom_smooth(
      method = 'gam',
      method.args = list(family = 'binomial')
    ) + 
    ggplot2::geom_hline(yintercept = average_rate) +
    ggplot2::annotate('text', y = average_rate, x = min(the_var, na.rm = TRUE),  label = avg_rate_name, vjust = -1) +
    ggplot2::labs(x = var_label, y = rate_name) +
    ggplot2::theme_minimal()
  if(x_log_scale) g_outcome <- g_outcome + ggplot2::scale_x_log10()
  
  
  
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


plot_density <- function(data, variable, show_unknown = FALSE, ...) {
  
}
