#' Create a data frame object easily recognized by the other `loan` package functions with the relevant credit-loan-portfolio-indicative.
#' It is designed to work seamlessly with aliases defined using the `define_aliases` function, facilitating consistent data analysis workflows in credit portfolio analysis.
#' @description
#' The function takes an existing data frame containing columns that are typically available for the loan portfolio data sets
#' (e.g., loan status, application created_at, client id, first payment due, etc.) and turns them into a new data frame object recognized by the other
#' `loan` package functions for the automatic and easy data analysis.
#' 
#' @return A `loan_df` object, which can be seamlessly used with the other `loan` package functions under standardized column names.
#'
#' @param df A data.frame or tibble containing the loan portfolio dataset. This dataset should include columns that correspond to the aliases defined in the `aliases` argument.
#' @param aliases A list of alias mappings created using the `define_aliases` function. This list specifies how columns in `df` should be renamed or transformed to fit standardized analysis templates. The `aliases` argument can directly take the output of `define_aliases`, which allows for a flexible and powerful way to manage dataset preprocessing. For more information on how to use `define_aliases`, see its detailed documentation and examples.
#' @param data_downloaded_at The timestamp indicating when the data was downloaded or last updated. This parameter defaults to the current system time but can be overridden to reflect the actual data acquisition time.
#'
#' @details
#' The `loan_df` function is part of a suite of tools designed for credit portfolio analysis. It leverages the `aliases` defined by the user to standardize dataset columns, making it easier to apply consistent analysis and modeling techniques across different datasets. The function evaluates the `aliases` within the context of the provided dataset, applying the necessary transformations and mappings.
#'
#' The integration with `define_aliases` allows for a flexible and powerful way to manage dataset preprocessing, enabling analysts to define complex mappings and transformations in a structured and reusable manner.
#'
#' @examples
#' # Load the fintech dataset included with the package
#' data(fintech)
#'
#' # Transform 'fintech' into a 'loan_df' object with standardized aliases
#' df <- loan_df(fintech,
#'              define_aliases(
#'                application_status = list(application_status = list(ACCEPTED = 'LOAN_ISSUED', 
#'                                                                    REJECTED = c('REJECTED'),
#'                                                                    CANCELLED = 'CANCELLED',
#'                                                                    IN_PROCESS = c('APPROVED', 'PENDING'))),
#'                loan_status = list(loan_status = list(
#'                  PAID = 'PAID_OFF',
#'                  DEFAULTED = 'DEFAULTED',
#'                  VOIDED = 'VOIDED',
#'                  ACTIVE = c('PERFORMING', 'PAST_DUE')
#'                )),
#'                days_past_due_outcome = c('fpd'),
#'                application_id = c('application_id'),
#'                loan_id = c('loan_id'),
#'                client_id = c('client_id'),
#'                application_created_timestamp = c('app_created_at')
#'              ),
#'              data_downloaded_at = as.Date('2021-10-30')
#' )
#' 
#' @export
loan_df <- function(df,
                    aliases = list(),
                    workflow = NA,
                    data_downloaded_at = Sys.time()) {
  
  # Create a temporary environment and copy 'df' into it
  temp_env <- new.env()
  temp_env$df <- df
  
  # Evaluate 'FUN' within the temporary environment
  # 'FUN' is expected to be a function that 'base_f' will call with additional arguments as needed
  aliases <- eval(substitute(aliases), envir = temp_env)
  
  if(!is.null(aliases$predictors_colnames)) {
    predictors <- aliases$predictors_colnames
    aliases$predictors_colnames <- NULL
  } else {
    predictors = NA
  }
  
  df <- new_loan_df(
    df,
    data_downloaded_at = data_downloaded_at,
    predictors = predictors,
    aliases = aliases
  )


  return(df)
}

new_loan_df <- function(df = data.frame(),  predictors = NA,
                        aliases = list(), data_downloaded_at = NA) {
  stopifnot(is.data.frame(df))
  stopifnot(is.list(aliases))
  
  # Extract primary aliases, assuming the first element in each list is the primary alias
  primary_aliases <- sapply(aliases, function(x) names(x)[[1]]) #assuming that the first element are the primary aliases
  
  # Identify the primary outcome based on predefined outcome types
  primary_outcome <- primary_aliases[names(primary_aliases) %in% c('binary_outcome', 'multinomial_outcome', 'loan_status')]
  if(length(primary_outcome) >= 1) {
    primary_outcome <- primary_outcome[1] 
  } else {
    primary_outcome <- NA
  }
  
  # Return the structured 'loan_df' object with additional metadata
  structure(
    tibble::as_tibble(df),
    class = c('loan_df', class(df)),
    data_downloaded_at =  data_downloaded_at,
    predictors = predictors,
    aliases = aliases,
    loan_df_info = list(
      aliases_defined = names(aliases),
      primary_aliases = primary_aliases,
      primary_outcome = primary_outcome
    )
  )
}

print.loan_df <- function(df, ...) {

  cat('Loans data set contains ', nrow(df), ' rows and ', ncol(df), ' columns. \n \n')
  
  #non_na <- unlist(lapply(attributes(df)[["aliases"]], function(df) names(df)[!is.na(df)]))
  #if(length(non_na) > 0) {
  #  cat(paste0('With the following column markers: ', paste(non_na, collapse = ', '), '\n \n'))
  #  if(any(non_na == 'application_created_at')) cat(paste0('Application_created_at quantiles: ', paste0(
  #    as.Date(summary(df[, attributes(df)[["column_markers"]][['timestamp_fields']][['application_created_at']]])), collapse = ', '
  #  ), '\n \n'))
  #} else {
  #  cat('There are no column  markers.\n \n')
  #}
  
  non_na <- lapply(attributes(df)[["aliases"]], names)
  if(length(non_na) > 0) {
    cat('There have been defined the following aliases: \n')
    for(i in 1:length(non_na)) {
      cat(paste0(names(non_na)[[i]], ': '), non_na[[i]], '\n')
    }
  }
  
  cat('The data downloaded at ', as.character(attributes(df)[['data_downloaded_at']]), '\n \n')

  print(tibble::as_tibble(df)) #!!!

  invisible(df)
}


#' Extract columns from loan_df object
#'@description
#'Extracts columns based on predefined aliases from the *loan_df* object.
#'
#' @param df The loan_df object from which to extract the column.
#' @param name The name of the variable to extract, using unified naming.
#' @return The extracted column as a vector.
#' 
#' @method $ loan_df
#' @export
#' 
#' @examples 
#' # Example usage omitted for brevity
`$.loan_df` <- function(df, name) {
  if(substring(name, 1, 1) == '.') {
    orig_name <- name
    name <- sub('.', '', name)
    aliases_defined <- attributes(df)$loan_df_info$aliases_defined
    
    if(name %in% aliases_defined || (name == 'outcome' && !is.na(attributes(df)$loan_df_info$primary_outcome))) {
      
      if(name == 'outcome') {
        col_name <- attributes(df)$loan_df_info$primary_outcome
        name <- names(col_name)
      } else {
        col_name <- attributes(df)$loan_df_info$primary_aliases
        col_name <- col_name[[name]]
        out_var_info <- attributes(df)$aliases[[name]][[col_name]]
      }
      out_var_info <- attributes(df)$aliases[[name]][[col_name]]
      output_var <- map_the_variable(df[[col_name]], out_var_info)
      
      return(output_var)
    } else {
      name <- orig_name ## Restore the original name if alias not found
    }
  }
  
  NextMethod("$")
}
