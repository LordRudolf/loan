#' Define Aliases for Credit Portfolio Dataset Columns
#'
#' This function facilitates standardized processing and analysis of credit portfolio data by allowing users to define aliases for various dataset columns. It supports defining aliases for key aspects of loan data, such as application statuses, loan performance, binary outcomes for customer behavior, multinomial outcomes, days past due (DPD), and unique identifiers. For specific aliases, detailed mappings are required, especially for `application_status` and `loan_status`, to ensure accurate analysis.
#'
#' @param df A data.frame or tibble containing the dataset where aliases will be applied. If `df` is not directly provided, the function attempts to find it in the parent environment.
#' @param binary_outcome A character vector or list specifying the column(s) and their mappings for binary outcomes.
#' @param multinomial_outcome Similar to binary_outcome, but for multinomial outcomes.
#' @param days_past_due_outcome Column name(s) for days past due outcomes, expected to be numeric.
#' @param cumulative_financial_outcome Column name(s) for cumulative financial outcomes.
#' @param loan_status Column name(s) for loan status with optional mappings.
#' @param application_status Column name(s) for application status with optional mappings.
#' @param other_outcome_col_names Additional outcome column names not covered by the above.
#' @param application_id Column name(s) for application IDs.
#' @param loan_id Column name(s) for loan IDs.
#' @param client_id Column name(s) for client IDs.
#' @param application_created_timestamp Column name(s) for application creation timestamps.
#' @param predictors_colnames Specifies the predictor columns, '.auto' for automatic detection.
#'
#' @return A structured list of the processed aliases and their mappings, which can be used as an argument for creating a dataset via the `loan_df` function.
#'
#' @details
#' **Specific Aliases and Their Keys:**
#'
#' - `application_status`: Maps the process stages of loan applications. Keys include:
#'   - `ACCEPTED`: The application has been accepted, resulting in an issued loan.
#'   - `REJECTED`: The application has been rejected due to internal lending rules and/or the scorecard.
#'   - `CANCELLED`: The client started creating the application, but it was cancelled (often unfinished) before evaluation.
#'   - `IN_PROCESS`: The application is currently being evaluated. The status will change to `REJECTED` if it does not pass the internal evaluation, or `ISSUED` if it passes and no further client action is required.
#'
#' - `loan_status`: Corresponds to the performance of accepted loans. Keys include:
#'   - `ACTIVE`: Loans that are currently open, which may include performing loans or delayed loans expected to recover.
#'   - `PAID`: Loans that have been fully paid off.
#'   - `DEFAULTED`: Loans that have caused losses, including those sold to debt collection, written off, or any other status indicating financial loss.
#'   - `RESTRUCTURED`: Loans considered in a grey zone where the original terms have been altered to favor client solvency, still active.
#'   - `VOIDED`: Loans that were returned in full immediately after disbursement, often due to incorrect client banking details or other reasons.
#'
#' - `binary_outcome`, `multinomial_outcome`, `days_past_due_outcome`, and unique identifiers (`application_id`, `loan_id`, `client_id`, `application_created_timestamp`) are also supported, with the user able to specify detailed mappings or direct column references.
#'
#' Users may specify multiple columns for each alias, either as lists for detailed mappings or as character vectors for direct column name references.
#'
#' @return A structured list containing the processed aliases and their mappings, ready for further analysis or processing within the package.
#'
#' @examples
#' df <- data.frame(
#'   app_id = 1:3,
#'   application_status = c("ACCEPTED", "ACCEPTED", "ACCEPTED", "REJECTED"),
#'   loan_status = c("ACTIVE", "DEFAULTED", "PAID", NA),
#'   class = c('GOOD', 'BAD', 'GOOD', NA),
#'   dpd_30 = c(FALSE, TRUE, FALSE, NA),
#'   fpd = c(5, 186, 0, NA),
#'   dpd_current_date = c(0, 186, 0, NA),
#'   dpd_in_MOB3 = c(0, 90, 0, NA)
#' )
#' 
#' aliases <- define_aliases(
#'   df,
#'   application_status = "application_status",
#'   loan_status = "loan_status",
#'   binary_outcome = list(class = list(GOOD = 'GOOD', BAD = 'BAD'), dpd_30 = list(GOOD = FALSE, BAD = TRUE)),
#'   days_past_due_outcome = c('fpd', 'dpd_current_date', 'dpd_in_MOB3')
#' )
#'
#' @export
define_aliases <- function(df = NULL,
                           
                           #outcomes
                           binary_outcome = NULL,
                           multinomial_outcome = NULL,
                           days_past_due_outcome = NULL,
                           cumulative_financial_outcome = NULL,
                           loan_status = NULL,
                           application_status = NULL,
                           other_outcome_col_names = NULL, #not been added yet
                           
                           #ids
                           application_id = NULL,
                           loan_id = NULL,
                           client_id = NULL,
                           other_id_colnames = NULL, #not been added yet
                           
                           application_created_timestamp = NULL,
                           other_timestamp_colnames = NULL, #not been added yet
                           
                           predictors_colnames = '.auto') {

  
  # 'df' is expected to be found in the current evaluation environment
  if(is.null(df)) {
    if (!exists("df", envir = parent.frame())) {
      stop("Data frame 'df' not found in the current context.")
    }
    df <- get('df', envir = parent.frame())
  }
  
  ## Checking correctness of aliases and assigning dummy aliases where required
  if(!is.null(application_status)) {
    # To DO
  } else {
    # TO DO
  }
  
  ## get the sequence of the arguments provided
  arg_sequence <- match.call() %>%
    as.list()
  arg_sequence <- arg_sequence[-1]
  arg_sequence <- names(arg_sequence)

  
  ############################################################################################################
  aliases_mapping <- list()
  
  ## processing outcomes
  if(!is.null(application_status)) { ## application_status
    aliases_mapping$application_status <-  process_argument(df, 
                                                            arg = application_status,
                                                            mapping_rules = getMappingRulesTemplate('application_status'),
                                                            auto_mapping_function = NA,
                                                            mapping_variable = 'application_status')
  }
  if(!is.null(loan_status)) { ## loan_status
    aliases_mapping$loan_status <-  process_argument(df,
                                                     arg = loan_status,
                                                     mapping_rules = getMappingRulesTemplate('loan_status'),
                                                     auto_mapping_function = NA,
                                                     mapping_variable = 'loan_status')
  }
  if(!is.null(binary_outcome)) { ## binary_outcome
    aliases_mapping$binary_outcome <- process_argument(df, 
                                                       arg = binary_outcome,
                                                       mapping_rules = getMappingRulesTemplate('binary_outcome'),
                                                       auto_mapping_function = detect_binary_outcome,
                                                       mapping_variable = 'binary_outcome')
  }
  if(!is.null(multinomial_outcome)) { ## multinomial outcome
    aliases_mapping$multinomial_outcome <-  process_argument(df, 
                                                             arg = multinomial_outcome,
                                                             mapping_rules = getMappingRulesTemplate('multinomial_outcome'),
                                                             auto_mapping_function = NA,
                                                             mapping_variable = 'multinomial_outcome')
  }
  if(!is.null(days_past_due_outcome)) { ## days_past_due_outcome
    aliases_mapping$days_past_due_outcome <-  process_argument(df, 
                                                               arg = days_past_due_outcome,
                                                               mapping_rules = getMappingRulesTemplate('days_past_due_outcome'),
                                                               auto_mapping_function = NA,
                                                               mapping_variable = 'days_past_due_outcome')
  }
  if(!is.null(application_id)) { ## application_id
    aliases_mapping$application_id <-  process_argument(df, 
                                                        arg = application_id,
                                                        mapping_rules = getMappingRulesTemplate('application_id'),
                                                        auto_mapping_function = NA,
                                                        mapping_variable = 'application_id')
  }
  if(!is.null(loan_id)) { ## loan_id
    aliases_mapping$loan_id <-  process_argument(df, 
                                                 arg = loan_id,
                                                 mapping_rules = getMappingRulesTemplate('loan_id'),
                                                 auto_mapping_function = NA,
                                                 mapping_variable = 'loan_id')
  }
  if(!is.null(client_id)) { ## client_id
    aliases_mapping$client_id <-  process_argument(df, 
                                                   arg = client_id,
                                                   mapping_rules = getMappingRulesTemplate('client_id'),
                                                   auto_mapping_function = NA,
                                                   mapping_variable = 'client_id')
  }
  if(!is.null(application_created_timestamp)) { ## application_created_timestamp
    aliases_mapping$application_created_timestamp <-  process_argument(df, 
                                                                       arg = application_created_timestamp,
                                                                       mapping_rules = getMappingRulesTemplate('application_created_timestamp'),
                                                                       auto_mapping_function = NA,
                                                                       mapping_variable = 'application_created_timestamp')
  }
  
  if(!is.null(predictors_colnames)) {
    dataset_cols <- colnames(df)
    used_cols <- lapply(aliases_mapping, names) %>% unlist()
    if(predictors_colnames[[1]] == '.auto') {
      predictors <- dataset_cols[(!dataset_cols %in% used_cols)] ## TO DO: automatically do not include timestamp columns as predictors
    } else {
      non_existant_predictors <- predictors_colnames[!predictors_colnames %in% dataset_cols]
      if(length((non_existant_predictors) > 0)) stop(sprintf("The following predictors haven't been found in the dataset: ",
                                                             paste(non_existant_predictors, collapse = ', ')))
      
      overlapping_predictors <- predictors_colnames[predictors_colnames %in% used_cols]
      if(length(overlapping_predictors) > 0 ) stop(sprintf("The following predictors have been already used in aliases: ",
                                                          paste(overlapping_predictors, collapse = ', ')))
      predictors <- predictors_colnames
    }
    aliases_mapping$predictors_colnames <- predictors
  }
    
  
  # reorder the elements of the list in the order the arguments were provided
  aliases_mapping <- aliases_mapping[arg_sequence]
  aliases_mapping_ordered <- list()
  for(arg in arg_sequence) {
    if(!is.null(aliases_mapping[[arg]])) {
      aliases_mapping_ordered[[arg]] <- aliases_mapping[[arg]]
    }
  }
  
  ## to do: check whether the column names or not duplicating along the arguments
  
  return(
    structure(
      aliases_mapping_ordered,
      class = c('loan_df_aliases', 'list')
    )
  )
}


#' Process Argument for Alias Definition
#'
#' This internal helper function processes an argument provided to define aliases within a dataset. 
#' It supports both character vectors and lists as inputs, handling column name validation, 
#' duplication checks, and mapping based on predefined rules.
#'
#' @param df A data frame containing the dataset where aliases will be applied.
#' @param arg The argument provided by the user, which can be a character vector of column names 
#' or a list where keys are column names and values are the mappings for those columns.
#' @param mapping_rules A list containing rules for how the argument should be processed, 
#' including keys for valid mappings and actions for missing or unmapped values.
#' @param auto_mapping_function (Unused in this example) Placeholder for a function that could 
#' automatically determine mappings based on dataset analysis.
#' @param mapping_variable The name of the variable being processed, used for error and warning messages 
#' to provide context to the user.
#'
#' @return A list of mapped values if `arg` is a list with mappings, or the direct column values 
#' from `df` if `arg` is a character vector. The function also performs checks and validations, 
#' stopping execution with an error message or issuing warnings as necessary based on `mapping_rules`.
#'
#' @note This function is intended for internal use within the package and is not exported for 
#' end-user direct usage.
#'
#' @examples
#' # This function is not intended to be used directly by end users, so no examples are provided.
#'
#' @noRd
process_argument <- function(df, arg, mapping_rules, auto_mapping_function, mapping_variable) {
  
  # Validate argument type
  if(all(class(arg) != c('character', 'list'))) stop(sprintf("The '%s' argument should be either a character vector or a list.", mapping_variable))
  
  # Process based on argument type
  if(class(arg) == 'character') {
    arg_names <- arg
  } else if(class(arg) == 'list') {
    arg_names <- names(arg)
    if(any(duplicated(arg_names))) {
      duplicated_names <- arg_names[duplicated(arg_names)]
      stop(sprintf("Duplicate column names found in '%s' alias definition: %s. Each column name must be unique.",
                   mapping_variable, paste(duplicated_names, collapse = ", ")))
    }
    if(is.null(arg_names)) {
      stop(sprintf("The '%s' alias is provided as a list without named elements. Please ensure each element has a name corresponding to a column name in the dataset.", mapping_variable))
    }
  }
  
  cols <- arg_names[arg_names %in% colnames(df)]
  
  # Check for missing aliases
  if (length(cols) == 0) {
    stop(sprintf("Could not find any aliases '%s' that match with the column names of the dataset.", mapping_variable))
  }
  if (length(cols) < length(arg_names)) {
    missing_aliases <- arg_names[!arg_names %in% cols]
    warning(sprintf("The following aliases for '%s' were not found in the dataset's column names: %s.", mapping_variable, paste(missing_aliases, collapse = ', ')))
  }
  
  if(class(arg) == 'list') {
    if(all(is.na(mapping_rules$mapped_keys))) stop(sprintf("Mapping keys for '%s' are required but were not provided, or they are not applicable for numeric types.", mapping_variable))
    do_mapping <- TRUE 
  } else {
    do_mapping <- FALSE
  }
  
  # Process columns for mapping
  for(arg_name in cols) {
    
    if(do_mapping) {
      if(!is.na(mapping_rules$mandatory_mapped_values) && !all(mapping_rules$mandatory_mapped_values %in% names(arg[[arg_name]]))) {
        stop(sprintf("The alias '%s' for column '%s' is missing mandatory mapped values. Expected keys: %s.", mapping_variable, arg_name, paste(mapping_rules$mandatory_mapped_values, collapse = ', ')))
      }
      the_var_mapped <- map_the_variable(df[[arg_name]], list(mapping = arg[[arg_name]], output_class = list(class = mapping_rules$output_class)))
      
      if(any(!unique(na.omit(the_var_mapped)) %in% mapping_rules$mapped_keys) && mapping_rules$non_mapped_values != 'accept') {
        not_mapped <- unique(na.omit(the_var_mapped))[!unique(na.omit(the_var_mapped)) %in% mapping_rules$mapped_keys]
        
        if(mapping_rules$non_mapped_values == 'throw_error') {
          stop(sprintf("The following values in column '%s' for alias '%s' have not been mapped to any predefined keys: %s.", arg_name, mapping_variable, paste(not_mapped, collapse = ', ')))
        } else if(mapping_rules$non_mapped_values == 'warning') {
          warning(sprintf("Unmapped values found in column '%s' for alias '%s': %s. Consider adding them to the predefined keys or adjusting your data.", arg_name, mapping_variable, paste(not_mapped, collapse = ', ')))
        } else {
          stop("Functionality for handling non-mapped values in this context has not been implemented yet.")
        }
      }
    } else {
      the_var_mapped <- df[[arg_name]]
    }
    
    if(any(is.na(the_var_mapped)) && mapping_rules$NA_action != 'accept') {
      stop(sprintf("Missing values ('NA') found in column '%s' for alias '%s' are not allowed based on the current mapping rules.", arg_name, mapping_variable))
    } 
    
    if(length(mapping_rules$additional_conditions) > 0) {
      for(i in 1:length(mapping_rules$additional_conditions)) {
        eval(mapping_rules$additional_conditions[[i]])
      }
    }
    
  } #end of each column check loop
  
  mapped_values <- list()
  for(arg_name in cols) {
    if(class(arg) == 'list') {
      additional_processing <- NULL # Placeholder for potential future logic
      output_class <- if(mapping_rules$output_class == 'factor' && mapping_rules$non_mapped_values == 'throw_error') {
        list(class = 'factor', levels = mapping_rules$mapped_keys)
      } else if (mapping_rules$output_class == 'factor' && mapping_rules$non_mapped_values != 'throw_error') {
        list(class = 'factor', levels = NULL)
      }
      
      mapped_values[[arg_name]] <- list(
        mapping = arg[[arg_name]],
        output_class = output_class,
        additional_processing = additional_processing
      )
    } else {
      mapped_values[[arg_name]] <- list(
        mapping = NA,
        output_class = list(class = mapping_rules$output_class),
        additional_processing = NULL
      )
    }
  }
  
  return(mapped_values)
}



#' Maps variable values based on defined aliases and conversion rules
map_the_variable <- function(output_var, out_var_info) {
  if(is.factor(output_var)) output_var <- as.character(output_var)
  
  # Apply mapping based on the provided dictionary
  if(all(!is.na(out_var_info$mapping[[1]]))) {
    the_dict <- as.list(setNames(unlist(out_var_info$mapping, use.names = FALSE), rep(names(out_var_info$mapping), lengths(out_var_info$mapping)) ))
    for(i in 1:length(the_dict)) output_var[output_var %in% the_dict[[i]]] <- names(the_dict)[[i]]
  }
  
  if(out_var_info$output_class$class == 'factor') {
    if(!is.null(out_var_info$output_class$levels)) {
      output_var <- factor(output_var, levels = out_var_info$output_class$levels) 
    } else {
      output_var <- factor(output_var)
    }
  } else if (out_var_info$output_class$class == 'integer') {
    output_var <- as.integer(output_var)
  }
  
  return(output_var)
}

getMappingRulesTemplate <- function(aliases) {
  temp_list <- list(
    application_status = list(
      mapped_keys = c('ACCEPTED', 'REJECTED', 'CANCELLED', 'IN_PROCESS'),
      mandatory_mapped_values = c('ACCEPTED'),
      output_class = 'factor',
      NA_action = 'throw_error',
      non_mapped_values = 'warning',
      additional_conditions = list(),
      additional_processing = list()
    ),
    loan_status = list(
      mapped_keys = c('ACTIVE', 'PAID', 'DEFAULTED', 'RESTRUCTURED', 'VOIDED'),
      mandatory_mapped_values = NA,
      output_class = 'factor',
      NA_action = 'accept', #TO DO: accept only of application status is not ACCEPTED
      non_mapped_values = 'throw_error',
      additional_conditions = list(
        ## TO DO: checking if there are no samples with non-missing loan status and not accepted application status
      ),
      additional_processing = list()
    ),
    binary_outcome = list(
      mapped_keys = c('BAD', 'GOOD'),
      mandatory_mapped_values = NA,
      output_class = 'factor',
      NA_action = 'accept',
      non_mapped_values = 'throw_error',
      additional_conditions = list(),
      additional_processing = list()
    ),
    multinomial_outcome = list(
      mapped_keys = NA,
      must_be_unique = FALSE,
      mandatory_mapped_values = NA,
      output_class = 'factor',
      NA_action = 'accept',
      non_mapped_values = 'accept',
      additional_conditions = list(),
      additional_processing = list()
    ),
    days_past_due_outcome = list(
      mapped_keys = NA,
      mandatory_mapped_values = NA,
      output_class = 'integer',
      NA_action = 'accept',
      non_mapped_values = 'accept',
      additional_conditions = list(
        integer_requirement = expression(if(!is.integer(the_var_mapped)) stop('All the days_past_due_outcome aliases must be integers'))
      ),
      additional_processing = list()
    ),
    application_id = list(
      mapped_keys = NA,
      mandatory_mapped_values = NA,
      output_class = 'character',
      NA_action = 'accept',
      non_mapped_values = 'accept',
      additional_conditions = list(
        must_be_unique = expression({
          if(any(duplicated(the_var_mapped)))stop('Application ids must be unique for each row')
        })
      ),
      additional_processing = list()
    ),
    loan_id = list(
      mapped_keys = NA,
      mandatory_mapped_values = NA,
      output_class = 'character',
      NA_action = 'accept',
      non_mapped_values = 'accept',
      additional_conditions = list(), #TO DO: if duplicated loan id then check if loan_stauts and client_id are the same for all of these.
      additional_processing = list()
    ),
    client_id = list(
      mapped_keys = NA,
      mandatory_mapped_values = NA,
      output_class = 'character',
      NA_action = 'accept',
      non_mapped_values = 'accept',
      additional_conditions = list(),
      additional_processing = list()
    ),
    application_created_timestamp = list(
      mapped_keys = NA,
      mandatory_mapped_values = NA,
      output_class = 'timestamp',
      NA_action = 'warning',
      non_mapped_values = 'accept',
      additional_conditions = list(
        check_if_timestamp_format = expression({
          if(!class(the_var_mapped)[1] %in% c('Date', 'POSIXct', 'POSIXt')) stop('application_created_timestamp variables must be POSIXct or date classes')
        })
      ),
      additional_processing = list()
    )
    
    # Add more templates for other variable types as needed
  )
  
  return(temp_list[[aliases]])
}

list_aliases <- function(df) {
  if(!any(class(df) == 'loan_df')) stop('The function `list_aliases` is applicable only for the `loan_df` class objects')
  attri <- attributes(df)
  aliases <- attri$aliases
  
  if(is.null(aliases) | length(aliases) == 0) message("Aliases hasn't been defined")
  
  #very ugly way how to get this but simple
  print_table <- tibble::tibble(n = 1:length(unlist(aliases)), alias = NA, dataset_column = NA, keys = NA, mapped_keys = NA) 
  
  j <- 0
  for(i in 1:length(aliases)) {
    
    aliases_element <- aliases[[i]]
    
    for(l in 1:length(aliases_element)) {
      j <- j + 1
      if(l == 1) print_table$alias[[j]] <- names(aliases)[[i]]
      
      print_table$dataset_column[[j]] <- names(aliases_element)[l]
      
      if(any(is.na(aliases_element[[l]]$mapping))) {
        print_table$keys[[l]] <- NA
        print_table$mapped_keys[[l]] <- NA
      } else {
        
        mapping <- aliases_element[[l]]$mapping
        
        for(ii in 1:length(mapping)) {
          if(ii > 1) j <- j + 1
          print_table$keys[[j]] <- names(mapping)[[ii]]
          print_table$mapped_keys[[j]] <- paste(mapping[[ii]], collapse = ', ')
        }
        
        print_table$keys[[l]] <- NA
      }
      
    }
  }
  print_table <- print_table[!(is.na(print_table$dataset_column) & is.na(print_table$keys)), ]
  print_table$n <- NULL
  
  cat('The dataset has the following aliases with the corresponding keys:')
  cat('\n')
  print(as.data.frame(print_table))
}
