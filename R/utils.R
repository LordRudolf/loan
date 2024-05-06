check_name_existance <- function(the_name, x_names) {
  rlang::eval_tidy(the_name)
  if(!is.na(the_name)) {
    if(class(the_name) != 'character') stop(paste0("The argument '",  deparse(substitute(the_name)), "' must be a character"))
    if(length(the_name) != 1) stop(paste0("The argument '",  deparse(substitute(the_name)), "' must be of length 1"))
    if(!any(the_name == x_names)) stop(paste0("The argument '",  deparse(substitute(the_name)), "': there is no column name called '", the_name, "'"))
  }
  return(the_name)
}

check_timestamp_fields <- function(the_name, x) {
  if(!is.na(the_name)) {
    if(!any(c('POSIXt', 'Date') == class(x[, the_name]))) {
      if(sum(is.na(as.Date(x[, the_name]))) != sum(is.na(x[, the_name]))) {
        stop(paste0("The argument '",  deparse(substitute(the_name)), "' is not recognised as 'Date' or 'POSIXct' class. You may try manually coerce it as the 'Date'/'POSIXct'"))
      }
    }
  }
}

group_infrequent <- function(the_var, unique_val = 10, verbose = TRUE) {

  tt <- table(the_var) %>% as.data.frame() %>% arrange(desc(Freq))
  if(verbose && nrow(tt) > unique_val) warning("The number of unique variable values exceeds the number of unique values. In the result, grouping the infrequent values under the group 'value_other")
  the_var[!the_var %in% tt$the_var[1:unique_val] & !is.na(the_var)] <- 'value_other'

  if(verbose && anyNA(the_var)) warning("There been missing values in the variable. They will be replaced with 'value_NA'")
  the_var[is.na(the_var)] <- 'value_NA'

  #Place value_NA and value_other at the end
  level_names <- levels(factor(the_var))
  the_last_names <- level_names %in% c('value_other', 'value_NA')
  the_var <- factor(the_var, levels = c(level_names[!the_last_names], level_names[the_last_names]))

  return(the_var)
}

parse_attributes_df <- function(df, col) {
  attributes_df <- tibble(col = df)
}

discretize_values <- function(var, breaks, discretized_intervals = NULL) {

  if(!is.null(discretized_intervals)) {
    discretized_intervals <- as.character(discretized_intervals)
    new_var <- rep(NA, length = length(var))

    int_from <- stringr::str_extract(discretized_intervals[[i]], "-Inf|\\d+\\.*\\d*")
    for(i in 2:length(discretized_intervals)) {
      int_to <- stringr::str_extract(discretized_intervals[[i]], "-Inf|\\d+\\.*\\d*")
      new_var[var >= int_from & var < int_to & !is.na(var)] <- discretized_intervals[[i-1]]
      int_from <- int_to
    }
    new_var[is.na(var)] <- 'value_NA'
    var <- as.factor(new_var, levels = discretized_intervals)
  } else {
    var <- arules::discretize(var, breaks = breaks, infinity = TRUE)
    levels(var) <- c(levels(var), 'value_NA')
    var[is.na(var)] <- 'value_NA'
  }

  return(var)
}

make_cont_table_var <- function(the_var, breaks = 10, unique_val = 10) {
  if(is.numeric(the_var)) {
    the_var <- discretize_values(the_var, breaks = breaks)
  } else if(is.character(the_var) | is.factor(the_var)) {
    the_var <- group_infrequent(the_var, unique_val = unique_val, verbose = TRUE) #TO DO: the grouping shall used only accepted applications instead of all
  } else {
    stop('The variable is not either numeric or character')
  }
  
  return(the_var)
}

# 
# d1 <- data.frame(
#   target = c(1, 1, 1, 0, 0, 0),
#   ids = as.character(c(1, 2, 1, 2, 1, 3)),
#   var1 = 1:6,
#   var2 = letters[1:6]
# )
# d2 <- loan_df(d1,define_aliases(binary_outcome = list(target = list(GOOD = 1, BAD = 0)), loan_id = 'ids'))  
# 
# f <- function(d, binary_outcome, loan_id = NULL) {
#   variables_availability_check(d, required_vectors = 'binary_outcome', optional_vectors = 'loan_id')
#   if(is.null(loan_id)) {
#     out <- data.frame(v1 = binary_outcome)
#   } else {
#     out <- data.frame(v1 = binary_outcome, v2 = loan_id)
#   }
#   return(out)
# }
# f(d1) #Non loan_df class - expecting error as no argument provided
# f(d1, binary_outcome = 'target') # Providing the column name: works as expected
# 
# f(d2) #loan_df class - expecting it to automatically detect the relevant arguments
# f(d2, binary_outcome = 'var1') # Providing column name different than the loan_df aliases will override the aliases input
# f(d2, loan_id = FALSE) #If we do not want loan_df auto-providing the optional vector we can override it with providing argument FALSE
# 
# f(d1, binary_outcome = as.logical(d1$target))  # We can provide also arguments as vectors
variables_availability_check <- function(df, required_vectors, ..., optional_vectors = NULL, detect_unused_cols = FALSE) {
  
  is_df_loan_df_object <- any(class(df) == 'loan_df')
  if(is_df_loan_df_object) {
    aliases_available <- names(attributes(df)$loan_df_info$primary_aliases)
  }
  
  args <- modifyList(list(...), as.list(parent.frame()), keep.null = TRUE)
  args_table <- data.frame(
    arg = c(required_vectors, optional_vectors),
    mandatory = c(rep(TRUE, length(required_vectors)), rep(FALSE, length(optional_vectors))),
    indicated_as_not_for_use = FALSE,
    provided = NA,
    provided_as_column_name = NA
  )
  #removing duplicates. The mendatory arguments are the first in order; therefore, it they duplicating with optional vectors, the higher importance argument info will prevail
  args_table <- args_table[!duplicated(args_table$arg),]
  
  names_provided <<- names(args)[unlist(lapply(args, function(x) {
    !(class(x)[[1]] == 'name' | #in case if argument not provided
      is.null(x)) #do not consider it if the argument provided as null
  }))]
  args_table$provided <- args_table$arg %in% names_provided
  argsss<- args
  if(is_df_loan_df_object) { #providing arguments not for use is applicable only for loan_df
    
    #in this function we find in error-free way which arguments where shown as FALSE
    names_not_for_use <- names(args)[unlist(lapply(args, function(x) {
      if(!is.null(x)) {
        if(is.logical(x) && length(x) == 1) {
          !x
        } else {
          FALSE
        }
      } else {
        FALSE
      }
    }))]
    args_table$indicated_as_not_for_use <- args_table$arg %in% names_not_for_use & args_table$arg %in% aliases_available
    args_table$provided[args_table$indicated_as_not_for_use] <- FALSE
    
    for(override_arg in args_table$arg[args_table$indicated_as_not_for_use]) {
      #setting values to NULL to get the default values of these arguments
      assign(override_arg, NULL, envir = parent.frame())
    }
  }
  
  for(i in 1:nrow(args_table)) {
    arg_of_interest <- args[[args_table$arg[[i]]]]
    if(args_table$provided[[i]]) {
      args_table$provided_as_column_name[[i]] <- (length(arg_of_interest) == 1) && (class(arg_of_interest) == 'character')
    }
  }
  args_table$provided_as_column_name[is.na(args_table$provided_as_column_name)] <- FALSE
  
  #assigning the new variables to the parent frame
  for(i in 1:nrow(args_table)) {
    
    ##The structure below:
      #1 If argument been provided
      #1.1      if yes: take the provided value (and check how it was provided and verify if it is correct)
      #1.2      if not: check if it was required/mandatory
      #1.2.1          if yes: check if it is loan_df
      #1.2.1.1            if yes: check if it is within loan_df aliases
      #1.2.1.1.1              if yes: take it from there
      #1.2.1.1.2              if not: stop
      #1.2.1.2            if not (not loan_df): stop
      #1.2.2          if not (not mandatory): check if it is loan_df && is NOT indicates as not for use
      #1.2.2.1            if yes: check it it can be located in loan_df aliases
      #1.2.2.1.1              if yes: take it
      #1.2.2.1.2              if not: next
      #1.2.2.2            if not (not mandatory and cannot be found in loan_df aliases: next
    
    if(args_table$provided[[i]]) {
      #1.1
      
      #Argument provided
      reference_arg <- args[names(args) == args_table$arg[[i]] ][[1]]
      
      if(args_table$provided_as_column_name[[i]]) {
        if(all(colnames(df) != reference_arg)) stop(sprintf("The name '%s' provided as argument '%s' hasn't been found in the dataset",
                                                            reference_arg, args_table$arg[[i]]))
        assign(args_table$arg[[i]], df[[reference_arg]], envir = parent.frame()) #asigning variable if argument provided as column name 
      } else if (!args_table$provided_as_column_name[[i]]) {
        if(length(reference_arg) != nrow(df)) stop(sprintf("If the argument '%s' been provided as vector then it's length must match the number of rows in the dataset.", args_table$arg[[i]]))
        assign(args_table$arg[[i]], reference_arg, envir = parent.frame()) #asigning variable if argument provided as vector
      } else {
        stop('Some hypotethical situation which should not existi if everything else before been correctly configured')
      }
    } else { 
      #1.2 scenario Where the argument hasn't been provided
      if(args_table$mandatory[[i]]) {
        #1.2.1 argument required and it hasn't been provided. Only hope is if it can be located within loan_df aliases
        if(is_df_loan_df_object) {
          #1.2.1.1
          if(any(args_table$arg[[i]] == aliases_available)) { #check if this specific aliases been defined within loan_df
            #1.2.1.1.1
            ex <- parse(text = paste0('df$.', 'outcome')) 
            assign(args_table$arg[[i]], eval(ex), envir = parent.frame())
          } else {
            #1.2.1.1.2
            stop(sprintf("The argument '%s' hasn't been provided nor been defined as `loan_df` aliases. Please provide the argument `%s` or define it in your dataset aliases.", 
                         args_table$arg[[i]], args_table$arg[[i]] ))
          }
        } else {
          #1.2.1.2 Argument required and it is not loan_df
          stop(sprintf("The argument '%s' hasn't been provided. Please define '%s' or convert your dataset to `loan_df` object (check `?loan_df` for more information)", 
                       args_table$arg[[i]], args_table$arg[[i]] ))
        }
      } else {
        #1.2.2 argument not provided and is not required
        if(is_df_loan_df_object && !args_table$indicated_as_not_for_use[[i]]) {
          #1.2.2.1
          if((any(args_table$arg[[i]] == aliases_available))) {
            #1.2.2.1.1.
            ex <- parse(text = paste0('df$.', args_table$arg[[i]])) 
            assign(args_table$arg[[i]], eval(ex), envir = parent.frame())
          } else {
            #1.2.2.1.2
            next
          }
        } else {
          #1.2.2.2 argument not provided, and could not be found in loan_df aliases / or been explicitly told that not for use
          next
        }
      }
    }
  }
}


auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}
