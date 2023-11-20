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
}

generate_attribute_list <- function(df) {
  available_attributes <- attributes(df)[["column_markers"]]
  available_attributes <- c(available_attributes[[1]], available_attributes[[2]], available_attributes[[3]])
  not_available <- names(unlist(available_attributes))[is.na(unlist(available_attributes))]

  attributes_df <- tibble::as_tibble(lapply(available_attributes, function(x) if(is.na(x)) NA else df[[x]]))

  if('application_id' %in% not_available) {
    attributes_df$application_id <- 1:nrow(df)
  }

  if('application_status' %in% not_available) {
    if('loan_status' %in% available_attributes) {
      attributes_df$application_status <- ifelse(is.na(attributes_df$loan_status), 'REJECTED', 'ISSUED')
    } else if ('loan_id' %in% available_attributes) {
      attributes_df$application_status <- ifelse(is.na(attributes_df$loan_id), 'REJECTED', 'ISSUED')
    } else {
      # Assume that all applications were accepted and issued
      attributes_df$application_status <- 'ISSUED'
    }
  }

  ## TO DO
  #if('loan_status' %in% not_available) {
  #  #if loan status is not given then assume that all rows are individual loans
  #  #populate by loan id as in operation few lines above the loan id has been already defined
  #
  #  attributes_df$loan_id
  #}
  if('loan_status' %in% available_attributes) {
    attributes_df$loan_status <- df$loan_status
  }

  if('loan_id' %in% not_available) {
    if('application_status' %in% available_attributes) {
      xx <- attributes_df$application_status =='ISSUED'
      attributes_df$loan_id[xx] <- 1:sum(xx)
    } else if('loan_status' %in% available_attributes) {
      xx <-  !is.na(attributes_df$loan_status)
      attributes_df$loan_id <- 1:sum(xx)
    } else {
      # if loan_id is not given then assume that all the rows are individual loans
      attributes_df$loan_id <- 1:nrow(df)
    }
  }

  ##TO DO: Make smarter target variable creation
  attributes_df$target <- ifelse(df$fpd15 == 1, 'BAD', 'GOOD')

  print('Outcome from the generate_attribute_list:')
  print(attributes_df)

  return(attributes_df)
}

detect_target <- function(target_var) {
  tabl <- table(target_var, exclude = NA)
  unique_classes <- length(tabl)
  tabl_names <- names(tabl)

  possible_good <- stringr::str_detect(tolower(tabl_names),
                         'good|paid')
  possible_bad <- stringr::str_detect(tolower(tabl_names),
                                       'bad|default')

  if(length(unique(na.omit(target_var))) == 2) {
    if(sum(possible_good) == 2 | sum(possible_bad)) stop('Ambigious target class names')
    if(sum(possible_good) == 1) {
      target_good_name <- tabl_names[possible_good]
    } else if(sum(possible_bad) == 1) {
      target_good_name <- tabl_names[!possible_bad]
    } else {
      target_good_name <- names(tabl)[which.max(tabl)]
      warning(paste0('Assigning the `GOOD` target class to the majority of target clases currently named as ',
                     target_good_name))
    }

    target <- target_var
    target[!is.na(target_var) & as.character(target_var) == target_good_name] <- 'GOOD'
    target[!is.na(target_var) & as.character(target) != 'GOOD'] <- 'BAD'
  }

  ## TO DO: assign more classes

  return(target)
}

detect_application_status <- function(application_status_var) {
  #tabl <- table(application_status_var, exclude = NA)

  application_status <- application_status_var

  ## TO DO: all of this
  return(application_status)
}


error_message <- function(vec) {
  stop(paste0('The argument ',
              vec,
              ', if defined by a user, must be a vector with length equal to the number of rows in `df`, or a character length == 1 corresponding to column names in `df`'))
}
  
variables_availability_check <- function(df, required_vectors, ..., optional_vectors = NULL) {
  # Extract additional arguments
  args <- modifyList(list(...), as.list(parent.frame()), keep.null = TRUE)
  if(length(optional_vectors) >= 0) required_vectors <- unique(c(required_vectors, optional_vectors))
  
  args <- args[names(args) %in% required_vectors]
  
  for(vec in names(args)) {
    if(is.null(args[[vec]])) {
      args[[vec]] <- NULL
    } else if (length(args[[vec]]) == 1 && is.na(args[[vec]])) {
      args[[vec]] <- NULL
    }
  }
  
  # Initialize a list to store the final variables
  final_vars <- list()
  
  # Loop through each required vector
  for (vec in required_vectors) {
    
    if (vec %in% names(args)) {
      if(length(args[[vec]]) == 1) { #The case when the vector can be found in `df` by column name
        if(!is.character(args[[vec]])) error_message(vec)
        the_var <- df[, args[[vec]] ]
        if(is.null(the_var)) stop(paste0('The variable ', vec, ' has not been found in `df`'))
        final_vars[[vec]] <- the_var
      } else if(length(args[[vec]]) == nrow(df)) {
        final_vars[[vec]] <- args[[vec]]
      } else {
        error_message(vec)
      }
      
      #End of args[[vec]]) == 1
      
    } else if(class(df)[1] == 'loan_df') {
      if(any(vec == names(attributes(df)$aliases))) {
        
        final_vars[[vec]] <- attributes(df)$aliases[[vec]]
        
      } else {
        stop(sprintf("The column `%s` hasn't been defined.", vec))
      }

      
    } else if (vec %in% optional_vectors) {
      
      ## Do nothing. The optional variable hasn't been found. Continue the loop
      
    } else {
      # If the vector is neither provided as an argument nor a column in df, throw an error
      stop(sprintf("The column `%s` hasn't been defined.", vec))
    }
  }
  
  return(final_vars)
}

