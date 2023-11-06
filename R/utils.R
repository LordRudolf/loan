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

discretize_values <- function(var, breaks) {
  #var <- as.character(infotheo::discretize(var, nbins = 10)[[1]])
  var <- arules::discretize(var, breaks = breaks, infinity = TRUE)
  levels(var) <- c(levels(var), 'value_NA')
  var[is.na(var)] <- 'value_NA'
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

produce_contingency_table <- function(x, ...) {
  UseMethod('produce_contingency_table')
}

produce_contingency_table.factor <- function(the_var, target, application_status = NULL, ...) {

  cont_table <- table(the_var) %>% as.data.frame()

  cont_loan <- table(the_var, target) %>% as.data.frame.matrix()
  colnames(cont_loan) <- paste0('count_', colnames(cont_loan))
  cont_loan$issued_loans_total <- rowSums(cont_loan)
  cont_loan$the_var <- rownames(cont_loan)

  if(!is.null(application_status)) {
    #TO DO: do not calculate issuance rate (and the the contingency table below) if the all applications have been approved
    cont_app <- table(the_var, application_status) %>% as.data.frame.matrix()
    colnames(cont_app) <- paste0('count_', colnames(cont_app))
    cont_app$applications_total <- rowSums(cont_app)
    cont_app$the_var <- rownames(cont_app)

    cont_table <- merge(subset(cont_table, select = 1),  cont_app, by = 'the_var', all.x = TRUE, sort = FALSE)
    cont_table <- merge(cont_table,  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
  } else {
    cont_table <- merge(subset(cont_table, select = 1),  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
  }

  ## Make a new class so some get_[stats] functions could be used for multiple different classes
  cont_table <- structure(
    cont_table,
    class = c('loan_cont_table', 'data.frame')
  )

  return(cont_table)
}

produce_contingency_table.character <- produce_contingency_table.factor


produce_contingency_table.numeric <- function(the_var, target, application_status = NULL, ...) {
  stop('how did we arrive here??')
  ### remove this function

  the_var_num <- discretize_values(the_var, breaks = 5)
  produce_contingency_table(the_var = the_var_num, target = target, application_status = application_status, ...)
}
