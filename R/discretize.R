## Variable preparation helpers: turning raw analysis variables into
## factors suitable for contingency tables (numeric -> discretized bins,
## character/factor -> infrequent levels grouped).

bin_variable <- function(variable, breaks = 10, unique_val = 10, ...) {
  check_variable(variable)
  if(is.numeric(variable)) {
    variable <- discretize_values(variable, breaks = breaks)
  } else {  # character, factor, logical
    variable <- group_infrequent(variable, unique_val = unique_val, verbose = TRUE) #TO DO: the grouping shall used only accepted applications instead of all
  }

  return(variable)
}

discretize_values <- function(variable, breaks, discretized_intervals = NULL, cuts = NULL) {

  if(!is.null(discretized_intervals)) {
    discretized_intervals <- as.character(discretized_intervals)

    if(!is.null(cuts)) {
      labels_no_nas <- discretized_intervals[discretized_intervals != 'value_NA']
      new_var <- cut(variable, breaks = cuts, include.lowest = TRUE, right = TRUE, labels = labels_no_nas)
      levels(new_var) <- c(labels_no_nas, 'value_NA')
      new_var[is.na(new_var)] <- 'value_NA'

      attributes(variable)$`discretized:breaks` <- cuts

    } else {
      new_var <- rep(NA, length = length(variable))

      intervals <- stringr::str_extract(discretized_intervals, "-Inf|Inf|\\d+\\.*\\d*")
      if(anyNA(intervals)) {
        if(is.na(intervals[[length(intervals)]])) {
          intervals[[length(intervals)]] <- Inf
        } else {
          stop('Incorrect invervals in the provided template.')
        }
      }

      int_from <- intervals[[1]]

      for(i in 2:length(discretized_intervals)) {
        int_to <- intervals[[i]]
        new_var[variable >= int_from & variable < int_to & !is.na(variable)] <- discretized_intervals[[i-1]]
        int_from <- int_to
      }
      if(anyNA(variable)) {
        new_var[is.na(variable)] <- 'value_NA'
      }
    }

    variable <- factor(new_var, levels = discretized_intervals)

  } else {
    variable <- arules::discretize(variable, breaks = breaks, infinity = TRUE)
    levels(variable) <- c(levels(variable), 'value_NA')
    variable[is.na(variable)] <- 'value_NA'
  }

  return(variable)
}

group_infrequent <- function(variable, unique_val = 10, verbose = TRUE) {

  ## Work on character: assigning a new label ('value_NA', 'value_other') into a
  ## factor yields NA instead, which silently dropped the missing-value group.
  ## A factor keeps its own level order; a character vector is sorted.
  level_order <- if(is.factor(variable)) levels(variable) else sort(unique(as.character(stats::na.omit(variable))))
  variable <- as.character(variable)

  tt <- table(variable) %>% as.data.frame() %>% arrange(desc(Freq))
  if(verbose && nrow(tt) > unique_val) warning("The number of unique variable values exceeds the number of unique values. In the result, grouping the infrequent values under the group 'value_other")
  variable[!variable %in% tt$variable[1:unique_val] & !is.na(variable)] <- 'value_other'

  if(verbose && anyNA(variable)) warning("There been missing values in the variable. They will be replaced with 'value_NA'")
  variable[is.na(variable)] <- 'value_NA'

  ## value_other and value_NA go last
  present <- unique(variable)
  specials <- c('value_other', 'value_NA')   # an already-binned input carries these in its levels
  variable <- factor(variable, levels = c(setdiff(intersect(level_order, present), specials),
                                          intersect(specials, present)))

  return(variable)
}
