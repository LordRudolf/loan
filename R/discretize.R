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
      ## left-closed [a, b), as arules::discretize() built the template's bins --
      ## right = TRUE moved every value lying on a cut point into the next bin
      new_var <- cut(variable, breaks = cuts, include.lowest = TRUE, right = FALSE, labels = labels_no_nas)
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

## A genuine value spelled like a label the package is about to create
## (`reserved`) is renamed make.unique() style, with a message, so the two stay
## distinguishable. Only an actual collision renames. Missing values stay NA.
rename_reserved <- function(variable, reserved = character()) {
  level_order <- if(is.factor(variable)) levels(variable) else sort(unique(as.character(stats::na.omit(variable))))
  level_order <- level_order[!is.na(level_order)]
  renamed <- utils::tail(make.unique(c(reserved, level_order), sep = '_'), length(level_order))
  collisions <- level_order != renamed
  if(any(collisions)) {
    message('Renamed reserved variable value(s): ',
            paste0('`', level_order[collisions], '` -> `', renamed[collisions], '`',
                   collapse = ', '), '.')
  }
  factor(renamed[match(as.character(variable), level_order)], levels = renamed)
}

## Missing values become the `value_NA` group (a genuine `value_NA` is renamed
## first). Used by the contingency workhorse -- the one place this happens --
## and by plot grouping.
nominal_groups <- function(variable) {
  variable <- rename_reserved(variable, if(anyNA(variable)) 'value_NA')
  if(!anyNA(variable)) return(variable)
  factor(ifelse(is.na(variable), 'value_NA', as.character(variable)),
         levels = c(levels(variable), 'value_NA'))
}

## Merge values beyond the `unique_val` most frequent into `value_other`.
## Missing values stay NA: the contingency workhorse makes them `value_NA`.
group_infrequent <- function(variable, unique_val = 10, verbose = TRUE) {
  ## `value_other` is created only when there are more values than `unique_val`
  ## (the package's own `value_other` does not count, so re-grouping is a no-op)
  n_values <- length(setdiff(unique(as.character(stats::na.omit(variable))), 'value_other'))
  variable <- rename_reserved(variable, if(n_values > unique_val) 'value_other')
  level_order <- levels(variable)
  variable <- as.character(variable)   # a factor cannot take the new label 'value_other'

  tt <- table(variable) %>% as.data.frame() %>% arrange(desc(Freq))
  if(verbose && nrow(tt) > unique_val) warning("The number of unique variable values exceeds the number of unique values. In the result, grouping the infrequent values under the group 'value_other")
  variable[!is.na(variable) & !variable %in% utils::head(tt[[1L]], unique_val)] <- 'value_other'

  present <- unique(stats::na.omit(variable))
  factor(variable, levels = c(setdiff(intersect(level_order, present), 'value_other'),
                              intersect('value_other', present)))
}
