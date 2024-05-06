detect_binary_outcome <- function(target_var) {
  tabl <- table(target_var, exclude = NA)
  unique_classes <- length(tabl)
  tabl_names <- names(tabl)
  
  possible_good <- stringr::str_detect(tolower(tabl_names),
                                       'good|paid')
  possible_bad <- stringr::str_detect(tolower(tabl_names),
                                      'bad|default')
  
  if(length(unique(na.omit(target_var))) == 2) {
    if(sum(possible_good) == 2 | sum(possible_bad) == 2) stop('Ambigious target class names')
    if(sum(possible_good) == 1) {
      target_good_name <- tabl_names[possible_good]
    } else if(sum(possible_bad) == 1) {
      target_good_name <- tabl_names[!possible_bad]
    } else {
      target_good_name <- names(tabl)[which.max(tabl)]
      warning(paste0('Assigning the `GOOD` target class to the majority of target clases currently named as ',
                     target_good_name))
    }
    
    target <- as.character(target_var)
    target[!is.na(target_var) & as.character(target_var) == target_good_name] <- 'GOOD'
    target[!is.na(target_var) & as.character(target) != 'GOOD'] <- 'BAD'
  } else {
    stop('More then two diffent classes detected')
  }
  
  return(as.factor(target))
}

detect_application_status <- function(application_status_var) {
  #tabl <- table(application_status_var, exclude = NA)
  
  application_status <- application_status_var
  
  ## TO DO: all of this
  return(application_status)
}