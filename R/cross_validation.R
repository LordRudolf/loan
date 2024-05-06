
create_cv_folds <- function(target, X, resample_splits = NULL, resample_method = 'auto', application_created_at = NULL, 
                            rounded_vintages = TRUE, ...) {
  
  #commenting out this part as different ordering will mess up the vector values
  #if(!is.null(application_created_at)) {
  #  X <- X[order(application_created_at), ]
  #}
  
  n_minority_class <- min(table(target))
  computation_load <- nrow(X) * ncol(X) * 0.1
  
  if(n_minority_class < 50) warning(paste0('Number of observations in the minority class is only ', n_minority_class, '. In the result, you may get very unstalble scorecards'))
  
  ## auto
  if(resample_method == 'auto' && is.null(resample_splits)) {
    ## TO DO: use more scientific principles for choosing the optimal resampling method
    
    resample_method <- 'bootstrap'
    resample_splits <- calculate_resample_splits(computation_load, n_minority_class)
    
    if(computation_load > 5*10^8) resample_method <- 'validation_time_split'
  }
  
  ## vintage based
  if(resample_method == 'vintage_based' && is.null(resample_splits)) {
    if(is.null(application_created_at)) stop('application_created_at must be provided for using vintage-based resampling method')
    if(n_minority_class < 200) warning(paste0(
      'It is recommended having at least 200 minority class obsevartions for using vintage-based resampling method'))
    resample_splits <- ceiling(50/computation_load^0.1) #some very unscientific way how to deal with that
    resample_splits <- min(resample_splits, n_minority_class / 500)
    resample_splits <- max(resample_splits, 4)
  }
  
  if(resample_method == 'vintage_based') {
    if(rounded_vintages) { #ensure that one vintage starts and ends within specific month
      app_months <- format(application_created_at, format = '%Y-%m')
      vintages <- table(app_months, target) %>% #in 95% of the cases vintages will be months, not years or weeks
        as.data.frame.matrix()
      vintages$n_minority_class <- apply(vintages, 1, min, na.rm = TRUE)
      exp_obs_per_vintage <- n_minority_class / resample_splits
      i <- 1
      n_rows <- nrow(vintages)
      while(i < n_rows) {
        if(vintages$n_minority_class[[i]] < exp_obs_per_vintage * 0.5) {
          vintages[i+1, ] <- vintages[i+1, ] +  vintages[i, ]
          app_months[app_months == rownames(vintages)[i]] <- rownames(vintages)[i + 1]
          vintages <- vintages[-i, ]
          n_rows <- n_rows - 1
        } else {
          i <- i + 1
        }
      }
      if(vintages$n_minority_class[[i]] < exp_obs_per_vintage * 0.5) {
        vintages[i-1, ] <- vintages[i, ] +  vintages[i-1, ]
        app_months[app_months == rownames(vintages)[i]] <- rownames(vintages)[i - 1]
        vintages <- vintages[-i, ]
      }
      
      cv_folds <- lapply(rownames(vintages), function(x) which(app_months == x))
      names(cv_folds) <- rownames(vintages)
      
    } else {
      application_created_at_map <- order(application_created_at)
      names(application_created_at_map) <- 1:length(application_created_at)
      
      application_created_at_map <- sort(application_created_at_map)
      
      resample_splits_mapped <- split(application_created_at_map, sort(application_created_at_map %% resample_splits))
      
      cv_folds <- lapply(resample_splits_mapped, function(x) application_created_at[as.numeric(names(x))])
      names(cv_folds) <- lapply(resample_splits_mapped, function(x) min(application_created_at[application_created_at_map[x]]) %>%
                                  format(., format = '%Y-%m')) %>%
        unlist()
    }
    
    return(cv_folds)
  } #end of vintage based resample method
  
  if(resample_method == 'cv') {
    if(is.null(resample_splits)) {
      resample_splits <- calculate_resample_splits(computation_load, n_minority_class)
    }
    cv_folds <-  rsample::vfold_cv(X, v = resample_splits, ...) %>% pool_in_id()
    
  } else if(resample_method == 'repeatedcv') {
    if(!is.null(resample_splits) && !exists('repeats')) {
      n <- calculate_resample_splits(computation_load, n_minority_class)
      repeats <- max(min(ceiling(n / resample_splits), 50), 3)
    }
    if(is.null(resample_splits)) {
      n <- calculate_resample_splits(computation_load, n_minority_class)
      if(exists('repeats')) {
        resample_splits <- max(min(ceiling(n / repeats), 50), 3)
      } else {
        repeats <- min(floor(sqrt(n)) + 1,  50)
        resample_splits <- max(min(ceiling(n / repeats), 50), 3)
      }
      cv_folds <- rsample::vfold_cv(X, v = resample_splits, repeats = repeats, ...) %>% pool_in_id()
    }
    
  } else if (resample_method == 'validation') {
    cv_folds <- rsample::validation_split(X, ...) %>% pool_in_id
    
  } else if (resample_method == 'validation_time_split') {
    cv_folds <- rsample::validation_time_split(X, ...) %>% pool_in_id()
    
  } else if (resample_method == 'bootstrap') {
    if(is.null(resample_splits)) {
      resample_splits <- calculate_resample_splits(computation_load, n_minority_class)
    }
    
    cv_folds <- rsample::bootstraps(X, times = resample_splits, ...) %>% pool_in_id()
  }
  
  if(length(names(cv_folds)) == 0) {
    names(cv_folds) <- paste0('Fold_', 1:length(cv_folds))
  }
  
  return(cv_folds)
}

calculate_resample_splits <- function(computation_load, n_minority_class) {
  resample_splits <- ceiling(30/computation_load^0.1)
  resample_splits <- min(resample_splits, n_minority_class / 100)
  resample_splits <- max(resample_splits, 4)
  
  return(resample_splits)
}

pool_in_id <- function(resample_splits) {
  stopifnot(any(class(resample_splits) == 'rset'))
  
  lapply(resample_splits$splits, function(x) x$in_id)
}


#cv_folds <-  rsample::vfold_cv(df, v = 10)

#cv_folds <- caret::createFolds(df$target, k = 10)

#head(cv_folds)
