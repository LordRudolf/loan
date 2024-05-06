train_model <- function(rec, df, method = 'ranger', ..., ml_framework = 'caret', importance = 'impurity') {
  
  args <- list(...)
  
  if(ml_framework == 'caret') {
    
    if(is.null(args$train_control)) {
      
      if(!is.null(args$tuneLength) | !is.null(args$tuneGrid)) {
        if(!is.null(args$tuneLength)) {
          len <- args$tuneLength
        } else if(!is.null(args$tuneGrid)) {
          len <- args$tuneGrid
        }
        
        n_minority_class <- min(table(df$target))
        computation_load <- nrow(X) * ncol(X) * len * 3 #manual increase by 3 assuming there will be nested-cv
        
        resample_splits <- calculate_resample_splits(computation_load, n_minority_class)
        caret_resample_method <- 'boot'
        #if(computation_load > 5*10^8) resample_method <- 'validation_time_split'
        ## TO DO: validation set for caret
        
      } else {
        caret_resample_method <- 'none'
        resample_splits <- 1
      }
      train_control <- caret::trainControl(
        number = resample_splits,
        method = caret_resample_method,
        summaryFunction = caret::twoClassSummary,
        classProbs = TRUE
      )
    }
    model <- caret::train(
      rec,
      data = df,
      method = method,
      trControl = train_control,
      metric = 'roc',
      importance = importance,
      ...
    )
  }
  
  ## TO DO: add tidymodels framework
  
  ## TO DO: add base R framework (for GLM)
  
  return(model)
}
#model <- train_model(rec,cbind(data.frame(target = target[index_out]), X[index_out, ]), tuneLength = 4, method = 'glmnet')

train_scorecard <- function(df, ..., 
                            method = 'glm',
                            preproc = 'auto',
                            cross_val = TRUE,
                            cv_folds = NA) {
  args <- list(...)
  
  ## Getting vars we need
  ## If formula provided
  if (any(sapply(args, inherits, 'formula'))) {
    formul <- list(args[[sapply(args, inherits, 'formula')]])[[1]]
    
    target <- df[, all.vars(formul)[1] ]
    
    if( all.vars(formul)[2] == '.') {
      X <- df[, colnames(df) != all.vars(formul)[1] ]
    } else {
      X <- df[, all.vars(formul)[-1]]
    }
  }
  
  func_vars <- variables_availability_check(df, required_vectors = c(
    'target'), ..., optional_vectors = c('application_status', 'application_created_at'),
    detect_unused_cols = TRUE)
  
  if(!exists('formul')) {
    target <- func_vars$target
  }
  
  if(!is.null(func_vars$application_status)) {
    application_status <- func_vars$application_status
  } else {
    application_status <- ifelse(is.na(target), 'REJECTED', 'ISSUED')
  }
  if(!is.null(func_vars$application_created_at)) {
    application_created_at <- func_vars$application_created_at
  } else {
    application_created_at <- NULL
  }
  
  if(!exists('formul')) {
    if(!is.null(attributes(df)$predictors)) {
      X <- df[, attributes(df)$predictors]
    } else {
      X <- df[, colnames(df) %in% names(func_vars$unused_cols)]
    }
  }
  ## End of getting vars we need

  
  if(cv_folds == 'auto' && class(cv_folds) == 'character' && length(cv_folds) == 1) {
    cv_folds <- create_cv_folds(target, X)
  }
  
  ########################
  ## Training here
  
  cv_res_stats <- list()
  
  ## preparation for the model training
  if(preproc == 'auto' && class(preproc) == 'character' && length(preproc) == 1) {
    rec <- autopreproc(target, X, method = method)
  }
  
  for(i in 1:length(cv_folds)) {
    ## TO DO: implement parallel computing here
    index_in_all <-  cv_folds[[i]]
    index_in <- index_in_all[!is.na(target[index_in_all])]
    
    index_out_all <- (1:nrow(X))[-index_in]
    index_out <- index_out_all[!is.na(target[index_out_all])]
    
    model <- train_model(rec, )
    model <- caret::train(
      rec,
      data = cbind(data.frame(target = target[index_in]), X[index_in, ]),
      method = 'glm',
      trControl = caret::trainControl(
        method = 'none',
        summaryFunction = caret::twoClassSummary
      ),
      metric = 'roc'
    )
    
    preds <- predict(model, X[index_out, ], type = 'probs')$BAD
    auroc(preds, target[index_out] == 'BAD')
  }
}

#train_scorecard(df = df, marriage_status  ~ gross_salary  + education_level  )
#train_scorecard(df)
#train_scorecard(df = as_tibble(df), target =  'marriage_status')




train_scorecard_glm <- function(target, X) {
  if(anyNA(X)) stop('There are missing values in the train dataset.')
  
  target_num <- as.numeric(target == 'Bad')
  
  model <- glm.fit(x = X[, c('principal_disbursed', 'education_level', 'open_loans')], y = target, family = binomial())
}

predict.glm.loan_df <- function(obj, newdata) {
  
}