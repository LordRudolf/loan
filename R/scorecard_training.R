fit_model <- function(rec, data, method = 'ranger', ..., ml_framework = 'caret', importance = 'impurity') {

  args <- list(...)

  if(ml_framework == 'caret') {

    if(is.null(args$train_control)) {

      if(!is.null(args$tuneLength) | !is.null(args$tuneGrid)) {
        if(!is.null(args$tuneLength)) {
          len <- args$tuneLength
        } else if(!is.null(args$tuneGrid)) {
          len <- args$tuneGrid
        }

        n_minority_class <- min(table(data$target))
        computation_load <- nrow(X) * ncol(X) * len * 3 #manual increase by 3 assuming there will be nested-cv

        resample_splits <- suggest_resample_splits(computation_load, n_minority_class)
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
      data = data,
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
