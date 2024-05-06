autopreproc <- function(target, X, method) {
  
  ## TO DO: data type checks
  
  rec_base <- recipes::recipe(target ~ ., cbind(target, X) %>% dplyr::filter(!is.na(target))) %>%
    recipes::step_zv(recipes::all_numeric(), recipes::all_nominal_predictors()) %>%
    recipes::step_factor2string(recipes::all_nominal_predictors()) #if there are factor then we convert them to string to be able easily adjust them
  
  if(method %in% c('glmnet', 'glm')) {
    rec <- rec_base %>%
      recipes::step_other(recipes::all_nominal_predictors(), other = '_OTHER_VALUE') %>%
      recipes::step_novel(recipes::all_nominal_predictors(), new_level = '_NOVEL_VALUE') %>%
      recipes::step_unknown(recipes::all_nominal_predictors(), new_level = '_NA_VALUE') %>%
      recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal_predictors()) %>%
      recipes::step_corr(recipes::all_numeric(), threshold = 0.75) %>%
      recipes::step_impute_median(recipes::all_numeric()) %>% ## TO DO: median impute + add dummy columns that shows whether value is missing
      recipes::step_nzv(recipes::all_numeric())
      
  }
  
  if(method %in% 'ranger') {
    rec <- rec_base %>%
      recipes::step_other(recipes::all_nominal_predictors(), other = '_OTHER_VALUE') %>%
      recipes::step_novel(recipes::all_nominal_predictors(), new_level = '_NOVEL_VALUE') %>%
      recipes::step_unknown(recipes::all_nominal_predictors(), new_level = '_NA_VALUE') %>%
      recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
      recipes::step_corr(recipes::all_numeric(), threshold = 0.95) %>%
      recipes::step_impute_median(recipes::all_numeric()) ## TO DO: extreme value imputation
  }
  
  return(rec)
}


## TO DO: add the following recipes steps:
# * median + dummy columns imputation (recipes::step_indicate_na() (???))
# * extreme value imputation
# * WEO
# * syntetic valid names for all nominal variables
