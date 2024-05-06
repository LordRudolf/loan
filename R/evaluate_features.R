
evaluate_features <- function(df, 
                              new_features, 
                              cv_folds = NA,
                              method = NA,
                              target = NA,
                              ...) {
  
  ## validating availability of dependent and independent variables
  if(!is.na(target)) {
    if(any(class(df) == 'loan')) print('Warning: using the user-defined target variable')
    
    if(length(target) == 1 & typeof(target) == 'character') {
      count_occurrences <- sum(target == colnames(df))
      
      if(count_occurrences < 0) stop('The name of the dependent variable has not been found in the dataset `df`')
      if(count_occurrences > 1) stop(paste0('Has been found multiple columns named ', target, '. There
                                            can be only one dependent variable'))
      X <- df
      X[[target]] <- NULL
      target <- df[[target]]
      #X <- X[, colnames(X) %in% attributes(X)$predictors]
    } else if (length(target) == nrow(df)) {
      X <- df
    } else {
      stop('The target variable shall be length 1 or equal to the observations in the dataset `df`')
    }
    
    ## TO DO: check the type of the dependent variable
    ## TO DO: different evaluation stats for different types
    
  } else if((any(class(df) == 'loan')))  {
    target <- df$target
    X <- df[, attributes(df)$predictors] ## TO DO: select only features
  } else {
    stop('Provide name or vector of the dependent variable `y`, or convert the dataset `df` to class `loan`')
  }
  dfx <- as_tibble(X)######### !!!!!!! Change this once alias var problem has been fixed
  dfx$target <- assign('target', target)
  dfx <- dfx[!is.na(dfx$target), ] #TO DO: keep missing values for the reject inference analysis
  
  ## validating features
  ## TO DO: option providing features list
  stopifnot(typeof(new_features) == 'character')
  count_occurrences <- sum(new_features %in% colnames(X))
  if(count_occurrences == 0) stop('None of the named new features has been found in the dataset')
  if(count_occurrences < length(new_features)) {
    missing_features <- setdiff(new_features, colnames(X))
    print('The following names of the features has not been found in the dataset: ')
    print(paste(missing_features, collapse = ', '))
  }
  
  train_set_list <- list()
  train_set_list$vanilla <- setdiff(colnames(X), new_features)
  train_set_list$all_variables <- colnames(X)
  
  ## validating cv folds
  if(is.na(cv_folds)) {
    
    cv_folds <- create_cv_folds(dfx$target, dfx[, colnames(dfx) != 'target'])
    
    #folds_vector <- unlist(cv_folds, recursive = FALSE) %>% sort() %>% names()
    #folds_vector <- substr(folds_vector, 1, 5)
  }
  
  ## validating train method
  if(is.na(method)) {
    method <- 'ranger'
    ml_framework <- 'caret'
  }

  ## The actual cross-validation is happening here
  cv_preds <- as.data.frame(matrix(NA, nrow(dfx), 1+length(train_set_list)))
  names(cv_preds) <- c('target', paste0(names(train_set_list)))
  cv_preds$target <- dfx$target

  model_stats <- as.data.frame(matrix(NA, length(train_set_list), length(cv_folds)))
  rownames(model_stats) <- names(train_set_list)
  colnames(model_stats) <- names(cv_folds)
  
  variable_importances <- data.frame(variable = colnames(X))
  #variable_importances_temp 

  for(i in 1:length(train_set_list)) {
    
    rec <- autopreproc(dfx$target, dfx[, train_set_list[[i]]], method = method)
    
    all_obs <- (matrix(NA, nrow(dfx), length(cv_folds)))
    
    variable_importances_temp <- data.frame(variable = colnames(dfx)[-ncol(dfx)])[, 1, drop=FALSE]
    for(f in 1:length(cv_folds)) {
      test_samp <<- dfx[-cv_folds[[f]], ]
      
      train_samp <<- dfx[cv_folds[[f]], ]
      
      model <- train_model(rec, train_samp, method = method, ml_framework = ml_framework)
      print('x1')
     
      pred <- predict(model, test_samp, type = 'prob')$BAD
      print('x2')
      model_stats[i,f] <- auroc(pred, test_samp$target == 'BAD')
      print('x3')
      print(model_stats)
      print('x4')
      all_obs[-cv_folds[[f]], f] <- pred ##error here
      print('x5')
      
      imps <- caret::varImp(model)$importance 
      imps$variable <- rownames(imps)
      variable_importances_temp <- merge(variable_importances_temp, imps, by = 'variable', all.x = TRUE)
      names(variable_importances_temp)[f + 1] <- names(cv_folds)[f]
      
      #cv_preds[cv_preds$cv_fold == f, names(train_set_list)[i]] <- pred
    }
    
    cv_preds[,i + 1] <- apply(all_obs, 1, mean, na.rm = TRUE)
    
    variable_importances_temp$avg_imp <- apply(variable_importances_temp[, 2:ncol(variable_importances_temp)], 1, mean, na.rm = TRUE)
    variable_importances <- merge(variable_importances, variable_importances_temp[, c(1, ncol(variable_importances_temp))], by = 'variable', all.x = TRUE)
    names(variable_importances)[ncol(variable_importances)] <- names(train_set_list)[i]
  }
  
  output <- list(
    model_stats = model_stats,
    cv_preds = cv_preds,
    variable_importances = variable_importances
  )
  
  return(output)
}


visualize_paired_u_test <- function(output) {
  model_stats <- output$model_stats
  model_stats$variable_set <- rownames(model_stats)
  var_order <- model_stats$variable_set
  
  model_stats <- reshape2::melt(model_stats, id.vars = 'variable_set')
  model_stats$variable_set <- factor( model_stats$variable_set, 
                                      levels = var_order)
  colnames(model_stats)[2] <- 'cv_fold'
  colnames(model_stats)[3] <- 'AUC'
  
  ## TO DO: replace t-test with ANOVA and Tukey if there are multiple comparisons
  pval_t <- t.test(model_stats$AUC[model_stats$variable_set == 'vanilla'], 
                   model_stats$AUC[model_stats$variable_set != 'vanilla'],
                   paired  = TRUE)$p.value %>% round(4)
  pval_w <- wilcox.test(model_stats$AUC[model_stats$variable_set == 'vanilla'], 
                   model_stats$AUC[model_stats$variable_set != 'vanilla'],
                   paired  = TRUE)$p.value %>% round(4)
  
  ggplot(model_stats, aes(x = variable_set, y = AUC, group = cv_fold)) +
    geom_point() +
    geom_line() +
    stat_summary(inherit.aes = F,aes(variable_set,AUC),
                 geom = "point", fun = "median", col = "red", 
                 size = 3, shape = 24,fill = "red"
    ) +
    stat_summary(inherit.aes = F,aes(variable_set,AUC),
                 geom = "point", fun = "mean", col = "#FF9999", 
                 size = 3, shape = 20,fill = "#FF9999"
    ) +
    annotate("text", x = 1.5, y = max(model_stats$AUC)+sd(model_stats$AUC)/4 , label = paste0('Paired t-test P-Value: ', pval_t, '\n',
                                                     'Paired wilcox-test P-Value: ', pval_w)) +
    theme_minimal()
}

visualize_variable_importance <- function(output) {
  variable_importances <- output$variable_importances
  variable_importances$spec_vars <- is.na(variable_importances$vanilla)
  variable_importances$var_global_importance <- apply(variable_importances[, -1], 1, max, na.rm = TRUE)
  variable_importances <- variable_importances[order(-variable_importances$var_global_importance), ]
  
  imps <- reshape2::melt(variable_importances[1:min(10, nrow(variable_importances)),
                                              -ncol(variable_importances)], id.vars = c('variable', 'spec_vars'))
  colnames(imps)[3:4] <- c('variable_set', 'variable_importance')
  
  imps$labels <- ifelse(imps$variable_set == 'vanilla', imps$variable, ' ')
  imps$spec_labels <- ifelse(imps$spec_vars, imps$variable, '')
  
  ggplot(imps, aes(x = variable_set, y = variable_importance, group = variable,
                   label = labels)) +
    geom_point() +
    ggrepel::geom_label_repel() +
    ggrepel::geom_label_repel(aes(label = spec_labels), color = 'red') +
    geom_line() +
    theme_minimal()
}

visualize_worth <- function(output, worth_good = 100, worth_bad = -100, variable_set = NA) {
  cv_preds <- output$cv_preds
  
  if(is.na(variable_set)) {
    variable_set <- colnames(cv_preds)[3]
  }
  cv_preds$new_preds <- cv_preds[[variable_set]]
  
  cv_preds <- cv_preds[order(cv_preds$vanilla), ]
  cv_preds$cum_goods_vanilla <- cumsum(cv_preds$target == 'GOOD')
  cv_preds$cum_bads_vanilla <- cumsum(cv_preds$target == 'BAD')
  cv_preds$bad_rate_vanilla <- cv_preds$cum_bads_vanilla / (cv_preds$cum_bads_vanilla + cv_preds$cum_goods_vanilla)
  cv_preds$profit_vanilla <- cv_preds$cum_goods_vanilla * worth_good + cv_preds$cum_bads_vanilla * worth_bad
  cv_preds$acceptance_rate_vanilla <- (1:nrow(cv_preds))/nrow(cv_preds)
  
  cv_preds <- cv_preds[order(cv_preds$new_preds), ]
  cv_preds$cum_goods <- cumsum(cv_preds$target == 'GOOD')
  cv_preds$cum_bads <- cumsum(cv_preds$target == 'BAD')
  cv_preds$bad_rate <- cv_preds$cum_bads / (cv_preds$cum_bads +  cv_preds$cum_goods)
  cv_preds$profit <- cv_preds$cum_goods * worth_good + cv_preds$cum_bads * worth_bad
  cv_preds$acceptance_rate <- (1:nrow(cv_preds))/nrow(cv_preds)
  
  profit_compare <- data.frame(
    acceptance_rate = cv_preds$acceptance_rate,
    number_of_accepted_cases = 1:nrow(cv_preds),
    bad_rate = cv_preds$bad_rate,
    profit = cv_preds$profit
  )
  
  profit_compare$bad_rate_vanilla <- cv_preds$bad_rate_vanilla[order(cv_preds$vanilla)]
  profit_compare$profit_vanilla <- cv_preds$profit_vanilla[order(cv_preds$vanilla)]
  
  profit_compare$profit_increase <- profit_compare$profit - profit_compare$profit_vanilla
  profit_compare$profit_increase_per_loan <- profit_compare$profit_increase / profit_compare$number_of_accepted_cases
  profit_compare$profit_increase_per_application <- profit_compare$profit_increase / nrow(profit_compare)
  
  
  profit_compare$bad_rate_increase <- profit_compare$bad_rate - profit_compare$bad_rate_vanilla
  
  ggplot(profit_compare[profit_compare$number_of_accepted_cases > 10, ], 
         aes(x = acceptance_rate, y = profit_increase_per_application)) +
    geom_smooth(method="loess", span=1) + ##TO DO: use confidence bands instead of interval
    geom_hline(yintercept = 0, color = 'red', size = 1) +
    geom_jitter(alpha = 0.15) +
    theme_minimal()
}


#GermanCredit$Class <- ifelse(GermanCredit$Class == 'Bad', 'BAD', 'GOOD')
#output <- evaluate_features(GermanCredit, new_features = 'Amount', target = 'Class')
#visualize_paired_u_test(output)
#visualize_variable_importance(output)
#visualize_worth(output, 10, -25)
