
func_calc_loocv <- function(field_data){
  error_cv <- c()
  
  for(acc in accessions_known){
    
    accessions_training_cv <- accessions_known[-which(acc==accessions_known)]
    accessions_testing_cv <- accessions_known[which(acc==accessions_known)]
    
    idx_train_cv <- which(field_data$accession %in% accessions_training_cv)
    idx_test_cv <- which(field_data$accession %in% accessions_testing_cv)
    
    train_cv <- field_data[idx_train_cv, model_colnames_factor]
    test_cv <- field_data[idx_test_cv, model_colnames_factor]
    
    colnames(train_cv) <- colnames(test_cv) <- model_colnames_short
    
    #regressionACC_factor_cv <- lm(LT50ACC ~ glc + frc + suc + raf + aa + mal + fum + pro + 
    #                                  factor(bbch) + factor(anthoscore), data = train_cv)
    
    regressionACC_factor_cv <- lm(LT50ACC ~ glc + frc + suc + raf + aa + mal + fum + pro + 
                                           bbch + anthoscore, data = train_cv)
    
    regressionACC_factor_cv$xlevels$`bbch` <- union(regressionACC_factor_cv$xlevels$`bbch`,
                                                           levels(test_cv$bbch))
    
    error_cv <- c(error_cv, 
                         sum(((predict(regressionACC_factor_cv, test_cv)) - 
                                (test_cv$LT50ACC))^2)/ sum((test_cv$LT50ACC)^2))
    
    # error_cv <- c(error_cv,
    #                      mean(abs(predict(regressionACC_factor_cv, test_cv) - (test_cv$LT50ACC))) )
  }
  
  names(error_cv) <- accessions_known
  
  return(error_cv)
}




func_calc_loocv_caret <- function(field_data = field4, 
                                  method_val = "glmnet", 
                                  eGrid_val = eGrid){
  error_cv <- c()
  pred_cv <- c()
  
  for(acc in accessions_known){
    
    accessions_training_cv <- accessions_known[-which(acc==accessions_known)]
    accessions_testing_cv <- accessions_known[which(acc==accessions_known)]
    
    idx_train_cv <- which(field_data$accession %in% accessions_training_cv)
    idx_test_cv <- which(field_data$accession %in% accessions_testing_cv)
    
    x_train_cv <- field_data[idx_train_cv, model_colnames[6:15]]
    x_test_cv <- field_data[idx_test_cv, model_colnames[6:15]]
    y_train_cv <- field_data[idx_train_cv, "LT50ACC"]
    y_test_cv <- field_data[idx_test_cv, "LT50ACC"]
    
    fit_caret_cv <- train(x = x_train_cv, 
                          y = y_train_cv,
                          method = method_val,
                          tuneGrid = eGrid_val,
                          trControl = Control)
    
    pred_cv <- c(pred_cv, 
                 predict(fit_caret_cv, newdata = x_test_cv))

    
  }
  
  names(pred_cv) <- field_data$accession[which(field_data$accession %in% accessions_known)]
  
  return(pred_cv)
}