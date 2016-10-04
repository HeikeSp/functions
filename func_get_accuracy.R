

func_get_accuracy <- function(model, predictors, trials){
  
  res_accuracy <- rep(NA, length(trials))
  
  for (i in trials){
    j <- which(trials == i)
    
    input <- get_subset_breeder2(trial_name = i)
    info <- get_subset_breeder(trial_name = i)
    
    res_accuracy[j] <- confusionMatrix(table(predict(model, input[ ,predictors]), info[,"tol_cat3_fve"]))$overall[1]
    names(res_accuracy)[j] <- trials[j]
  }
  
  return(res_accuracy)
}