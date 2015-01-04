func_replace_outlier <- function(values, threshold, original_values, output="original"){
  idx_outlier_col = 1
  
  while(length(idx_outlier_col)>0){
    
    outlier_pvalue <- rep(NA, ncol(values))
    outlier_name <- rep(NA, ncol(values))
    
    for (i in 1:ncol(values)){
      outlier_pvalue[i] <- find_one_outlier(values[,i], output="p.value")
      outlier_name[i] <- find_one_outlier(values[,i], output="name")
    }
    
    idx_outlier_col <- which(outlier_pvalue < threshold)
    
    # replace outliers by NA
    for (i in idx_outlier_col){
      values[outlier_name[i], i] <- NA
    }
    
    for (i in idx_outlier_col){
      original_values[outlier_name[i], i] <- NA
    }
    
  }
  
  if (output=="original")
    return(original_values)
  else 
    return(values)
}
