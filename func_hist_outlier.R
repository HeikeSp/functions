func_hist_outlier <- function(values, threshold){
  
  outlier_pvalue <- rep(NA, ncol(values))
  outlier_name <- rep(NA, ncol(values))
  
  for (i in 1:ncol(values)){
    outlier_pvalue[i] <- find_one_outlier(values[,i], output="p.value")
    outlier_name[i] <- find_one_outlier(values[,i], output="name")
  }
  
  idx_outlier_col <- which(outlier_pvalue < threshold)
    
  
  for (i in idx_outlier_col){
    hist(values[,i], breaks=20, col="grey",
         main=paste(colnames(values)[i], 
                    "\n p-value grubbs test: ",
                    outlier_pvalue[i]))
  }
  
}