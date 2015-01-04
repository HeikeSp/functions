
func_corr_test <- function(matrix, factors, fac1, val1, threshold){
  subset1 <- subset(matrix, factors[,fac1] == val1)
  colnames(subset1) <- analytes_sel_exp_sort$name_short
  
  library(psych)
  res_corr <- corr.test(subset1, adjust="BH")
  
  print(length(which(res_corr$p < threshold)))
  
  res_corr_combined <- res_corr$r
  res_corr_combined[upper.tri(res_corr_combined)] <- res_corr$p[upper.tri(res_corr$p)]
  
  return(res_corr_combined)
  
}



func_corr_test_2fac <- function(matrix, factors, fac1, val1, fac2, val2, threshold){
  subset1 <- subset(matrix, factors[,fac1] == val1 & factors[,fac2] == val2)
  colnames(subset1) <- analytes_sel_exp_sort$name_short
  
  library(psych)
  res_corr <- corr.test(subset1, adjust="BH")
  
  print(length(which(res_corr$p < threshold)))
  
  res_corr_combined <- res_corr$r
  res_corr_combined[upper.tri(res_corr_combined)] <- res_corr$p[upper.tri(res_corr$p)]
  
  return(res_corr_combined)
  
}