#===============================================================================
# Name   : Perform t-test 
# Author : Heike Sprenger
# Date   : 2014-08-12
# Version: 0.1
#===============================================================================

func_ttest_treatment <- function(normalized_values, trial_factors, fac1, val1, threshold, 
                                 contrast_fac="treatment", contrast_val1="control", contrast_val2="drought stress"){

  subset1 <- subset(normalized_values, trial_factors[ ,fac1] == val1 & trial_factors[,contrast_fac] == contrast_val1)
  subset2 <- subset(normalized_values, trial_factors[ ,fac1] == val1 & trial_factors[,contrast_fac] == contrast_val2)

  res_ttest <- rep("NA", ncol(normalized_values))

  for (i in 1:ncol(normalized_values)) {
    
    if (sum(is.na(subset1[,i])) > length(subset1[,i]) - 2)
      res_ttest[i] <- NA
    
    else if (sum(is.na(subset2[,i])) > length(subset1[,i]) - 2)
      res_ttest[i] <- NA
    
    else
      res_ttest[i] <- t.test(subset1[,i], subset2[,i])$p.value
  }
  
  res_ttest <- as.numeric(res_ttest)
  #names(res_ttest) <- analytes
  
  # p-value adjustment
  res_ttest_adj <- p.adjust(res_ttest, method="BH")
  
  print(paste("number of analytes with signifcant difference \n between", contrast_val1, "and", 
              contrast_val2, "for", val1, "samples, threshold:", threshold))
  print(length(which(res_ttest_adj < threshold)))

  return(res_ttest_adj)
}

#######################################################

func_ttest_treatment_all <- function(normalized_values, trial_factors, threshold){
  
  subset1 <- subset(normalized_values, trial_factors[,"treatment"] == "control")
  subset2 <- subset(normalized_values, trial_factors[,"treatment"] == "drought stress")
  
  res_ttest <- rep("NA", ncol(normalized_values))
  
  for (i in 1:ncol(normalized_values)) {
    
    if (sum(is.na(subset1[,i])) > length(subset1[,i]) - 2)
      res_ttest[i] <- NA
    
    else if (sum(is.na(subset2[,i])) > length(subset1[,i]) - 2)
    res_ttest[i] <- NA
    
    else
      res_ttest[i] <- t.test(subset1[,i], subset2[,i])$p.value
  }
  
  res_ttest <- as.numeric(res_ttest)
  #names(res_ttest) <- analytes
  
  # p-value adjustment
  res_ttest_adj <- p.adjust(res_ttest, method="BH")
  
  print(paste("number of analytes with signifcant difference \n between control and stress at timepoint, threshold:", threshold))
  print(length(which(res_ttest_adj < threshold)))
  
  return(res_ttest_adj)
}