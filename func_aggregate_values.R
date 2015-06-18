#===============================================================================
# Name   : Aggregate normalized values using 3 factors
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================


func_agg_4fac <- function(normalized_values, trial_factors, factor1, factor2, factor3, factor4, function_name, analytes){
  agg_mean <- aggregate(normalized_values, 
                        by=list(trial_factors[,factor1], trial_factors[,factor2], trial_factors[,factor3], trial_factors[,factor4]),
                        function_name, na.rm=TRUE)
  colnames(agg_mean)[1:4] <- c(factor1, factor2, factor3, factor4)
  colnames(agg_mean)[5:ncol(agg_mean)] <- as.character(analytes$name_short)
  agg_mean_names <- interaction(agg_mean[,1:4])
  agg_mean2 <- cbind(agg_mean_names, agg_mean[,-(1:4)])
  colnames(agg_mean2)[1] <- "sample"
  print(agg_mean2[1:3,1:6])
  return(agg_mean2)  
}


func_agg_3fac <- function(normalized_values, trial_factors, factor1, factor2, factor3, function_name, analytes){
  agg_mean <- aggregate(normalized_values, 
                        by=list(trial_factors[,factor1], trial_factors[,factor2], trial_factors[,factor3]),
                        function_name, na.rm=TRUE)
  colnames(agg_mean)[1:3] <- c(factor1, factor2, factor3)
  colnames(agg_mean)[4:ncol(agg_mean)] <- as.character(analytes$name_short)
  agg_mean_names <- interaction(agg_mean[,1:3])
  agg_mean2 <- cbind(agg_mean_names, agg_mean[,-(1:3)])
  colnames(agg_mean2)[1] <- "sample"
  print(agg_mean2[1:3,1:5])
  return(agg_mean2)  
}


func_agg_2fac <- function(normalized_values, trial_factors, factor1, factor2, function_name, analytes){
  agg_mean <- aggregate(normalized_values, 
                        by=list(trial_factors[,factor1], trial_factors[,factor2]),
                        function_name, na.rm=TRUE)
  colnames(agg_mean)[1:2] <- c(factor1, factor2)
  colnames(agg_mean)[3:ncol(agg_mean)] <- as.character(analytes$name_short)
  agg_mean_names <- interaction(agg_mean[,1:2])
  agg_mean2 <- cbind(agg_mean_names, agg_mean[,-(1:2)])
  colnames(agg_mean2)[1] <- "sample"
  print(agg_mean2[1:3,1:5])
  return(agg_mean2)  
}


func_agg_1fac <- function(normalized_values, trial_factors, factor1, function_name, analytes){
  agg_mean <- aggregate(normalized_values, 
                        by=list(trial_factors[,factor1]),
                        function_name, na.rm=TRUE)
  colnames(agg_mean)[1] <- factor1
  colnames(agg_mean)[2:ncol(agg_mean)] <- as.character(analytes$name_short)
  agg_mean_names <- agg_mean[,1]
  agg_mean2 <- cbind(agg_mean_names, agg_mean[,-1])
  colnames(agg_mean2)[1] <- "sample"
  print(agg_mean2[1:3,1:5])
  return(agg_mean2)  
}




# function to combine mean and sd for two groups, e.g. cultivar and treatment

func_combine_mean_sd <- function(phenotypes, variable_name, 
                                 factor1 = "cultivar", factor2 = "treatment")
  {
  
  res_mean <- aggregate(phenotypes[ , variable_name],
                        by=list(phenotypes[ , factor1],
                                phenotypes[ , factor2]),
                        mean, na.rm=TRUE)
  
  res_sd <- aggregate(phenotypes[ , variable_name],
                        by=list(phenotypes[ , factor1],
                                phenotypes[ , factor2]),
                        sd, na.rm=TRUE)
  
  colnames(res_mean) <- c(factor1, factor2, "mean") 
  colnames(res_sd) <- c(factor1, factor2, "sd") 
  
  # convert treatment column into 2 columns (1 for control, 1 for stress)
  res_mean <- cast(res_mean, cultivar ~ treatment, value="mean")
  res_sd <- cast(res_sd, cultivar ~ treatment, value="sd")
  
  colnames(res_mean)[3] <- "drought_stress"
  colnames(res_sd)[3] <- "drought_stress"
  
  # combine mean and sd
  res_combined <- data.frame(cultivar = res_mean$cultivar,
                             mean_control = res_mean$control,
                             sd_control = res_sd$control,
                             mean_drought_stress = res_mean$drought_stress,
                             sd_drought_stress = res_sd$drought_stress)
  
  return(res_combined)
}