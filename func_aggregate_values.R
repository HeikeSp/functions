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