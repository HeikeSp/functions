#===============================================================================
# Name   : Calculate log2FC
# Author : Heike Sprenger
# Date   : 2014-08-13
# Version: 0.1
#===============================================================================

# subset based on sample_time!
func_log2FC <- function(normalized_values, trial_factors, sample_time, average_func = median){
  
  subset1 <- subset(normalized_values, trial_factors[,"sample_time"] == sample_time & trial_factors[,"treatment"] == "control")
  subset2 <- subset(normalized_values, trial_factors[,"sample_time"] == sample_time & trial_factors[,"treatment"] == "drought stress")
  
  res_log2FC <- rep("NA", ncol(normalized_values))
  
  for (i in 1:ncol(normalized_values)) {
    res_log2FC[i] <- log2( average_func(subset2[,i], na.rm=TRUE) / average_func(subset1[,i], na.rm=TRUE) )
  }
  
  res_log2FC <- as.numeric(res_log2FC)
  
  return(res_log2FC)
}

#############################################################

# subset based on ONE factor
func_log2FC_1fac <- function(normalized_values, trial_factors, average_func = median){
  
  subset1 <- subset(normalized_values, trial_factors[,"treatment"] == "control")
  subset2 <- subset(normalized_values, trial_factors[,"treatment"] == "drought stress")
  
  res_log2FC <- rep("NA", ncol(normalized_values))
  
  for (i in 1:ncol(normalized_values)) {
    res_log2FC[i] <- log2( average_func(subset2[,i], na.rm=TRUE) / average_func(subset1[,i], na.rm=TRUE) )
  }
  
  res_log2FC <- as.numeric(res_log2FC)
  
  return(res_log2FC)
}

#############################################################

# subset based on TWO factors
func_log2FC_2fac <- function(normalized_values, trial_factors, average_func = median, fac1, fac2, fac1_val, fac2_val1, fac2_val2){
  
  subset1 <- subset(normalized_values, trial_factors[,fac1] == fac1_val & trial_factors[,fac2] == fac2_val1)
  subset2 <- subset(normalized_values, trial_factors[,fac1] == fac1_val & trial_factors[,fac2] == fac2_val2)
  
  res_log2FC <- rep("NA", ncol(normalized_values))
  
  for (i in 1:ncol(normalized_values)) {
    res_log2FC[i] <- log2( average_func(subset2[,i], na.rm=TRUE) / average_func(subset1[,i], na.rm=TRUE) )
  }
  
  res_log2FC <- as.numeric(res_log2FC)
  
  return(res_log2FC)
}

#############################################################

# function to give back a list of up/down regulated metabolites
func_log2FC_dir <- function(res_log2FC){
  
  res_up <- which(res_log2FC > 0)
  res_down <- which(res_log2FC < 0)
  res_dir_list <- list(up = res_up, down = res_down)
  
  print(lapply(res_dir_list, length))
  
  return(res_dir_list)
}

# function to give back a vector with up or down as value
func_log2FC_up_down <- function(res_log2FC){
  
  res_up_down <- rep(NA, length(res_log2FC))
  res_up_down[which(res_log2FC > 0)] <- "up"
  res_up_down[which(res_log2FC < 0)] <- "down"
  
  res_up_down <- as.factor(res_up_down)
  
  return(res_up_down)
}

#############################################################

# function to give back a list of SIGIFICANTLY up/down regulated metabolites
func_log2FC_dir_sig <- function(res_dir_list, res_ttest, threshold){
  
  # significantly up-regulated metabolites: intersect between
  res_up_sig <- intersect(res_dir_list$up, which(res_ttest < threshold) )
  
  # significantly down-regulated metabolites
  res_down_sig <- intersect(res_dir_list$down, which(res_ttest < threshold) )
  
  res_dir_sig_list <- list(up = res_up_sig, down = res_down_sig)
  
  print(lapply(res_dir_sig_list, length))
  
  return(res_dir_sig_list)
}

  