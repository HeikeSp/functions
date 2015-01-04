#===============================================================================
# Name   : Calculate, print and plot CV values per replicate group
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

# define Cv function, option na.rm = TRUE is necessary to avoid NAs as CV value!
Cv <- function (x) { sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE) } 

# calculate cv values per replicate group (defined using e.g. treatment, sample_time and cultivar)
func_calculate_cv <- function(normalized_values, trial_factors, all_factors, keep_factors, IA_factors)
{
  cv_values <- matrix(1, nrow=length(levels(interaction(trial_factors[,IA_factors]))), 
                      ncol=ncol(normalized_values))
  for (i in 1:ncol(normalized_values))
  {
    cv_values[,i] <- sapply( split(
      RemoveFactors(y=normalized_values[,i], sam=trial_factors, facs=all_factors, keep=keep_factors, output="y_norm"), 
      interaction(trial_factors[,IA_factors]))
      ,Cv)
  }
  print("dimensions of resulting matrix")
  print(dim(cv_values))
  colnames(cv_values) <- colnames(normalized_values)
  rownames(cv_values) <- levels(interaction(trial_factors[,IA_factors]))
  hist(cv_values, breaks=400)
  return(cv_values)
}

###########################################################################

# print replicate groups with CV > threshold (e.g. 0.25)
func_print_cv <- function(cv_values, threshold){
  print(paste("number of replicate groups with CV >", threshold))
  print(length(which(cv_values > threshold)))
  
  print(paste("replicate groups with CV >",threshold))
  
  for (i in 1:ncol(cv_values)){
    if(sum(cv_values[,i] > threshold, na.rm=T)>0){
      print(cbind(i, which(cv_values[,i] > threshold)))  
    }
  }
}


# plot histogram of cv values per analyte
func_plot_cv <-  function(cv_values){
  for (i in 1:ncol(cv_values)){
    hist(cv_values[,i], breaks=20, main=paste(colnames(cv_values)[i], "count of NA:",sum(is.na(cv_values[,i]))))
  }
}