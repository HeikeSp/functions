#===============================================================================
# Name   : Check normal distribution using shapiro test and histograms
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

### plot histograms of normalized values
func_plot_dist <- function(normalized_values){
  for (i in 1:ncol(normalized_values)){
    hist(normalized_values[,i], breaks=25, freq=FALSE, col="grey", 
         main=paste(colnames(normalized_values)[i], 
                  "\n p-value shapiro-test: ",
                  shapiro.test(normalized_values[,i])$p.value, 
                  "\n count of NA:",
                  sum(is.na(normalized_values[,i]))))
    lines(density(normalized_values[,i], na.rm=TRUE))
  }
}


# shapiro test for normal distribution per analyte
func_shapiro_test <- function(normalized_values, threshold){
  res_shapiro <- rep(0, ncol(normalized_values))
  for (i in 1:ncol(normalized_values)){
    if( nrow(normalized_values) - sum(is.na(normalized_values[,i])) < 4)
      res_shapiro[i] <- NA
    else
      res_shapiro[i] <- shapiro.test(normalized_values[,i])$p.value
  }
  print(paste("number of analytes with p-value <", threshold))
  print(length(which(res_shapiro < threshold)))
  print(paste("which analytes have p-value <", threshold))
  print(which(res_shapiro < threshold))
  
  return(res_shapiro)
}
