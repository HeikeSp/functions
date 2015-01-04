#===============================================================================
# Name        : Log-Median-Transformation
# Description : Calculate the median metabolite level for metabolite (over all samples) and
#               calculate log10-ratio this value from the respective metabolite levels in all samples
# Author      : Heike Sprenger
# Date        : 2014-07-15
# Version     : 0.1
#===============================================================================


# input matrix with samples in columns and analytes in rows

func_log_transform <- function(samples) {
    
  median_samples <- apply(samples, 1, median, na.rm=TRUE) # calculate median rowwise --> per analyte over all samples
  
  transformed <- data.frame(matrix(rep(NA, nrow(samples)*ncol(samples)), nrow=nrow(samples)))
  
  for (i  in 1:length(samples[1,])) {
    for (j in 1:length(samples[ ,1])) {
      transformed[j,i] <- log10(samples[j,i]/median_samples[j]) # calculate log10 of ratio of value/median
    }
  }
  colnames(transformed) <- colnames(samples)
  rownames(transformed) <- rownames(samples)
  return(transformed)
}