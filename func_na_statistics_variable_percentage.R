#===============================================================================
# Name   : Print/Plot NA statistics
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# prints NA statistics and returns analytes with more than XX% NAs
# matrix should be with analytes in columns and samples in rows!

func_print_na_statistics <- function(matrix, percentage, analytes) {
  print(paste("percentage of NAs: ", round((sum(is.na(matrix)) / prod(dim(matrix)) * 100), digits=3), "%" ))
  #print(paste("NAs per samples (=rows): ", sort(apply(matrix, 1, function(x) sum(is.na(x)))) ))
  #print(paste("NAs per analytes (=column): ", sort(apply(matrix, 2, function(x) sum(is.na(x)))) ))
  
  # count how many analytes have more than XX% NAs out of the samples
  high_na_analytes <- which(apply(matrix, 2, function(x) sum(is.na(x))) > (nrow(matrix) * percentage) )
  print(paste("num of analytes with more than", percentage*100,"% NAs: ", length(high_na_analytes)))
  print("num of NAs for problematic analytes:")
#   if(length(high_na_analytes == 1))
#     print(cbind(as.character(analytes_6sel_exp_sort$name[high_na_analytes]), 
#                 sum(is.na(all_pot_merge[,high_na_analytes])) ))
# 
#   else
    print(cbind(as.character(analytes$name[high_na_analytes]), 
                apply(matrix[,high_na_analytes], 2, function(x) sum(is.na(x)) )))
  # return(high_na_analytes) # gives only colnumber of analyte
}

# plot histograms about NA stats

func_plot_na_statistics <- function(matrix, percentage) {
  # hist NAs per samples (=rows)
  hist(apply(matrix, 1, function(x) sum(is.na(x))), breaks=38, 
       xlab="NAs per sample", col="lightblue", main="Histogram of NAs across all analytes per sample")
  
  # hist NAs per analytes (=column), red line at XX% NAs per analyte
  hist(apply(matrix, 2, function(x) sum(is.na(x))), breaks=38, 
       xlab="NAs per analyte", col="lightblue" ,main="Histogram of NAs across all samples per analyte")
  abline(v=percentage*nrow(matrix), lwd=2, col="red")
}