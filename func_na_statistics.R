#===============================================================================
# Name   : Print/Plot NA statistics
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# prints NA statistics and returns analytes with more than 40% NAs
# matrix should be with analytes in columns and samples in rows!

func_print_na_statistics <- function(matrix, analytes_name = analytes_6sel_exp_sort$name) {
  print(paste("percentage of NAs: ", round((sum(is.na(matrix)) / prod(dim(matrix)) * 100), digits=3), "%" ))
  #print(paste("NAs per samples (=rows): ", sort(apply(matrix, 1, function(x) sum(is.na(x)))) ))
  #print(paste("NAs per analytes (=column): ", sort(apply(matrix, 2, function(x) sum(is.na(x)))) ))
  
  # count how many analytes have more than 40% NAs out of the samples
  high_na_analytes <- which(apply(matrix, 2, function(x) sum(is.na(x))) > (nrow(matrix) * 0.4) )
  print(paste("num of analytes with more than 40% NAs: ", length(high_na_analytes)))
  print("num of NAs for problematic analytes:")
  if(length(high_na_analytes) == 1){
    print(data.frame(as.character(analytes_name[high_na_analytes]), 
                sum(is.na(matrix[,high_na_analytes])) ))
    }

  else{
    print(data.frame("analyte_name" = as.character(analytes_name[high_na_analytes]), 
                     "number_of_NAs" = as.numeric(apply(matrix[,high_na_analytes], 2, function(x) sum(is.na(x)) ))))
  }
  # return(high_na_analytes) # gives only colnumber of analyte
}

# plot histograms about NA stats

func_plot_na_statistics <- function(matrix) {
  # hist NAs per samples (=rows)
  hist(apply(matrix, 1, function(x) sum(is.na(x))), breaks=38, 
       xlab="NAs per sample", col="lightblue", main="Histogram of NAs across all analytes per sample")
  
  # hist NAs per analytes (=column), red line at 40% NAs per analyte
  hist(apply(matrix, 2, function(x) sum(is.na(x))), breaks=38, 
       xlab="NAs per analyte", col="lightblue" ,main="Histogram of NAs across all samples per analyte")
  abline(v=0.4*nrow(matrix), lwd=2, col="red")
}