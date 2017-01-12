#===============================================================================
# Name   : Prep and PCA
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

library(pcaMethods)

func_prep_pca <- function(values_matrix, scale_method, center_option, pca_method, pc_number){
  prep_res <- prep(values_matrix, scale=scale_method, center=center_option)
  pca_res <- pcaMethods::pca(prep_res, nPcs=pc_number, method=pca_method)
  
  print("explained variances by PCs")
  print(pca_res@R2)
  
  return(pca_res)
}