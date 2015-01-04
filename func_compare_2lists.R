#===============================================================================
# Name   : Compare two list of up/down-regulated analytes
# Author : Heike Sprenger
# Date   : 2014-08-13
# Version: 0.1
#===============================================================================

# compare lists of up-regulated analytes from 2 timepoints  

func_compare_2lists_up <- function(list1, list2){
  print("results for comparison of up-regulated analytes")
  print(paste("number of analytes in first list", 
              length(list1$up)))
  print(paste("number of analytes in second list", 
              length(list2$up)))  
  print(paste("number of overlapping analytes:", 
              length( intersect(list1$up, list2$up))))
  print(paste("number of analytes specific for first list", 
              length( setdiff (list1$up, list2$up) )))
  print(paste("number of analytes specific for second list", 
              length( setdiff (list2$up, list1$up) )))
}


# compare lists of down-regulated analytes from 2 timepoints

func_compare_2lists_down <- function(list1, list2){
  print("results for comparison of down-regulated analytes")
  print(paste("number of analytes in first list", 
              length(list1$down)))
  print(paste("number of analytes in second list", 
              length(list2$down)))  
  print(paste("number of overlapping analytes:", 
              length( intersect(list1$down, list2$down))))
  print(paste("number of analytes specific for first list", 
              length( setdiff (list1$down, list2$down) )))
  print(paste("number of analytes specific for second list", 
              length( setdiff (list2$down, list1$down) )))
}