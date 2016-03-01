#===============================================================================
# Name   : Create tolerance factor
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# create factor for tolerance (Milva and Alegria: sensitive | Desiree and Saturna: tolerant)
func_create_tolerance_factor <- function(trial_matrix) {
  trial_tolerance <- rep("NA", nrow(trial_matrix))
  
  if(length(levels(trial_matrix$genotype_name))==4){
    trial_tolerance[which(trial_matrix$genotype_name %in% c("Milva", "Alegria"))] <- "sensitive"
    trial_tolerance[which(trial_matrix$genotype_name %in% c("Desiree", "Saturna"))] <- "tolerant"
  }
  
  trial_tolerance <- as.factor(trial_tolerance)
}