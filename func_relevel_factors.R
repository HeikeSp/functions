#===============================================================================
# Name   : relevel factors (treatment, cultivar, sample_time)
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# Relevel treatment, cultivar and sample_time factors
func_relevel_factors <- function(trial_matrix,
                           treatment_levels=c("control", "drought stress"),
                           cultivar_levels=c("Alegria", "Desiree", "Milva", "Saturna"),
                           cultivar_levels_sorted=c("Alegria", "Milva", "Desiree", "Saturna"),
                           sample_time_levels=c("early/before","early/after", "late/before", "late/after")) {
  
  # drop unused levels
  trial_matrix$treatment <- droplevels(trial_matrix$treatment)
  trial_matrix$cultivar <- droplevels(trial_matrix$cultivar)
  trial_matrix$sample_time <- droplevels(trial_matrix$sample_time)
  
  # rename levels of treatment factor
  # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
  levels(trial_matrix$treatment) <- treatment_levels
  
  # sample time need to be reordered, not renamed
  trial_matrix$sample_time <- factor(trial_matrix$sample_time, levels=sample_time_levels)
  
  # cultivar and sample time need to be renamed AND reordered!
  levels(trial_matrix$cultivar) <- cultivar_levels
  trial_matrix$cultivar <- factor(trial_matrix$cultivar, levels=cultivar_levels_sorted)
  
  print("levels of treatment after relevel:")
  print(levels(trial_matrix$treatment))
  print("levels of cultivar after relevel:")
  print(levels(trial_matrix$cultivar))
  print("levels of sample_time after relevel:")
  print(levels(trial_matrix$sample_time))
  
  return(trial_matrix)
}


#############################################################

# Relevel cultivar and treatment
func_relevel_factors_2 <- function(trial_matrix,
                                 treatment_levels=c("control", "drought stress"),
                                 cultivar_levels=c("Alegria", "Desiree", "Milva", "Saturna"),
                                 cultivar_levels_sorted=c("Alegria", "Milva", "Desiree", "Saturna"),
                                 sample_time_levels=c("early/before","early/after", "late/before", "late/after")) {
  
  # drop unused levels
  trial_matrix$treatment <- droplevels(trial_matrix$treatment)
  trial_matrix$cultivar <- droplevels(trial_matrix$cultivar)
  
  # rename levels of treatment factor
  # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
  levels(trial_matrix$treatment) <- treatment_levels
  
  # cultivar needs to be renamed AND reordered!
  levels(trial_matrix$cultivar) <- cultivar_levels
  trial_matrix$cultivar <- factor(trial_matrix$cultivar, levels=cultivar_levels_sorted)
  
  print("levels of treatment after relevel:")
  print(levels(trial_matrix$treatment))
  print("levels of cultivar after relevel:")
  print(levels(trial_matrix$cultivar))
  
  return(trial_matrix)
}