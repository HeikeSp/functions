#===============================================================================
# Name   : relevel factors (treatment, genotype_name, sample_time)
# Author : Heike Sprenger
# Date   : 2015-11-10
# Version: 0.2
#===============================================================================

# Relevel treatment, genotype_name and sample_time factors
func_relevel_factors <- function(trial_matrix,
                           treatment_levels=c("control", "drought stress"),
                           genotype_levels=c("Alegria", "Desiree", "Milva", "Saturna"),
                           genotype_levels_sorted=c("Alegria", "Milva", "Desiree", "Saturna"),
                           sample_time_levels=c("early/before","early/after", "late/before", "late/after")) {
  
  # VALDIS: use only the subset of the factors matrix where the genotype_name is within the wanted genotypes
  if(length(genotype_levels) > 34){
    trial_matrix <- subset(trial_matrix, trial_matrix$genotype_name %in% genotype_levels)
  }
  
  # drop unused levels
  trial_matrix$treatment <- droplevels(trial_matrix$treatment)
  trial_matrix$genotype_name <- droplevels(trial_matrix$genotype_name)
  trial_matrix$sample_time <- droplevels(trial_matrix$sample_time)
  
  # rename levels of treatment factor
  # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
  levels(trial_matrix$treatment) <- treatment_levels
  
  # sample time need to be reordered, not renamed
  trial_matrix$sample_time <- factor(trial_matrix$sample_time, levels=sample_time_levels)
  
  # genotype_name needs to be renamed AND reordered!
  # do the following only for TROST experiments, with less than 35 genotypes
  if(length(levels(trial_matrix$genotype_name)) < 35){
    levels(trial_matrix$genotype_name) <- genotype_levels
    trial_matrix$genotype_name <- factor(trial_matrix$genotype_name, levels=genotype_levels_sorted)
  } 
  
  # add column for genotype class (parents, EA or AR) ONLY FOR VALDIS DATA (> 34 genotypes)
  if(length(levels(trial_matrix$genotype_name)) > 34){
    AxR <- which(grepl("^AR", trial_matrix$genotype_name))
    ExA <- which(grepl("^EA", trial_matrix$genotype_name))
    parents <- which(trial_matrix$genotype_name %in% c('Albatros', 'Ramses', 'Euroresa'))
    
    genotype_class <- as.character(trial_matrix$genotype_name)
    genotype_class[AxR] <- "AR"
    genotype_class[ExA] <- "EA"
    genotype_class[parents] <- "parents"
    trial_matrix$genotype_class <- factor(genotype_class)
  } else
    trial_matrix$genotype_class <- trial_matrix$genotype_name
  
  
  print("levels of treatment after relevel:")
  print(levels(trial_matrix$treatment))
  print("levels of genotypes after relevel:")
  print(levels(trial_matrix$genotype_name))
  print("levels of sample_time after relevel:")
  print(levels(trial_matrix$sample_time))
  
  return(trial_matrix)
}


#############################################################

# Relevel cultivar and treatment
# func_relevel_factors_2 <- function(trial_matrix,
#                                  treatment_levels=c("control", "drought stress"),
#                                  cultivar_levels=c("Alegria", "Desiree", "Milva", "Saturna"),
#                                  cultivar_levels_sorted=c("Alegria", "Milva", "Desiree", "Saturna")) {
#   
#   # drop unused levels
#   trial_matrix$treatment <- droplevels(trial_matrix$treatment)
#   trial_matrix$cultivar <- droplevels(trial_matrix$cultivar)
#   
#   # rename levels of treatment factor
#   # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
#   levels(trial_matrix$treatment) <- treatment_levels
#   
#   # cultivar needs to be renamed AND reordered!
#   # do the following only for experiments with four cultivars
#   if(length(levels(trial_matrix$cultivar)) == 4){
#     levels(trial_matrix$cultivar) <- cultivar_levels
#     trial_matrix$cultivar <- factor(trial_matrix$cultivar, levels=cultivar_levels_sorted)
#   } 
#   
#   print("levels of treatment after relevel:")
#   print(levels(trial_matrix$treatment))
#   print("levels of cultivar after relevel:")
#   print(levels(trial_matrix$cultivar))
#   
#   return(trial_matrix)
# }