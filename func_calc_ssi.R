# function to calculate Stress Sensitivity Index (SSI)

func_calc_ssi <- function(yield_trial, plant_lines, si_value, keep_replicates = "yes"){
  
  # calculate relative Starch yield (relSY) per line for each replicate
  relSY <- func_calc_relSY_valdis(yield_trial, plant_lines)
  
  # calculate median of relSY per line
  relSY_median <- func_calc_relSY_median(relSY)
  
  # calculate 
  ssi <- list()
  for (plant_line in plant_lines){
    if(keep_replicates == "yes"){
      ssi[[plant_line]] <- (1 - unique(relSY[[plant_line]]) ) / si_value # use all replicates per line
    } else{
      ssi[[plant_line]] <- (1 - unique(relSY_median[[plant_line]]) ) / si_value # use median per line
    }
  }
  
  return(ssi)
}

