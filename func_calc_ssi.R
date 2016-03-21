# function to calculate Stress Sensitivity Index (SSI)

func_calc_ssi <- function(yield_trial, plant_lines, si_value, keep_replicates = "yes"){
  
  # calculate relative Starch yield (relSY) per line for each replicate
  relSY <- func_calc_relSY_valdis(yield_trial, plant_lines)
  
  # calculate MEDIAN of relSY per line
  # relSY_median <- func_calc_relSY_median(relSY)
  
  # calculate MEAN of relSY per line
  relSY_mean <- func_calc_relSY_mean(relSY)
  
  # calculate SSI
  ssi <- list()
  for (plant_line in plant_lines){
    if(keep_replicates == "yes"){
      ssi[[plant_line]] <- (1 - unique(relSY[[plant_line]]) ) / si_value # use all replicates per line
    } else{
      ssi[[plant_line]] <- (1 - unique(relSY_mean[[plant_line]]) ) / si_value # use mean per line
    }
  }
  
  return(ssi)
}

