# function to calculate Stress Sensitivity Index (SSI)

func_calc_ssi_table <- function(yield_trial, si_value){
  
  # calculate SSI
  yield_trial$ssi <- (1 - yield_trial$relSY) / si_value
  
  return(yield_trial)
}



