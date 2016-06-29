## Calculate DRYM using single replicates of RelSY (all plants)

func_calc_drym_table <- function(yield_trial){
  
  yield_trial$drym <- yield_trial$relSY - median(unique(yield_trial$relSY), na.rm = T)
  
  return(yield_trial)
}
