func_calc_stress_index <- function(yield_trial){
  starch_yield_mean <- aggregate(yield_trial$starch_yield_kg_per_plant, by=list(yield_trial$treatment), mean, na.rm=T)
  stress_index <- 1- (starch_yield_mean$x[2] / starch_yield_mean$x[1])
  return(stress_index)
}
