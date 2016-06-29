
# for valdis data with more lines
func_calc_relSY_valdis_table <- function(yield_trial, plant_lines){
  
  # subset of control values
  control <- subset(yield_trial, yield_trial$treatment_name == "control")
  
  # mean of control values per plant lines (= alias)
  control_mean <- aggregate(control$starch_yield_g_per_plant, 
                            by = list(control$alias), 
                            mean, na.rm = T)
  
  colnames(control_mean) <- c("alias", "starch_yield_g_per_plant")

  # subset of stress values
  stress <- subset(yield_trial, yield_trial$treatment_name == "drought stress")
  head(stress)
 
  # join control/stress subset with control mean SY
  stress_joined <- join(stress, control_mean, by = "alias")
  control_joined <- join(control, control_mean, by = "alias")
  
  colnames(stress_joined)[ncol(stress_joined)] <- "control_mean_SY"
  colnames(control_joined)[ncol(control_joined)] <- "control_mean_SY"
  
  # calculate ratio of stress/control
  stress_joined$relSY <- stress_joined$starch_yield_g_per_plant / stress_joined$control_mean_SY

  # Combine data.frames by row, filling in missing columns
  relSY_table <- rbind.fill(stress_joined, control_joined)
  
  return(relSY_table)
  
  
}

# 
# # MEDIAN of relative SY
# func_calc_relSY_table_median <- function(relSY_table){
#   relSY_median <- aggregate(relSY_table$relSY, 
#                             by = list(relSY_table$alias),
#                             median, na.rm = T)
#   return(relSY_median)
# }
# 
# # MEAN of relative SY
# func_calc_relSY_table_mean <- function(relSY_table){
#   relSY_mean <- aggregate(relSY_table$relSY, 
#                           by = list(relSY_table$alias),
#                           mean, na.rm = T)
#   return(relSY_mean)
# }

