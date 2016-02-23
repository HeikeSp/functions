func_starch_yield_feld <- function(yield_data, culture_id, num_per_plot){
  
  # get yield data subset for one experiment
  yield_trial <- droplevels(subset(yield_data, yield_data$culture %in% culture_id))
  
  # drop levels of factors (after getting subset)
  yield_trial$alias <- droplevels(yield_trial$alias)
  
  # rename levels of factors
  levels(yield_trial$treatment) <- c("control", "drought stress")
  
  # get subset for tuber_FW (in kg), starch content and starch yield
  tuber_FW_kg <- subset(yield_trial, yield_trial$attribute == "absolutes Frischgewicht")
  starch_g_per_kg <- subset(yield_trial, yield_trial$attribute == "Staerkegehalt")
  #starch_yield_g_per_plant <- subset(yield_trial, yield_trial$attribute == "Staerkeertrag")
  
  # order subsets by plantID
  tuber_FW_kg <- tuber_FW_kg[order(tuber_FW_kg$plant_id),]
  starch_g_per_kg <- starch_g_per_kg[order(starch_g_per_kg$plant_id),]
  #starch_yield_g_per_plant <- starch_yield_g_per_plant[order(starch_yield_g_per_plant$plant_id),]
  
  # # calculate starch yield in g/plot
  starch_yield_g_per_plot <- tuber_FW_kg$number * starch_g_per_kg$number
  starch_yield_g_per_plant2 <- starch_yield_g_per_plot/num_per_plot
  
  # generate complete tuber data
  # 1: alias, 2: culture, 3: plant_id, 4: line_id, 5: name, 6: treatment, 7: treatment_name
  # 11: population, 12: SP1, 13: SP2, 14: SP3, 15: SP, 10: number
  tuber_data <- cbind(tuber_FW_kg[ ,c(1:7,11:15,10)], 
                      starch_g_per_kg$number,
                      starch_yield_g_per_plant2,
                      starch_yield_g_per_plot)
  
  colnames(tuber_data)[13:16] <- c("tuber_FW_kg_per_plot", "starch_g_per_kg",
                                  "starch_yield_g_per_plant", "starch_yield_g_per_plot")
  
  return(tuber_data)

}
