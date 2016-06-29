# for trost data with 34 cultivars
func_calc_relSY <- function(yield_trial, cultivar_names = names_cultivars_34){
  control <- subset(yield_trial, yield_trial$treatment=="control")
  control_mean <- aggregate(control$starch_yield_kg_per_plant, by=list(control$cultivar), mean, na.rm=T)
  colnames(control_mean) <- c("cultivar", "starch_yield_kg_per_plant")
  control_mean_cultivars <- list()
  for (cultivar_name in cultivar_names){
    control_mean_cultivars[[cultivar_name]] <- subset(control_mean$starch_yield_kg_per_plant, 
                                                   control_mean$cultivar == cultivar_name)
  }
  
  stress <- subset(yield_trial, yield_trial$treatment=="drought stress")
  stress_cultivars <- list()
  for (cultivar_name in cultivar_names){
    stress_cultivars[[cultivar_name]] <- c(subset(stress$starch_yield_kg_per_plant, 
                                                stress$cultivar == cultivar_name))
 }
 
 relSY <- list()
 for (cultivar_name in cultivar_names){
   relSY[[cultivar_name]] <- stress_cultivars[[cultivar_name]] / control_mean_cultivars[[cultivar_name]]
 }
 return(relSY)
}

# for valdis data with more lines
func_calc_relSY_valdis <- function(yield_trial, plant_lines){
  
  # subset of control values
  control <- subset(yield_trial, yield_trial$treatment_name=="control")
  
  # mean of control values per plant lines (= alias)
  control_mean <- aggregate(control$starch_yield_g_per_plant, by=list(control$alias), mean, na.rm=T)
  colnames(control_mean) <- c("plant_line", "starch_yield_g_per_plant")
  
  # convert control mean table to list
  control_mean_plant_lines <- list()
  for (plant_line in plant_lines){
    control_mean_plant_lines[[plant_line]] <- subset(control_mean$starch_yield_g_per_plant, 
                                                      control_mean$plant_line == plant_line)
    }

  # subset of stress values
  stress <- subset(yield_trial, yield_trial$treatment_name=="drought stress")
  
  # convert stress table to list
  stress_plant_lines <- list()
  for (plant_line in plant_lines){
    stress_plant_lines[[plant_line]] <- c(subset(stress$starch_yield_g_per_plant, 
                                                  stress$alias == plant_line))
    }
  
  # calculate ratio of stress/control
  relSY <- list()
  for (plant_line in plant_lines){
    relSY[[plant_line]] <- unique(stress_plant_lines[[plant_line]]) / control_mean_plant_lines[[plant_line]]
    # unique is necessary because some lines occurr as duplicates (belong to two SP)
  }
  return(relSY)
}

# MEDIAN of relative SY
func_calc_relSY_median <- function(relSY){
  relSY_median <- lapply(relSY, median, na.rm=T)
  return(relSY_median)
}

# MEAN of relative SY
func_calc_relSY_mean <- function(relSY){
  relSY_mean <- lapply(relSY, mean, na.rm=T)
  return(relSY_mean)
}

