func_calc_relSY <- function(yield_trial, cultivar_names = c("Albatros","Alegria","Burana","Desiree",
                                                         "Eldena","Eurobravo","Euroflora","Euronova",
                                                         "Euroresa","Eurostarch","Eurotango","Golf",
                                                         "Jasia","Jumbo","Karlena","Kiebitz",
                                                         "Kolibri","Kormoran","Kuras","Logo","Maxi",
                                                         "Maxilla","Milva","Pirol","Power", "Priamos",
                                                         "Ramses","Saturna","Sibu","Sommergold","Tomba",
                                                         "Tomensa","Ulme","Verdi")){
  control <- subset(yield_trial, yield_trial$treatment=="control")
  control_mean <- aggregate(control$starch_yield_kg_per_plant, by=list(control$cultivar_name), mean, na.rm=T)
  colnames(control_mean) <- c("cultivar", "starch_yield_kg_per_plant")
  control_mean_cultivars <- list()
  for (cultivar_name in cultivar_names){
    control_mean_cultivars[[cultivar_name]] <- subset(control_mean$starch_yield_kg_per_plant, 
                                                   control_mean$cultivar == cultivar_name)
  }
  # return(control_mean_cultivars)
  
  stress <- subset(yield_trial, yield_trial$treatment=="drought stress")
  stress_cultivars <- list()
  for (cultivar_name in cultivar_names){
    stress_cultivars[[cultivar_name]] <- c(subset(stress$starch_yield_kg_per_plant, 
                                                stress$cultivar_name == cultivar_name))
 }
  
  #return(stress_cultivars)
 
 relSY <- list()
 for (cultivar_name in cultivar_names){
   relSY[[cultivar_name]] <- stress_cultivars[[cultivar_name]] / control_mean_cultivars[[cultivar_name]]
 }
 return(relSY)
}


func_calc_relSY_median <- function(relSY){
  relSY_median <- lapply(relSY, median, na.rm=T)
  return(relSY_median)
}
