func_modify_yield_data <- function(yield_query_result, project = "trost"){

  # transform several columns to factor
  yield_query_result$treatment <- as.factor(yield_query_result$treatment)
  yield_query_result$plant_id <- as.factor(yield_query_result$plant_id)
  yield_query_result$entity_id <- as.factor(yield_query_result$entity_id)
  yield_query_result$value_id <- as.factor(yield_query_result$value_id)
  
  #sum(is.na(yield_query_result$number))
  # 32
  
  # transform number to numeric (thereby NULL values are replaced by NAs)
  yield_query_result$number <- as.numeric(as.character(yield_query_result$number))
  
  #sum(is.na(yield_query_result$number))
  # 164
  
  if(project == "trost"){
    
    yield_query_result$cultivar_id <- as.factor(yield_query_result$cultivar_id)
    yield_query_result$culture <- as.factor(yield_query_result$culture)
    
    levels(yield_query_result$cultivar) <- c("Albatros","Alegria","Burana","Desiree","Eldena",
                                             "Eurobravo","Euroflora","Euronova","Euroresa",
                                             "Eurostarch","Eurotango","Golf","Jasia","Jumbo",
                                             "Karlena","Kiebitz","Kolibri","Kormoran","Kuras",
                                             "Logo","Maxi","Maxilla","Milva","Pirol","Power",
                                             "Priamos","Ramses","Saturna","Sibu","Sommergold",
                                             "Tomba","Tomensa","Ulme","Verdi")
    
    # modify treatment factor 
    # Dethlingen trials: 
    # remove 172 = rB = 30%nFC 
    remove_treatment <- which(yield_query_result$treatment == 172)
    yield_query_result <- yield_query_result[-remove_treatment,]
      
    # replace 171 = oB = 50%nFC by control (169)
    replace_treatment <- which(yield_query_result$treatment == 171)
    yield_query_result$treatment[replace_treatment] <- 169
    
    yield_query_result$treatment <- droplevels(yield_query_result$treatment)
    
    # remove wrong plant (Saturna plant from MPITest2)
    remove_plant = which(yield_query_result$plant_id == 1117465)
    yield_query_result <- yield_query_result[-remove_plant,]
    
  }
  
  else if(project == "valdis"){
    
    yield_query_result$alias <- as.factor(yield_query_result$alias)
    yield_query_result$name <- as.factor(yield_query_result$name)
    
    # change alias names
    #idx_euroresa <- which(yield_query_result$name )
    
  }
  
  return(yield_query_result)

}