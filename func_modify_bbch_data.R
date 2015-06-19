### Function to modify BBCH table

func_modify_bbch_data <- function(dataset){
  
  dataset$treatment <- as.factor(dataset$treatment)
  levels(dataset$treatment) <- c("control", "drought stress")
  
  if ( !is.null(dataset$description) )
    
  {
    levels(dataset$description)
    dataset$description <- as.factor(dataset$description)
    dataset$description <- factor(dataset$description, levels=c("early","early/before", "early/after","late", "late/before", "late/after"))
  
    # generate timepoint factor so that late/before and late/after are late
    timepoint <- rep("early", nrow(dataset))
    timepoint [dataset$description %in% c("late/before", "late/after", "late")] <- "late"
    timepoint <- as.factor(timepoint)
    
    dataset <- cbind(dataset, timepoint)
  }
  
 return(dataset)

}