#===============================================================================
# Name   : Get GMD experiment ID
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

func_get_experiment_id <- function(experiment_name) {
  as.character( trost_TagList$id[which(trost_TagList$experiment==experiment_name)])  
}