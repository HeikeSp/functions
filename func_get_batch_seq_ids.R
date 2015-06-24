#===============================================================================
# Name   : Get BatchID and SequenceID
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# http://stackoverflow.com/questions/1676990/split-a-string-vector-r
# as.factor(unlist(lapply(strsplit(as.character(trost_gmd_ids_jkitest1$GMD_id),"_"), function(x) x[1])))

func_get_batch_ids <- function(trial_matrix) {
  # Extract BatchID from GMD_id, e.g. 11290if_15 as NUMERIC --> 11290if
  # http://stackoverflow.com/questions/1676990/split-a-string-vector-r
  trial_BatchIDs <- as.factor(unlist(lapply(strsplit(as.character(trial_matrix$GMD_id),"_"), function(x) x[1])))       
}

func_get_sequence_ids <- function(trial_matrix) {
  # Extract Sequence from GMD_id e.g. 11290if_15 as NUMERIC --> 15
  trial_SequenceIDs <- as.numeric(unlist(lapply(strsplit(as.character(trial_matrix$GMD_id),"_"), function(x) x[2])))    
}