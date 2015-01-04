log2_fold_change <- function(samples) {
  fold_change <- data.frame(matrix(rep(NA, nrow(samples)*(ncol(samples)/2)), nrow=nrow(samples)))
   
  for (i  in 1:(ncol(samples)/2)) {
    for (j in 1:length(samples[ ,1])) {
      fold_change[j,i] <- log2(samples[j,i+(ncol(samples)/2)]/samples[j,i])
    }
  }
  colnames(fold_change) <- colnames(samples)[1:(ncol(samples)/2)]
  rownames(fold_change) <- rownames(samples)
  return(fold_change)
}



log2_fold_change_sub <- function(samples) {
  fold_change <- data.frame(matrix(rep(NA, nrow(samples)*(ncol(samples)/2)), nrow=nrow(samples)))
   
  for (i  in 1:(ncol(samples)/2)) {
    for (j in 1:length(samples[ ,1])) {
      fold_change[j,i] <- log2(samples[j,i+12]/samples[j,i])
    }
  }
  colnames(fold_change) <- colnames(samples)[1:12]
  rownames(fold_change) <- rownames(samples)
  return(fold_change)
}