func_CV <- function(var){
  (sd(var, na.rm = T)/mean(var, na.rm = T)) * 100
}