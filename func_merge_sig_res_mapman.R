func_merge_sig_res_mapman <- function(res){
  res_top_sig <- res
  res_top_sig <- cbind(res_top_sig, pgsc_dmg = rownames(res_top_sig))
  res_mapman <- unique(join(res_top_sig, merge_mapman_pgsc[,c(2,5:7)], by = "pgsc_dmg"))
  res_mapman <- res_mapman[order(res_mapman[,"BINCODE"]),]
  return(res_mapman)
}

func_merge_res_stress_bin <- function(res, mapman_data){
  res <- cbind(res, pgsc_dmg = rownames(res))
  res_mapman <- unique(join(res, mapman_data[,c(2,5:7)], by = "pgsc_dmg", type = "inner"))
  res_mapman <- res_mapman[order(res_mapman[,"pgsc_dmg"]),]
  return(res_mapman)
}