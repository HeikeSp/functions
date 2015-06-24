## Calculate DRYM using single replicates of RelSY (all plants)

func_calc_drym <- function(relSY, relSY_median, cultivar_names = c("Albatros","Alegria","Burana","Desiree",
                                                                 "Eldena","Eurobravo","Euroflora","Euronova",
                                                                 "Euroresa","Eurostarch","Eurotango","Golf",
                                                                 "Jasia","Jumbo","Karlena","Kiebitz",
                                                                 "Kolibri","Kormoran","Kuras","Logo","Maxi",
                                                                 "Maxilla","Milva","Pirol","Power", "Priamos",
                                                                 "Ramses","Saturna","Sibu","Sommergold","Tomba",
                                                                 "Tomensa","Ulme","Verdi")){
  drym <- list()
    for (cultivar_name in cultivar_names){
      drym[[cultivar_name]] <- relSY[[cultivar_name]] - median(unlist(relSY), na.rm=T)
    }
  return(drym)
}


## Calculate DRYM using median of RelSY (per cultivar and drought)

func_calc_drym_old <- function(relSY, relSY_median, cultivar_names = c("Albatros","Alegria","Burana","Desiree",
                                                                   "Eldena","Eurobravo","Euroflora","Euronova",
                                                                   "Euroresa","Eurostarch","Eurotango","Golf",
                                                                   "Jasia","Jumbo","Karlena","Kiebitz",
                                                                   "Kolibri","Kormoran","Kuras","Logo","Maxi",
                                                                   "Maxilla","Milva","Pirol","Power", "Priamos",
                                                                   "Ramses","Saturna","Sibu","Sommergold","Tomba",
                                                                   "Tomensa","Ulme","Verdi")){
  drym <- list()
  for (cultivar_name in cultivar_names){
    drym[[cultivar_name]] <- relSY_median[[cultivar_name]] - median(unlist(relSY), na.rm=T)
  }
  return(drym)
}
  
