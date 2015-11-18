func_combine_two_final_DGE_lists <- function(final_DGE_list_1, final_DGE_list_2){

  # remove column with pgsc_dmp (1) and MapMan Description (9)
  final_DGE_list_1_unique <- unique(final_DGE_list_1[,-c(1,9)])
  final_DGE_list_2_unique <- unique(final_DGE_list_2[,-c(1,9)])

  # only unique entries (per dmg)
  final_DGE_list_merged <- merge(final_DGE_list_1_unique, final_DGE_list_2_unique, 
                                 all = T, by.x = "pgsc_dmg", by.y = "pgsc_dmg",
                                 suffixes = c("_tol", "_sens"))
  
  #return(final_DGE_list_merged)
  
  final_DGE_list_merged2 <- func_combine_two_vectors(table = final_DGE_list_merged, col1 = "func_tol", 
                                                     col2 = "func_sens", name_col = "function")
  
  final_DGE_list_merged2 <- func_combine_two_vectors(final_DGE_list_merged2, col1 = "BINCODE_tol", 
                                                     col2 = "BINCODE_sens", name_col = "Bincode")
  
  final_DGE_list_merged2 <- func_combine_two_vectors(final_DGE_list_merged2, col1 = "NAME_tol", 
                                                     col2 = "NAME_sens", name_col = "Name")
  
  # function to create a type for specific or common DGE comparing tolerant and sensitive cultivars
  final_DGE_list_merged2 <- func_create_type(final_DGE_list_merged2,
                                             column1 = "dir_tol", 
                                             column2 = "dir_sens",
                                             val1 = "tolerant specific",  
                                             val2 = "sensitive specific", 
                                             val3 = "common")

   return(final_DGE_list_merged2)
}


func_combine_two_final_DGE_lists_2 <- function(final_DGE_list_1, final_DGE_list_2){
  
  # remove column with pgsc_dmp (1) and MapMan Description (9)
  final_DGE_list_1_unique <- unique(final_DGE_list_1[,-c(1,9)])
  final_DGE_list_2_unique <- unique(final_DGE_list_2[,-c(1,9)])
  
  # only unique entries (per dmg)
  final_DGE_list_merged <- merge(final_DGE_list_1_unique, final_DGE_list_2_unique, 
                                 all = T, by.x = "pgsc_dmg", by.y = "pgsc_dmg",
                                 suffixes = c("_greenhouse", "_field"))
  
  #return(final_DGE_list_merged)
  
  final_DGE_list_merged2 <- func_combine_two_vectors(table = final_DGE_list_merged, col1 = "func_greenhouse", 
                                                     col2 = "func_field", name_col = "function")
  
  final_DGE_list_merged2 <- func_combine_two_vectors(final_DGE_list_merged2, col1 = "BINCODE_greenhouse", 
                                                     col2 = "BINCODE_field", name_col = "Bincode")
  
  final_DGE_list_merged2 <- func_combine_two_vectors(final_DGE_list_merged2, col1 = "NAME_greenhouse", 
                                                     col2 = "NAME_field", name_col = "Name")
  
  # function to create a type for specific or common DGE comparing greenhouse and field cultivars
  final_DGE_list_merged2 <- func_create_type(final_DGE_list_merged2,
                                             column1 = "dir_greenhouse", 
                                             column2 = "dir_field",
                                             val1 = "greenhouse specific",  
                                             val2 = "field specific", 
                                             val3 = "common")
  
  return(final_DGE_list_merged2)
}