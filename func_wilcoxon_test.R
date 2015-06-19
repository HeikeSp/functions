#===============================================================================
# Name   : Perform wilcoxon test
# Author : Heike Sprenger
# Date   : 2015-06-19
# Version: 0.1
#===============================================================================


# wilcoxon test for comparing phenotypic traits, e.g. BBCH --> ranks!
# compare control vs. drought stress per cultivar

func_wilcoxon_test_treatment <- function(phenotypes, variable_name){
  
  cultivars <- levels(phenotypes$cultivar)
  
  res_wilcoxon_test <- c()
  
  for (i in 1:length(cultivars))
  {
    res_wilcoxon_test[i] <- wilcox.test(subset(phenotypes[ , variable_name], 
                                         phenotypes$cultivar==cultivars[i] & phenotypes$treatment=="control"),
                                  
                                  subset(phenotypes[ , variable_name], 
                                         phenotypes$cultivar==cultivars[i] & phenotypes$treatment=="drought stress")
                           
    )$p.value
  }
  
  names(res_wilcoxon_test) <- cultivars
  return(res_wilcoxon_test)
  
}