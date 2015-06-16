#===============================================================================
# Name   : Perform ANOVA for 2 factors (with/without interaction)
# Author : Heike Sprenger
# Date   : 2014-08-05
# Version: 0.2
#===============================================================================


# using 2 factors (with interaction)

func_anova_2fac_ia <- function(normalized_values, num_variable, factor1, factor2, threshold){
  #res_anova <- matrix("NA", ncol=4, nrow=ncol(normalized_values))

  p_value <- summary(aov(normalized_values[,num_variable] ~ 
                                   normalized_values[,factor1] * normalized_values[,factor2]))[[1]][["Pr(>F)"]] 
  f_value <- summary(aov(normalized_values[,num_variable] ~ 
                           normalized_values[,factor1] * normalized_values[,factor2]))[[1]][["F value"]] 

  p_value <- p_value[-4]
  f_value <- f_value[-4]
  p_value <- as.numeric(p_value)
  f_value <- as.numeric(f_value)
  
  anova_res <- rbind(p_value, f_value)
  colnames(anova_res) <- c(factor1, factor2, "interaction")
  
  return(anova_res)
}

# combine anova results from several experiments --> 4
func_combine_anova_res <- function(res1, res2, res3, res4, p_f1, p_f2, factor1, factor2){
  combine_anova_res <- cbind(c(res1[p_f1, factor1], res2[p_f1, factor1], res3[p_f1, factor1], res4[p_f1, factor1]), # factor1/p_f1
                             c(res1[p_f2, factor1], res2[p_f2, factor1], res3[p_f2, factor1], res4[p_f2, factor1]), # factor1/p_f2
                             c(res1[p_f1, factor2], res2[p_f1, factor2], res3[p_f1, factor2], res4[p_f1, factor2]), # factor2/p_f1
                             c(res1[p_f2, factor2], res2[p_f2, factor2], res3[p_f2, factor2], res4[p_f2, factor2])) # factor2/p_f2
  colnames(combine_anova_res) <- c(paste(factor1,"_",p_f1, sep=""),
                                   paste(factor1,"_",p_f2, sep=""),
                                   paste(factor2,"_",p_f1, sep=""),
                                   paste(factor2,"_",p_f2, sep=""))
  return(combine_anova_res)  
}


# combine anova results from several experiments --> 7
func_combine_anova_res7 <- function(res1, res2, res3, res4, res5, res6, res7, p_f1, p_f2, factor1, factor2){
  combine_anova_res <- cbind(c(res1[p_f1, factor1], res2[p_f1, factor1], res3[p_f1, factor1], res4[p_f1, factor1], 
                               res5[p_f1, factor1], res6[p_f1, factor1], res7[p_f1, factor1]), # factor1/p_f1
                             c(res1[p_f2, factor1], res2[p_f2, factor1], res3[p_f2, factor1], res4[p_f2, factor1], 
                               res5[p_f2, factor1], res6[p_f2, factor1], res7[p_f2, factor1]), # factor1/p_f2
                             c(res1[p_f1, factor2], res2[p_f1, factor2], res3[p_f1, factor2], res4[p_f1, factor2], 
                               res5[p_f1, factor2], res6[p_f1, factor2], res7[p_f1, factor2]), # factor2/p_f1
                             c(res1[p_f2, factor2], res2[p_f2, factor2], res3[p_f2, factor2], res4[p_f2, factor2],
                               res5[p_f2, factor2], res6[p_f2, factor2], res7[p_f2, factor2])) # factor2/p_f2
  colnames(combine_anova_res) <- c(paste(factor1,"_",p_f1, sep=""),
                                   paste(factor1,"_",p_f2, sep=""),
                                   paste(factor2,"_",p_f1, sep=""),
                                   paste(factor2,"_",p_f2, sep=""))
  return(combine_anova_res)  
}


# combine anova results from several experiments --> 6
func_combine_anova_res6 <- function(res1, res2, res3, res4, res5, res6, p_f1, p_f2, factor1, factor2){
  combine_anova_res <- cbind(c(res1[p_f1, factor1], res2[p_f1, factor1], res3[p_f1, factor1], 
                               res4[p_f1, factor1], res5[p_f1, factor1], res6[p_f1, factor1]), # factor1/p_f1
                             c(res1[p_f2, factor1], res2[p_f2, factor1], res3[p_f2, factor1], 
                               res4[p_f2, factor1], res5[p_f2, factor1], res6[p_f2, factor1]), # factor1/p_f2
                             c(res1[p_f1, factor2], res2[p_f1, factor2], res3[p_f1, factor2], 
                               res4[p_f1, factor2], res5[p_f1, factor2], res6[p_f1, factor2]), # factor2/p_f1
                             c(res1[p_f2, factor2], res2[p_f2, factor2], res3[p_f2, factor2], 
                               res4[p_f2, factor2], res5[p_f2, factor2], res6[p_f2, factor2])) # factor2/p_f2
  colnames(combine_anova_res) <- c(paste(factor1,"_",p_f1, sep=""),
                                   paste(factor1,"_",p_f2, sep=""),
                                   paste(factor2,"_",p_f1, sep=""),
                                   paste(factor2,"_",p_f2, sep=""))
  return(combine_anova_res)  
}


# combine anova results from several experiments --> 6 WITH Interactions!
func_combine_anova_res6_ia <- function(res1, res2, res3, res4, res5, res6, p_f1, p_f2, factor1, factor2){
  combine_anova_res <- cbind(c(res1[p_f1, factor1], res2[p_f1, factor1], res3[p_f1, factor1], 
                               res4[p_f1, factor1], res5[p_f1, factor1], res6[p_f1, factor1]), # factor1/p_f1
                             c(res1[p_f2, factor1], res2[p_f2, factor1], res3[p_f2, factor1], 
                               res4[p_f2, factor1], res5[p_f2, factor1], res6[p_f2, factor1]), # factor1/p_f2
                             c(res1[p_f1, factor2], res2[p_f1, factor2], res3[p_f1, factor2], 
                               res4[p_f1, factor2], res5[p_f1, factor2], res6[p_f1, factor2]), # factor2/p_f1
                             c(res1[p_f2, factor2], res2[p_f2, factor2], res3[p_f2, factor2], 
                               res4[p_f2, factor2], res5[p_f2, factor2], res6[p_f2, factor2]), # factor2/p_f2
                             c(res1[p_f1, "interaction"], res2[p_f1, "interaction"], res3[p_f1, "interaction"], 
                               res4[p_f1, "interaction"], res5[p_f1, "interaction"], res6[p_f1, "interaction"]), # interaction/p_f1
                             c(res1[p_f2, "interaction"], res2[p_f2, "interaction"], res3[p_f2, "interaction"], 
                               res4[p_f2, "interaction"], res5[p_f2, "interaction"], res6[p_f2, "interaction"])) # interaction/p_f2
  colnames(combine_anova_res) <- c(paste(factor1,"_",p_f1, sep=""),
                                   paste(factor1,"_",p_f2, sep=""),
                                   paste(factor2,"_",p_f1, sep=""),
                                   paste(factor2,"_",p_f2, sep=""),
                                   paste("interaction_",p_f1, sep=""),
                                   paste("interaction_",p_f2, sep=""))
  return(combine_anova_res)  
}