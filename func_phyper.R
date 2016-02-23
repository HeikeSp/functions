# hypergeometric test 
# uses the hypergeometric distribution to calculate the statistical significance of having drawn a specific k successes (out of n total draws) from the aforementioned population

func_phyper <- function(nTotal, Set1, Set2){

  nSet1 <- length(Set1)
  nSet2 <- length(Set2)
  nOverlap <- length(intersect(Set1, Set2))
  
  1 - phyper((nOverlap-1), nSet1, (nTotal-nSet1), nSet2)
}