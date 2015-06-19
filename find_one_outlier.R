# code adapted from Lukasz Komsta's grubbs.test

# outlier value ("o" in grubbs.test) is stored in $outlier_value
# row name of the outlier ("G" in grubbs.test) is stored in $outlier_rowname

library(outliers)

find_one_outlier <- function (x, output="none", opposite = FALSE)
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  
  if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
    alt = paste("lowest value", x[1], "is an outlier")
    o <- x[1]
    d <- x[2:n]
  }
  else {
    alt = paste("highest value", x[n], "is an outlier")
    o <- x[n]
    d <- x[1:(n - 1)]
  }
  g <- abs(o - mean(x))/sd(x)
  u <- var(d)/var(x) * (n - 2)/(n - 1)
  pval <- 1 - pgrubbs(g, n, type = 10)
  method <- "Grubbs test for one outlier"
  
  RVAL <- list(statistic = c(G = g, U = u), alternative = alt,
               p.value = pval, method = method, data.name = DNAME,
               outlier_value = o, outlier_rowname = g)
  class(RVAL) <- "htest"
  if (output=="name")
    return(names(RVAL$outlier_value))
  else if (output=="p.value")
    return(RVAL$p.value)
  else
    return(RVAL)
}


# set.seed(1234)
# x = rnorm(10)
# print(x)
# 
# names(x) <- c("A","B","C","D","E","F","G","H","I","J")
# 
# result <- find_one_outlier(x)
# 
# names(result$outlier_value)
