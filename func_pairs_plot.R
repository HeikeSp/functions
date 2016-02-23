# panel.cor function
# source: http://www.gettinggeneticsdone.com/2011/07/scatterplot-matrices-in-r.html

# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
# pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
#       lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, main="Iris Scatterplot Matrix")



# pairs.annot function
# source http://r-epid.blogspot.de/2008/11/correlation-pairs-plot.html

pairs.annot <- function(data, ...) {
    
    panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                          cex = 1, col.lm = "red", lwd=par("lwd"), ...) {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
          abline(lm(y~x,subset=ok), col = col.lm, ...)
      }
    
    panel.sse <- function(y, x, digits=2) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        
        model <- summary(lm(y~x))
        r2<- model$r.squared
        r<-sqrt(r2)*sign(model$coef[2,1])
        p<- model$coef[2,4]
        
        txt <- round(r, digits)
        txt <- bquote(r == .(txt))
        text(0.5, 0.7, txt, cex=1.5)
        
        txt <- round(r2, digits)
        txt <- bquote(r^2 == .(txt))
        text(0.5, 0.5, txt, cex=1.5)
        
        txt <- round(p, digits)
        txt <- bquote(P == .(txt))
        text(0.5, 0.3, txt, cex=1.5)
      }
    
    pairs(data,lower.panel=panel.sse,upper.panel=panel.lm)
  }