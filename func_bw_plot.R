
func_bw_plot <- function(variable, title, xlab_text = "concentration in µmol/g FG"){
  bwplot(variable, 
         main = title,
         xlab = xlab_text, 
         aspect = 1)
}


func_bw_plot_cat1 <- function(variable, cat1 = cat1_value, title, 
                              xlab_text = "BBCH category",
                              ylab_text = "concentration in µmol/g FG"){
  bwplot(variable ~ cat1, 
         main = title,
         xlab = xlab_text,
         ylab = ylab_text,
         aspect = 1)
}

# Boxplot with 2 panels (one per category of cat2)
func_bw_plot_cat2 <- function(variable, cat1 = cat1_value, 
                              cat2 = cat2_value, title, 
                              xlab_text = "anthocyanin score (visual scoring)",
                              ylab_text = "concentration in µmol/g FG"){
  bwplot(variable ~ cat1 | cat2, 
         main = title,
         xlab = xlab_text,
         ylab = ylab_text,
         aspect = 1, 
         scales = list(rot=90), 
         layout = c(2, 1), 
         index.cond = list(1:2))
}



# trellis settings have to be made BEFORE plotting graphs!
new.dot=trellis.par.get("box.dot")
new.dot$pch="|"  
new.dot$col="black"
new.rectangle=trellis.par.get("box.rectangle")
new.rectangle$col="black"
new.rectangle$fill="white"     # olivdrab3  #9ACD32
new.umbrella=trellis.par.get("box.umbrella")
new.umbrella$col="black"
new.umbrella$lty=1 # Continous line, not dotted
new.symbol=trellis.par.get("plot.symbol")
new.symbol$col="black"        # (dark olive #5E8510)
new.strip.background=trellis.par.get("strip.background")
new.strip.background$col="white" # Background colour in the upper label (light olive #C6E686)
new.strip.shingle=trellis.par.get("strip.shingle")
new.strip.shingle$col="black" # Border colour around the upper label

new.axis.text=trellis.par.get("axis.text")             
new.axis.text$cex=1.3 # font size axisntitel, default 0.8
new.par.main.text=trellis.par.get("par.main.text")
new.par.main.text$cex= 1.5 # font size title, default 1.2
#new.par.sub.text=trellis.par.get("par.sub.text")
#new.par.sub.text$cex=1.3 # font size  default 1
new.par.xlab.text=trellis.par.get("par.xlab.text")
new.par.xlab.text$cex= 1.3 # font size x-axis label default 1
new.par.ylab.text=trellis.par.get("par.ylab.text")
new.par.ylab.text$cex= 1.3 # font size x-axis label default 1
new.par.strip.text=trellis.par.get("par.strip.text")
new.par.strip.text$cex =1.2

trellis.par.set(box.dot = new.dot, 
                box.rectangle = new.rectangle, 
                box.umbrella = new.umbrella, 
                plot.symbol = new.symbol, 
                strip.background = new.strip.background, 
                strip.shingle = new.strip.shingle,
                axis.text = new.axis.text, 
                par.main.text = new.par.main.text, 
                par.xlab.text = new.par.xlab.text, 
                par.ylab.text = new.par.ylab.text, 
                par.strip.text = new.par.strip.text)

