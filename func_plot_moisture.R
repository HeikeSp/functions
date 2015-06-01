
func_plot_moisture_for_hour <- function(hour_value, return_table = TRUE, moisture_values = moisture)
  {
  
  Date_Time_sub <- Date_Time[which(Date_Time$hour == hour_value)]
  moisture_sub <- cbind(Date = Date_Time_sub, 
                       moisture_values[which(Date_Time$hour == hour_value),])
  
  moisture_sub_melt <- melt(moisture_sub, id = "Date")
  head(moisture_sub_melt)
  
  # join moisture subset with metainformation about sensor
  moisture_sub_joined <-join(moisture_sub_melt, meta_sub_ordered2, by="variable")
  
  # replace moisture of 600 by NA
  moisture_sub_joined$value[moisture_sub_joined$value==600] <- NA
  
  # sequence from 1 to 2*length of dates --> for every second tick on x-axis, because of control/drought boxes
  length_sub <- length(Date_Time_sub)
  xaxis_ticks_sub <- seq(1, 2*length_sub, 2) 
  length(xaxis_ticks_sub)
  
  ## labels of x-axis with dates
  labels_sub = as.Date(Date_Time_sub)
  
  ########################################################################
  # if return_table = TRUE, return the moisture subset for specific hour #
  ########################################################################
  if(return_table)  
  {
    return( list("sub" = moisture_sub, 
                 "sub_melt" = moisture_sub_melt, 
                 "sub_joined" = moisture_sub_joined,
                 "xaxis_ticks" = xaxis_ticks_sub,
                 "xaxis_labels" = labels_sub))
  }
  
  ################################################
  # if return_table = FALSE, return the boxplot! #
  ################################################
  else
  
  # boxplot of moisture for control/drought separately
  boxplot(moisture_sub_joined$value ~ moisture_sub_joined$Treatment * moisture_sub_joined$Date,  
          col=c("#4F81BD","#B94441"), xaxt="n", main=paste("all sensors at ", hour_value), ylim=c(0,600))
  axis(1,at=xaxis_ticks_sub,labels=labels_sub, las=2, cex.axis=0.8)
}

