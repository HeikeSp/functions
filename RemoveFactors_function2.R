#===============================================================================
# Name   : RemoveFactors by ANOVA
# Author : Jan Lisec
# Date   : 2013-07-12
# Version: 0.1
# Aim    : Provide an interface to remove the influence of technical/biiological factors from experimental variation using an ANOVA model
# Mail   : <<<lisec@mpimp-golm.mpg.de>>>
#===============================================================================
RemoveFactors2 <- function(y=NULL, sam=NULL, facs=NULL, keep=NULL, output=c("y_norm","y_lm","anova_y","anova_y_norm","boxplot")) {
# y : data to normalize (numeric + in same order as sam
# sam : dataframe containing the factors/numerical vars for ANOVA model
# facs : all factors to be incorporated in the model in the desired order
# keep : all factors to be retained in the normalized data
	tdf <- data.frame(y, sam[,facs]) # set up dataframe for anova
	y.lm <- lm(y ~ ., data=tdf) # set up anova model
	ce <- coef(y.lm) # these are the coefficients of the individual factors
	if (any(is.na(ce))) {
		ce[is.na(ce)] <- 0
		warning("Some coefficients were NA and had to be set to 0.\nYou probably have nested factors. Please check if ANOVA is appropriate.")
	}
	tm <- rep(ce[1], length(y)) # this is the total mean
	re <- residuals(y.lm) # residuals
	y.norm <- y # this maintaines any names y might have had
	fi <- is.finite(y) # this preserves the NAs in y by restoring only the finite values
	y.norm[fi] <- tm[fi] + re
	for (i in 1:length(keep)) {
		if (!is.factor(sam[,keep[i]])) {
			warning(paste("Can't keep numeric factors.", keep[i], "was removed."))
		} else {
			fac_eff <- sapply(levels(sam[,keep[i]]), function(x) {ifelse(is.na(ce[paste(keep[i],x,sep="")]),0,ce[paste(keep[i],x,sep="")])})
			y.norm[fi] <- y.norm[fi] + fac_eff[as.numeric(sam[,keep[i]])][fi] # add group mean values
		}
	}
	y.norm[fi] <- y.norm[fi] + (mean(y[fi]) - mean(y.norm[fi])) # correct for the offset
	if (output=="y_norm") return(y.norm)
	else if (output=="y_lm") return(y.lm)
	else if (output=="anova_y") return(anova(y.lm))
	else if (output=="anova_y_norm") {
		tdf <- data.frame(y.norm, sam[,facs])
		return(anova(lm(y.norm ~ ., data=tdf)))
	}
	else if (output=="boxplot") {
		par(mfrow=c(1,2))
			plot(y ~ interaction(sam[,keep]), xlab=paste(facs, collapse=", "))
			plot(y.norm ~ interaction(sam[,keep]), xlab=paste(keep, collapse=", "))
		par(mfrow=c(1,1))
		invisible(NULL)
	}
	invisible(NA)
}
