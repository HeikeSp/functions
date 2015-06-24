func_starch_yield_mpipruef_check <- function(yield_data, culture_id,
                                             cultivars=c("Alegria", "Desiree", "Milva", "Saturna")){
  
# get yield data subset for one experiment
yield_trial <- subset(yield_data, yield_data$culture == culture_id & yield_data$cultivar %in% cultivars)
#table(yield_trial$attribute)
#table(yield_trial$attribute, yield_trial$entity_name)

# drop levels of factors (after getting subset)
yield_trial$cultivar <- droplevels(yield_trial$cultivar)
yield_trial$treatment <- droplevels(yield_trial$treatment)
# rename levels of factors
levels(yield_trial$treatment) <- c("control", "drought stress")
#levels(yield_trial$cultivar) <- c("Desiree", "Alegria", "Milva", "Saturna")

# reorder cultivar names (by tolerance)
yield_trial$cultivar <- factor(yield_trial$cultivar, levels=c("Alegria", "Milva", "Desiree", "Saturna"))

# get subset for tuber_FW, tubercore_FW and tubercore_DW
tuber_FW <- subset(yield_trial, 
                              yield_trial$attribute == "absolutes Frischgewicht" & 
                                yield_trial$entity_name=="Knolle")

tubercore_FW <- subset(yield_trial, 
                                  yield_trial$attribute == "absolutes Frischgewicht"& 
                                    yield_trial$entity_name=="Knollenbohrkern")

tubercore_DW <- subset(yield_trial, 
                                  yield_trial$attribute == "absolutes Trockengewicht"& 
                                    yield_trial$entity_name=="Knollenbohrkern")

# order subsets by plantID
tuber_FW <- tuber_FW[order(tuber_FW$plant_id),]
tubercore_FW <- tubercore_FW[order(tubercore_FW$plant_id),]
tubercore_DW <- tubercore_DW[order(tubercore_DW$plant_id),]

# calculate tuber_FW in kg
tuber_FW_kg <- tuber_FW$number/1000

# calculate drymatter content in % from tubercore DW/FW
drymatter <- tubercore_DW$number*100/tubercore_FW$number

drymatter2 <- rep(0,length(drymatter))
for (i in 1:length(drymatter)){
  if(is.na(drymatter[i]))
    drymatter2[i] <- mean(drymatter, na.rm=T)
  else
    drymatter2[i] <- drymatter[i]
}

# calculate starch content in g per kg
starch_g_per_kg <- (drymatter2-6.0313)*10

# calculate starch_yield in g per plant from tuber_FW (kg) and starch_content (g/kg)
starch_yield_g_per_plant <- tuber_FW_kg * starch_g_per_kg
starch_yield_kg_per_plant <- starch_yield_g_per_plant/1000

# generate complete tuber data
tuber_data <- cbind(tuber_FW[,c(1:7,13)], 
                               tuber_FW_kg,
                               tubercore_FW$number, 
                               tubercore_DW$number, 
                               drymatter2,
                               starch_g_per_kg,
                               starch_yield_kg_per_plant,
                               starch_yield_g_per_plant)

colnames(tuber_data)[8:15] <- c("tuber_FW_g", "tuber_FW_kg", "tubercore_FW_g", "tubercore_DW_g", "drymatter_percent", "starch_g_per_kg", "starch_yield_kg_per_plant", "starch_yield_g_per_plant")

return(tuber_data)

}