func_starch_yield_jkitest <- function(yield_data, culture_id){
  
# get yield data subset for one experiment
yield_trial <- subset(yield_data, yield_data$culture == culture_id)
#table(yield_trial$attribute)
#table(yield_trial$attribute, yield_trial$entity_name)

# drop levels of factors (after getting subset)
yield_trial$cultivar <- droplevels(yield_trial$cultivar)
yield_trial$treatment <- droplevels(yield_trial$treatment)
# rename levels of factors
levels(yield_trial$treatment) <- c("control", "drought stress")
# levels(yield_trial$cultivar) <- c("Alegria", "Desiree", "Milva", "Saturna")

# reorder cultivar names (by tolerance)
yield_trial$cultivar <- factor(yield_trial$cultivar, levels=c("Milva", "Alegria", "Desiree", "Saturna"))

# get subset for tuber_FW (in kg), starch content and starch yield
tuber_FW_kg <- subset(yield_trial, yield_trial$attribute == "absolutes Frischgewicht")
starch_g_per_kg <- subset(yield_trial, yield_trial$attribute == "Staerkegehalt")
starch_yield_kg_per_plant <- subset(yield_trial, yield_trial$attribute == "Staerkeertrag")

# order subsets by plantID
tuber_FW_kg <- tuber_FW_kg[order(tuber_FW_kg$plant_id),]
starch_g_per_kg <- starch_g_per_kg[order(starch_g_per_kg$plant_id),]
starch_yield_kg_per_plant <- starch_yield_kg_per_plant[order(starch_yield_kg_per_plant$plant_id),]

# calculate starchyield in g/plant
starch_yield_g_per_plant <- starch_yield_kg_per_plant$number*1000

# generate complete tuber data
tuber_data <- cbind(tuber_FW_kg[,c(1:7,13)], 
                               starch_g_per_kg$number,
                               starch_yield_kg_per_plant$number,
                               starch_yield_g_per_plant)

colnames(tuber_data)[8:11] <- c("tuber_FW_kg","starch_g_per_kg", "starch_yield_kg_per_plant", "starch_yield_g_per_plant")

return(tuber_data)

}