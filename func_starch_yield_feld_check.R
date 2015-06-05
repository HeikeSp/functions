func_starch_yield_feld_check <- function(yield_data, culture_id, num_per_plot,
                                         cultivars=c("Alegria", "Desiree", "Milva", "Saturna")){
  
# get yield data subset for one experiment
yield_trial <- subset(yield_data, yield_data$culture == culture_id & yield_data$cultivar_name %in% cultivars)
#table(yield_trial$attribute)
#table(yield_trial$attribute, yield_trial$entity_name)

# drop levels of factors (after getting subset)
yield_trial$cultivar_name <- droplevels(yield_trial$cultivar_name)
yield_trial$treatment <- droplevels(yield_trial$treatment)
# rename levels of factors
levels(yield_trial$treatment) <- c("control", "drought stress")
#levels(yield_trial$cultivar_name) <- c("Alegria", "Desiree", "Milva", "Saturna")

# reorder cultivar names (by tolerance)
yield_trial$cultivar_name <- factor(yield_trial$cultivar_name, levels=c("Alegria", "Milva", "Desiree", "Saturna"))

# get subset for tuber_FW (in kg), starch content and starch yield
tuber_FW_kg <- subset(yield_trial, yield_trial$attribute == "absolutes Frischgewicht" & yield_trial$entity_name == "Knolle")
starch_g_per_kg <- subset(yield_trial, yield_trial$attribute == "Staerkegehalt" & yield_trial$entity_name == "Knolle")
starch_yield_kg_per_plot <- subset(yield_trial, yield_trial$attribute == "Staerkeertrag" & yield_trial$entity_name == "Knolle")

# order subsets by plantID
tuber_FW_kg <- tuber_FW_kg[order(tuber_FW_kg$plant_id),]
starch_g_per_kg <- starch_g_per_kg[order(starch_g_per_kg$plant_id),]
starch_yield_kg_per_plot <- starch_yield_kg_per_plot[order(starch_yield_kg_per_plot$plant_id),]

# calculate starchyield in g/plot
starch_yield_g_per_plot <- starch_yield_kg_per_plot$number*1000

# calculate starchyield in g/plot and kg/plot
starch_yield_kg_per_plant <- starch_yield_kg_per_plot$number/num_per_plot
starch_yield_g_per_plant <- starch_yield_g_per_plot/num_per_plot


# generate complete tuber data
tuber_data <- cbind(tuber_FW_kg[,c(1:7,13)], 
                    starch_g_per_kg$number,
                    starch_yield_kg_per_plot$number,
                    starch_yield_g_per_plot,
                    starch_yield_kg_per_plant,
                    starch_yield_g_per_plant)

colnames(tuber_data)[8:13] <- c("tuber_FW_kg","starch_g_per_kg", "starch_yield_kg_per_plot", "starch_yield_g_per_plot",
                                "starch_yield_kg_per_plant", "starch_yield_g_per_plant")

return(tuber_data)

}