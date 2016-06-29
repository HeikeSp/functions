
##############
### colors ###
##############

library(RColorBrewer)
library(gplots)

#mypalette = palette( brewer.pal(7, "Dark2"))
#pie(1:7,brewer.pal(7, "Dark2"), col=mypalette)
#brewer.pal(7, "Dark2")
# "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
# tuerkis,   orange,   lila,     pink,     gruen,     gelb,     ocker

par(mfrow=c(3,2))

heike_palette_7 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02", "#265298")
# tuerkis,   orange,   lila,     pink,     gruen,     gelb,     dunkelblau
#pie(rep(1,7),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02", "#265298"), col=heike_palette_7, main="7 colors")

heike_palette_6 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02")
# tuerkis,   orange,   lila,     pink,     gruen,     gelb
#pie(rep(1,6),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02"), col=heike_palette_6)

heike_palette_5 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E")
# tuerkis,   orange,   lila,     pink,     gruen
#pie(rep(1,5),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E"), col=heike_palette_5)

heike_palette_4 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F")
# tuerkis,   orange,   lila,     pink
#pie(rep(1,4),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F"), col=heike_palette_4)

heike_palette_3 = c("#04A5CA", "#BF5300", "#66A61E")
# tuerkis,   orange,   gruen

heike_palette_2 = c("#BF5300", "#265298")
# blau rot
#pie(rep(1,2),c("#BF5300", "#265298"), col=c("#BF5300", "#265298"))

cols_treatment <- c("#4F81BD", "#B94441") # e.g. control / stress
#pie(rep(1,2), cols_treatment, col=cols_treatment, main="cols_treatment")

cols_sample_time2 <- c("#66A61E", "#7570B3") # 2 time points
#                       gruen        lila
#pie(rep(1,2), cols_sample_time2, col=cols_sample_time2, main="cols_sample_time")

cols_sample_time <- c("#74A0D4", "#155096", "#E27572","#6B0705") # 4 time points
#                       hellblau  dunkelblau hellrot  dunkelrot
#pie(rep(1,4), cols_sample_time, col=cols_sample_time, main="cols_sample_time")

cols_treatment_sample_time <- c("#74A0D4", "#E27572", "#155096", "#6B0705") # 4 conditions
#                             hellblau    hellrot   dunkelblau  dunkelrot

cols_early <- c("#74A0D4", "#155096") # 2 time points (early/before - early/after)
#             hellblau  dunkelblau 

cols_late <- c("#E27572","#6B0705") # 2 time points (late/before - late/after)
#               hellrot  dunkelrot

#cols_4B <- c("skyblue", "#FF9973", "darkblue", "orangered4") # 4 conditions
#pie(1:4, cols_4B, col=cols_4B)

cols_cultivar_check <- c("forestgreen", "darkorange3","darkblue", "deeppink4") # 4 cultivars
#pie(rep(1,4), cols_cultivar_check, col=cols_cultivar_check, main="cols_cultivar")

cols_cultivar2 <- c("#68C468", "#FFA54D","#3D3DF7", "#BA3E82") # 4 cultivars

cols_cultivar_34 <- rainbow(34) # 34 cultivars

# 34 distinct colors from: http://tools.medialab.sciences-po.fr/iwanthue/
cols_distinct_34 <- c("#D34428", "#7E3E24", "#DD8935", "#A47C3B", "#CFB031", "#7F7563", "#545921", 
                      "#A1AD4D", "#88BE30", "#AAAF7E", "#3F7D29", "#46CC33", "#59C763", "#61AC72", 
                      "#3B604A", "#5AC7AB", "#74AAAD", "#66ADD8", "#415D77", "#728CDA", "#AF9CBB", 
                      "#584D8D", "#7C63D5", "#C67FD4", "#D150E1", "#6A4D57", "#8B357C", "#DA44B4", 
                      "#D881AA", "#D54185", "#8E3851", "#CFA295", "#D53F5A", "#D37765")

cols_lines_63 <- rainbow(63) # 63 all lines in 2015
cols_lines_60 <- rainbow(60) # 60 common lines in 2015

cols_lines_2014 <- rainbow(193) # 193 lines
cols_crossing <- brewer.pal(3, "Dark2")  # AR, EA, parents

cols_cultivar_treatment <- c("forestgreen","#68C468", "darkorange3","#FFA54D", "darkblue","#3D3DF7", "deeppink4", "#BA3E82") # 4 cultivars / 2 treatments
#pie(rep(1,8), cols_cultivar_treatment, col=cols_cultivar_treatment, main="cols_cultivar_treatment")
par(mfrow=c(1,1))

# convert R color names to HEX code
col2hex("forestgreen")
#228B22 --> hell: #68C468
col2hex("darkorange3")
#CD6600 --> hell: #FFA54D
col2hex("darkblue")
#00008B --> hell: #3D3DF7
col2hex("deeppink4")
#8B0A50 --> hell: #BA3E82

cols_treatment_tolerance <- brewer.pal(4, "Paired")

#display.brewer.pal(n = 6, name = "Greens")
#display.brewer.pal(n = 6, name = "Oranges")

cols_trial <- c (brewer.pal(n = 6, name = "Greens")[4:6], brewer.pal(n = 6, name = "Oranges")[4:6])
#pie(rep(1,6), cols_trial, col=cols_trial, main="cols_trial")
# Greens: "#74C476" "#31A354" "#006D2C" --> for field
# Oranges: "#FD8D3C" "#E6550D" "#A63603" --> for greenhouse
# cols_trial <- c("#74C476", "#FD8D3C", "#E6550D", "#A63603", "#31A354", "#006D2C")

# colors for TROST project report
cols_treatment_report <- c("#558ED5", "#77933C") # control/stress
cols_genotype_report <- c("grey", "#BF5300", "#5778B9", "#00756D", "#F7B944") 
# grey      orange    blau      tuerkis   gelb
# Albatros  AxR       Euroresa  ExA       Ramses

cols_sp <- c("darkgrey", "#00A844", "#076993", "#EA7B00")
# grey gruen blau orange --> parents, SP1, SP2, SP3

# without parents 
cols_sp_no_parents <- c("#00A844", "#076993", "#EA7B00")

cols_sp_treatment <- c("darkgrey", "lightgrey", "#00A844", "#9EEABD", "#076993", "#9CCEE4", "#EA7B00", "#FFD6AA")
# grey/lightgrey  gruen/hellgruen blau/hellblau orange/hellorange

cols_sp_par_treatment <- c("darkgrey", "lightgrey", "#00A844", "#9EEABD", "#076993", "#9CCEE4", "#EA7B00", "#FFD6AA")


