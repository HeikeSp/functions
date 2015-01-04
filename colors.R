
##############
### colors ###
##############

library(RColorBrewer)
#mypalette = palette( brewer.pal(7, "Dark2"))
#pie(1:7,brewer.pal(7, "Dark2"), col=mypalette)
#brewer.pal(7, "Dark2")
# "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
# tuerkis,   orange,   lila,     pink,     gruen,     gelb,     ocker

par(mfrow=c(3,2))

heike_palette_7 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02", "#265298")
# tuerkis,   orange,   lila,     pink,     gruen,     gelb,     dunkelblau
pie(rep(1,7),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02", "#265298"), col=heike_palette_7, main="7 colors")

heike_palette_6 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02")
# tuerkis,   orange,   lila,     pink,     gruen,     gelb
#pie(rep(1,6),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E", "#E6AB02"), col=heike_palette_6)

heike_palette_5 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E")
# tuerkis,   orange,   lila,     pink,     gruen
#pie(rep(1,5),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F", "#66A61E"), col=heike_palette_5)

heike_palette_4 = c("#04A5CA", "#BF5300", "#7570B3", "#BB417F")
# tuerkis,   orange,   lila,     pink
#pie(rep(1,4),c("#04A5CA", "#BF5300", "#7570B3", "#BB417F"), col=heike_palette_4)

heike_palette_2 = c("#BF5300", "#265298")
# blau rot
#pie(rep(1,2),c("#BF5300", "#265298"), col=c("#BF5300", "#265298"))

cols_treatment <- c("#4F81BD", "#B94441") # e.g. control / stress
pie(rep(1,2), cols_treatment, col=cols_treatment, main="cols_treatment")

cols_sample_time2 <- c("#66A61E", "#7570B3") # 2 time points
#                       gruen        lila
pie(rep(1,2), cols_sample_time2, col=cols_sample_time2, main="cols_sample_time")

cols_sample_time <- c("#74A0D4", "#155096", "#E27572","#6B0705") # 4 time points
#                       hellblau  dunkelblau hellrot  dunkelrot
pie(rep(1,4), cols_sample_time, col=cols_sample_time, main="cols_sample_time")

cols_treatment_sample_time <- c("#74A0D4", "#E27572", "#155096", "#6B0705") # 4 conditions
#                             hellblau    hellrot   dunkelblau  dunkelrot

cols_early <- c("#74A0D4", "#155096") # 2 time points (early/before - early/after)
#             hellblau  dunkelblau 

cols_late <- c("#E27572","#6B0705") # 2 time points (late/before - late/after)
#               hellrot  dunkelrot

#cols_4B <- c("skyblue", "#FF9973", "darkblue", "orangered4") # 4 conditions
#pie(1:4, cols_4B, col=cols_4B)

cols_cultivar <- c("forestgreen", "darkorange3","darkblue", "deeppink4") # 4 cultivars
pie(rep(1,4), cols_cultivar, col=cols_cultivar, main="cols_cultivar")

cols_cultivar2 <- c("#68C468", "#FFA54D","#3D3DF7", "#BA3E82") # 4 cultivars

cols_cultivar_treatment <- c("forestgreen","#68C468", "darkorange3","#FFA54D", "darkblue","#3D3DF7", "deeppink4", "#BA3E82") # 4 cultivars / 2 treatments
pie(rep(1,8), cols_cultivar_treatment, col=cols_cultivar_treatment, main="cols_cultivar_treatment")
par(mfrow=c(1,1))

func_convert_color_name_to_hex <- function(color_name){
  rgb(red = col2rgb(color_name)[1], green = col2rgb(color_name)[2], blue = col2rgb(color_name)[3], maxColorValue=255)
}

func_convert_color_name_to_hex("forestgreen")
#228B22 --> hell: #68C468
func_convert_color_name_to_hex("darkorange3")
#CD6600 --> hell: #FFA54D
func_convert_color_name_to_hex("darkblue")
#00008B --> hell: #3D3DF7
func_convert_color_name_to_hex("deeppink4")
#8B0A50 --> hell: #BA3E82


cols_treatment_tolerance <- brewer.pal(4, "Paired")
