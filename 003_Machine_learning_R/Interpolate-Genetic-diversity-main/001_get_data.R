# install.packages("CoordinateCleaner")
library(readxl)
library(sf)
library(terra)
#define folder
dir <- "D:/Interpolate-Genetic-diversity-main"

#loading GenDivRange
sp_df <- readxl::read_xlsx(paste0(dir,"/GenDivRange/","spec_tab_v2025-03-31.xlsx"))
pop_df <- readxl::read_xlsx(paste0(dir,"/GenDivRange/","pop_tab_v2025-03-31.xlsx"))

#subsetting to Herbaceous
plants <- sp_df[which(sp_df$Life_form=="Herbaceous plants"),]
#only choosing Beta vulgaris n>80
sp <- sp_df[which(sp_df$Spec_Latin_GenDivRange=="Beta vulgaris"),]
pop_df_i <- pop_df[which(pop_df$Spec_id%in%sp$Spec_id),]

#pop_df_i_to <- pop_df_i[,c("Pop_id","Latitude","Longitude","He","Ar")]
#saving
write.csv(pop_df_i,paste0(dir,"/","pop_data.csv"),row.names = F)

#getting n of records
# pop_herb <- pop_df[pop_df$Spec_id %in% plants$Spec_id,]
# 
# tapply(pop_herb$Spec_id,pop_herb$Spec_id,length)[
#   order(tapply(pop_herb$Spec_id,pop_herb$Spec_id,length),decreasing = T)
# ]
# 
# 
# unique(pop_df_i$Geog_1)

