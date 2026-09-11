require(CoordinateCleaner)
require(readxl)
require(wallace)
x <- as.data.frame(readxl::read_xlsx("D:/BOLDER/bactris_gasipaes/data.xlsx",col_names = T)
                   )
x

flags <- clean_coordinates(x = x, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           capitals_rad=5000,
                           inst_rad = 5000,
                           centroids_rad=5000,
                           species = "species",
                           tests = c("capitals", 
                                     "centroids","equal", 
                                     "gbif", "institutions", 
                                     "outliers", "seas", 
                                     "zeros")
                           ) # most test are on by default


summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")


#Exclude problematic records
dat_cl <- x[flags$.summary,]

#The flagged records
dat_fl <- x[!flags$.summary,]

colnames(dat_cl)[1:3]<- c("scientific_name","latitude","longitude") 
write.csv(dat_cl,"D:/BOLDER/bactris_gasipaes/occurences.csv")

