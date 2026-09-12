require(geodata)
require(factoextra);library(ranger);library(caret)
library(doParallel);require(tmaptools)
#downloading worlclim

if(!file.exists("D:/Interpolate-Genetic-diversity-main/spat_data.tif")){
  
  if(!dir.exists("D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY")){
    dir.create("D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY")
  }
  
  x <- worldclim_global(var = "bio", res=5,
                        path="D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY"
                        # country=c("France","Spain","Portugal","Morocco" )
  )  
  
  
  #downloading selected countries
  shp <- gadm(country = c("France","Spain","Portugal","Morocco" ), level = 0, 
              path = "D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY")  # level 0 = national boundaries
  
  #cutting and masking to FR,ES,PT,MO
  x2 <- terra::crop(x,shp)
  x2 <- terra::mask(x2,shp)
  
  #saving stack raster
  writeRaster(x2,"D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY/spat_data.tif")
} else {
  x <- rast("D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY/spat_data.tif")
}