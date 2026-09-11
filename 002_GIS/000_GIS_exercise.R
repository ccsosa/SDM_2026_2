################################################################################
library(sf)
library(geodata)
library(terra)
################################################################################
# Download level 1 boundaries (e.g., States/Provinces) for Coolombia (COL)
#https://gadm.org/download_country.html
col_gadm <- geodata::gadm(
  country = "COL", 
  level = 1, 
  path = "D:/REPO_GITHUB/SDM_2026_2/GIS",       # Directory to save the downloaded file
  version = "latest"     # Downloads the most recent version (e.g., 4.1)
)

#Fit to Coffee region
subset <- col_gadm[col_gadm$NAME_1 %in% c("Caldas","Risaralda","Quindío"),]
plot(subset)
#save
terra::writeVector(subset,"D:/REPO_GITHUB/SDM_2026_2/GIS/shp/COF.shp",overwrite=T)

################################################################################
#load rasters file
folder <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/Raster_S-America"

names <- list.files(folder,".tif")

sub_raster <- lapply(1:length(names), function(i){
  x <- terra::rast(paste0(folder,"/",names[[i]]))
  x <- terra::crop(x,subset)
  x <- terra::mask(x,subset)
  names(x) <- names[[i]]
  writeRaster(x,paste0("D:/REPO_GITHUB/SDM_2026_2/GIS/raster/",names[[i]]),overwrite=T)
  return(x)
})
sub_raster <- rast(sub_raster)

plot(sub_raster)
################################################################################
#load WDPA https://www.protectedplanet.net/country/COL
# Repair the invalid polygons

WDPA <- sf::st_read("D:/REPO_GITHUB/SDM_2026_2/GIS/WDPA_WDOECM_Sep2026_Public_COL.gdb")
WDPA_valid <- sf::st_make_valid(WDPA)
plot(st_geometry(WDPA_valid))

################################################################################
#Shapefile operations (Lets clip WDPA to coffee region)
subset_shp <- st_as_sf(subset)
WDPA_subset <- sf::st_intersection(WDPA_valid,subset_shp)
#adding area
area_WDPA <- st_area(WDPA_subset)
WDPA_subset$area <- as.numeric(area_WDPA/1000000)
sf::write_sf(WDPA_subset,"D:/REPO_GITHUB/SDM_2026_2/GIS/shp/WDPA_subset.shp",overwrite=T)
