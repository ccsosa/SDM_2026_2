################################################################################
library(sf)
library(geodata)
library(terra)
################################################################################
# Download level 1 boundaries (e.g., States/Provinces) for Colombia (COL)
#https://gadm.org/download_country.html
col_gadm <- geodata::gadm(
  country = "COL", 
  level = 1, 
  path = "D:/REPO_GITHUB/SDM_2026_2/GIS",       # Directory to save the downloaded file
  version = "latest"     # Downloads the most recent version (e.g., 4.1)
)
terra::writeVector(col_gadm,"D:/REPO_GITHUB/SDM_2026_2/GIS/col_adm_0.shp")
#Fit to Coffee region
subset <- col_gadm[col_gadm$NAME_1 %in% c("Caldas","Risaralda","Quindío"),]
plot(subset)
#save
terra::writeVector(subset,"D:/REPO_GITHUB/SDM_2026_2/GIS/COF.shp",overwrite=T)

################################################################################
#load rasters file
folder <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/Raster_S-America"
names <- list.files(folder,".tif")
names
sub_raster <- lapply(1:length(names), function(i){
  x <- terra::rast(paste0(folder,"/",names[[i]]))
  x <- terra::crop(x,subset)
  x <- terra::mask(x,subset)
  names(x) <- names[[i]]
  writeRaster(x,paste0("D:/REPO_GITHUB/SDM_2026_2/GIS/",names[[i]]),overwrite=T)
  return(x)
})
sub_raster <- rast(sub_raster)

plot(sub_raster)
################################################################################
#load WDPA https://www.protectedplanet.net/country/COL
# Repair the invalid polygons

WDPA <- sf::st_read("D:/REPO_GITHUB/SDM_2026_2/002_GIS/shp/WDPA_subset.shp")
WDPA_valid <- sf::st_make_valid(WDPA)
plot(st_geometry(WDPA_valid))
WDPA_valid <- WDPA_valid[WDPA_valid$DESIG %in% c("Parque Nacional Natural",
                                                 "Reserva Natural de la Sociedad Civil",
                                                 "Parques Naturales Regionales",
                                                 "Santuario de Fauna y Flora"
                                     ),]
plot(st_geometry(WDPA_valid))

################################################################################
#Shapefile operations (Lets clip WDPA to coffee region)
subset_shp <- st_as_sf(subset)
WDPA_subset <- sf::st_intersection(WDPA_valid,subset_shp)

plot(st_geometry(subset_shp),col="gray")
plot(st_geometry(WDPA_subset),col="green",add=T)
#adding area
area_WDPA <- st_area(WDPA_subset)
WDPA_subset$area <- as.numeric(area_WDPA/1000000)
sf::write_sf(WDPA_subset,"D:/REPO_GITHUB/SDM_2026_2/GIS/WDPA_subset.shp",overwrite=T)

ext_mean <- terra::extract(sub_raster[[1]], WDPA_subset, fun = mean, na.rm = TRUE)
WDPA_subset$aridity <- ext_mean[, 2]


library(ggplot2)

ggplot() +
  # 1. Fondo del Eje Cafetero (grays)
  geom_sf(data = subset_shp, fill = "gray95", color = "gray70", linewidth = 0.5) +
  # 2. Áreas protegidas coloreadas por el valor medio
  geom_sf(data = WDPA_subset, aes(fill = aridity), color = "black", linewidth = 0.2) +
  # 3. Escala de colores y leyenda automática (estilo viridis)
  scale_fill_viridis_c(name = "Valor promedio de aridez", option = "plasma") +
  # 4. Estética y títulos
  theme_minimal() +
  labs(
    title = "Valor Medio en Áreas Protegidas",
    subtitle = "Eje Cafetero - Colombia",
    x = "Longitud",
    y = "Latitud"
  )


ext_mean <- terra::extract(sub_raster[[5]], WDPA_subset, fun = mean, na.rm = TRUE)
WDPA_subset$prec_mean <- ext_mean[, 2]


ggplot() +
  # 1. Fondo del Eje Cafetero (grays)
  geom_sf(data = subset_shp, fill = "gray95", color = "gray70", linewidth = 0.5) +
  # 2. Áreas protegidas coloreadas por el valor medio
  geom_sf(data = WDPA_subset, aes(fill = prec_mean), color = "black", linewidth = 0.2) +
  # 3. Escala de colores y leyenda automática (estilo viridis)
  scale_fill_viridis_c(name = "Valor promedio de precipitación media anual",
                       option = "plasma") +
  # 4. Estética y títulos
  theme_minimal() +
  labs(
    title = "Valor Medio en Áreas Protegidas",
    subtitle = "Eje Cafetero - Colombia",
    x = "Longitud",
    y = "Latitud"
  )

################################################################################
#subset eje cafetero

J_NEOTROPICA <- terra::vect("D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/data/Juglans neotropica.shp")
plot(J_NEOTROPICA)

#
Juglans_en_WDPA <- terra::intersect(J_NEOTROPICA, terra::vect(WDPA_subset))

# 3. Visualizar el resultado
plot(terra::vect(subset_shp), col = "gray95", main = "Juglans neotropica en Áreas Protegidas")
plot(terra::vect(WDPA_subset), border = "blue", add = TRUE)
plot(Juglans_en_WDPA, col = "forestgreen", add = TRUE)

################################################################################
#extract pa suramerica
#cargar capas
folder <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/Raster_S-America"
names <- list.files(folder,".tif")
S_america_layers <- lapply(1:length(names), function(i){
  x <- terra::rast(paste0(folder,"/",names[[i]]))
  return(x)
})
S_america_layers <- rast(S_america_layers)  

EXT_JN <- terra::extract(S_america_layers,J_NEOTROPICA)

require(FactoMineR)
require(factoextra)

PC_ENV <- PCA(EXT_JN[,-1],ncp = 5,scale.unit = T)
library(factoextra)

# 1. Asegúrate de tener tu PCA calculado (ej: con tus datos normalizados)
# res.pca <- prcomp(mis_datos, scale = TRUE)

# 2. Generar el Biplot conjunto
fviz_pca_biplot(PC_ENV, 
                # Personalización de los individuos (puntos)
                col.ind = "cos2",         # Colorea según la calidad de representación
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                # Personalización de las variables (flechas)
                col.var = "contrib",      # Colorea según su contribución a las dimensiones
                
                # Opciones generales
                repel = TRUE,             # Evita que se amontonen los textos
                title = "Biplot PCA - Individuos y Variables"
)
