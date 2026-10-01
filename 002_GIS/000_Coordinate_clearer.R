library(rgbif)
library(CoordinateCleaner)
library(dplyr)
library(countrycode)
library(ggplot2)
library(sf)

data_gbif <- occ_search(scientificName = "Bactris gasipaes",limit=50000,fields="minimal")
View(data_gbif$data)

dat <- data_gbif$data
dat <- dat[!is.na(dat$decimalLongitude),]
#plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = dat,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()


################################################################################
flags <- CoordinateCleaner::clean_coordinates(
  x = dat,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  countries = NULL,
  capitals_rad = 5000,
  inst_rad = 5000,
  centroids_rad = 5000,
  species = "scientificName",
  tests = c(
    "capitals",
    "centroids",
    "equal",
    "gbif",
    "institutions",
    "outliers",
    "seas",
    "zeros"
  )
)


dat_cl <- dat[flags$.summary, ]   # Good records: passed all tests
dat_fl <- dat[!flags$.summary, ]  # Flagged records: failed one or more tests


#plotear resultados
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = dat_cl,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()

write.csv(dat_cl,"D:/REPO_GITHUB/SDM_2026_2/002_GIS/puntos_clear_Bg.csv",row.names = F)


################################################################################
data_clase <- read.csv("D:/REPO_GITHUB/SDM_2026_2/002_GIS/PUNTOS_CLASE.csv")


wm <- borders("world", colour = "gray50", fill = "gray50")

library(ggplot2)
library(maps) # Asegúrate de tener este paquete cargado

# Obtener los datos del mapa mundial
world_map <- map_data("world")

x_plot <- ggplot() +
  coord_fixed() +
  # Usar geom_polygon en lugar de borders() para evitar conflictos con los paneles
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray50", 
               colour = "gray50") +
  geom_point(data = data_clase,
             aes(x = longitud, y = latitud),
             colour = "darkred",
             size = 0.5) +
  facet_wrap(~ especie) +  # Reemplaza 'species' por el nombre de tu columna de especie
  theme_bw()

x_plot


flags <- CoordinateCleaner::clean_coordinates(
  x = data_clase,
  lon = "longitud",
  lat = "latitud",
  countries = NULL,
  capitals_rad = 5000,
  inst_rad = 5000,
  centroids_rad = 5000,
  species = "especie",
  tests = c(
    "capitals",
    "centroids",
    "equal",
    "gbif",
    "institutions",
    "outliers",
    "seas",
    "zeros"
  )
)


dat_cl <- data_clase[flags$.summary, ]   # Good records: passed all tests
dat_fl <- data_clase[!flags$.summary, ]  # Flagged records: failed one or more tests

write.csv(dat_cl,"D:/REPO_GITHUB/SDM_2026_2/002_GIS/PUNTOS_CLASE_COORD_CLEAR.csv",row.names = F)
