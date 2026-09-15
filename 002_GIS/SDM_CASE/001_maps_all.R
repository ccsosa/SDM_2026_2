library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

# 1. Definir rutas para las 4 especies ----------------------------------------
rutas_prob <- c(
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Juglans neotropica_suitability_1.tif",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Magnolia hernandezii_suitability_1.tif",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Quercus humboldtii_suitability_1.tif"
)

rutas_bin <- c(
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Juglans neotropica_suitability_binary_1.tif",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Magnolia hernandezii_suitability_binary_1.tif",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Quercus humboldtii_suitability_binary_1.tif"
)

rutas_occs <- c(
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/data/Juglans neotropica.shp",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/data/Magnolia hernandezii.shp",
  "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/data/Quercus humboldtii.shp"
)

# 2. Cargar mapa base de países -----------------------------------------------
world <- world(path = tempdir()) |> st_as_sf()

# 3. Función para generar los dos paneles respetando el extent propio ----------
crear_paneles_especie <- function(ruta_p, ruta_b, ruta_occ) {
  
  # Cargar rasters individuales
  r_prob <- terra::rast(ruta_p)
  r_bin  <- terra::rast(ruta_b)
  r_bin[r_bin == 0] <- NA
  
  # Bounding box de la especie
  bbox_raster <- st_bbox(r_prob)
  
  # Cargar y proyectar ocurrencias
  occs <- st_read(ruta_occ)
  df_occ <- st_as_sf(occs, coords = c("longitude", "latitude"), crs = st_crs(r_prob))
  
  # Cortar el vector de países y puntos ÚNICAMENTE al bbox de ESTA especie
  sf_use_s2(FALSE)
  latam_esp <- world |> 
    st_make_valid() |> 
    st_crop(bbox_raster)
  
  df_occ_crop <- st_crop(df_occ, bbox_raster)
  sf_use_s2(TRUE)
  
  # Panel Probabilidad + Ocurrencias
  p_prob <- ggplot() +
    geom_spatraster(data = r_prob) +
    scale_fill_viridis_c(option = "viridis", na.value = "transparent", name = "") +
    geom_sf(data = latam_esp, fill = NA, color = "black", linewidth = 0.3) +
    geom_sf(data = df_occ_crop, color = "red", fill = "red", shape = 21, size = 1.2, stroke = 0.3) +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
  
  # Panel Binario
  p_bin <- ggplot() +
    geom_spatraster(data = r_bin) +
    scale_fill_gradient(
      low = "darkgreen", 
      high = "darkgreen", 
      na.value = "transparent", 
      breaks = 1
    ) +
    geom_sf(data = latam_esp, fill = NA, color = "black", linewidth = 0.3) +
    coord_sf(expand = FALSE) +
    guides(fill = "none") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
  
  return(list(p_prob, p_bin))
}

# 4. Generar la lista con los 8 paneles ---------------------------------------
lista_graficos <- list()

for (i in 1:3) {
  paneles <- crear_paneles_especie(rutas_prob[i], rutas_bin[i], rutas_occs[i])
  lista_graficos <- c(lista_graficos, paneles)
}

# 5. Unir paneles (4 filas x 2 columnas, Etiquetas A a H) --------------------
mapa_final <- wrap_plots(lista_graficos, ncol = 2) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

# Visualizar
# mapa_final

# 6. Guardar a 1000 DPI --------------------------------------------------------
ggsave(
  filename = "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/maps/Summary.png",
  plot = mapa_final,
  width = 12,
  height = 20,
  dpi = 1000,
  units = "in"
)