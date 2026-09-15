library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

# 1. Parámetros de entrada ----------------------------------------------------
dir_base <- "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE"

# Lista dinámica de especies (puedes agregar N especies)
especies <- c(
  "Juglans neotropica",
  "Magnolia hernandezii",
  "Quercus humboldtii"
)

# 2. Construcción automática de rutas ----------------------------------------
rutas_prob <- file.path(dir_base, "rasters", paste0(especies, "_suitability_1.tif"))
rutas_bin  <- file.path(dir_base, "rasters", paste0(especies, "_suitability_binary_1.tif"))
rutas_occs <- file.path(dir_base, "data", paste0(especies, ".shp"))

# 3. Cargar mapa base de países -----------------------------------------------
world <- world(path = tempdir()) |> st_as_sf()

# 4. Función para generar los dos paneles por especie -------------------------
crear_paneles_especie <- function(ruta_p, ruta_b, ruta_occ) {
  
  # Cargar rasters
  r_prob <- terra::rast(ruta_p)
  r_bin  <- terra::rast(ruta_b)
  r_bin[r_bin == 0] <- NA
  
  bbox_raster <- st_bbox(r_prob)
  
  # Cargar shapefile de ocurrencias y homogeneizar CRS
  occs <- st_read(ruta_occ, quiet = TRUE)
  df_occ <- st_transform(occs, crs = st_crs(r_prob))
  
  # Cortar países y puntos al bbox exacto del raster
  sf_use_s2(FALSE)
  latam_esp   <- world |> st_make_valid() |> st_crop(bbox_raster)
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

# 5. Iteración dinámica sobre N especies --------------------------------------
lista_graficos <- list()

for (i in seq_along(especies)) {
  paneles <- crear_paneles_especie(rutas_prob[i], rutas_bin[i], rutas_occs[i])
  lista_graficos <- c(lista_graficos, paneles)
}

# 6. Unir paneles y asignar etiquetas dinámicas (A, B, C...) -------------------
mapa_final <- wrap_plots(lista_graficos, ncol = 2) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

# 7. Guardar a 1000 DPI con alto proporcional al número de especies -----------
n_especies <- length(especies)

ggsave(
  filename = file.path(dir_base, "maps", "Summary.png"),
  plot = mapa_final,
  width = 12,
  height = n_especies * 5, # Asigna 5 pulgadas de alto por cada especie (fila)
  dpi = 1000,
  units = "in"
)