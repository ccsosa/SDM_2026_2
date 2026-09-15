library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

# 1. Cargar rasters ------------------------------------------------------------
r_prob <- terra::rast("D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Juglans neotropica_suitability_1.tif")
r_binario <- terra::rast("D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/rasters/Juglans neotropica_suitability_binary_1.tif")

# Mantener solo los valores 1 (convertir 0 a NA)
r_binario[r_binario == 0] <- NA

#2. occs
  occs <- sf::st_read("D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/data/Juglans neotropica.shp")
df_occ <- st_as_sf(occs, coords = c("longitude", "latitude"), crs = st_crs(r_prob))
# 3. Vector de países ---------------------------------------------------------
world <- world(path = tempdir()) |> st_as_sf()
sf_use_s2(FALSE)
latam_sub <- world |> st_make_valid() |> st_crop(st_bbox(r_prob))
sf_use_s2(TRUE)
bbox_raster <- st_bbox(r_prob)
df_occ_crop <- st_crop(df_occ, bbox_raster)
# 4. Panel A: Probabilidades --------------------------------------------------
p_a <- ggplot() +
  geom_spatraster(data = r_prob) +
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", name = "") +
  geom_sf(data = latam_sub, fill = NA, red = "black", linewidth = 0.3) +
  geom_sf(data = df_occ_crop, color = "red", fill = "red", shape = 21, size = 1.2, stroke = 0.3) + # Capa de ocurrencias
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

# 5. Panel B: Presencia Binaria -----------------------------------------------
p_b <- ggplot() +
  geom_spatraster(data = r_binario) +
  scale_fill_gradient(
    low = "darkgreen", 
    high = "darkgreen", 
    na.value = "transparent", 
    breaks = 1
  ) +
  geom_sf(data = latam_sub, fill = NA, color = "black", linewidth = 0.3) +
  coord_sf(expand = FALSE) +
  guides(fill = "none") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

# 6. Unir paneles con etiquetas A y B -----------------------------------------
mapa_final <- p_a + p_b + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

# Visualizar
mapa_final

# 7. Guardar en alta resolución (1000 DPI) ------------------------------------
ggsave(
  filename = "D:/REPO_GITHUB/SDM_2026_2/002_GIS/SDM_CASE/maps/J_neotropica.png",
  plot = mapa_final,
  width = 12,        # Ancho en pulgadas
  height = 7,        # Alto en pulgadas
  dpi = 1000,        # Resolución para publicación
  units = "in"
)