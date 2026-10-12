library(ENMeval)
# library(ggplot2)
library(blockCV)
library(terra)
library(dismo)
# library(sdm)
library(geodata)
# library(spThin)
################################################################################
#define dir
dir <- "D:/REPO_GITHUB/SDM_2026_2/003_Machine_learning_R"

#load rasters
raster_dir <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/Raster_S-America"
env_pred <- terra::rast(list.files(raster_dir,pattern = "\\.tif$",full.names = T))

rm_env <- which(names(env_pred) %in% c("bio_8",
                                       "bio_9", 
                                       "bio_10", 
                                       "bio_11", 
                                       "bio_13", 
                                       "bio_14", 
                                       "bio_18", 
                                       "bio_19"))
# remove some climatic variables (problematic)
env_pred <- env_pred[[-rm_env]]
bio1 <- env_pred[[which(names(env_pred)=="bio_1")]]
################################################################################
#World´s shapefile 
world_shp <- geodata::world(resolution = 4,level=0,path=dir)
################################################################################
#calling raster of Coffee region!
raster_dir <- "D:/REPO_GITHUB/SDM_2026_2/003_Machine_learning_R/INPUTS/raster_EJE_CAFETERO"
env_pred_proj_area <- terra::rast(list.files(raster_dir,pattern = "\\.tif$",full.names = T))
names(env_pred_proj_area) <- sub("\\.tif","",names(env_pred_proj_area))
rm_env <- which(names(env_pred_proj_area) %in% c(
                                       "aridity",
                                       "bio_8",
                                       "bio_9", 
                                       "bio_10", 
                                       "bio_11", 
                                       "bio_13", 
                                       "bio_14", 
                                       "bio_18", 
                                       "bio_19"))
# remove some climatic variables (problematic)
env_pred_proj_area <- env_pred_proj_area[[-rm_env]]
################################################################################
#load occurrences
presence_original <- read.csv(paste0(dir,"/","INPUTS","/","PUNTOS_CLASE_COORD_CLEAR.csv"))
#unique species
unique(presence_original$especie)
#subset to B. gasipaes
presence_original <- presence_original[which(presence_original$especie=="Bactris gasipaes"),]
#remove duplicated coords
presence_original <- presence_original[!duplicated(presence_original[, c("longitud", "latitud")]),]
presence <- presence_original
#convert presence to point shapefile
v <- vect(presence, geom = c("longitud", "latitud"), crs = "EPSG:4326")
#remove presence below 5 km
v_thinned <- terra::thin(v, 5, unit = "km")
#reconvert to dataframe
presence <- as.data.frame(v_thinned, geom = "XY")
#convert x to y to lon and lat
names(presence)[names(presence) == "x"] <- "lon"
names(presence)[names(presence) == "y"] <- "lat"
################################################################################
# diagnosis
par(mfrow=c(1,2))
plot(world_shp, xlim = c(min(presence_original$longitud)-5, max(presence_original$longitud)+5), 
     ylim = c(min(presence_original$latitud)-5, max(presence_original$latitud)+5), 
     col = "gray90", border = "gray70")
points(presence_original[, c("longitud", "latitud")], col = "blue", pch = 16, cex = 0.8)

plot(world_shp, xlim = c(min(presence$lon)-5, max(presence$lon)+5), 
     ylim = c(min(presence$lat)-5, max(presence$lat)+5), 
     col = "gray90", border = "gray70")
points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.8);par(mfrow=c(1,1))
################################################################################
#creating buffer concave hull + buffer
occ_sp <- vect(presence, geom = c("lon", "lat"), crs = "EPSG:4326")
# calculate distance of longest axis of presence points
largedist <- max(as.vector(terra::distance(occ_sp)), na.rm = T)
# define hull around presence points
hull <- terra::hull(occ_sp,type="concave_ratio")
# 10% longest axis distance buffer # TF: changed to 20%
hull_ext <- terra::buffer(hull, width = 0.10 * largedist)
hullRaster <- terra::rasterize(hull_ext, env_pred[[1]], field = 1)
hullRaster <- hullRaster
hullRaster <- terra::mask(hullRaster, bio1)
################################################################################
#background
# #option 1(random)
set.seed(42)
bg_pts_1 <- terra::spatSample(hullRaster, size = 10000, method = "random", xy = TRUE, na.rm = TRUE)
bg_coords_1 <- bg_pts_1[, c("x", "y")]
#convert presence to point shapefile
v <- vect(bg_pts_1, geom = c("x", "y"), crs = "EPSG:4326")
#remove presence below 5 km
v_thinned <- terra::thin(v, 5, unit = "km")
#reconvert to dataframe
bg_coords_1 <- as.data.frame(v_thinned, geom = "XY")
################################################################################
# prepare presence and background data for the spatial block cross-validation
p <- presence[,c("lon","lat")]
p$type = 1
a <- bg_coords_1[,c(2,3)]
colnames(a) <- c("lon","lat")
a$type = 0
dat <- rbind(p,a)

# convert to sf points
dat <- sf::st_as_sf(dat, coords = c("lon", "lat"), crs = terra::crs(bio1))
################################################################################
#remove points outside the predictors
# Obtener la extensión exacta del ráster bio1
ext_raster <- terra::ext(bio1)
# Filtrar los puntos (sf) para conservar SOLO los que están dentro del extent del ráster
coords_sf <- sf::st_coordinates(dat)
keep <- coords_sf[, 1] >= ext_raster[1] & coords_sf[, 1] <= ext_raster[2] & 
  coords_sf[, 2] >= ext_raster[3] & coords_sf[, 2] <= ext_raster[4]

dat <- dat[keep, ]
#spatial validation
cv_spatial <- cv_spatial(dat,column="type",k=5,hexagon =F,r=bio1)
all_folds <- cv_spatial$folds_ids
################################################################################
# 6. CALIBRACIÓN CON ENMEVAL
# Extraer matrices separadas de presencias y background para ENMevaluate
data <- as.data.frame(sf::st_coordinates(dat))
data$type <- as.numeric(dat$type)

#only get occs and background
occs_mat <- data[which(data$type==1), c("X", "Y")]
bg_mat <- data[which(data$type==0), c("X", "Y")]

occs_grp <- all_folds[dat$type == 1]
bg_grp   <- all_folds[dat$type == 0]

#creating list of presence - absence for ENMEval
user_groups <- list(
  occs.grp = occs_grp, 
  bg.grp = bg_grp
)

#ENM Eval calibration
e_model <- ENMevaluate(
  occs = occs_mat,
  envs = env_pred,
  bg = bg_mat,
  algorithm = "maxnet",
  partitions = "user",
  user.grp = user_groups,
  tune.args = list(fc = c("L", "LQ", "H", "LQH"), rm = seq(1, 3, 1)),
  raster.preds = FALSE,
  parallel = F,
  numCores = 1,
  quiet=F,
  updateProgress = T
)
################################################################################
##getting best model based on AUC
bestmod <- which.max(e_model@results$auc.val.avg)
# get model evaluation metrics of this model
bestmod_eval <- e_model@results[bestmod,]
# bestmod_eval
################################################################################

# save model evaluation metriccs
setwd(dir)
write.csv(bestmod_eval, paste0("model_evaluation", ".csv"))
pr <- terra::predict(env_pred, e_model@models[[bestmod]], type = 'cloglog',na.rm=T)
# make predictions for the presence and background points
suit_pres <- terra::extract(x= pr, y = occs_mat)
suit_bg <- terra::extract(x= pr, y = bg_mat)

# evaluate predictive ability of model
suit_pres <- c(suit_pres[,2])
suit_bg <- c(suit_bg[,2])
ev <- dismo::evaluate(p = suit_pres, a = suit_bg)

# get possible thresholds to convert suitability to presence/absence
thr <- dismo::threshold(ev)

# add "10% omission" threshold
thr$thr_om <- quantile(suit_pres, 0.1, na.rm = TRUE)
#map on predictor areas
pr_thr <- pr > thr$thr_om
pr_thr*1
pr_thr[which(pr_thr[]==0)] <- NA
################################################################################
# 1. Obounding box for plot 
ext_proj <- terra::ext(pr_thr)
xlims_proj <- c(ext_proj[1], ext_proj[2])
ylims_proj <- c(ext_proj[3], ext_proj[4])
################################################################################
# 2. Plotting maxent model in area used for calibration
par(mfrow = c(1, 2))
# Map 1:  (cloglog)
plot(pr, main = "Cloglog", 
     xlim = xlims_proj, ylim = ylims_proj)
plot(world_shp, add = TRUE, border = "gray50", col = NA)
points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.6)

# Map 2: (Thresholded)
plot(pr_thr, main = "10% Omission", 
     xlim = xlims_proj, ylim = ylims_proj)
plot(world_shp, add = TRUE, border = "gray50", col = NA)
points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.6)

par(mfrow = c(1, 1))
################################################################################

################################################################################
#projecting to coffee region
pr_new_area <- terra::predict(env_pred_proj_area, e_model@models[[bestmod]], type = 'cloglog',na.rm=T)
# apply 10% omission threshold
pr_new_area_thr <- pr_new_area > thr$thr_om
pr_new_area_thr*1
pr_new_area_thr[which(pr_new_area_thr[]==0)] <- NA
################################################################################
# 1. Obounding box for plot 
ext_proj <- terra::ext(env_pred_proj_area)
xlims_proj <- c(ext_proj[1], ext_proj[2])
ylims_proj <- c(ext_proj[3], ext_proj[4])
################################################################################
# 2. Plotting maxent model in coffee region
par(mfrow = c(1, 2))
# Map 1:  (cloglog)
plot(pr_new_area, main = "Cloglog", 
     xlim = xlims_proj, ylim = ylims_proj)
plot(world_shp, add = TRUE, border = "gray50", col = NA)
points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.6)

# Map 2: (Thresholded)
plot(pr_new_area_thr, main = "10% Omission", 
     xlim = xlims_proj, ylim = ylims_proj)
plot(world_shp, add = TRUE, border = "gray50", col = NA)
points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.6)

par(mfrow = c(1, 1))
################################################################################
#saving all
save.image(paste0(dir,"/","model.RData"))
###
write.csv(thr, paste0("model_thresholds", ".csv"))
###
terra::writeRaster(pr,paste0(dir,"/","cloglog.tif"))
terra::writeRaster(pr_thr,paste0(dir,"/","cloglog_thr.tif"))
###
terra::writeRaster(pr_new_area,paste0(dir,"/","cloglog_Target_region.tif"))
terra::writeRaster(pr_new_area_thr,paste0(dir,"/","cloglog_thr_Target_region.tif"))

################################################################################
# #option 2 (avoid sampling  bias by regular)
# bg_pts_2 <- terra::spatSample(hullRaster, size = 5000, method = "regular", xy = TRUE, na.rm = TRUE)
# bg_coords_2 <- bg_pts_2[, c("x", "y")]
# #option 3 (bias sampling)
# # 1. Crear un buffer de exclusión de 20 km alrededor de cada presencia
# pres_buffer <- terra::buffer(occ_sp, width = 20000) 
# 
# # 2. Rasterizar el área de exclusión (asignando NA donde esté el buffer)
# exclusion_mask <- terra::rasterize(pres_buffer, env_pred[[1]], field = NA, background = 1)
# 
# # 3. Combinar el hullRaster con la máscara de exclusión
# hull_exclusive <- terra::mask(hullRaster, exclusion_mask)
# 
# # 4. Muestrear los puntos aleatorios fuera de la zona prohibida
# set.seed(42)
# bg_pts_3 <- terra::spatSample(hull_exclusive, size = 10000, method = "random", xy = TRUE, na.rm = TRUE)
# bg_coords_3 <- bg_pts_3[, c("x", "y")]
# 
# #plot
# # Configurar la ventana gráfica en 1 fila y 3 columnas
# par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))
# 
# # Definir límites geográficos comunes para que todas tengan el mismo zoom
# xlims <- c(min(presence$lon) - 5, max(presence$lon) + 5)
# ylims <- c(min(presence$lat) - 5, max(presence$lat) + 5)
# 
# # --- Opción 1: Background Aleatorio Simple ---
# plot(world_shp, xlim = xlims, ylim = ylims, col = "gray90", border = "gray70", 
#      main = "1. Aleatorio Simple")
# points(bg_coords_1, col = adjustcolor("blue", alpha.f = 0.3), pch = 16, cex = 0.4)
# points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.7)
# 
# # --- Opción 2: Background Regular / Malla ---
# plot(world_shp, xlim = xlims, ylim = ylims, col = "gray90", border = "gray70", 
#      main = "2. Regular (Malla)")
# points(bg_coords_2, col = adjustcolor("darkgreen", alpha.f = 0.4), pch = 16, cex = 0.4)
# points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.7)
# 
# # --- Opción 3: Background con Exclusión ---
# plot(world_shp, xlim = xlims, ylim = ylims, col = "gray90", border = "gray70", 
#      main = "3. Con Buffer de Exclusión")
# points(bg_coords_3, col = adjustcolor("purple", alpha.f = 0.3), pch = 16, cex = 0.4)
# points(presence[, c("lon", "lat")], col = "red", pch = 16, cex = 0.7)
# 
# # Restaurar la ventana gráfica por defecto a 1x1
# par(mfrow = c(1, 1))