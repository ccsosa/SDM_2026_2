library(ENMeval)
# library(ggplot2)
library(blockCV)
library(terra)
library(dismo)
# library(sdm)
library(geodata)
library(usdm)
library(randomForest)
#https://rspatial.org/raster/sdm/raster_SDM.pdf
# library(spThin)
library(kernlab)
################################################################################
library(maptools)
data(wrld_simpl)
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
presence_vect <- terra::vect(presence)
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
bg_coords_1_vect <- terra::vect(bg_coords_1)
################################################################################
#remove VIF >10
ext_p <- terra::extract(x=env_pred,
                        y=presence_vect,ID=FALSE)
bg_coords_ext_p <- terra::extract(x=env_pred,
                                  y=bg_coords_1_vect,ID=FALSE)
vars_retained <- usdm::vifstep(ext_p)
vars_retained <- vars_retained@variables

ext_p <- ext_p[,vars_retained]
bg_coords_ext_p <- bg_coords_ext_p[,vars_retained]
ext_p$pb <- 1
bg_coords_ext_p$pb <- 0
pa <- rbind(ext_p,bg_coords_ext_p)
################################################################################
nr <- nrow(ext_p)
set.seed(9)                                   # CAMBIO: semilla para reproducibilidad
s <- sample(nr, 0.25 * nr)
pres_train <- ext_p[-s, ]
pres_test <- ext_p[s, ]

nr <- nrow(bg_coords_ext_p)
set.seed(9)
s <- sample(nr, 0.25 * nr)
back_train <- bg_coords_ext_p[-s, ]
back_test <- bg_coords_ext_p[s, ]

################################################################################
envtrain <- rbind(pres_train, back_train)
envtrain <- na.omit(envtrain)
resp  <- colnames(envtrain)[ncol(envtrain)]
preds <- colnames(envtrain)[-ncol(envtrain)]
################################################################################
#preparing formula

fmla <- as.formula(paste(resp, "~", paste(preds, collapse = " + ")))
################################################################################
###Random forest
rf1 <- randomForest(fmla, data=envtrain)
erf <- evaluate(pres_test, back_test, rf1)
pr_RF <- predict(env_pred, rf1)
# evaluate predictive ability of model
suit_pres <- terra::extract(x = pr_RF, presence[row.names(pres_test), c("lon","lat")], ID = FALSE)[, 1]
suit_bg   <- terra::extract(x = pr_RF, bg_coords_1[row.names(back_test), c("x","y")], ID = FALSE)[, 1]
ev_RF <- dismo::evaluate(p = suit_pres, a = suit_bg)

# get possible thresholds to convert suitability to presence/absence
thr_RF <- dismo::threshold(ev_RF)             # CAMBIO: antes ev

# add "10% omission" threshold
thr_RF$thr_om <- quantile(suit_pres, 0.1, na.rm = TRUE)

#get thresholded
pr_thr_RF <- pr_RF > thr_RF$thr_om
pr_thr_RF <- pr_thr_RF*1
pr_thr_RF[which(pr_thr_RF[]==0)] <- NA
#map on predictor areas

par(mfrow=c(1,2))
plot(pr_RF, main='Random Forest, regression')
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pr_thr_RF, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presence[row.names(pres_train), c("lon","lat")], pch='+')
points(bg_coords_1[row.names(back_train), c("x","y")], pch='-', cex=0.25)


################################################################################
##glm
gm1 <- glm(fmla,
           family = binomial(link = "logit"), data=envtrain)
summary(gm1)
coef(gm1)

eglm <- evaluate(pres_test, back_test, gm1)
pr_glm <- predict(env_pred, gm1, type = "response")   # CAMBIO: probabilidades, no escala link
# make predictions for the presence and background points
# evaluate predictive ability of model
suit_pres <- terra::extract(x = pr_glm, presence[row.names(pres_test), c("lon","lat")], ID = FALSE)[, 1]
suit_bg   <- terra::extract(x = pr_glm, bg_coords_1[row.names(back_test), c("x","y")], ID = FALSE)[, 1]
ev_glm <- dismo::evaluate(p = suit_pres, a = suit_bg)

# get possible thresholds to convert suitability to presence/absence
thr_glm <- dismo::threshold(ev_glm)           # CAMBIO: antes ev

# add "10% omission" threshold
thr_glm$thr_om <- quantile(suit_pres, 0.1, na.rm = TRUE)


#get thresholded
pr_thr_glm <- pr_glm > thr_glm$thr_om
pr_thr_glm <- pr_thr_glm*1
pr_thr_glm[which(pr_thr_glm[]==0)] <- NA

#map on predictor areas
par(mfrow=c(1,2))
plot(pr_glm, main='Logistic regression')
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pr_thr_glm, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presence[row.names(pres_train), c("lon","lat")], pch='+')
points(bg_coords_1[row.names(back_train), c("x","y")], pch='-', cex=0.25)

################################################################################
##svm
svm <- ksvm(fmla, data=envtrain)
# esvm <- evaluate(pres_test, back_test, svm)
pr_svm <- predict(env_pred, svm,na.rm=T)
# make predictions for the presence and background points
# evaluate predictive ability of model
suit_pres <- terra::extract(x = pr_svm, presence[row.names(pres_test), c("lon","lat")], ID = FALSE)[, 1]
suit_bg   <- terra::extract(x = pr_svm, bg_coords_1[row.names(back_test), c("x","y")], ID = FALSE)[, 1]
esvm <- dismo::evaluate(p = suit_pres, a = suit_bg)

# get possible thresholds to convert suitability to presence/absence
thr_svm <- dismo::threshold(esvm)             # CAMBIO: antes ev

# add "10% omission" threshold
thr_svm$thr_om <- quantile(suit_pres, 0.1, na.rm = TRUE)


#get thresholded
pr_thr_svm <- pr_svm > thr_svm$thr_om
pr_thr_svm <- pr_thr_svm*1
pr_thr_svm[which(pr_thr_svm[]==0)] <- NA

#map on predictor areas
par(mfrow=c(1,2))
plot(pr_svm, main='Support vector machine')
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pr_thr_svm, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presence[row.names(pres_train), c("lon","lat")], pch='+')
points(bg_coords_1[row.names(back_train), c("x","y")], pch='-', cex=0.25)

################################################################################
#ensemble
models <- c(pr_RF,pr_glm,pr_svm)
names(models) <- c("rf","glm", "svm")
plot(models)
m <- median(models,na.rm=T)
# plot(m, main='median score')
suit_pres <- terra::extract(x = m, presence[row.names(pres_test), c("lon","lat")], ID = FALSE)[, 1]
suit_bg   <- terra::extract(x = m, bg_coords_1[row.names(back_test), c("x","y")], ID = FALSE)[, 1]
e <- dismo::evaluate(p = suit_pres, a = suit_bg)

# get possible thresholds to convert suitability to presence/absence
thr <- dismo::threshold(e)                    # CAMBIO: antes ev

# add "10% omission" threshold
thr$thr_om <- quantile(suit_pres, 0.1, na.rm = TRUE)


#get thresholded
pr_thr_median <- m > thr$thr_om               # CAMBIO: antes pr (no existe)
pr_thr_median <- pr_thr_median*1
pr_thr_median[which(pr_thr_median[]==0)] <- NA

#map on predictor areas
par(mfrow=c(1,2))
plot(m, main='Ensemble (Median)')             # CAMBIO: antes pr
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pr_thr_median, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presence[row.names(pres_train), c("lon","lat")], pch='+')
points(bg_coords_1[row.names(back_train), c("x","y")], pch='-', cex=0.25)
################################################################################
#ensemble (thresholded)
models_thr <- c(pr_thr_RF,pr_thr_glm,pr_thr_svm)
names(models_thr) <- c("rf","glm", "svm")
plot(models_thr)
m_sum <- sum(models_thr,na.rm=T)
par(mfrow=c(1,1))
plot(m_sum, main='Ensemble (Number of models)')
plot(wrld_simpl, add=TRUE, border='dark grey')
################################################################################
#compare metrics
evs <- list(RF = ev_RF, GLM = ev_glm, svm = esvm,ensemble = e)
thrs <- list(RF = thr_RF, GLM = thr_glm, svm = thr_svm,ensemble =thr)

# Resumen global + umbrales
resumen <- do.call(rbind, lapply(names(evs), function(m) {
  e   <- evs[[m]]
  thr <-  thrs[[m]]
  data.frame(model = m, 
             AUC = e@auc,
             COR = e@cor, 
             np = e@np, 
             na = e@na,
             no_omission = thr$no_omission,
             spec_sens   = thr$spec_sens,
             kappa_max   = thr$kappa,
             OR10 = thr$thr_om
  )
}))
resumen