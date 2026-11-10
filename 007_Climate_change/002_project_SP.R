library(maxnet)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)


out_dir <- "D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future"
cc_dir  <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/RASTERS/future"
MODELS  <- c("ACCESS-CM2", "MPI-ESM1-2-HR", "MRI-ESM2-0")
SSPS    <- c("245", "370")      # "126", "245", "370", "585"
TIMES   <- c("2041-2060")       # "2021-2040","2041-2060", "2061-2080", "2081-2100"

model <- readRDS("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model_SP.RDS")
# saveRDS(model,"D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model_SP.RDS")
thr   <- read.csv("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model_thresholds_SP.csv")
# x <- rast("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/cloglog_Target_region_SP.tif")
# x <- x > thr$thr_om
# x[which(x[]==0)] <- NA
# write.csv(thr,"D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model_thresholds_SP.csv")
phr_current <- terra::rast("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/cloglog_thr_Target_region_SP.tif")
# writeRaster(x,"D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/cloglog_thr_Target_region_SP.tif",overwrite=T)
changes_list_times <- list()

for(i in seq_along(TIMES)){
  
  changes_list <- list()
  for(j in seq_along(SSPS)){
    
    # carpeta: Future/ssp245/2041-2060
    ssp_dir <- file.path(out_dir, paste0("ssp", SSPS[j]), TIMES[i])
    dir.create(ssp_dir, recursive = TRUE, showWarnings = FALSE)
    
    SDM_GCM <- list()
    for(k in seq_along(MODELS)){
      clim_layers <- terra::rast(file.path(cc_dir,
                                           paste0(MODELS[k], "_ssp", SSPS[j], "_", TIMES[i], "_bioc.tif")))
      names(clim_layers) <- gsub("^bio", "bio_", names(clim_layers))
      
      pr_new_area <- terra::predict(clim_layers, model, type = 'cloglog', na.rm = TRUE)
      
      # apply 10% omission threshold
      pr_new_area_thr <- (pr_new_area > thr$thr_om) * 1
      pr_new_area_thr[which(pr_new_area_thr[] == 0)] <- NA
      names(pr_new_area_thr) <- MODELS[k]
      SDM_GCM[[k]] <- pr_new_area_thr
    }
    SDM_GCM <- rast(SDM_GCM)
    writeRaster(SDM_GCM, file.path(ssp_dir, "gcm_binary_SP.tif"), overwrite = TRUE)
    
    SDM_GCM_sum <- sum(SDM_GCM, na.rm = TRUE)
    writeRaster(SDM_GCM_sum, file.path(ssp_dir, "gcm_sum_SP.tif"), overwrite = TRUE)
    
    SDM_GCM_conc <- SDM_GCM_sum
    SDM_GCM_conc[which(SDM_GCM_conc[] < length(MODELS))]  <- 0
    SDM_GCM_conc[which(SDM_GCM_conc[] == length(MODELS))] <- 1
    writeRaster(SDM_GCM_conc, file.path(ssp_dir, "gcm_consensus_SP.tif"), overwrite = TRUE)
    
    SDM_GCM_conc[which(SDM_GCM_conc[] == 1)] <- 2
    
    changes <- SDM_GCM_conc - phr_current
    names(changes) <- paste0("ssp", SSPS[j], "_changes_", TIMES[i])
    writeRaster(changes, file.path(ssp_dir, "changes_SP.tif"), overwrite = TRUE)
    
    changes_list[[j]] <- changes
  }
  changes_list_times[[i]] <- terra::rast(changes_list)
}

changes_list_times <- rast(changes_list_times)
plot(changes_list_times)


areas <- terra::expanse(changes_list_times,unit="km",byValue=T,usenames=T)
df <- areas

df$ssp    <- sub("_changes.*", "", df$layer)
df$cambio <- factor(df$value, levels = c(-1, 1),
                    labels = c("Pérdida", "Estable"))
df$area_plot <- ifelse(df$value == -1, -df$area, df$area)

p <- ggplot(df, aes(x = cambio, y = area_plot, fill = ssp)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = format(round(area_plot, 1), big.mark = ","),
                vjust = ifelse(area_plot < 0, 1.3, -0.4)),
            position = position_dodge(width = 0.8), size = 3.5) +
  scale_fill_manual(values = c(ssp245 = "#4C9F70", ssp370 = "#D9822B"),
                    labels = c("SSP2-4.5", "SSP3-7.0")) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
  labs(x = NULL, y = expression("Área (km"^2*")"), fill = NULL,
       title = "Cambio en idoneidad 2041-2060 vs. actual") +
  theme_classic(base_size = 13)

p
ggsave("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future/barplot_changes_SP.png",
       p, width = 6, height = 4.5, dpi = 300)

################################################################################
#por municipio


shp_adm2 <- terra::vect("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/INPUTS/COL_adm2.shp")
shp_adm2 <- shp_adm2[shp_adm2$NAME_1 %in% c("Caldas","Quindío","Risaralda"),]
# writeVector(shp_adm2, file.path(out_dir, "COL_adm2.shp"), overwrite = TRUE)
########################
shp <- shp_adm2
r   <- terra::crop(changes_list_times, shp)

# área por municipio y clase: -1 pérdida, 1 estable, 2 ganancia
res_list <- lapply(names(r), function(nm){
  lay <- r[[nm]]
  a   <- terra::cellSize(lay, unit = "km")
  z   <- sapply(c(loss = -1, stable = 1, gain = 2), function(v){
    terra::zonal(a * (lay == v), shp, fun = "sum", na.rm = TRUE)[, 1]
  })
  data.frame(mpio = paste0(shp$NAME_2, " (", shp$NAME_1, ")"),
             ssp  = sub("_changes.*", "", nm),
             loss = -z[, "loss"], stable = z[, "stable"], gain = z[, "gain"])
})
mun <- bind_rows(res_list)
write.csv(mun, "D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future/changes_by_municipality_SP.csv",
          row.names = FALSE)

# ref <- "ssp245"   # SSP usado para ordenar
# ord <- mun %>% filter(ssp == ref) %>% arrange(gain) %>% pull(mpio)
# 
# mun_long <- mun %>%
#   pivot_longer(c(stable, gain), names_to = "tipo", values_to = "area") %>%
#   mutate(mpio = factor(mpio, levels = ord),
#          tipo = factor(tipo, levels = c("stable", "gain"), labels = c("Estable", "Ganancia")))
#  
# mun_long
# p_mun <- ggplot(mun_long, aes(x = mpio, y = area, fill = tipo)) +
#   geom_col(width = 0.75) +
#   coord_flip() +
#   facet_wrap(~ ssp, labeller = as_labeller(c(ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0"))) +
#   scale_fill_manual(values = c("Estable" = "#4C9F70", "Ganancia" = "#2E86C1")) +
#   labs(x = NULL, y = expression("Área (km"^2*")"), fill = NULL,
#        title = "Idoneidad estable y ganada por municipio, 2041-2060 vs. actual") +
#   theme_classic(base_size = 10)
# 
# ggsave("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future/barplot_changes_municipios_SP.png",
#        p_mun, width = 10, height = max(6, 0.17 * length(ord)), dpi = 300, limitsize = FALSE)
# 
# # solo los municipios con mayor ganancia
# top_n <- 25
# p_top <- p_mun %+% filter(mun_long, mpio %in% tail(ord, top_n))
# ggsave("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future/barplot_changes_municipios_top_SP.png",
#        p_top, width = 10, height = 8, dpi = 300)
