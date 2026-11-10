library(maxnet)
library(terra)
library(ggplot2)

out_dir <- "D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future"
cc_dir  <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/RASTERS/future"
MODELS  <- c("ACCESS-CM2", "MPI-ESM1-2-HR", "MRI-ESM2-0")
SSPS    <- c("245", "370")      # "126", "245", "370", "585"
TIMES   <- c("2041-2060")       # "2021-2040","2041-2060", "2061-2080", "2081-2100"

model <- readRDS("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model.RDS")
thr   <- read.csv("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/model_thresholds.csv")
phr_current <- terra::rast("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/current/cloglog_thr_Target_region.tif")

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
    writeRaster(SDM_GCM, file.path(ssp_dir, "gcm_binary.tif"), overwrite = TRUE)
    
    SDM_GCM_sum <- sum(SDM_GCM, na.rm = TRUE)
    writeRaster(SDM_GCM_sum, file.path(ssp_dir, "gcm_sum.tif"), overwrite = TRUE)
    
    SDM_GCM_conc <- SDM_GCM_sum
    SDM_GCM_conc[which(SDM_GCM_conc[] < 2)]  <- 0
    SDM_GCM_conc[which(SDM_GCM_conc[] >= 2)] <- 1
    SDM_GCM_conc[which(SDM_GCM_conc[] ==0)]  <- NA
    
    writeRaster(SDM_GCM_conc, file.path(ssp_dir, "gcm_consensus.tif"), overwrite = TRUE)
    
    SDM_GCM_conc[which(SDM_GCM_conc[] == 1)] <- 2
    
    changes <- subst(SDM_GCM_conc, NA, 0) - subst(phr_current, NA, 0)
    changes[which(changes[] ==0)]  <- NA
    
    names(changes) <- paste0("ssp", SSPS[j], "_changes_", TIMES[i])
    writeRaster(changes, file.path(ssp_dir, "changes.tif"), overwrite = TRUE)
    
    changes_list[[j]] <- changes
  }
  changes_list_times[[i]] <- terra::rast(changes_list)
}

changes_list_times <- rast(changes_list_times)
plot(changes_list_times)


areas <- terra::expanse(changes_list_times,unit="km",byValue=T,usenames=T)
df <- areas

df$ssp    <- sub("_changes.*", "", df$layer)
df$cambio <- factor(df$value, levels = c(-1, 1,2),
                    labels = c("Pérdida", "Estable","Nuevo"))
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
ggsave("D:/REPO_GITHUB/SDM_2026_2/007_Climate_change/Future/barplot_changes.png",
       p, width = 6, height = 4.5, dpi = 300)


#cambio relativo
SSP_CR <- list()
for(ssp in c("ssp245","ssp370")){
  print(ssp)
  # x <- df[df$value %in% c(-1,1),]
  x <- df[which(df$ssp== ssp),]
  # print(x)
  
  SSP_CR[[ssp]] <- data.frame(
    SSP=ssp,
    CR=
      (
        ((sum(x$area[x$value %in% c(2,1)])-
            sum(x$area[x$value %in% c(-1,1)])))
        /
          sum(x$area[x$value %in% c(-1,1)])
      )*100
  )  
}
SSP_CR <- do.call(rbind,SSP_CR)