library(geodata)
library(terra)
library(parallel)

# ---- Parámetros ----
STEP    <- 5                         # grados entre puntos de muestreo de teselas
N_CORES <- 6                    # fijo; modificable
RES     <- 2.5                  # 10, 5, 2.5 (minutos) o 0.5 (= 30 s)
VAR     <- "bioc"               # "bioc", "tmin", "tmax", "prec"
MODELS  <- c("ACCESS-CM2", "MPI-ESM1-2-HR", "MRI-ESM2-0")
SSPS    <- c("245", "370")      # "126", "245", "370", "585"
TIMES   <- c("2041-2060")  # "2021-2040","2041-2060", "2061-2080", "2081-2100"
OUT_DIR <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/RASTERS"

RASTER_AREA <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/RASTERS/current/bio_1.tif"   # raster que define el área
ALINEAR <- T                     # TRUE = remuestrear a la grilla de tu raster
BORRAR_TILES <- TRUE                 # borra la tesela cruda tras recortar
RAW_DIR <- paste0(OUT_DIR,"/cmip6_raw")
OUT_DIR <- paste0(OUT_DIR,"/future")
# --------------------

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Extent del raster en lon/lat (WGS84), con buffer de ~1 celda
molde  <- rast(RASTER_AREA)
ext_ll <- ext(project(as.polygons(ext(molde), crs = crs(molde)), "EPSG:4326"))
buf <- 0.5 / 60
AREA <- c(xmin = xmin(ext_ll) - buf, xmax = xmax(ext_ll) + buf,
          ymin = ymin(ext_ll) - buf, ymax = ymax(ext_ll) + buf)

# Puntos que garantizan tocar todas las teselas del área (esquinas + malla)
lons <- unique(c(seq(AREA["xmin"], AREA["xmax"], by = STEP), AREA["xmax"]))
lats <- unique(c(seq(AREA["ymin"], AREA["ymax"], by = STEP), AREA["ymax"]))
PTS  <- expand.grid(lon = lons, lat = lats)

grid <- expand.grid(model = MODELS, ssp = SSPS, time = TIMES,
                    stringsAsFactors = FALSE)

procesar <- function(i) {
  library(terra); library(geodata)
  g <- grid[i, ]
  tryCatch({
    e <- ext(AREA["xmin"], AREA["xmax"], AREA["ymin"], AREA["ymax"])
    piezas <- list(); archivos <- character()
    vistos <- character()
    for (k in seq_len(nrow(PTS))) {
      t <- geodata::cmip6_tile(lon = PTS$lon[k], lat = PTS$lat[k],
                               model = g$model, ssp = g$ssp, time = g$time,
                               var = VAR, path = RAW_DIR)
      src <- sources(t)[1]
      if (src %in% vistos) next          # misma tesela ya procesada
      vistos <- c(vistos, src)
      archivos <- c(archivos, sources(t))
      piezas[[length(piezas) + 1]] <- crop(t, e, snap = "out")
    }
    r <- if (length(piezas) == 1) piezas[[1]] else merge(sprc(piezas))
    if (ALINEAR) {
      molde <- rast(RASTER_AREA)
      fact  <- round(mean(res(molde)) / mean(res(r)))
      if (fact > 1) r <- aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
      r <- project(r, molde, method = "bilinear")
      r <- mask(r, molde[[1]])
    }
    
    if (VAR == "bioc") names(r) <- paste0("bio", 1:19)
    f <- file.path(OUT_DIR, sprintf("%s_ssp%s_%s_%s.tif",
                                    g$model, g$ssp, g$time, VAR))
    writeRaster(r, f, overwrite = TRUE)
    if (BORRAR_TILES) file.remove(unique(archivos))
    "ok"
  }, error = function(e) conditionMessage(e))
}

cl <- makeCluster(N_CORES)
clusterExport(cl, c("grid", "VAR", "AREA", "PTS", "RASTER_AREA", "ALINEAR",
                    "BORRAR_TILES", "RAW_DIR", "OUT_DIR", "procesar"))
res <- parLapply(cl, seq_len(nrow(grid)), procesar)
stopCluster(cl)

grid$status <- unlist(res)
print(grid)