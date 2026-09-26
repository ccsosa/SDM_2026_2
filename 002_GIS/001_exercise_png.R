library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc <- nc[c(5, 9:15)]

nc2 <- nc[nc$NAME %in%
            c("Northampton","Warren","Rockingham"),]

##plot shapefile
plot(st_geometry(nc))
plot(st_geometry(nc2))

#plot panel
par(mfrow=c(1,2))
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc2),col="green")

#plot panel NC + condados
par(mfrow=c(1,2))
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc2),col="green",add=T)

#plot panel NC + condados
#(Si no son los condados que yo quiero, negro)
nc3 <- nc[!nc$NAME %in%
            c("Northampton","Warren","Rockingham"),]

par(mfrow=c(1,2))
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc2),col="green",add=T)
plot(st_geometry(nc3),col="black",add=T)

###guardar pdf
setwd("D:/")
pdf("test_sf.pdf",height = 6,width = 9)
par(mfrow=c(1,2))
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc2),col="green",add=T)
plot(st_geometry(nc3),col="black",add=T)
dev.off()

###guardar png

setwd("D:/")
png("test_sf.png",height = 6,width = 9,units="in",res=300)
par(mfrow=c(1,2))
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc),col="gray")
plot(st_geometry(nc2),col="green",add=T)
plot(st_geometry(nc3),col="black",add=T)
dev.off()


p1 <- st_point(c(-8.395835, 43.37087))
p2 <- st_point(c(-7.555851, 43.01208))
p3 <- st_point(c(-7.864641, 42.34001))
p4 <- st_point(c(-8.648053, 42.43362))
sfc <- st_sfc(list(p1, p2, p3, p4))
cprov <- st_sf(names = c('Coruña (A)', 'Lugo', 'Ourense', 'Pontevedra'),
               geom = sfc)
# 1. Graficar la geometría de los puntos
png("test_points.png",height = 6,width = 9,units="in",res=1000)
par(mfrow=c(1,1))
plot(st_geometry(cprov), pch = 19, col = "red", cex = 10, 
     main = "Provincias")
# 2. Añadir los nombres usando las coordenadas de los puntos
text(st_coordinates(cprov), labels = cprov$names, pos = 1, cex = 0.8, font = 9)
dev.off()


