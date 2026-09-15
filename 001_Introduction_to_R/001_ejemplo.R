#ejemplo de clase 1
A <- 2
B <- 4
C <- A + B

#ejemplo de un número decimal
A2 <- 3.1416
#ejemplo de un caracter
A3 <- "Bucaramanga"

#clase de objeto
class(A)
class(A2)

is.character(A2)
is.character(A3)
is.numeric(A3)

#vector
students <- c("Katherine","Natalia","Leonardo")
#indices
students[1]
students[2]
students[3]
students[4]
#dimensiones del vector
length(students)

#matrix
mdat <- matrix(
  c(1,2,3, 
    11,12,13), 
  nrow = 2,
  ncol = 3, 
  byrow = TRUE,
  dimnames = list(c("row1", "row2"),
                  c("C.1", "C.2", "C.3")))

#ver una posición de matriz
mdat[1,1] #fila,columna
mdat[1,1] #fila,columna


#cargar datos
?read.csv

points <- read.csv("D:/PROGRAMAS/Dropbox/SDM_2026_2/POINTS/occurrences_SDM.csv",
                   header=T)

#reto (plotear coordenadas)

#1 cargar datos
points <- read.csv("D:/PROGRAMAS/Dropbox/SDM_2026_2/POINTS/occurrences_SDM.csv",
                   header=T)
#2 comprobar datos de lon, lat
class(points$longitude)
class(points$latitude)

#3 plot
plot(points$longitude,points$latitude,col = "dark red",pch=16)



# Instalar si no lo tienes
# install.packages("leaflet")
# install.packages("sf")

library(leaflet)
mapa <- leaflet(points) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,      # ajusta al nombre real de tu columna de longitud
    lat = ~latitude,       # ajusta al nombre real de tu columna de latitud
    radius = 6,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.7,
    popup = ~as.character("Bactris gasipaes")  # opcional, si tienes una columna con etiquetas
  )

mapa


############################################################################
#programa

#1. cargar datos,
#2. revisar que sean long y lat numericos
#3. Si no son numericos convertirlos
#4. mapa interactivo
#5. pop up (long, lat)
#6. Guardar en un HTML

# cargar librerias
library(htmlwidgets)
library(leaflet)
library(dplyr)
map_interactive_points <- function(ruta_input,ruta_salida){
  
  #1. cargar datos
  points <- read.csv(ruta_input,
                     header=T)
  
  #2. revisar que sean long y lat numericos
  A <- class(points$longitude)
  B <- class(points$latitude)
  #3. Si no son numericos convertirlos
  #long
  if(A!="numeric"){
    points$longitude <- as.numeric(points$longitude)
  }
  #lat
  if(B!="numeric"){
    points$latitude <- as.numeric(points$latitude)
  }
  
  #4. mapa interactivo
  
  mapa <- leaflet(points) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude,      # ajusta al nombre real de tu columna de longitud
      lat = ~latitude,       # ajusta al nombre real de tu columna de latitud
      radius = 6,
      color = "red",
      stroke = TRUE,
      fillOpacity = 0.7,
      #5. pop up (long, lat)
      popup = ~as.character(paste(points$longitude,"/",points$latitude))  # opcional, si tienes una columna con etiquetas
    )
    #6. Guardar en un HTML
  saveWidget(mapa, file = ruta_salida, selfcontained = TRUE)
  
   return(mapa)
}

ruta_input <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/POINTS/occurrences_SDM.csv"
ruta_salida <- "D:/PROGRAMAS/Dropbox/SDM_2026_2/POINTS/Bactris.html"

x <- map_interactive_points(ruta_input,ruta_salida)
x


sum_propia <- function(A,B,C){
  #OPCION 1
  # resultado <- sum(A,B,C)
  #OPCION 2
   resultado <- A + B + C
   #OPCION 3
  # r1 <- A + B
  # resultado <- r1 + C
  # resultado <- sum(c(A,B,C))
  # print(resultado)
  return(resultado)
}

A <- 4
B <- 5
C <- 6

sum_propia(A,B,C)


A <- c(4,4,4,4,4)
B <- c(5,5,5,5,5)
C <- c(6,6,6,6,6)
sum_propia(A,B,C)




tarea <- function(points){
  #. nrow print
  print()
  #promedio lon
  print()
  #promedio lat
  
  return(tapply(points$countryCode,points$countryCode,length))
}