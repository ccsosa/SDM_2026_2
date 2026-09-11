# R basics

# ------------------------------------------ #
# Types of values
# ------------------------------------------ #

myvar <- 2
myvar
class(myvar)

myvar2 <- "Camilo Mendez"
class(myvar2)

is(myvar)
is(myvar2)

# Not allowed
2var <- 3
first variable <- 1

# ------------------------------------------ #
# Vectors
# ------------------------------------------ #

empty_vct <- c()
logical_vct_one <- c(TRUE)
logical_vct_two <- c(T, F)
INTEGER_vct <- c(1, 2, 3, 4)
charctr_vct <- c("Chrystian","Andres","Wanjiku","Joseph","Harold")
longtxt <- c("This is my first text in R")
strgtxt <- c("This","is","my","first","text","in","R")

numeric_vct <- 1:10
numeric_vct <- 10:1

dim(numeric_vct)
length(numeric_vct)

# ------------------------------------------ #
# Matrices
# ------------------------------------------ #
numeric_vct <- 1:10
m1 <- matrix(data = numeric_vct, nrow = 2, ncol = 5, byrow = TRUE)
m2 <- matrix(data = numeric_vct, nrow = 5, ncol = 2, byrow = TRUE)
m3 <- matrix(data = numeric_vct, nrow = 1, ncol = 10, byrow = TRUE)
m3
class(m3)
m4 <- matrix(data = numeric_vct, nrow = 10, ncol = 1, byrow = TRUE)
m4
class(m4)

dim(m1)

charctr_vct <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
charctr_mtx <- matrix(data = charctr_vct, nrow = 3, ncol = 2, byrow = TRUE)
charctr_mtx

charctr_vct <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday',1,2,3)
charctr_mtx <- matrix(data = charctr_vct, nrow = 3, ncol = 3, byrow = TRUE)
charctr_mtx

# ------------------------------------------ #
# Indexing
# ------------------------------------------ #

numeric_vct <- 1:100
numeric_vct[20]
2,4,6,8
numeric_vct[c(2,4,6,8)]

numeric_vct2 <- 1:10
m5 <- matrix(data = numeric_vct2, nrow = 5, ncol = 2, byrow = T)
m5
m5[3,2] # Access to one element
v1 <- m5[1,]
class(v1)

m5[,2]

# ------------------------------------------ #
# Data frames
# ------------------------------------------ #

dfm <- data.frame(name = c("Andres","Patricia","Lizeth"),
                  last_name = c("Mendez","Alvarez","Llanos"),
                  person_id = 1:3)
dfm
class(dfm)
dim(dfm)
str(dfm)

m1[1,5]
dfm[2 , 3]
dfm$person_id[2]
dfm$last_name[2]
dfm$name[1:2]

dfm[3,]
dfm[,3]
dfm[c(1,3),]

#             Rows   Columns
# data.frame [      ,       ]
dfm[ , c(1,3)]
dfm[ , c("name","person_id")]

dfm[ dfm$person_id >= 2 , ]

dfm2load <- read.csv("C:/Users/haachicanoy/Downloads/CSA.csv")

# ------------------------------------------ #
# Functions
# ------------------------------------------ #

myfun <- function(input1, input2, input3) {
  
  calcsum <- input1 + input2 + input3
  return(calcsum)
  
}

out1 <- myfun(input1 = 2, input2 = 3, input3 = 12)
out1
myfun(input1 = 1:2, input2 = 3:2, input3 = 7:8)



pacman::p_load(imager)
img <- imager::load.image('https://intriper.com/wp-content/uploads/elementor/thumbs/Santiago-de-Cali-el-destino-turistico-emergente-lider-en-America-del-Sur-que-no-te-puedes-perder-de-conocer-3-qxbsd4qlh1l3lv9xo90ae7rutlsbt19jtrzvmdt6s4.png')
plot(img)

img
dim(img)
dim(img[,,1])

class(img[,,1])
img[1:10,1:10,1]
