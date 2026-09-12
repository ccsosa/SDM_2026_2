require(geodata)
require(dplyr)
require(factoextra);library(ranger);library(caret)
library(doParallel);require(tmaptools)
library(sf);library(terra);library(usdm)
library(ggpmisc);library(ggplot2)
library(tmap);library(RColorBrewer)
library(bit)
################################################################################
#loading bioclimatic data
bios <- terra::rast("D:/Interpolate-Genetic-diversity-main/PREDICT_GEN_DIVERSITY/spat_data.tif")
#loading population data
data <- read.csv("D:/Interpolate-Genetic-diversity-main/pop_data.csv",header = T)
################################################################################
#creating mask!
mask <- bios[[1]]
mask[which(!is.na(mask[]))] <- 1
# plot(mask)
################################################################################
#formatting population data as sf
my_sf_object <- sf::st_as_sf(data,
                         coords = c("Longitude", "Latitude"),
                         crs = 4326) # Example CRS for WGS84 (latitude/longitude)


#obtaining bioclimatic data
ext <- terra::extract(bios,my_sf_object)

#getting not correlated variables
vars_selected <- usdm::vifstep(ext[,-1])
# corrplot::corrplot(cor(ext), method = 'square', type = 'lower', insig='blank',
#                    addCoef.col ='black', number.cex = 0.4, diag=FALSE)#,

vars <- vars_selected@results[,1]
#subset of bioclimatic dat
ext <- ext[,vars]

#fixing seeds
set.seed(1)

#choosing data to interpolate
data_sel <- data[,c("Longitude","Latitude","He")]
data_sel <- cbind(data_sel,ext)


#getting the formula for Random forest regression
formula_rf <- as.formula(paste("He ~", paste(vars, collapse = " + ")))


#tuning
num_trees = seq(0,10000,50)
num_trees[1] <- 10
#m_try = sqrt(5)

RF_list <- list()
tuning_df <- list()

for(i in 1:length(num_trees)){
  # i <- 1
  x <- as.data.frame(matrix(ncol=5,nrow=1))
  colnames(x) <- c("mtry","ntree","OOB","RMSE","i")
  first_rf <- ranger(formula_rf, 
                     num.trees = num_trees[i], #mtry = m_try, #
                     importance = "impurity",
                     data = data_sel,oob.error = T,num.threads = 4,replace = F,seed = 1)
  
  RF_list[[i]] <- first_rf
  x$mtry <-  first_rf$mtry
  x$ntree <- num_trees[i]
  x$OOB <- first_rf$r.squared 
  x$RMSE <- first_rf$prediction.error
  x$i <- i
  tuning_df[[i]] <- x
  rm(x)
  rm(first_rf)
};rm(i)


#best hyperparameters
tuning_df <- do.call(rbind,tuning_df)
tuning_df <- tuning_df[order(tuning_df$OOB,decreasing = T),]

#predicting using the data
x <- predict(RF_list[tuning_df$i[1]][1], ext)


################################################################################
#comparing observed vs predicted
x_o <- data.frame(Observed=data_sel$He,predicted=x[[1]]$predictions)
# using default formula, label and methods
ggplot(data = x_o, aes(x = Observed, y = predicted)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point()
################################################################################
#RMSE of data
Metrics::rmse(data_sel$He,x[[1]]$predictions)
res <- caret::postResample(x[[1]]$predictions, data_sel$He)
res

rm(x)
################################################################################
#importance values
imps <- data.frame(var = colnames(ext),
                   imps = RF_list[tuning_df$i[1]][[1]]$variable.importance/max(RF_list[tuning_df$i[1]][[1]]$variable.importance))
imps %>% 
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 10, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)
################################################################################
#subsetting to only cells with values (reduce computing time)
sample_p2 <- mask
sample_p2[] <- 1:ncell(sample_p2)
#dajusting to poverty raster
sample_p2_cells <- sample_p2[]
sample_p2_cells <- sample_p2_cells[which(!is.na(sample_p2_cells))]
##############################################################################
bios_Selected <- bios[[colnames(ext)]]
##############################################################################
#predict using raster
temp.dt <- mask[]
# temp.dt2 <- temp.dt
#joining all raster values in a ff object
# temp.dt <- ff(1,dim=c(ncell(temp.dt),dim(ext)),vmode="double")

#filling out each column
temp.dt <- terra::values(bios_Selected)

#subsetting to cells with values
temp.dt <- as.data.frame(temp.dt[sample_p2_cells,])
temp.dt$cell <- sample_p2_cells
#subsetting ff object to dataframe and only keeping cells with complete values for the 10 predictors
temp.dt <- temp.dt[which(complete.cases(temp.dt)),]
colnames(temp.dt)[1:(ncol(temp.dt)-1)] <- names(bios_Selected)
temp.dt$prediction <- NA

#save.image(paste0("D:/DRYAD/RATTUS/",VAR,"_RN",".RData"))

#predicting by chunks to ommit memory problems (chunks of 100 thousands records)
x_chunks <- bit::chunks(1,  nrow(temp.dt), by=1000) #200000
for(i in 1:length(x_chunks)){
  #i <- 1
  message(paste(round(i/length(x_chunks)*100,2)," %"))
  x_i1 <- as.numeric(as.character(x_chunks[[i]])[1])
  x_i2 <- as.numeric(as.character(x_chunks[[i]])[2])
  
  x <- predict(RF_list[tuning_df$i[1]][1], temp.dt[x_i1:x_i2,c(1:5)])[[1]]$predictions
  
  temp.dt$prediction[x_i1:x_i2] <- x
  rm(x)  
};rm(i)

#saving an empty raster
sample_p2_cells3 <- mask
sample_p2_cells3[which(!is.na(sample_p2_cells3[]))] <- NA
sample_p2_cells3[temp.dt$cell] <- temp.dt$prediction


################################################################################
#Making a map
Map3 <- tm_shape(sample_p2_cells3,
                 # bbox = sp_NA,
                 raster.downsample=F) + 
  tm_raster(style= "equal", n=5, palette=brewer.pal(7, "Blues"), title="Observed heterozygosity")+
  
  tm_layout(legend.outside=TRUE, legend.outside.position="right") +
  tm_shape(World #,
           # bbox = sp_NA
           ) + 
  
  tm_shape(my_sf_object) +
  tm_dots(col = "black",            # colorea según columna He
          size = 01,           # ajusta tamaño (depende de la escala)
          border.col = "black",
) +
  tm_grid(lines=T) +
  tm_borders("black") +
  tm_facets(nrow = 1, sync = TRUE)+
  tm_layout(inner.margins=0,
            legend.text.size=10,
            legend.title.size=10,
            legend.position = c("left","bottom"),
            legend.bg.color = "white", legend.bg.alpha=.2)#;gc()
#saving
tmap_save(filename=paste0("D:/Interpolate-Genetic-diversity-main/","He_RR",".pdf"),tm=Map3,dpi=900,width =180,height=100,units = "cm");gc()
