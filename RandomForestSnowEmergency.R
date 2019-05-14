# Using Random Forest prediction on sample snow emergency data 

library("randomForest")
library("raster")

setwd("/Users/student1/Development/r/cleanup_snow_project/data")

dataframe <- read.csv(file="SNOW_TAG_TOW_TYPES.csv")
head(dataframe)

# Ward (1, 2, 3....), Tow_Zone (1 - 6), Day (1, 2, 3) are numerical and interpreted as numeric type. 
# But here, they should be treated as categorical data, so convert to factors 
dataframe$Ward <- factor(dataframe$Ward)
dataframe$Tow_Zone <- factor(dataframe$Tow_Zone)
# dataframe$Day <- factor(dataframe$Day)

# Create categories for driving distance and driving duration 
# Help from http://rcompanion.org/handbook/E_05.html categorizing data 
per_00 <- min(dataframe$distance)
per_25 <- quantile(dataframe$distance, 0.25)
per_50 <- quantile(dataframe$distance, 0.5)
per_75 <- quantile(dataframe$distance, 0.55)
per_100 <- max(dataframe$distance)

dataframe$distanceCat[dataframe$distance >= per_00 & dataframe$distance < per_25] = 1
dataframe$distanceCat[dataframe$distance >= per_25 & dataframe$distance < per_50] = 2
dataframe$distanceCat[dataframe$distance >= per_50 & dataframe$distance < per_75] = 3
dataframe$distanceCat[dataframe$distance >= per_75 & dataframe$distance <= per_100] = 4

# Repeat for duration. Todo look up if there's a built-in way to do this in R
per_00 <- min(dataframe$duration)
per_25 <- quantile(dataframe$duration, 0.25)
per_50 <- quantile(dataframe$duration, 0.5)
per_75 <- quantile(dataframe$duration, 0.55)
per_100 <- max(dataframe$duration)

dataframe$durationCat[dataframe$duration >= per_00 & dataframe$duration < per_25] = 1
dataframe$durationCat[dataframe$duration >= per_25 & dataframe$duration < per_50] = 2
dataframe$durationCat[dataframe$duration >= per_50 & dataframe$duration < per_75] = 3
dataframe$durationCat[dataframe$duration >= per_75 & dataframe$duration <= per_100] = 4

# Create a numerical column from Tag and Tow for Random Forest 
dataframe$was_tow[dataframe$Type == "TOW"] = 1
dataframe$was_tow[dataframe$Type == "TAG"] = 0

# Save categories to file 
summary(dataframe)
write.csv(dataframe, "categorize_snow_emergency.csv")

############### Running Random Forest Model #################3

# Run the random forest model with the columns given 

# random_forest <- randomForest( Type ~ Ward + Community + Day + Tow_Zone + STREET_TYPE + distanceCat + durationCat, data=dataframe, ntree=500, importance=TRUE, proximity=TRUE)
random_forest <- randomForest( was_tow ~ Ward + Community + Day + Tow_Zone + STREET_TYPE + distanceCat + durationCat, data=dataframe, ntree=500, importance=TRUE, proximity=TRUE)

importance(random_forest)
# dev.off()
varImpPlot(random_forest)

# Day is by far the most important factor. Number of Tows and tags for each day 
table(dataframe[dataframe$Day == 1, ]$Type)  
table(dataframe[dataframe$Day == 2, ]$Type)
table(dataframe[dataframe$Day == 3, ]$Type)

# STREET_TYPE is the next most important. How many of each tag/tow for each STREET_TYPE ?  
library(plyr)
count(dataframe, var=c("STREET_TYPE", "Day", "Type"))

############### Creating predictive raster layer ###############


# Create coordinates for dataframe, which converts dataframe to a SpatialPointsDataFrame
coordinates(dataframe) <- ~Longitude+Latitude

## Create rasters for each column of interest 

# Extent of points in Minneapolis
lonMin <- -93.327527
lonMax <- -93.205057
latMin <- 44.891232
latMax <- 45.050941

cell_size <- 0.001
ncols <- (( lonMax - lonMin) / cell_size) + 1
nrows <- (( latMax - latMin) / cell_size) + 1

ext <- extent(lonMin, lonMax, latMax, latMax)

r_d <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
day_raster = rasterize(dataframe, r_d, "Day", fun="min", filename="Day.tif", background=0, overwrite=TRUE)

r_di <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
distance_raster = rasterize(dataframe, r_di, "distanceCat", fun="min", filename="distanceCat.tif", background=0, overwrite=TRUE)

r_du <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
duration_raster = rasterize(dataframe, r_du, "durationCat", fun=mean, filename="durationCat.tif", background=0, overwrite=TRUE)

# Everything else is a factor - how to convert to Raster? What value to write for factor's levels? 
r_w <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
ward_raster = rasterize(dataframe, r_w, "Ward", fun=function(x, na.rm) { max(as.numeric(x)) },  background=0, filename="Ward.tif", overwrite=TRUE)

r_t <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
tow_zone_raster = rasterize(dataframe, r_t, "Tow_Zone", fun=function(x, na.rm) { max(as.numeric(x)) }, background=0, filename="Tow_Zone.tif", overwrite=TRUE)

r_c <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
community_raster = rasterize(dataframe, r_c, "Community", fun=function(x, na.rm) { max(as.numeric(x)) }, background=0, filename="Community.tif", overwrite=TRUE)

r_s <- raster(ncols=ncols, nrows=nrows, xmn=lonMin, xmx=lonMax, ymn=latMin, ymx=latMax)
street_type_raster = rasterize(dataframe, r_s, "STREET_TYPE", fun=function(x, na.rm) { max(as.numeric(x)) }, background=0, filename="STREET_TYPE.tif", overwrite=TRUE)

# Vector of rasters 
raster_combo <- c(ward_raster, community_raster, day_raster, tow_zone_raster, street_type_raster, distance_raster, duration_raster)

# Set the extents of the rasters to be the same. Uses real, actual, R syntax 
for (r in raster_combo) {
  extent(r) <- ext
}

# Create a stack of all the rasters 
raster_stack <- stack(raster_combo)

# set names, must match column names in the dataframe used to generate 
names(raster_stack) <- c("Ward", "Community", "Day", "Tow_Zone", "STREET_TYPE", "distanceCat", "durationCat")

# The output raster is blank. What am I doing wrong? 
predict_raster_layer <- predict(raster_stack, random_forest, "predictive_snow_emergency_raster.tif", overwrite=TRUE)
#dev.off()
plot(predict_raster_layer)





