rm(list=ls())
library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)

# load raster map
epping <- rast("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/perc_cover/10new/data/LCM.tif")
print(epping)

#load site data and convert to spatial
site <- read.csv("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/perc_cover/sample_points.csv")
print(site)
site <- st_as_sf(site, coords=c('Longitude','Latitude'), crs=4326)
str(site)

#clean the data
# Transform the CRS of the data to match the raster (EPSG:27700)
site <- st_transform(site, crs = st_crs(epping))
print(site)
# Get the bounding box and convert it to a polygon
sites_region <- st_as_sfc(st_bbox(site))
# Buffer the region by 0.1 degrees (approx. 10 km)
sites_region <- st_buffer(sites_region, 0.1)
# Crop the raster
sites_landcover <- crop(epping, sites_region)

# Extract the first layer (assuming it contains land cover classification)
lcm <- epping[[1]]  
print(lcm)
# Plot to visualize
plot(lcm)
unique(values(lcm))
# Define forest class codes
forest_classes <- c(1, 2)


# Create binary map: 1 = forest, 0 = non-forest

binary_forest <- classify(lcm, cbind(forest_classes, 1), others = 0)

# Plot the binary forest map
plot(binary_forest, col = c("white", "green"), main = "Binary Forest Map")
plot(st_geometry(site), add=TRUE)

# Save and reproject binary raster
writeRaster(binary_forest, "Binary_Forest_Map.tif", overwrite=TRUE)
sites_forest <- rast("Binary_Forest_Map.tif")
print(sites_forest)
sites_utm23S <- st_transform(site, 27700)

sites_forest_utm23S <- project(sites_forest, "epsg:27700", res=25, method='near') #landcover class is categorical, we HAVE to use method=near, resolution of 30m
plot(sites_forest_utm23S)
plot(st_geometry(sites_utm23S), add=TRUE)

plot(st_geometry(site), axes=TRUE)


# TEST 50M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 50, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C50'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                          timevar='metric', idvar='plot_id')
summary(lsm_epping_forest, n=12)

View(lsm_epping_forest)
head(lsm_epping_forest, n=12)

site <- merge(site, lsm_sil_forest, by.x='Site', by.y='plot_id')
View(site)



# TEST 100M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 100, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))
View(lsm)

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C100'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                             timevar='metric', idvar='plot_id')
summary(lsm_epping_forest, n=12)

View(lsm_epping_forest)
print(lsm_epping_forest)
tail(lsm_sil_forest, n=12)
site <- merge(site, lsm_sil_forest, by.x='Site', by.y='plot_id')
View(site)



# TEST 150M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 150, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))
View(lsm)

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C150'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                             timevar='metric', idvar='plot_id')

summary(lsm_epping_forest)

# TEST 200M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 200, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))
View(lsm)

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C200'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                             timevar='metric', idvar='plot_id')

summary(lsm_epping_forest)

# TEST 250M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 250, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))
View(lsm)

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C250'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                             timevar='metric', idvar='plot_id')

summary(lsm_epping_forest)

# TEST 300M
lsm <- sample_lsm(sites_forest_utm23S, sites_utm23S, 
                  shape = "circle", size = 300, 
                  plot_id = sites_utm23S$Site, 
                  what = c('lsm_c_pland'))
View(lsm)

test <- subset(lsm, plot_id=='0A1')
print(test, n=23)
# Drop down to the forest class and also reduce to the three core fields
lsm_epping_forest <- subset(lsm, class == 1, select=c(metric, value, plot_id))
# name - the local landscape details are recorded

names(lsm_epping_forest)[2] <- 'C300'
# Reshape that dataset so that each metric has its own column
lsm_epping_forest <- reshape(data.frame(lsm_epping_forest), direction='wide', 
                             timevar='metric', idvar='plot_id')

summary(lsm_epping_forest)
