# Martin Holdrege

# Script started April 14, 2021

# Purpose is to make a map of the site locations

# dependencies ------------------------------------------------------------

library(tidyverse)
library(tmap) # for making the map
library(raster) # reading in raster
library(sf)
# read in data ------------------------------------------------------------

# * site locations -----------------------------------------------------

# info on the 200 sites simulations will be run for
site1 <- readxl::read_xlsx("../dbWeather/200sites.xlsx")

# * aridity index ---------------------------------------------------------

# tiff of global aridity index based on data from 1970-2000
r1 <- raster("data-raw/aridity_index/ai_et0/ai_et0.tif")

# parse site locations ----------------------------------------------------

# just need coordinates
site2 <- site1[ , c("site_id", "X_WGS84", "Y_WGS84")]


# parse raster ------------------------------------------------------------

# is  unprojected wgs 84
crs <- r1@crs

# r2 <- r1*0.0001 # convert bag into correct units, as per readme for this dataset

# map ------------------------------------------------------------

# Next--bounding box update so that the range isn't weird.
states <- tigris::states(cb = TRUE, class = "sf")
class(states)
states2 <- st_transform(states, crs)
site3 <- SpatialPoints(site2[2:3], proj4string = crs)

# bounding box
bbox_new <- st_bbox(states2) # original bbox
bbox_new[1:4] <- c(-120, 22, -95, 48)

r2 <- crop(r1, bbox_new) # cut raster
r3 <- r2*0.0001
r4 <- raster::aggregate(r3, fact = 8, fun = mean)

jpeg("figures/maps/site_map.jpg",
     res = 600, height = 4, width = 4, units = "in")

tmap_mode("plot")
tm_style(style = "white") +
  tm_shape(r4) +
  tm_raster()+
tm_shape(states2, bbox = bbox_new) +
  tm_borders(col = "black") +
tm_shape(site3) +
  tm_dots()

dev.off()


