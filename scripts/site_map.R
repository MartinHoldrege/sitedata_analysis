# Martin Holdrege

# Script started April 14, 2021

# Purpose is to make a map of the site locations, and show extent of sagebrush
# range on that map

# dependencies ------------------------------------------------------------

library(tidyverse)
library(tmap) # for making the map
library(terra) # for working with raster data
library(sf)
library(sp) # still using spatialpoints dataframe below, could update to sf

# read in data ------------------------------------------------------------

# * site locations --------------------------------------------------------

# info on the 200 sites simulations will be run for
# this information was also in 200sites.xlsx file
site1 <- read_csv("data-raw/site_locations.csv")


# * aridity index ---------------------------------------------------------

# aridity (generated in climate script)--aridity of individual sites
# calculated from stepwat output
aridity1 <- read_csv("data-processed/aridity_by_site.csv")%>%
  dplyr::select(-matches("_SD$"))

aridity2 <- aridity1 %>%
  right_join(site1, by = c("site" = "site_id"))


# * sagebrush extent --------------------------------------------------------
# raster of sagebrush extent, from GAP data, compiled in sagebrush_extent.R

r3 <- rast("data-processed/sagebrush_extent.tif")

# parse site locations ----------------------------------------------------

# just need coordinates
site2 <- site1[ , c("site_id", "X_WGS84", "Y_WGS84")]


# select sagebrush pixels -------------------------------------------------
# values are proportions of fine scale pixels (30 x 30 m), that were big sagebrush
# ecosystems.
m <- c(0, 0.1, NA, # cells with very little sagebrush become NA
       0.1, 1.1, 1)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
r3 <- classify(r3, rcl = rclmat, right = TRUE)
# convert formats/crs--------------------------------------------------------

# want to re-project everything to match the raster
crs <- crs(r3, proj4 = TRUE)
states <- tigris::states(cb = TRUE, class = "sf")
class(states)

states2 <- st_transform(states, crs)
site1 <- SpatialPointsDataFrame(
  coords = aridity2[c("X_WGS84", "Y_WGS84")],
  data = aridity2[c("site", "aridity_index", "PRECIP_ppt_Mean")],
  # this is the current crs of the data
  proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

site2 <- spTransform(site1, CRS(crs))

# bounding box
bbox_new <- st_bbox(states2) # original bbox
bbox_new[1:4] <- c(-2500000, 900000, -500000, 3200000)
states3 <- st_crop(states2, bbox_new)


r4 <- raster::raster(r3) # convert to raster class for use with tmap
r4 <- raster::crop(r4, bbox_new) # cut to size of states

# map of aridity points ------------------------------------------------------

# just showing points (colored by aridity) and underlying extent of sagebrush

jpeg("figures/maps/site_map_sagebrush.jpg",
     res = 600, height = 4.5, width = 4.5, units = "in")
tmap_mode("plot")

tm_style(style = "white",
         frame = "white",
         legend.position = c("left", "bottom"),
         legend.outside = TRUE
         ) +
  tm_shape(r4) +
  tm_raster("GAP_states_combined",
            style = "cat",
            palette = "#016c59", # "#a1d99b",
            legend.show = FALSE) +
  tm_shape(states3, bbox = bbox_new) +
  tm_borders(col = "black") +
  tm_fill(col = NA, alpha = 0) +
  tm_shape(site2) +
  tm_dots(col = "aridity_index",
          breaks = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 10),
          palette = RColorBrewer::brewer.pal(11, "RdYlBu")[c(2:7, 9)],
          labels = c("< 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                     "0.6 - 0.7", "> 0.7"),
          title = "Aridity Index",
          size = 0.4,
          shape = 16)

dev.off()



