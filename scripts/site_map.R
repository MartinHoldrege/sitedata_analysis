# Martin Holdrege

# Script started April 14, 2021

# Purpose is to make a map of the site locations, and show extent of sagebrush
# range on that map

# dependencies ------------------------------------------------------------

library(tidyverse)
library(tmap) # for making the map
library(terra) # for working with raster data
library(sf)

# read in data ------------------------------------------------------------

# * site locations --------------------------------------------------------

# info on the 200 sites simulations will be run for
site1 <- readxl::read_xlsx("../dbWeather/200sites.xlsx")


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


# r2 <- r1*0.0001

# convert formats/crs--------------------------------------------------------

crs <- crs(r3)
states <- tigris::states(cb = TRUE, class = "sf")
class(states)
crs <- st_crs(states) # states map is unprojected
states2 <- st_transform(states, crs)
site1 <- SpatialPointsDataFrame(
  coords = aridity2[c("X_WGS84", "Y_WGS84")],
  data = aridity2[c("site", "aridity_index", "PRECIP_ppt_Mean")],
  # not projected
  proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

# bounding box
bbox_new <- st_bbox(states2) # original bbox
bbox_new[1:4] <- c(-124.5, 24.5, -96.5, 50)
states3 <- st_crop(states2, bbox_new)


# map aridity points ------------------------------------------------------

# just showing points (colored by aridity)
jpeg("figures/maps/site_map_points-only.jpg",
     res = 600, height = 4.5, width = 4.5, units = "in")
tmap_mode("plot")

tm_style(style = "white",
         frame = "white",
         legend.position = c("left", "bottom")
         #legend.outside = TRUE
         ) +
  tm_shape(states3, bbox = bbox_new) +
  tm_borders(col = "black") +
  tm_fill(col = "gray", alpha = 0.4) +
  tm_shape(site5) +
  tm_dots(col = "aridity_index",
          breaks = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 10),
          palette = RColorBrewer::brewer.pal(11, "RdYlBu")[c(2:7, 9)],
          labels = c("< 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                     "0.6 - 0.7", "> 07"),
          title = "Aridity Index",
          size = 0.4,
          shape = 16)

dev.off()



