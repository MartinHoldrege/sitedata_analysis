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

# aridity (generated in climate script)--aridity of individual sites
# calculated from stepwat output
aridity1 <- read_csv("data-processed/aridity_by_site.csv")%>%
  dplyr::select(-matches("_SD$"))


# * cover data ------------------------------------------------------------

# using to see what percent of sites fall within the soil texture grid
# cells being used
dat <- stack("data-raw/soils/dat_newLD_BBD.grd")

# parse site locations ----------------------------------------------------

# just need coordinates
site2 <- site1[ , c("site_id", "X_WGS84", "Y_WGS84")]


# parse raster ------------------------------------------------------------

# is  unprojected wgs 84
crs <- r1@crs

# r2 <- r1*0.0001

# convert formats/crs--------------------------------------------------------

states <- tigris::states(cb = TRUE, class = "sf")
class(states)
states2 <- st_transform(states, crs)
site3 <- SpatialPoints(site2[2:3], proj4string = crs)


dat2 <- dat
crs(dat2) <- crs

# bounding box
bbox_new <- st_bbox(states2) # original bbox
bbox_new[1:4] <- c(-124.5, 24.5, -96.5, 50)
states3 <- st_crop(states2, bbox_new)

r2 <- crop(r1, bbox_new) # cut raster
r3 <- r2*0.0001 # convert into correct units, as per readme for this dataset
r3 <- mask(r3, states3) # crop to extent of states
# making coarser so that easier
r4 <- raster::aggregate(r3, fact = 2, fun = mean)


# calculate % sites -------------------------------------------------------
# need to calculate % sites within soil texture grid cells


# some of the sites fall outside soil data
# plot(site3)
# plot(dat2$clay, add = TRUE)


x <- extract(dat2$clay, site3) # grid cells at site locations

sites_text <- site3[!is.na(x)] # sites that have texture data

# plot(dat2$clay)
# plot(sites_text, add = TRUE)

# % sites outside soil data:
length(sites_text)/length(site3)*100


# compare aridity index values --------------------------------------------
# gridded product vs stepwat
site4 <- site2
site4$aridity_gridded <- extract(r3, site3)
site4 <- site4 %>%
  left_join(aridity1, by = c("site_id" = "site"))

plot(aridity_index ~ aridity_gridded, data = site4,
     xlab = "aridity (gridded product)",
     ylab = "aridity (STEPWAT2)")
abline(0, 1)

# map ---------------------------------------------------------------------

colors <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(3:7, 9)]

jpeg("figures/maps/site_map.jpg",
     res = 600, height = 4, width = 4, units = "in")

# so plotgs more pixels

tmap_options(max.raster = c(plot = 10494000, view = 10494000))
tmap_mode("plot")
tm_style(style = "white",
         frame = FALSE,
         legend.position = c("left", "bottom")) +
  tm_shape(r4) +
  tm_raster(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 10),
            labels = c("< 0.2", "0.2 - 0.4", "0.4  -0.6", "0.6 - 0.8", "0.8 - 1",
                       "> 1"),
            palette = colors,
            title = "Aridity Index")+
tm_shape(states3, bbox = bbox_new) +
  tm_borders(col = "black") +
tm_shape(site3) +
  tm_dots(size = 0.1)

dev.off()


