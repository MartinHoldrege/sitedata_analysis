# Martin Holdrege

# Script started 11/7/2021

# Description--purpose is to create a raster object of gridcells showing
# where sagebrush dominated ecosystems are in the western united states
# using GAP landcover datasets. Purpose is to isolate those sagebrush gridcells,
# reduce the resolution, and combine into one raster, for later mapping.

# the GAP land cover data datasets were downloaded for each of the western
# states from here:
# https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-download?qt-science_center_objects=0#qt-science_center_objects

# this code can take a while to run

# dependencies ------------------------------------------------------------

library(terra)

# attributes --------------------------------------------------------------

# attributes of of the gap data
attr <- read.table("data-raw/GAP/gaplf2011lc_v30_AZ/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",
                   sep = "\t", header = TRUE)
head(attr)
attr[stringr::str_detect(attr$ECOLSYS_LU, "[Ss]agebrush"),
     c("Value", "Count", "ECOLSYS_LU")]

# the 3 vegetation types Schlaepfer et al (2012, in Ecohydrology)
# defines as veg types for which big sagebrush is a substantial component
# are: 489, 490, and 491. Those are the ones I'll use below


# read in data ------------------------------------------------------------

# * paths -----------------------------------------------------------------

# get paths to all rasters
# rasters for each western state are in seperate folders.
r_paths <- list.files("data-raw/GAP/", recursive = TRUE,
                      pattern = "^gaplf2011lc_v30_[a-z]{2}.tif$",
                      full.names = TRUE)
r_paths

# * load rasters ----------------------------------------------------------

# combining into a single virtual raster tile as per:
# https://github.com/rspatial/terra/issues/210
r1 <- vrt(r_paths, "data-raw/GAP/GAP_states_combined.vrt", overwrite=TRUE)

# coarsen -----------------------------------------------------------------
# reduce the resolution--currently 30*30 m, now combining every set
# of 100 cells into one cell, based on the most common (modal) value

r2 <- aggregate(r1, fact = 10, fun = "modal")

# select sagebrush pixels -------------------------------------------------

# pixels that are big sagebrush dominant, as defined by Schlaepfer 2012
# get converted to 1, everything else becomes NA

m <- c(0, 488, NA, # this row means values from 0 to 488 become NA
       489, 491, 1, # 489-491 are big sagebrush ecosystems
       492, 7000, NA)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
r3 <- classify(r2, rcl = rclmat, right = NA) # reclassify matrix values

plot(r3)


# save file ---------------------------------------------------------------

# can't save this as an RDS file
writeRaster(r3, "data-processed/sagebrush_extent.tif", overwrite=TRUE)



