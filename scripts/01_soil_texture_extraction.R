# Martin Holdrege

# Script started 3/1/21


# Purpose is to summarize soil texture, and select soil texture classes
# to run STEPWAT2 for

# The stack has several layers, including sand, clay and soil depth.
# The soil data in that stack were pulled from gridded STATSGO for the extent
# of the sagegrouse management zones.


# dependencies ------------------------------------------------------------

library(tidyverse)
library(raster)
library(soiltexture) # for soil triangles
library(data.table)
source("scripts/functions.R")

# load raster data -----------------------------------------------------

# Data provided by John Bradford
# The stack has several layers, including sand, clay and soil depth.
# The soil data in that stack were pulled from gridded STATSGO for the extent
# of the sagegrouse management zones.

dat <- stack("data-raw/soils/dat_newLD_BBD.grd")

# explore raster stack
if (FALSE){
layers <- names(dat)
par(mfrow = c(2, 2))
for (layer in layers) {
  print(plot(dat[[layer]], main = layer))
}

r <- reclassify(dat$gap_bse, c(0.01, .25, 2,
                               .25, .65, 3,
                               .65, 1, 4))
plot(r)
}

# subset ------------------------------------------------------------------

# subsetting rasters to cells with > 66% sagebrush

clay <- dat$clay
sand <- dat$sand

# gap_bse is the proportion of each pixel that is big sagebrush
# In prior analyses JB filtered data to keep only pixels with >66% big
# sagebrush ecosystems
sand[dat$gap_bse <= 0.66] <- NA
clay[dat$gap_bse <= 0.66] <- NA

# examine
# plot(clay)
# plot(sand)

# put into df -------------------------------------------------------------

# soiltexture package can't use tibble
df1 <- data.frame(SAND = as.numeric(as.matrix(sand)),
             CLAY = as.numeric(as.matrix(clay))) %>%
  mutate(SILT = 100 - (SAND + CLAY)) %>%
  filter(!is.na(SILT)) %>% # NAs are locations without sagebrush
  dplyr::select(CLAY, SILT, SAND) # column order needed by soiltexturepackage

map(df1, range)
map_dbl(df1, mean) %>%
  sum()

# calculate soil texture classes ------------------------------------------

cdfs <- map(df1, ecdf) # distributions soil texture classes

# soil
quants <- c(0.01, 0.02, 0.05, 0.5, 0.95, 0.98, 0.99) # quantiles to examine

# quantiles by soil texture
quant_df <- map_dfc(cdfs, .f = quantile, quants) %>%
  mutate(quant = quants)

# medians
med_df <- quant_df[quant_df$quant == 0.5, ]

# median soil
med <- list(CLAY =  round(med_df$CLAY), SILT = 100 - round(med_df$SAND) - round(med_df$CLAY),
         SAND = round(med_df$SAND), type = "silt_loam")
names(med) <- str_extract(names(med), "[A-z]+")

# percentiles
p05 <- quant_df[quant_df$quant == 0.05, ]
p50 <- quant_df[quant_df$quant == 0.5, ]
p95 <- quant_df[quant_df$quant == 0.95, ]

third_class <- function(list) {
  # sum rounded elements of list of length 2, subtract from 100
  # ie calculating third texture class, based on list with 2 texture classes
  stopifnot(length(list) ==2)
  x <- sum(round(c(list[[1]], list[[2]])))
  out <- 100 - x
  out
}

# 95th with eyeballing the other two (issue w/ the above technique)
# is that it doesn't allow for a good balance of the other soil classes
# here just using a fixed percentile for the dominant class

# previously clay set at 11
sap <- list(SAND = p95$SAND)
sap$CLAY <- conditional_texture(dt = data.table(df1[ ,c("SAND", "CLAY")]),
                            x_conditional = p95$SAND, y_by = 0.1)
sap$SILT <- third_class(sap)

# silt:
# previously sand set at 17
sip <- list(SILT = p95$SILT)
sip$CLAY <- conditional_texture(dt = data.table(df1[ ,c("SILT", "CLAY")]),
                                x_conditional = p95$SILT, y_by = 0.1)
sip$SAND <- third_class(sip)

# clay
# previously sand set at 30
clp <- list(CLAY = p95$CLAY)
clp$SAND <- conditional_texture(dt = data.table(df1[ ,c("CLAY", "SAND")]),
                                x_conditional = p95$CLAY, y_by = 0.1)
clp$SILT <- third_class(clp)


soilsp <- bind_rows(sap, sip, clp, med) %>%
  .[c("CLAY", "SILT", "SAND")] %>%
  round() %>%
  as.data.frame()

soilsp$name <- c("sand", "silt", "clay", "median")
rowSums(soilsp) # need to be 100

# figures -----------------------------------------------------------------

geo <- TT.geo.get()
# for contour plot
kde.res <- TT.kde2d(
  geo = geo,
  tri.data = df1,
  n = 100
) #

pdf("figures/soils/texture_triangles1.pdf")

# all points shown
TT.plot(
  class.sys = "USDA.TT",
  tri.data = df1,
  cex = .001,
  pch = 1,
  col = alpha("black", 0.05),
  frame.bg.col = "white",
  grid.show = FALSE,
  add = TRUE,
  main = "Soil texture in sagebrush grid cells"
)

# adding 4 texture groups
TT.points(
  geo = geo,
  tri.data = soilsp,
  col = "blue",
  pch = 3,
  cex = 3
)


# contour plot

TT.contour(
  x = kde.res,
  geo = geo,
  main = "Probability density estimate of the texture data",
  lwd = 0.5,
  col = "dark green"
) #
#
TT.plot(
  class.sys = "USDA.TT",
  geo = geo,
  grid.show = FALSE,
  add = TRUE, # <<-- important
  col = "gray"
)
TT.points(
  geo = geo,
  tri.data = soilsp,
  col = "blue",
  pch = 3,
  cex = 3
)

dev.off()




