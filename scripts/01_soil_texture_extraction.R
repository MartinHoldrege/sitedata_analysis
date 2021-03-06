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
quants <- c(0.01, 0.05, 0.5, 0.95, 0.99) # quantiles to examine

# quantiles by soil texture
quant_df <- map_dfc(cdfs, .f = quantile, quants) %>%
  mutate(quant = quants)

# medians
med_df <- quant_df[quant_df$quant == 0.5, ]
med <- list(CLAY =  med_df$CLAY, SILT = 100 - med_df$SAND - med_df$CLAY,
         SAND = med_df$SAND, type = "silt_loam")
names(med) <- str_extract(names(med), "[A-z]+")

# 5th percentile
p05 <- quant_df[quant_df$quant == 0.05, ]

# sand
sa05 <- c("CLAY" = p05$CLAY, SILT = p05$SILT, SAND = 100 - p05$CLAY - p05$SILT)

# silt
si05 <- c("CLAY" = p05$CLAY, SILT = 100 - p05$SAND - p05$CLAY, SAND = p05$SAND)

# clay
cl05 <- c("CLAY" = 100 - p05$SAND - p05$SILT, "SILT" = p05$SILT, "SAND" = p05$SAND)

# 4 soil texture classes
foursoils <- bind_rows(sa05, si05, cl05) %>%
  mutate(type = c("sand", "silt", "clay"))
names(foursoils) <- str_extract(names(foursoils), "[A-z]+")

foursoils <- med %>%
  bind_rows(foursoils) %>%
  as.data.frame()

# figures -----------------------------------------------------------------

geo <- TT.geo.get()

pdf("figures/soils/texture_triangles1.pdf")

# all points shown
TT.plot(
  class.sys = "USDA.TT",
  tri.data = df1,
  cex = 0.2,
  col = alpha("black", 0.01),
  frame.bg.col = "white",
  grid.show = FALSE,
  add = TRUE,
  main = "Soil texture in sagebrush grid cells"
)

# adding 4 texture groups
TT.points(
  geo = geo,
  tri.data = foursoils,
  col = "blue",
  pch = 3,
  cex = 3
)

# contour plot
kde.res <- TT.kde2d(
  geo = geo,
  tri.data = df1,
  n = 100
) #


TT.contour(
  x = kde.res,
  geo = geo,
  main = "Probability density estimate of the texture data",
  lwd = 1,
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
  tri.data = foursoils,
  col = "blue",
  pch = 3,
  cex = 3
)

dev.off()
