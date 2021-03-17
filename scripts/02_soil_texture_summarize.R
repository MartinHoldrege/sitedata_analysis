# Martin Holdrege

# Script started 3/1/21

# The purpose here is calculate mean soil texture by site
# and get texture for soil


# dependencies ------------------------------------------------------------

library(tidyverse)
# read in data ------------------------------------------------------------

sgrids1 <- readRDS("data-raw/soil_profiles_200sites.RDS")


# summarize texture -------------------------------------------------------

# for weighted averages
weights_lookup <- c("0-5" = 5, "5-15" = 10, "15-30" = 15, "30-60" = 30,
                    "60-100" = 40, "100-200" = 100)/200

# means of soil texture at each site
site_means1 <- sgrids1@horizons %>%
  as_tibble() %>%
  #filter(label != "100-200") %>%   # removing the deepest soil
  #filter(label == "0-5") %>%
  mutate(depth_weight = weights_lookup[label],
         depth_weight = 1) %>%
  select(id, claymean, sandmean, siltmean, depth_weight) %>%
  group_by(id) %>%
  summarize(clay = weighted.mean(claymean, w = depth_weight),
            silt = weighted.mean(siltmean, w = depth_weight),
            sand = weighted.mean(sandmean, w = depth_weight),
            .groups = "drop") %>%
  mutate(total = clay + silt + sand)


sum(is.na(site_means1)) # not sure what missing values

# mean(site_means1$clay, na.rm = TRUE) + mean(site_means1$sand, na.rm = TRUE) + mean(site_means1$silt, na.rm = TRUE)


site_means1 %>%
  summarise_at(.vars = vars(sand, silt, clay),
               .funs = list(med = median, min = min, max = max),
               na.rm = TRUE)


# percentiles -------------------------------------------------------------


cdfs <- map(site_means1[ , c("sand", "silt", "clay")], ecdf)

# 5th percentile
perc05 <- map_dbl(cdfs, quantile, 0.05)

sand_soil <- c(sand = 100 - sum(perc05[c("silt", "clay")]),
               perc05[c("silt", "clay")])




