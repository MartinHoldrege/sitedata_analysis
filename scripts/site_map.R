# Martin Holdrege

# Script started April 14, 2021

# Purpose is to make a map of the site locations

# dependencies ------------------------------------------------------------

library(tidyverse)

# load site locations -----------------------------------------------------

# info on the 200 sites simulations will be run for
site1 <- readxl::read_xlsx("../dbWeather/200sites.xlsx")

# parse site locations ----------------------------------------------------

# just need coordinates

site2 <- site1[ , c("site_id", "X_WGS84", "Y_WGS84")]


# map ------------------------------------------------------------


states <- map_data("state")

jpeg("figures/maps/site_map.jpg",
     res = 600, height = 4, width = 4, units = "in")

ggplot(states, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "dark gray") +
  geom_point(data = site2, aes(x = X_WGS84, y = Y_WGS84)) +
  coord_map(xlim = c(-130, -97)) +
  ggthemes::theme_map()

dev.off()
