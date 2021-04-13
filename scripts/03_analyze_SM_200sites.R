# Martin Holdrege

# Script start 4/13/21

# Purpose is to summarize soil moisture data (means of yearly values)
# Use descriptive stats/figures to compare ambient and increased intensity

# dependencies ------------------------------------------------------------

library(tidyverse)
theme_set(theme_classic())
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")

# soil moisture metrics by PFT and layer --do this next

# soil moisture metrics by layer, (not by PFT)
lyr_all1 <- read_csv("data-processed/site_means/yr_mean_by_lyr-all_v1.csv")

# summary dfs -------------------------------------------------------------

lyr_all1 %>%
  mutate(SoilTreatment = soil_name(SoilTreatment))
# * soil moisture ----------------------------------------------------------

# total transpiration (to provide a total transpiration across layers)
tot_transp <- lyr_all1 %>%
  # later group by SoilTreatment when appropriate
  mutate(intensity = intensity_lookup[intensity]) %>%
  group_by(site, intensity) %>%
  summarize(TRANSP = sum(TRANSP), .groups = "drop") %>%
  pivot_wider(names_from = "intensity", values_from = "TRANSP",
              names_prefix = "TRANSP_") %>%
  mutate(TRANSP_diff = TRANSP_2x_intensity - TRANSP_ambient) %>%
  left_join(aridity, by = "site")


# * differences -----------------------------------------------------------

# note: VWCBULK and VWCMATRIC look identical (1:1 line)
lyr_all_diff1 <- lyr_all1 %>%
  select(site, intensity, layer, VWCBULK, WETDAY, TRANSP) %>%
  pivot_wider(names_from = "intensity",
              # KP suggested VWCMATRIC may be better to use than VWCBULK
              values_from = c("VWCMATRIC", "WETDAY", "TRANSP")) %>%
  mutate(VWC_diff = VWCBULK_2x_intensity - VWCBULK_ambient,
         WETDAY_diff = WETDAY_2x_intensity - WETDAY_ambient,
         TRANSP_diff = TRANSP_2x_intensity - TRANSP_ambient,
         depth = lyr2depth(layer)) %>%
  left_join(climate1, by = "site")





# figures -----------------------------------------------------------------


# * fig params ------------------------------------------------------------

caption <- paste("Data from preliminary STEPWAT2 run for 14 sites.",
                 "PPT intensity doubled by adding odd to even events.",
                 "\nEvents defined as one or more consecutive PPT days.")
transp_lab1 <- "Transpiration difference (2x intensity - ambient; cm)"
vwc_lab1 <- "VWC difference (2x intensity - ambient; cm/cm)"
depth_lab  <- "Soil depth (cm)"
wetday_lab1 <- "Wet day difference (2x intensity - ambient; # days > -1.5 MPa)"
aridity_lab <- "Aridity index (MAP/PET)"
# base of figures by soil layers
lyr_base <- function() {
  list(stat_summary(fun = mean, geom = "line", color = "blue"),
       stat_summary(fun = mean, geom = "point", color = "blue"),
       geom_hline(yintercept = 0, linetype = 2, alpha = 0.7),
       labs(x = depth_lab,
           caption = caption),
       xlim(c(-155, 0)),
      coord_flip()
  )
}

line_base <- function() {
  list(geom_line(aes(group = site), alpha = 0.2),
       labs(subtitle = "Gray lines show each site, blue is the mean"))
}

boxplot_base <- function() {
  list(geom_boxplot(aes(group = -depth)),
       labs(subtitle = "Blue line shows the mean across sites"))
}

aridity_base <- function() {
  list(geom_hline(yintercept = 0, alpha = 0.7, linetype = 2),
       geom_point(aes(x = aridity_index)),
       facet_wrap(~as.factor(depth)),
       geom_smooth(aes(x = aridity_index), method = "lm", se = FALSE),
       labs(x = aridity_lab,
            caption = caption,
            subtitle = "Soil layers (cm) shown in separate panels")
       )
}

# create both boxplot and line graphs
box_and_line <- function(g) {
  g1 <- g + boxplot_base() + lyr_base()
  g2 <- g + line_base() + lyr_base()
  list(g1, g2)
}
# * sm sum across lyrs ----------------------------------------------------

pdf("figures/14sites/SM_across_lyrs-PFT_v1.pdf")
# soil moisture across layers (ie transpiration or other cumulative metrics)

ggplot(tot_transp, aes(x = TRANSP_diff)) +
  geom_boxplot() +
  geom_rug(color = "red") +
  labs(x = transp_lab1,
       title = "Intensity effect on total transpiration across soil layers",
       caption = caption) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

ggplot(tot_transp, aes(x = aridity_index, y = TRANSP_diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = transp_lab1,
       x = aridity_lab,
       title = "Total transpiration differnce across soil layers vs aridity",
       caption = caption)

dev.off()


# * SM by lyrs across PFT ---------------------------------------------------

pdf("figures/14sites/SM_by_lyr_across_PFT_v1.pdf")

# ** VWC ------------------------------------------------------------------
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = VWC_diff)) +
  labs(y = vwc_lab1,
       title = "Intensity effect on volumetric water content")

box_and_line(g)

ggplot(lyr_all_diff1, aes(y = VWC_diff)) +
  labs(y = vwc_lab1,
       title = "Change in volumetric water content vs. aridity") +
  aridity_base()

# WETDAYS
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = WETDAY_diff)) +
  labs(y = wetday_lab1,
       title = "Intensity effect on wet days")

box_and_line(g)

ggplot(lyr_all_diff1, aes(y = WETDAY_diff)) +
  labs(y = wetday_lab1,
       title = "Change in number of wet days vs. aridity") +
  aridity_base()

# total transpiration
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Intensity effect on total transpiration from each layer")

box_and_line(g)

g1 <- ggplot(lyr_all_diff1, aes(y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Change in transpiration vs. aridity") +
  aridity_base()

g1 +
  facet_wrap(~factor(depth), scales = "free_y")

dev.off()


