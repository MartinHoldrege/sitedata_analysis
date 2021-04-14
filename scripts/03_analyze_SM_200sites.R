# Martin Holdrege

# Script start 4/13/21

# Purpose is to summarize soil moisture data (means of yearly values)
# Use descriptive stats/figures to compare ambient and increased intensity

# dependencies ------------------------------------------------------------

library(tidyverse)
theme_set(theme_classic())
source("scripts/functions.R")
source("scripts/fig_params.R")

# read in data ------------------------------------------------------------

# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")

# soil moisture metrics by PFT and layer
lyr_pft1 <- read_csv("data-processed/site_means/yr_mean_by_lyr-PFT_v1.csv")


# soil moisture metrics by layer, (not by PFT)
lyr_all1 <- read_csv("data-processed/site_means/yr_mean_by_lyr-all_v1.csv")

# summary dfs -------------------------------------------------------------

aridity1 <- aridity1 %>%
  select(-matches("_SD$"))

lyr_all1 <- lyr_all1 %>%
  mutate(SoilTreatment = soil_name(SoilTreatment),
         depth = lyr2depth(layer))

lyr_pft1 <- lyr_pft1 %>%
  filter(PFT != "tree") %>% # no trees being modeled
  mutate(SoilTreatment = soil_name(SoilTreatment),
         depth = lyr2depth(layer))

# * total transp ---------------------------------------------------------

# ** across pfts ----------------------------------------------------------

# total transpiration (to provide a total transpiration across layers)
tot_transp <- lyr_all1 %>%
  # later group by SoilTreatment when appropriate
  group_by(site, intensity, SoilTreatment) %>%
  summarize(TRANSP = sum(TRANSP), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# diff between intensity and ambient
tot_transp_diff <- tot_transp %>%
  group_by(site, SoilTreatment) %>%
  mutate(TRANSP_diff = calc_diff(TRANSP, intensity),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity)) %>%
  # diffs for ambient are 0
  filter(intensity != "ambient")

# ** by pft ---------------------------------------------------------------

tot_transp_pft <- lyr_pft1 %>%
  # later group by SoilTreatment when appropriate
  group_by(site, intensity, SoilTreatment, PFT) %>%
  summarize(TRANSP = sum(TRANSP), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# diff between intensity and ambient
tot_transp_pft_diff <- tot_transp_pft %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(TRANSP_diff = calc_diff(TRANSP, intensity),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity)) %>%
  # diffs for ambient are 0
  filter(intensity != "ambient")

# * differences by depth -------------------------------------------------

# across all PFTs
# note: VWCBULK and VWCMATRIC look identical (1:1 line)
# this way of calculating differences allows for including
# of more intensity levels down the road
lyr_all_diff1 <- lyr_all1 %>%
  select(site, intensity, depth, SoilTreatment, VWCMATRIC, WETDAY, TRANSP) %>%
  group_by(site, depth, SoilTreatment) %>%
  mutate(VWC_diff = calc_diff(VWCMATRIC, intensity),
         WETDAY_diff = calc_diff(WETDAY, intensity),
         TRANSP_diff = calc_diff(TRANSP, intensity),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity)) %>%
  filter(intensity != "ambient") %>%
  left_join(aridity1, by = "site")

# diffs by pft (total is a pft category)
lyr_pft_diff1 <- lyr_pft1 %>%
  select(site, intensity, depth, SoilTreatment, PFT, TRANSP) %>%
  group_by(site, depth, PFT, SoilTreatment) %>%
  mutate(TRANSP_diff = calc_diff(TRANSP, intensity),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity)) %>%
  filter(intensity != "ambient") %>%
  left_join(aridity1, by = "site")

# figures -----------------------------------------------------------------

# * fig params ------------------------------------------------------------

caption <- paste("STEPWAT2 run for 200 sites.",
                 "PPT intensity doubled by adjusting markov coefficients.")
transp_lab1 <- "Transpiration difference (2x intensity - ambient; cm)"
transp_lab2 <- "% transpiration change"

se = FALSE # confidence interval

# these labels will need to be adjusted once no longer just 2x intensity
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
       geom_point(aes(x = aridity_index), alpha = 0.3),
       facet_grid(as.factor(depth)~SoilTreatment),
       geom_smooth(aes(x = aridity_index), method = "loess", se = se),
       labs(x = aridity_lab,
            caption = caption,
            subtitle = "Soil depths (cm) shown in separate panels")
       )
}

# create both boxplot and line graphs
box_and_line <- function(g) {
  g1 <- g + boxplot_base() + lyr_base() +
    facet_wrap(~SoilTreatment)
  g2 <- g + line_base() + lyr_base() +
    facet_wrap(~SoilTreatment)
  list(g1, g2)
}

# * sm sum across lyrs ----------------------------------------------------

pdf("figures/soil_moisture/SM_across_lyrs_v1.pdf")
# soil moisture across layers (ie transpiration or other cumulative metrics)

perc_sub <-" % change in transpiration (2x intensity vs ambient) across soil layers"
ggplot(tot_transp_diff, aes(x = SoilTreatment, y = TRANSP_diff)) +
  geom_boxplot() +
#  geom_rug(color = "red") +
  labs(y = transp_lab1,
       title = "Intensity effect on total transpiration across soil layers",
       caption = caption)

g <- ggplot(tot_transp_diff) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~SoilTreatment) +
  labs(x = aridity_lab,
       caption = caption)

# difference
g +
  geom_point(aes(x = aridity_index, y = TRANSP_diff)) +
  geom_smooth(aes(x = aridity_index, y = TRANSP_diff), method = "loess",
              se = se) +
  labs(y = transp_lab1,
       title = "Total transpiration difference across soil layers vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = TRANSP_perc_diff)) +
  geom_smooth(aes(aridity_index, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       subtitle = paste(perc_sub, "vs aridity"))

# % change vs MAP
g +
  geom_point(aes(PRECIP_ppt_Mean, y = TRANSP_perc_diff)) +
  geom_smooth(aes(PRECIP_ppt_Mean, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       x = "Mean annual precipitation (cm)",
       subtitle = paste(perc_sub, "vs MAP"))

# % change vs Tmax
g +
  geom_point(aes(TEMP_max_C_Mean, y = TRANSP_perc_diff)) +
  geom_smooth(aes(TEMP_max_C_Mean, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       x = "Mean daily Tmax (C)",
       subtitle = paste(perc_sub, "vs Tmax"))

# % change vs MAP by PFT
g2 <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2) +
    labs(x = aridity_lab,
       y = transp_lab2,
       caption = caption)
g2 +
  geom_point(data = tot_transp_pft_diff,
             aes(aridity_index, y = TRANSP_perc_diff), alpha = 0.3) +
  geom_smooth(data = tot_transp_pft_diff,
              aes(aridity_index, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(subtitle = paste(perc_sub, "vs aridity, by PFT")) +
  facet_grid(SoilTreatment~PFT)

# just showing loam, for presentation
g2 +
  geom_point(data = filter(tot_transp_pft_diff, SoilTreatment == "loam"),
             aes(aridity_index, y = TRANSP_perc_diff), alpha = 0.3) +
  geom_smooth(data = filter(tot_transp_pft_diff, SoilTreatment == "loam"),
              aes(aridity_index, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(subtitle = paste(perc_sub, "vs aridity, by PFT, loam soil")) +
  facet_wrap(~PFT)

dev.off()

# * SM by lyrs across PFT ---------------------------------------------------

pdf("figures/soil_moisture/SM_by_lyr_across_PFT_v1.pdf")

# ** VWC ------------------------------------------------------------------
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = VWC_diff)) +
  labs(y = vwc_lab1,
       title = "Intensity effect on volumetric water content")

box_and_line(g)

ggplot(lyr_all_diff1, aes(y = VWC_diff)) +
  labs(y = vwc_lab1,
       title = "Change in volumetric water content vs. aridity") +
  aridity_base() +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02))

# WETDAYS
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = WETDAY_diff)) +
  labs(y = wetday_lab1,
       title = "Intensity effect on wet days")

box_and_line(g)

ggplot(lyr_all_diff1, aes(y = WETDAY_diff)) +
  labs(y = wetday_lab1,
       title = "Change in number of wet days vs. aridity") +
  aridity_base() +
  scale_y_continuous(breaks = c(-50, 0, 50))

# total transpiration
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Intensity effect on total transpiration from each layer")

box_and_line(g)

g1 <- ggplot(lyr_all_diff1, aes(y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Change in transpiration vs. aridity") +
  aridity_base()
g1

# percent change total transp
g <- ggplot(lyr_all_diff1, aes(x = -depth, y = TRANSP_perc_diff)) +
  labs(y = transp_lab2,
       title = "Intensity effect on transpiration change from each layer")

box_and_line(g)

ggplot(lyr_all_diff1, aes(y = TRANSP_perc_diff)) +
  labs(y = transp_lab2,
       title = "% change in transpiration vs aridity") +
  aridity_base() +
  scale_y_continuous(#breaks = c(0, 100),
                     minor_breaks = seq(from = -50, to = 200, by = 50))

dev.off()

# * transp by lyrs and PFT ------------------------------------------------

pdf("figures/soil_moisture/transp_by_lyr-PFT_v1.pdf")

# transp diff
ggplot(lyr_pft_diff1, aes(x = -depth, y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Intensity effect on total transpiration from each layer") +
  boxplot_base() +
  lyr_base() +
  facet_grid(SoilTreatment~PFT, scales = "free_x")

# % transp diff
ggplot(lyr_pft_diff1, aes(x = -depth, y = TRANSP_perc_diff)) +
  labs(y = transp_lab2,
       title = "Intensity effect on transpiration change from each layer") +
  boxplot_base() +
  lyr_base() +
  facet_grid(SoilTreatment~PFT)

dev.off()

