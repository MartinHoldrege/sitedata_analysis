# Martin Holdrege

# Script start May 21, 2021

# Purpose is to summarize mean daily soilwat2 output (focus on soil water/transpiration)
# This is a large file but for now it was created only to include
# loam, but coded to deal with multiple soil types for future use.


# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# Continue HERE

# soilwat2 output for each day of year
dly1 <- read_csv("data-processed/site_means/sw2_dly_means_v1.csv")

# soilwat2 output for each day of year by layer, but not by PFT
dly_lyr_all1 <- read_csv("data-processed/site_means/dly_mean_by_lyr-all_v1.csv")


# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")

# summary dfs -------------------------------------------------------------

dly2 <- dly1 %>%
  rename(day = Day,
         EVAPSURFACE = EVAPSURFACE_evap_total_Mean,
         drain = DEEPSWC_lowLayerDrain_cm_Mean) %>%
  select(site, day, SoilTreatment, intensity, warm, EVAPSURFACE, drain) %>%
  trmts2factors()


# quantiles
quantile(aridity1$aridity_index, c(0.25, 0.5, 0.75))

cut_aridity <- function(x) {
  cut(x, breaks = c(0, 0.3, 0.5,100),
      labels = c("aridity < 0.3", "aridity 0.3 - 0.5", "aridity > 0.5 "))
}

aridity1 <- aridity1 %>%
  select(-matches("_SD$")) %>%
  mutate(aridity_group = cut_aridity(aridity_index))


# * total transp ---------------------------------------------------------

# ** across pfts ----------------------------------------------------------

# total transpiration (to provide a total transpiration across layers)
dly_tot_transp <- dly_lyr_all1 %>%
  group_by(site, day, intensity, warm, SoilTreatment) %>%
  summarize(TRANSP = sum(TRANSP),
            EVAPSOIL = sum(EVAPSOIL, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  left_join(dly2, by = c("site", "day", "intensity", "warm", "SoilTreatment")) %>%
  # total evaporation
  mutate(EVAPTOT = EVAPSOIL + EVAPSURFACE)

# diff and % diff between a given treatment and ambient intensity and warming
# consider also calculate difference between trmt and ambient intensity
# and level of warming of that trmt.
dly_tot_diff <- dly_tot_transp %>%
  group_by(site, day, SoilTreatment) %>%
  mutate_at(.vars = c("TRANSP", "EVAPTOT", "drain"),
            .funs = list(diff = calc_diff, perc_diff = calc_perc_diff),
            # argument to be passed to funs:
            intensity = quote(intensity),
            warm = quote(warm)) %>%
  # diffs for ambient are 0
  filter(!(intensity == "ambient" & warm == "ambient"))
