# Martin Holdrege

# Script start 4/13/21

# Purpose is to summarize soil moisture data (means of yearly values)
# Create descriptive stats of ambient and increased intensity and warming
# The basic soil moisture summary data files used here were generated in
# 01_summary_dfs_200sites.R

# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")

# soil moisture metrics by PFT and layer
lyr_pft1 <- read_csv("data-processed/site_means/yr_mean_by_lyr-PFT_v1.csv")


# soil moisture metrics by layer, (not by PFT)
lyr_all1 <- read_csv("data-processed/site_means/yr_mean_by_lyr-all_v1.csv")

# yearly soilwat2 output, not by layer
all1 <- read_csv("data-processed/site_means/sw2_yr_means_v1.csv")

# summary dfs -------------------------------------------------------------

aridity1 <- aridity1 %>%
  select(-matches("_SD$"))

lyr_all1 <- lyr_all1 %>%
  mutate(SoilTreatment = soil_name(SoilTreatment),
         depth = lyr2depth(layer)) %>%
  trmts2factors()


lyr_pft1 <- lyr_pft1 %>%
  filter(PFT != "tree") %>% # no trees being modeled
  mutate(SoilTreatment = soil_name(SoilTreatment),
         depth = lyr2depth(layer))%>%
  trmts2factors()


all2 <- all1 %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  rename(EVAPSURFACE = EVAPSURFACE_evap_total_Mean,
         drain = DEEPSWC_lowLayerDrain_cm_Mean) %>%
  select(site, SoilTreatment, intensity, warm, EVAPSURFACE, drain) %>%
  trmts2factors()


# * total transp ---------------------------------------------------------

# ** across pfts ----------------------------------------------------------

# total transpiration (to provide a total transpiration across layers)
tot_transp <- lyr_all1 %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  summarize(TRANSP = sum(TRANSP),
            EVAPSOIL = sum(EVAPSOIL, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  left_join(all2, by = c("site", "intensity", "warm", "SoilTreatment")) %>%
  # total evaporation
  mutate(EVAPTOT = EVAPSOIL + EVAPSURFACE,
         AET = TRANSP + EVAPTOT, # actual evapotranspiration
         T_AET = TRANSP/AET) # T/AET ratio

# diff and % diff between a given treatment and ambient intensity and warming
# consider also calculate difference between trmt and ambient intensity
# and level of warming of that trmt.
tot_transp_diff <- tot_transp %>%
  group_by(site, SoilTreatment) %>%
  mutate_at(.vars = c("TRANSP", "EVAPTOT", "drain", "AET", "T_AET"),
            .funs = list(diff = calc_diff, perc_diff = calc_perc_diff),
            # argument to be passed to funs:
            intensity = quote(intensity),
            warm = quote(warm)) %>%
  # diffs for ambient are 0
  filter(!(intensity == "ambient" & warm == "ambient"))

# ** by pft ---------------------------------------------------------------

tot_transp_pft <- lyr_pft1 %>%
  # later group by SoilTreatment when appropriate
  group_by(site, intensity, warm, SoilTreatment, PFT) %>%
  summarize(TRANSP = sum(TRANSP), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# diff between intensity and ambient
tot_transp_pft_diff <- tot_transp_pft %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(TRANSP_diff = calc_diff(TRANSP, intensity, warm),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity, warm)) %>%
  # diffs for ambient are 0
  filter(!(intensity == "ambient" & warm == "ambient"))

# * differences by depth -------------------------------------------------

# across all PFTs
# note: VWCBULK and VWCMATRIC look identical (1:1 line)
# this way of calculating differences allows for including
# of more intensity levels down the road
lyr_all_diff1 <- lyr_all1 %>%
  select(site, intensity, warm, depth, SoilTreatment, VWCMATRIC, WETDAY,
         TRANSP) %>%
  group_by(site, depth, SoilTreatment) %>%
  mutate(VWC_diff = calc_diff(VWCMATRIC, intensity, warm),
         WETDAY_diff = calc_diff(WETDAY, intensity, warm),
         TRANSP_diff = calc_diff(TRANSP, intensity, warm),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  left_join(aridity1, by = "site")

# diffs by pft (total is a pft category)
lyr_pft_diff1 <- lyr_pft1 %>%
  select(site, intensity, warm, depth, SoilTreatment, PFT, TRANSP) %>%
  group_by(site, depth, PFT, SoilTreatment) %>%
  mutate(TRANSP_diff = calc_diff(TRANSP, intensity, warm),
         TRANSP_perc_diff = calc_perc_diff(TRANSP, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  left_join(aridity1, by = "site")


# descriptive stats -------------------------------------------------------
q1
# overview for question 1 of results sections
tot_transp_diff %>%
  filter(warm == "ambient") %>%
  group_by(SoilTreatment, intensity) %>%
  summarize(
    n = n(),
    perc_pos = sum(TRANSP_diff > 0)/n*100,
    TRANSP_diff_m = mean(TRANSP_diff), # mean diff
    TRANSP_perc_diff_m = mean(TRANSP_perc_diff), # mean of percent diff
    TRANSP_diff_lwr = q1(TRANSP_diff), # 5th and 95th percentiles
    TRANSP_diff_upr = q2(TRANSP_diff),
  )
