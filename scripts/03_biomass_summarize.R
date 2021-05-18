# Martin Holdrege

# Script start 2/21/21

# Purpose is to summarize biomass data of stepwat runs on 1of 200sites
# Use descriptive stats to summarize biomass by plant functional type,
# soil type, and precipitation intensity and warming treatments


# dependencies ------------------------------------------------------------

library(tidyverse)
library(readr)
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# biomass files
# Biomass summaries from Biomass table
bio1 <- read_csv("data-processed/site_means/biomass_mean_v1.csv")

# aridity (generated in climate script)
aridity1 <- read_csv("data-processed/aridity_by_site.csv")%>%
  select(-matches("_SD$"))

# summary dfs -------------------------------------------------------------
bio1 <- bio1 %>%
  trmts2factors() %>%
  left_join(aridity1, by = "site")
# * biomass ---------------------------------------------------------------

# biomass difference for each PFT
bio_PFT_diff1 <- bio1 %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient"))

# biomass of primary PFTs
bio_prime_PFT1 <- bio1 %>%
  # primary pft groups
  mutate(prime_PFT = prime_PFT(PFT)) %>%
  filter(!is.na(.data$prime_PFT)) %>%
  group_by(site, SoilTreatment, prime_PFT, warm, intensity) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# biomass diff of primary PFTs
bio_prime_PFT_diff1 <- bio_prime_PFT1 %>%
  group_by(site, SoilTreatment, prime_PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient"))

# biomass for shrubs and grasses only
bio_SG1 <- bio1 %>%
  mutate(SG = SG_lookup[PFT]) %>%
  filter(!is.na(SG)) %>%
  group_by(site, intensity, warm, SoilTreatment, SG) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# shrub and grass biomass diff
bio_SG_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, SG) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  select(-biomass)

# shrub:grass ratio biomass diff
# NOTE: think about whether differences in ratios is really the best metric
# perhaps ratio of ratios would be better?
bio_SGr_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, intensity, warm) %>%
  # S:G ratio
  mutate(SGr = biomass[SG == "total_shrub"]/biomass[SG == "total_grass"]) %>%
  # otherwise rows duplicated
  filter(SG == "total_shrub") %>%
  ungroup() %>%
  select(-biomass, -SG) %>%
  group_by(site, SoilTreatment) %>%
  mutate(SGr_diff = calc_diff(SGr, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient"))

