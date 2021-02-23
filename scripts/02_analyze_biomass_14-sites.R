# Martin Holdrege

# Script start 2/21/21

# Purpose is to biomass data of preliminary stepwat run on 14 sites
# Use descriptive stats/figures to compare ambient and 2x precipitation intensity


# dependencies ------------------------------------------------------------

library(tidyverse)
library(readr)
source("scripts/functions.R")
# read in data ------------------------------------------------------------

# biomass files
# abiotic variables from Biomass table
# bio_abiotic1 <- read_csv("data-processed/14sites/biomass_mean_abiotic_14sites.csv")

# Biomass summaries from Biomass table
bio1 <- read_csv("data-processed/14sites/biomass_mean_14sites.csv") %>%
  mutate(intensity = intensity_lookup[intensity])

# summary dfs -------------------------------------------------------------

# * biomass ---------------------------------------------------------------

# biomass difference for each PFT
bio_PFT_diff1 <- bio1 %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(# difference in biomass
    bio_diff = biomass[intensity == "2x_intensity"] -
      biomass[intensity == "ambient"]) %>%
  # rows are now duplicated so filtering
  filter(intensity == "ambient") %>%
  select(-intensity)

# biomass for shrubs and grasses only
bio_SG1 <- bio1 %>%
  mutate(SG = str_extract(PFT, "shrub|grass")) %>%
  filter(!is.na(SG)) %>%
  group_by(site, intensity, SoilTreatment, SG) %>%
  summarize(biomass = sum(biomass), .groups = "drop")

bio_SG_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, SG) %>%
  mutate(# difference in biomass
    bio_diff = biomass[intensity == "2x_intensity"] -
      biomass[intensity == "ambient"]) %>%
  filter(intensity == "ambient") %>%
  select(-intensity)
