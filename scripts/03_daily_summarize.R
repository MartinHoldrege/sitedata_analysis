# Martin Holdrege

# Script start May 21, 2021

# Purpose is to summarize mean daily soilwat2 output (biomass and soil water)
# This is a large file but for now it was created only to include
# loam, but coded to deal with multiple soil types for future use.


# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# Continue HERE

# soilwat2 output for each day of year
dly1 <- read_csv("data-processed/site_means/sw2_dly_means_v1.csv")

# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")

# summary dfs -------------------------------------------------------------

aridity1 <- aridity1 %>%
  select(-matches("_SD$"))

dly2 <- trmts2factors(dly1)


