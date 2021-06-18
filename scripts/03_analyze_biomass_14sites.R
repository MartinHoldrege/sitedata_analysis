# Martin Holdrege

# Script start 2/21/21

# Purpose is to biomass data of preliminary stepwat run on 14 sites
# Use descriptive stats/figures to compare ambient and 2x precipitation intensity


# dependencies ------------------------------------------------------------

library(tidyverse)
library(readr)
source("scripts/functions.R")
source("scripts/02_climate_14sites.R")
theme_set(theme_classic())

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

# biomass of primary PFTs
bio_prime_PFT1 <- bio1 %>%
  # primary pft groups
  mutate(prime_PFT = prime_PFT(PFT)) %>%
  filter(!is.na(.data$prime_PFT)) %>%
  group_by(site, SoilTreatment, prime_PFT, intensity) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(climate1, by = "site")

# biomass diff of primary PFTs
bio_prime_PFT_diff1 <- bio_prime_PFT1 %>%
  group_by(site, SoilTreatment, prime_PFT) %>%
  mutate(# difference in biomass
    bio_diff = biomass[intensity == "2x_intensity"] -
      biomass[intensity == "ambient"]) %>%
  # rows are now duplicated so filtering
  filter(intensity == "ambient") %>%
  select(-intensity)

# biomass for shrubs and grasses only
bio_SG1 <- bio1 %>%
  mutate(SG = SG_lookup[PFT]) %>%
  filter(!is.na(SG)) %>%
  group_by(site, intensity, SoilTreatment, SG) %>%
  summarize(biomass = sum(biomass), .groups = "drop")

# shrub and grass biomass diff
bio_SG_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, SG) %>%
  mutate(# difference in biomass
    bio_diff = biomass[intensity == "2x_intensity"] -
      biomass[intensity == "ambient"]) %>%
  filter(intensity == "ambient") %>%
  select(-intensity, -biomass) %>%
  left_join(climate1, by = "site")

# shrub:grass ratio biomass diff
# NOTE: think about whether differences in ratios is really the best metric
# perhaps ratio of ratios would be better?
bio_SGr_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, intensity) %>%
  # S:G ratio
  mutate(SGr = biomass[SG == "total_shrub"]/biomass[SG == "total_grass"]) %>%
  # otherwise rows duplicated
  filter(SG == "total_shrub") %>%
  ungroup() %>%
  select(-biomass, -SG) %>%
  pivot_wider(id_cols = c(site, SoilTreatment),
              names_from = "intensity",
              values_from = SGr,
              names_prefix = "SGr_") %>%
  mutate(SGr_diff = SGr_2x_intensity - SGr_ambient) %>%
  left_join(climate1, by = "site")


# figures -----------------------------------------------------------------


# * fig params ------------------------------------------------------------

caption <- paste("Data from preliminary STEPWAT2 run for 14 sites.",
                 "PPT intensity doubled by adding odd to even events.",
                 "\nEvents defined as one or more consecutive PPT days.")
bio_lab <- "Biomass (g/m2)"
bio_diff_lab1 <- expression(Biomass[2*x]~-~Biomass[ambient]~"("*gm^-2*")")
SGr_diff_lab1 <- expression("shrub:grass difference ("*S*":"*G[2*x]~-~S*":"*G[ambient]*")")
PFT_lab <- "Plant functional type"
aridity_lab <- "Aridity index (MAP/PET)"
clear_x <- function(){
  list(theme(axis.ticks.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.x = element_blank()))
}
base <- function(){
  list(labs(caption = caption),
       geom_hline(yintercept = 0, alpha = 0.4, linetype = 2))
}


pdf("figures/14sites/bio_sensitivity_v1.pdf")
# * SG --------------------------------------------------------------------

# Shrub and grass diff
g <- ggplot(bio_SG_diff1, aes(y = bio_diff)) +
  labs(y = bio_diff_lab1) +
  base()

# s and g diff boxplot
g +  geom_boxplot(aes(x = SG)) +
  labs(x = PFT_lab,
       title = "Change in shrub and grass biomass")

# S and G diff across aridity
g +
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "lm", se = FALSE) +
  facet_wrap(~SG) +
  labs(x = aridity_lab,
       title = "Shrub and grass sensitivity with aridity")


# * SGr -------------------------------------------------------------------

g <- ggplot(bio_SGr_diff1, aes(y = SGr_diff)) +
  labs(y = SGr_diff_lab1) +
  base()

# SGr diff boxplot
g +  geom_boxplot() +
  clear_x() +
  labs(title = "Change in shrub:grass ratio with intensity")

# SGr diff across aridity
g +
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "lm", se = FALSE) +
  labs(title = "Shrub:grass ratio sensitivity across aridity",
       x = aridity_lab)


# * primary PFT diffs ------------------------------------------------------

# Shrub and grass diff
g <- ggplot(bio_prime_PFT_diff1, aes(y = bio_diff)) +
  labs(y = bio_diff_lab1) +
  base()

# PFT diff boxplots
g1 <- g +
  labs(x = PFT_lab, title = "Biomass sensitivity by PFT")

g1 + geom_boxplot(aes(x = prime_PFT))

# so can see different scales
g1 + geom_boxplot() +
  clear_x() +
  facet_wrap(~prime_PFT, scales = "free_y") +
  base() +
  labs(subtitle = "Note: scales differ")

# PFT diff across aridity
g +
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "lm", se = FALSE) +
  facet_wrap(~prime_PFT, scales = "free_y") +
  labs(x = aridity_lab,
       title = "Biomass sensitivity with aridity by PFT",
       subtitle = "Note: scales differ")

dev.off()


