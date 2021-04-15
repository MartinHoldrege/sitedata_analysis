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
# Biomass summaries from Biomass table
bio1 <- read_csv("data-processed/site_means/biomass_mean_v1.csv")

# aridity (generated in climate script)
aridity1 <- read_csv("data-processed/aridity_by_site.csv")%>%
  select(-matches("_SD$"))

# summary dfs -------------------------------------------------------------
bio1 <- bio1 %>%
  left_join(aridity1, by = "site")
# * biomass ---------------------------------------------------------------

# biomass difference for each PFT
bio_PFT_diff1 <- bio1 %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity),
    bio_perc_diff = calc_perc_diff(biomass, intensity)) %>%
  filter(intensity == "ambient")

# biomass of primary PFTs
bio_prime_PFT1 <- bio1 %>%
  # primary pft groups
  mutate(prime_PFT = prime_PFT(PFT)) %>%
  filter(!is.na(.data$prime_PFT)) %>%
  group_by(site, SoilTreatment, prime_PFT, intensity) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# biomass diff of primary PFTs
bio_prime_PFT_diff1 <- bio_prime_PFT1 %>%
  group_by(site, SoilTreatment, prime_PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity),
    bio_perc_diff = calc_perc_diff(biomass, intensity)) %>%
  filter(intensity != "ambient")

# biomass for shrubs and grasses only
bio_SG1 <- bio1 %>%
  mutate(SG = SG_lookup[PFT]) %>%
  filter(!is.na(SG)) %>%
  group_by(site, intensity, SoilTreatment, SG) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# shrub and grass biomass diff
bio_SG_diff1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, SG) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity),
    bio_perc_diff = calc_perc_diff(biomass, intensity)) %>%
  filter(intensity != "ambient") %>%
  select(-biomass)

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
  group_by(site, SoilTreatment) %>%
  mutate(SGr_diff = calc_diff(SGr, intensity)) %>%
  filter(intensity != "ambient")

# figures -----------------------------------------------------------------
theme_set(theme_classic())
theme_update(strip.background = element_blank(),
             strip.text = element_text(size = 17),
             axis.title = element_text(size = 16),
             # increasing right margin so numbers not cutoff
             plot.margin = unit(c(5.5, 10, 5.5, 5.5), "points"))

# * fig params ------------------------------------------------------------

caption <- paste("STEPWAT2 run for 200 sites.",
                 "PPT intensity doubled by adjusting markov coefficients.")
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



pdf("figures/biomass/bio_sensitivity_v1.pdf")

# * SG --------------------------------------------------------------------

# Shrub and grass diff
g <- ggplot(bio_SG_diff1, aes(y = bio_diff)) +
  labs(y = bio_diff_lab1) +
  base()

# s and g diff boxplot
g +  geom_boxplot(aes(x = SG)) +
  lemon::facet_rep_wrap(~SoilTreatment) +
  labs(x = PFT_lab,
       title = "Change in shrub and grass biomass")

# S and G diff across aridity
g +
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "loess", se = FALSE) +
  lemon::facet_rep_grid(SG~SoilTreatment, scales = "free_y") +
  labs(x = aridity_lab,
       title = "Shrub and grass sensitivity with aridity")

bio_SG_diff1 %>%
  filter(SoilTreatment == "loam") %>%
  ggplot(aes(y = bio_diff)) +
  labs(y = bio_diff_lab1) +
  base()+
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "loess", se = FALSE) +
  lemon::facet_rep_wrap(~SG, scales = "free_y", ncol = 1) +
  labs(x = aridity_lab,
       title = "Shrub and grass sensitivity with aridity, loam")

# * SGr -------------------------------------------------------------------

g <- ggplot(bio_SGr_diff1, aes(y = SGr_diff)) +
  labs(y = SGr_diff_lab1) +
  base()

# SGr diff boxplot
g +  geom_boxplot(aes(x = SoilTreatment)) +
  labs(title = "Change in shrub:grass ratio with intensity",
       x = NULL)

# SGr diff across aridity
g +
  geom_point(aes(x = aridity_index)) +
  geom_smooth(aes(x = aridity_index), method = "loess", se = FALSE) +
  labs(title = "Shrub:grass ratio sensitivity across aridity",
       x = aridity_lab) +
  lemon::facet_rep_wrap(~SoilTreatment)


# * primary PFT diffs ------------------------------------------------------

# Shrub and grass diff
g <- ggplot(bio_prime_PFT_diff1, aes(y = bio_diff)) +
  labs(y = bio_diff_lab1) +
  base()

# PFT diff boxplots
g1 <- g +
  labs(x = PFT_lab, title = "Biomass sensitivity by PFT")

g1 + geom_boxplot(aes(x = prime_PFT)) +
  facet_wrap(~SoilTreatment) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.2, hjust=0.2))

# so can see different scales
g1 + geom_boxplot() +
  clear_x() +
  lemon::facet_rep_grid(prime_PFT~SoilTreatment, scales = "free_y") +
  base() +
  labs(subtitle = "Note: scales differ")

# PFT diff across aridity
g +
  geom_point(aes(x = aridity_index), alpha = 0.3) +
  geom_smooth(aes(x = aridity_index), method = "loess", se = FALSE) +
  lemon::facet_rep_grid(prime_PFT~SoilTreatment, scales = "free_y") +
  labs(x = aridity_lab,
       title = "Biomass sensitivity with aridity by PFT",
       subtitle = "Note: scales differ")

dev.off()


