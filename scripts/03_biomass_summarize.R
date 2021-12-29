# Martin Holdrege

# Script start 2/21/21

# Purpose is to summarize biomass data of stepwat runs at 200sites
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

# change in total biomass
bio_tot_diff1 <- bio1 %>%
  # primary pft groups
  mutate(prime_PFT = prime_PFT(PFT)) %>%
  # just a way to filter out individual species, so not double counting
  # biomass
  filter(!is.na(.data$prime_PFT)) %>%
  group_by(site, SoilTreatment, warm, intensity) %>%
  summarize(biomass = sum(biomass)) %>%
  group_by(site, SoilTreatment) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient"))


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
bio_SGr1 <- bio_SG1 %>%
  group_by(site, SoilTreatment, intensity, warm) %>%
  # S:G ratio
  mutate(SGr = biomass[SG == "total_shrub"]/biomass[SG == "total_grass"]) %>%
  # otherwise rows duplicated
  filter(SG == "total_shrub") %>%
  ungroup() %>%
  select(-biomass, -SG)

bio_SGr_diff1 <- bio_SGr1 %>%
  group_by(site, SoilTreatment) %>%
  mutate(SGr_diff = calc_diff(SGr, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient"))

# mean ratio of all shrubs to all grasses
bio_SGr_m <- bio_SGr1 %>%
  group_by(SoilTreatment, intensity, warm) %>%
  summarize(SGr_se = plotrix::std.error(SGr), # across plots
            SGr = mean(SGr),
            .groups = "drop")


# ratio of shrubs to p.cool.grass, for dotplot in manuscript
# here comparing c3 grasses (excluding cheatgrass) to shrubs (c3)

# for now only including arid and semi-arid sites
bio_SC3Gr <- bio1 %>%
  filter(PFT %in% c("sagebrush", "shrub", "p.cool.grass"),
         aridity_index < 0.5) %>%
  mutate(PFT = ifelse(PFT == "p.cool.grass", "p.cool.grass", "shrub")) %>%
  group_by(SoilTreatment, intensity, warm, site, PFT) %>%
  summarise(biomass = sum(biomass),# summing across shrubs
            # next summarise relies on groups except PFT:
            .groups = "drop_last") %>%
  summarize(SGr = biomass[PFT == "shrub"]/biomass[PFT == "p.cool.grass"],
            .groups = "drop_last") %>% # collapsing to plot level
  filter(is.finite(SGr)) %>% # if 0 grass the ratio is undefined
  summarize(SGr_se = plotrix::std.error(SGr), # across plots
            SGr = mean(SGr),
            .groups = "drop")

# ratio of shrubs to p.warm.grass, for appendix
# here comparing c4 grasses  to shrubs.

# sites where c4 grasses present under ambient conditions
c4_sites <- bio1 %>%
  filter(intensity == "ambient",
         warm == "ambient",
         PFT == "p.warm.grass",
         biomass > 0) %>%
  pull(site) %>%
  unique()
length(c4_sites)

bio_SC4Gr <- bio1 %>%
  filter(PFT %in% c("sagebrush", "shrub", "p.warm.grass"),
         site %in% c4_sites) %>%
  mutate(PFT = ifelse(PFT == "p.warm.grass", "p.warm.grass", "shrub")) %>%
  group_by(SoilTreatment, intensity, warm, site, PFT) %>%
  summarise(biomass = sum(biomass),# summing across shrubs
            # next summarise relies on groups except PFT:
            .groups = "drop_last") %>%
  summarize(SGr = biomass[PFT == "shrub"]/biomass[PFT == "p.warm.grass"],
            .groups = "drop_last") %>% # collapsing to plot level
  filter(is.finite(SGr)) %>% # if 0 grass the ratio is undefined
  summarize(SGr_se = plotrix::std.error(SGr), # across plots
            SGr = mean(SGr),
            .groups = "drop")

# four main PFTs (c3 and c4 grasses seperate)
bio_pft4 <- bio1 %>%
  mutate(PFT = PFT_four(PFT)) %>%
  filter(!is.na(PFT)) %>%
  group_by(site, intensity, warm, SoilTreatment, PFT) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site")

# difference of pft4
bio_pft4_diff <-   bio_pft4 %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  select(-biomass)

# 3 main PFTs (c3 and c4 grasses combined)
bio_pft3_diff <- bio1 %>%
  mutate(PFT = PFT_three(PFT)) %>%
  filter(!is.na(PFT)) %>%
  group_by(site, intensity, warm, SoilTreatment, PFT) %>%
  summarize(biomass = sum(biomass), .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  group_by(site, SoilTreatment, PFT) %>%
  mutate(# difference in biomass
    bio_diff = calc_diff(biomass, intensity, warm),
    bio_perc_diff = calc_perc_diff(biomass, intensity, warm)) %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  select(-biomass)


# summary statistics ------------------------------------------------------

# percent positive
perc_pos <- function(x) sum(x > 0)/length(x)*100

pft4_summary <- bio_pft4_diff %>%
  filter(SoilTreatment == "loam") %>%
  group_by(intensity, warm, PFT) %>%
  # because c4 grasses sometimes when from 0 biomass to some biomass
  # there are many infinite % changes
  mutate(bio_perc_diff = ifelse(is.infinite(bio_perc_diff),
                                NA, bio_perc_diff)) %>%
  summarize_at(.vars = c("bio_diff", "bio_perc_diff"),
               .funs = list(~mean(., na.rm = TRUE), lwr = q1, upr = q2,
                            pos = perc_pos))
pft4_summary %>%
  filter(warm == "ambient", intensity == "2x intensity")
pft4_summary %>%
  filter(warm %in% c("3C warming", "5C warming")) %>%
  select(matches("perc"), everything()) %>%
  print.data.frame()

(69.7 - 37.4)/37.4*100 # ~percent magnitude difference between 3c warming and 2x intensiyt


# * primary PFTs ----------------------------------------------------------
# mean changes in biomass of all the main PFT categories
# (this lumps all forbs but keeps everything else seperate)
prime_pft_smry <- bio_prime_PFT_diff1 %>%
  filter(SoilTreatment == "loam") %>%
  ungroup() %>%
  group_by(prime_PFT, warm, intensity) %>%
  summarize(across(c("bio_diff", "bio_perc_diff"),
                   .fns = mean))

prime_pft_smry %>%
  filter((intensity == "2x intensity" & warm %in% c("ambient", "3C warming")) |
           (intensity == "ambient" & warm == "3C warming"))
# fitting loess curves ----------------------------------------------------

# geom_smooth uses stats::loess, which is what I'm using here


# * bio by PFT --------------------------------------------------
# dataframe includes both total transp and transp by pft

arid_range <- range(aridity1$aridity_index)

# so predictions are just inside the range of the data
arid_range <-  round(arid_range, 3) + c(0.001, -0.001)
# continuous data to predict on
newdata <- tibble(
  aridity_index = seq(from = arid_range[1], to = arid_range[2], by = 0.001)
)

yhat_pft_bio <- bio_pft4_diff %>%
  filter(warm == "ambient", SoilTreatment == "loam") %>%
  group_by(intensity, PFT) %>%
  nest() %>%
  # fitting loess curve
  mutate(mod = map(data, function(df) {
    loess(bio_diff ~ aridity_index, data = df)
  }),
  # model for percent change
  mod_perc = map(data, function(df) {
    loess(bio_perc_diff ~ aridity_index, data = df)
  }),
  yhat = map(mod, predict_newdata, newdata = newdata),
  yhat_perc = map(mod_perc, predict, newdata = newdata)) %>%
  select(-mod, -data, -mod_perc) %>%
  unnest(cols = c(yhat, yhat_perc)) %>%
  # absolute value
  mutate(yhat_abs = abs(yhat),
         yhat_perc_abs = abs(yhat_perc))

# summary of predicted bio changes by pft
yhat_pft_bio %>%
  summarize(
    # max predicted bio_diff
    max_yhat = max(yhat),
    min_yhat = min(yhat),
    # max_predicted % diff
    max_yhat_perc = max(yhat_perc),
    min_yhat_perc = min(yhat_perc),
    # aridity and max predicted
    arid_max_yhat = mean(aridity_index[yhat == max_yhat]),
    arid_min_yhat = mean(aridity_index[yhat == min_yhat]),
    # 'transition' or 'threshold' point. constraining to range of interest,
    # so don't get where 'tail' crosses over 0 twice.
    # aridity at which predicted value is 0.
    arid_0_yhat = mean(aridity_index[yhat_abs == min(yhat_abs[aridity_index < 0.8 & aridity_index > 0.3]) &
                                       aridity_index < 0.7 &
                                       aridity_index > 0.4])
  )
