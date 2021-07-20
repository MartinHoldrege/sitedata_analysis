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
         drain = DEEPSWC_lowLayerDrain_cm_Mean,
         RUNOFF = RUNOFF_net_Mean,
         SNOWLOSS = PRECIP_snowloss_Mean) %>%
  select(site, SoilTreatment, intensity, warm, EVAPSURFACE, drain, RUNOFF,
         SNOWLOSS) %>%
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
  mutate(EVAPTOT = EVAPSOIL + EVAPSURFACE + SNOWLOSS,
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

# overview for question 1 of results sections
descript <- tot_transp_diff %>%
  filter(warm == "ambient") %>%
  group_by(SoilTreatment, intensity) %>%
  # sum of delta drainage and evap
  mutate(edrain_diff = drain_diff + EVAPTOT_diff,
         # did site follow expected trend (if water loss (drain + evap) went
         # down transp went up, and vice versa?)
         edrain_transp_trend = (edrain_diff >= 0 & TRANSP_diff <= 0) |
           (edrain_diff < 0 & TRANSP_diff > 0),
         # percent of MAP that is going to AET and drainage (~88%)
         # the rest is going to snoloss
         ETDRAIN_perc = (AET + drain)/PRECIP_ppt_Mean *100,
         SNOWLOSS_perc = SNOWLOSS/PRECIP_ppt_Mean *100) %>%
  summarize(
    n = n(),
    perc_pos = sum(TRANSP_diff > 0)/n*100,
    TRANSP_diff_m = mean(TRANSP_diff), # mean diff
    TRANSP_perc_diff_m = mean(TRANSP_perc_diff), # mean of percent diff
    TRANSP_diff_lwr = q1(TRANSP_diff), # 5th and 95th percentiles
    TRANSP_diff_upr = q2(TRANSP_diff),
    TRANSP_perc_diff_lwr = q1(TRANSP_perc_diff), # 5th and 95th percentiles
    TRANSP_perc_diff_upr = q2(TRANSP_perc_diff),
    transp_diff_gt0 = sum(TRANSP_diff >0),
    expected_trend = sum(edrain_transp_trend),
    ETDRAIN_perc = mean(ETDRAIN_perc),
    SNOWLOSS_perc = mean(SNOWLOSS_perc)

  )

descript
descript$ETDRAIN_perc %>% mean()
descript %>%
  filter(SoilTreatment == 'loam') %>%
  select(intensity, matches("perc"))
#view(descript)

# % sites with decrease in water loss and and increase in transp or vice versa
descript %>%
  # avg across intensity trmts
  group_by(SoilTreatment) %>%
  summarize(perc_expected_trend = sum(expected_trend)/sum(n))

# percent of total transpiration that is by shrubs
tot_transp_pft %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  mutate(shrub_transp_perc = TRANSP[PFT == "shrub"]/TRANSP[PFT == "total"]) %>%
  filter(PFT == "shrub") %>%
  group_by(intensity, warm, SoilTreatment) %>%
  summarize(shrub_transp_perc = mean(shrub_transp_perc))

# fitting loess curves ----------------------------------------------------

# geom_smooth uses stats::loess, which is what I'm using here


# * transp by PFT and total --------------------------------------------------
# dataframe includes both total transp and transp by pft

arid_range <- range(aridity1$aridity_index)

# so predictions are just inside the range of the data
arid_range <-  round(arid_range, 3) + c(0.001, -0.001)
# continuous data to predict on
newdata <- tibble(
  aridity_index = seq(from = arid_range[1], to = arid_range[2], by = 0.001)
)

yhat_pft_transp <- tot_transp_pft_diff %>%
  filter(warm == "ambient", SoilTreatment == "loam") %>%
  group_by(intensity, PFT) %>%
  nest() %>%
  # fitting loess curves
  mutate(mod = map(data, function(df) {
    loess(TRANSP_diff ~ aridity_index, data = df)
  }),
  # model for percent change
  mod_perc = map(data, function(df) {
    loess(TRANSP_perc_diff ~ aridity_index, data = df)
  }),
  yhat = map(mod, predict_newdata, newdata = newdata),
  yhat_perc = map(mod_perc, predict, newdata = newdata)) %>%
  select(-mod, -data, -mod_perc) %>%
  unnest(cols = c(yhat, yhat_perc)) %>%
  # absolute value
  mutate(yhat_abs = abs(yhat),
         yhat_perc_abs = abs(yhat_perc))

# summary of predicted transp changes by pft
yhat_pft_transp_sum <- yhat_pft_transp %>%
  summarize(
    # max predicted transp_diff
    max_yhat = max(yhat),
    # max_predicted % diff
    max_yhat_perc = max(yhat_perc),
    # aridity and max predicted
    arid_max_yhat = mean(aridity_index[yhat == max_yhat]),
    arid_min_yhat = mean(aridity_index[yhat == min(yhat)]),
    # 'transition' or 'threshold' point. constraining to range of interest,
    # so don't get where 'tail' crosses over 0 twice
    arid_0_yhat = mean(aridity_index[yhat_abs == min(yhat_abs[aridity_index < 0.8]) &
                                       aridity_index < 0.8 &
                                       aridity_index > 0.3])
  )
yhat_pft_transp_sum

# aridity index at which predicted grass transp responses were most negative
yhat_pft_transp_sum %>%
  filter(PFT == 'grass') %>%
  pull(arid_min_yhat) %>%
  mean()
