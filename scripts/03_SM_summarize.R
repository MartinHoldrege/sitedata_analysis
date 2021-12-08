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


# soils diffs -------------------------------------------------------------
# plot level differences in responses between soil types, to better
# get at plot level soil texture effects

ref_soil <- "silt" # soil type comparing against


mutate_soil_diffs <- function(df, .vars, ref_soil) {
  # difference between given soil type and ref_soil
  out <- df %>%
    mutate_at(.vars = .vars,
              .funs = calc_soil_diff,
              # arguments for calc_soil_diff
              soil = quote(SoilTreatment),
              ref_soil = ref_soil) %>%
    filter(SoilTreatment != ref_soil)
  out
}

# across depths and pfts
tot_transp_diff_soil <- tot_transp_diff %>%
  group_by(site, intensity, warm) %>%
  mutate_soil_diffs(.vars = paste0(c("TRANSP", "EVAPTOT", "drain", "AET", "T_AET"),
                                   "_diff"),
                    ref_soil)

# across depths by pfts
tot_transp_pft_diff_soil <- tot_transp_pft_diff %>%
  group_by(site, intensity, warm, PFT) %>%
  mutate_soil_diffs(.vars = "TRANSP_diff",
                    ref_soil)

# by depth across pft
lyr_all_diff_soil <- lyr_all_diff1 %>%
  group_by(site, intensity, warm, depth) %>%
  # difference between given soil type and ref_soil
  mutate_soil_diffs(.vars = c("TRANSP_diff", "WETDAY_diff"),
                    ref_soil = ref_soil)

# by depth and pft
lyr_pft_diff_soil <- lyr_pft_diff1 %>%
  group_by(site, intensity, warm, depth, PFT) %>%
  # difference between given soil type and ref_soil
  mutate_soil_diffs(.vars = c("TRANSP", "TRANSP_diff", "TRANSP_perc_diff"),
                    ref_soil = ref_soil)

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
    SNOWLOSS_perc = mean(SNOWLOSS_perc),
    drain_diff_m = mean(drain_diff),
    drain_diff_lwr = q1(drain_diff),
    drain_diff_upr = q2(drain_diff),
    drain_perc_diff_m = mean(drain_perc_diff),
    drain_perc_diff_lwr = q1(drain_perc_diff),
    drain_perc_diff_upr = q2(drain_perc_diff),
    EVAPTOT_diff_m = mean(EVAPTOT_diff),
    EVAPTOT_perc_diff_m = mean(EVAPTOT_perc_diff),
    EVAPTOT_perc_diff_lwr = q1(EVAPTOT_perc_diff),
    EVAPTOT_perc_diff_upr = q2(EVAPTOT_perc_diff),
    .groups = "drop"
  )


descript %>%
  filter(SoilTreatment == 'loam') %>%
  select(intensity, matches("^drain|EVAPTOT"))
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


# * proportion total transp -----------------------------------------------
# percent of total transpiration from various depths

lyr_pft_perc_tot <- lyr_pft1 %>%
  select(-layer) %>%
  group_by(site, intensity, warm, PFT, SoilTreatment) %>%
  mutate(# total transpiration for pft across layers
    tot_transp = sum(TRANSP)) %>%
  # using seperate mutate calls so works with dtplyr
  mutate(# % of total transp, transpired from specific soil layer
         perc_transp = TRANSP/tot_transp*100) %>%
  group_by(site, PFT, SoilTreatment, depth) %>%
  mutate(perc_transp_diff = calc_diff(perc_transp, intensity, warm)) %>%
  group_by(PFT, intensity, warm,  SoilTreatment, depth) %>%
  summarise_at(.vars = c("TRANSP", "perc_transp", "perc_transp_diff"),
            .funs = list(m = mean))

df <- lyr_pft_perc_tot %>%
  filter(warm == "ambient", SoilTreatment == "loam")
# View(df)

# % changes by depth grouping
df2 <- df %>%
  mutate(depth_group = cut_depth(depth)) %>%
  group_by(PFT, intensity, warm, SoilTreatment, depth_group) %>%
  summarise_at(.vars = c("perc_transp_m", "perc_transp_diff_m"),
               .funs = sum)


# * depth group t-test ----------------------------------------------------
# check whether changes in transp from deep soil layers is significant
# for grasses

# (whether the data underlying df2 above, is significant)
grass_m_diff <- lyr_pft_diff1 %>%
  mutate(depth_group = cut_depth(depth)) %>%
  filter(SoilTreatment == "loam", intensity == "2x intensity",
         warm == "ambient",
         PFT == "grass", depth_group == '40-150 cm') %>%
  mutate(depth_group = cut_depth(depth)) %>%
  group_by(site) %>%
  # diff for deep soils for grass--summing differences across depths
  # in the deep category
  summarize(TRANSP_diff = sum(TRANSP_diff)) %>%
  pull(TRANSP_diff)

hist(grass_m_diff)
t.test(grass_m_diff) # significant difference

# fitting loess curves ----------------------------------------------------
# Next: look at residuals and regress against seasonality

# geom_smooth uses stats::loess, which is what I'm using here

# * transp by PFT and total --------------------------------------------------
# dataframe includes both total transp and transp by pft

arid_range <- range(aridity1$aridity_index)

# so predictions are just inside the range of the data
arid_range <-  round(arid_range, 3) + c(0.001, -0.001)
# continuous data to predict on (including columns needed by different
# models)
newdata <- tibble(
  aridity_index = seq(from = arid_range[1], to = arid_range[2], by = 0.001),
  PRECIP_ppt_Mean = seq(from = min(aridity1$PRECIP_ppt_Mean),
                        to = max(aridity1$PRECIP_ppt_Mean),
                        length.out = length(aridity_index))
)

yhat_pft_transp0 <- tot_transp_diff %>%
  ungroup() %>%
  select(site, intensity, warm, SoilTreatment, EVAPTOT_diff, drain_diff) %>%
  right_join(tot_transp_pft_diff,
             by = c("site", "intensity", "warm", "SoilTreatment")) %>%
  filter(warm == "ambient", SoilTreatment == "loam") %>%
  select(-warm, -SoilTreatment) %>%
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
  # model for MAP
  mod_map = map(data, function(df) {
    loess(TRANSP_diff ~ PRECIP_ppt_Mean, data = df)
  }),
  # model for evap
  # drainage and evap differences between PFTs don't apply, it's just
  # fitting the same model for each PFT
  mod_evap = map(data, function(df) {
    loess(EVAPTOT_diff ~ aridity_index, data = df)
  }),
  mod_drain = map(data, function(df) {
    loess(drain_diff ~ aridity_index, data = df)
  }),
  # predicting for original x data
  yhat_orig = map(mod, predict),
  # predicting on 'continuous' data
  yhat = map(mod, predict_newdata, newdata = newdata),
  yhat_perc = map(mod_perc, predict, newdata = newdata),
  yhat_map = map(mod_map, predict, newdata = newdata),
  evap_yhat = map(mod_evap, predict, newdata = newdata),
  drain_yhat = map(mod_drain, predict, newdata = newdata)
  )



# original x values
yhat_pft_transp_orig <- yhat_pft_transp0 %>%
  select(-yhat, -yhat_perc, -yhat_map, -matches("_yhat")) %>%
  unnest(cols = c(yhat_orig, data)) %>%
  mutate(resid = TRANSP_diff - yhat_orig)

# 'continous x'
yhat_pft_transp <- yhat_pft_transp0 %>%
  select(-mod, -data, -mod_perc, -yhat_orig) %>%
  unnest(cols = c(yhat, yhat_perc, yhat_map, evap_yhat, drain_yhat)) %>%
  # absolute value
  mutate(yhat_abs = abs(yhat),
         yhat_perc_abs = abs(yhat_perc),
         yhat_map_abs = abs(yhat_map))

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
    # AI at which evap reduced the most
    arid_min_evap = mean(aridity_index[evap_yhat == min(evap_yhat)]),
    # aridity index at which drainage increased the most
    arid_max_drain =  mean(aridity_index[drain_yhat == max(drain_yhat)]),
    # 'transition' or 'threshold' point. constraining to range of interest,
    # so don't get where 'tail' crosses over 0 twice
    arid_0_yhat = mean(aridity_index[yhat_abs == min(yhat_abs[aridity_index < 0.8]) &
                                       aridity_index < 0.8 &
                                       aridity_index > 0.3]),
    # Threshold when using MAP.
    map_0_yhat = mean(PRECIP_ppt_Mean[yhat_map_abs == min(yhat_map_abs[aridity_index < 0.8]) &
                                       aridity_index < 0.8 &
                                       aridity_index > 0.3])
  )
yhat_pft_transp_sum

# means across intensity treatments
yhat_pft_transp_sum %>%
  group_by(PFT) %>%
  summarise_if(is.numeric, mean)


