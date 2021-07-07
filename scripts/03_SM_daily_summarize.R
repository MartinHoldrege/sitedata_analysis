# Martin Holdrege

# Script start May 21, 2021

# Purpose is to summarize mean daily soilwat2 output (focus on soil water/transpiration)
# This is a large file but for now it was created only to include
# loam, but coded to deal with multiple soil types for future use.


# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/functions.R")

# read in data ------------------------------------------------------------

# soilwat2 output for each day of year
dly1 <- read_csv("data-processed/site_means/sw2_dly_means_v1.csv")

# soilwat2 output for each day of year by layer, but not by PFT
dly_lyr_all1 <- read_csv("data-processed/site_means/dly_mean_by_lyr-all_v1.csv")


# aridity
aridity1 <- read_csv("data-processed/aridity_by_site.csv")


# functions ---------------------------------------------------------------

# quantiles
q1 <- function(x) quantile(x, 0.05, na.rm = TRUE)
q2 <- function(x) quantile(x, 0.95, na.rm = TRUE)

cut_aridity <- function(x) {
  cut(x,
      breaks = c(0, 0.3, 0.5, 100),
      labels = c("aridity < 0.3", "aridity 0.3 - 0.5", "aridity > 0.5"))
}



# summary dfs -------------------------------------------------------------

dly2 <- dly1 %>%
  rename(day = Day,
         EVAPSURFACE = EVAPSURFACE_evap_total_Mean,
         drain = DEEPSWC_lowLayerDrain_cm_Mean) %>%
  select(site, day, SoilTreatment, intensity, warm, EVAPSURFACE, drain)

aridity1 <- aridity1 %>%
  select(-matches("_SD$")) %>%
  mutate(aridity_group = cut_aridity(aridity_index))


# * total transp ---------------------------------------------------------

# ** across pfts ----------------------------------------------------------

# logical so that this section of code can be turned off from
# scripts that source it (because slow, memory overloading)
run_dly_all <- if(!exists("run_dly_all")) {
  TRUE
} else {
  run_dly_all
}

if (run_dly_all){
# total transpiration (to provide a total transpiration across layers)
dly_tot_transp <- dly_lyr_all1 %>%
  group_by(site, day, intensity, warm, SoilTreatment) %>%
  summarize(TRANSP = sum(TRANSP),
            EVAPSOIL = sum(EVAPSOIL, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  left_join(dly2, by = c("site", "day", "intensity", "warm", "SoilTreatment")) %>%
  # total evaporation
  mutate(EVAPTOT = EVAPSOIL + EVAPSURFACE,
         AET = TRANSP + EVAPTOT, # actual evapotranspiration
         T_AET = TRANSP/AET)%>%
  trmts2factors()

# mean and 5th and 95th percentiles and cumulative values
dly_tot_mean <- dly_tot_transp  %>%
  # cumulative transp etc. throughout the year
  group_by(site, intensity, warm, SoilTreatment, aridity_group) %>%
  arrange(day) %>%
  mutate_at(vars(matches("TRANSP|drain|EVAPTOT")),
            .funs = list(cum = cumsum)) %>%
  group_by(day, intensity, warm, SoilTreatment, aridity_group) %>%
  # 5th and 95th percentiles calculated to show bands
  summarise(across(matches("TRANSP|drain|EVAPTOT|AET"),
                   .fns = list(mean = ~mean(.x, na.rm = TRUE), lwr = q1,
                               upr = q2)),
            .groups = "drop")


# diff and % diff between a given treatment and ambient intensity and warming
# consider also calculate difference between trmt and ambient intensity
# and level of warming of that trmt.
dly_tot_diff <- dly_tot_transp %>%
  group_by(site, day, SoilTreatment) %>%
  mutate_at(.vars = c("TRANSP", "EVAPTOT", "drain", "AET", "T_AET"),
            .funs = list(diff = calc_diff, perc_diff = calc_perc_diff),
            # argument to be passed to funs:
            intensity = quote(intensity),
            warm = quote(warm)) %>%
  # diffs for ambient are 0
  filter(!(intensity == "ambient" & warm == "ambient"))

# mean diffs by group
dly_tot_diff_means <- dly_tot_diff %>%
  group_by(day, intensity, warm, SoilTreatment, aridity_group) %>%
  # 5th and 95th percentiles calculated to show bands
  summarise(across(matches("_diff"),
                   .fns = list(mean = ~mean(.x, na.rm = TRUE), lwr = q1,
                               upr = q2)),
            .groups = "drop")


# free up memory, objects not needed
remove("dly_tot_transp", "dly_tot_diff")

}
# * by depth across pft --------------------------------------------------

remove("dly1", "dly2")

# logical so that this section of code can be turned off from
# scripts that source it (because slow, memory overloading)
run_dly_lyr_all <- if(!exists("run_dly_lyr_all")) {
  TRUE
} else {
  run_dly_lyr_all
}

if (run_dly_lyr_all){

library(dtplyr) # this code crashes (memory constraint I think) when
# just use regular dplyr

# grouping by depth group and aridity group
dly_lyr_means <- dly_lyr_all1 %>%
  #slice_sample(n = 100) %>%
  lazy_dt() %>%
  # slice_sample(n = 1000) %>%  # for testing
  mutate(depth_group = cut_depth(lyr2depth(layer))) %>%
  select(-EVAPSOIL, -layer) %>%
  group_by(site, day, intensity, warm, SoilTreatment, depth_group) %>%
  summarize(TRANSP = sum(TRANSP),
            VWCMATRIC = mean(VWCMATRIC),
            WETDAY = mean(WETDAY),
            .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  # now averaging across sites
  group_by(day, intensity, warm, SoilTreatment, depth_group, aridity_group) %>%
  # note for now not calculating upr and lwr to reduce computation
  summarize(across(c("TRANSP", "VWCMATRIC", "WETDAY"),
                   .fns = list(mean = ~mean(.x, na.rm = TRUE)))) %>%
  as_tibble() %>%
  trmts2factors()


# avaraged across aridity groups--for pub qual figure
# mean and 5th and 9th percentiles per depth category
dly_lyr_means_all <- dly_lyr_all1 %>%
  filter(warm == "ambient") %>%
  # slice_sample(n = 100) %>% for testing
  lazy_dt() %>%
  mutate(depth_group = cut_depth(lyr2depth(layer))) %>%
  select(-EVAPSOIL, -layer) %>%
  group_by(site, day, intensity, warm, SoilTreatment, depth_group) %>%
  summarize(TRANSP = sum(TRANSP),
            WETDAY = mean(WETDAY),
            .groups = "drop") %>%
  left_join(aridity1, by = "site") %>%
  # now averaging across sites
  group_by(day, intensity, warm, SoilTreatment, depth_group) %>%
  # across() didn't work with dtplyr so explicitly defining each column
  summarize(TRANSP_mean = mean(TRANSP, na.rm = TRUE),
            WETDAY_mean = mean(WETDAY),
            TRANSP_lwr = q1(TRANSP),
            TRANSP_upr = q2(TRANSP),
            WETDAY_lwr = q1(WETDAY),
            WETDAY_upr = q2(WETDAY)) %>%
  as_tibble() %>%
  trmts2factors()


remove("dly_lyr_all1")
}


# save files --------------------------------------------------------------

saveRDS(dly_lyr_means_all,
        "data-processed/dly_lyr_means_all.RDS")
