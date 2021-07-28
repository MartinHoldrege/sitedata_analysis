# Martin Holdrege

# script started 2/23/21

# For compiling the site level climate info. Used in downstream scripts

# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/fig_params.R")
source("scripts/functions.R")
theme_set(theme_classic())

# load data ---------------------------------------------------------------

# means by site/intensity/soil of sw2_yearly table
sw2_yrly1 <- read_csv("data-processed/site_means/sw2_yr_means_v1.csv")

sw2_mo1 <- read_csv("data-processed/site_means/sw2_mo_means_v1.csv")

# process -----------------------------------------------------------------

cols <- str_subset(names(sw2_yrly1), "TEMP_(min|avg|max)|PRECIP_ppt|PET_pet_cm")

# * temp/ppt differences --------------------------------------------------

# means of ambient runs
amb_means <- sw2_yrly1 %>%
  filter(intensity == "ambient" & warm == "ambient") %>%
  group_by(site) %>%
  select(site, all_of(cols)) %>%
  summarise_all(.funs = mean)

groups_df <- sw2_yrly1[, c("site", "intensity", "warm", "SoilTreatment")]

# want to have just the ambient means, but with rows repeated for all
# treatments/soils so can take difference
amb_means2 <-  left_join(groups_df, amb_means, by = c("site"))

climate_diffs <- sw2_yrly1 %>%
  select( c("site", "intensity", "warm", "SoilTreatment"), all_of(cols))

# difference between a given soil/intensity and the mean value for ambient intensity
climate_diffs[, cols] <- sw2_yrly1[, cols] - amb_means2[, cols]

climate_diffs <- trmts2factors(climate_diffs)

# percent differences
climate_perc_diffs <- climate_diffs
climate_perc_diffs[, cols] <- (sw2_yrly1[, cols] - amb_means2[, cols])/amb_means2[, cols]*100

# mean percent increase in interannual SD of precip
sd_means <- climate_perc_diffs %>%
  group_by(intensity) %>%
  summarise(perc_sd_change = mean(PRECIP_ppt_SD)) %>%
  mutate(label = paste0("mean = ", round(perc_sd_change, 1), "%"))

sd_means

# intensity       PRECIP_ppt_SD
# <fct>                   <dbl>
#   1 ambient               -0.0407
# 2 1.25x intensity       18.1
# 3 1.5x intensity        33.6
# 4 2x intensity          60.8


# * seasonality -----------------------------------------------------------

seas <- sw2_mo1 %>%
  filter(SoilTreatment == "loam",
         intensity == "ambient",
         warm == "ambient") %>%
         # growing season only.
  group_by(site) %>%
  summarize( # growing season ppt
    # growing season threshold 4.4 C as described in Sims et al. 1978 Journal of
    # Ecology
    seas_ppt = sum(PRECIP_ppt_Mean[TEMP_avg_C_Mean >= 4.4]),
    # CV of monthly precipitation
    ppt_mo_CV = sd(PRECIP_ppt_Mean)/mean(PRECIP_ppt_Mean*100),
    .groups = "drop")

# * aridity index/climate -------------------------------------------------

aridity1 <- amb_means %>%
  mutate(aridity_index = PRECIP_ppt_Mean/PET_pet_cm_Mean) %>%
  left_join(seas, by = "site") %>%
  # proportion of ppt that falls during the growing season
  mutate(prop_seas_ppt = seas_ppt/PRECIP_ppt_Mean)

write_csv(aridity1, "data-processed/aridity_by_site.csv")

# figures -----------------------------------------------------------------

vline_df <- tibble(
  warm = factor(c("ambient", "3C warming", "5C warming")),
  temp_diff = c(0, 3.07, 5.4)
)
# * climate differences ---------------------------------------------------

pdf("figures/climate/climate_differences_by_trmt_v2.pdf")
g <- ggplot(climate_diffs,
            aes(y = site, color = intensity, group = SoilTreatment)) +
  theme(legend.position = "top") +
  facet_wrap(~warm) +
  scale_color_manual(values = cols_intensity) +
  labs(caption = paste("Difference shown between mean for each intensity/soil",
                      "and the mean for ambient intensity (across all soils).",
                      "\nWarming treatments shown in separate panels",
                      "\nIndividual points shown for each of 4 soils."))

g +
  geom_point(aes(PRECIP_ppt_Mean)) +
  labs(x = "MAP difference (cm)",
       subtitle = "Mean annual precipitation difference") +
  geom_vline(xintercept = 0, linetype = 2)

g2 <- g +
  geom_vline(data = vline_df, aes(xintercept = temp_diff), linetype = 2) +
  facet_wrap(~warm, scales = "free_x")

g2 +
  geom_point(aes(TEMP_max_C_Mean)) +
  labs(x = "Tmax difference (C)",
       subtitle = "Mean Tmax difference")

g2 +
  geom_point(aes(TEMP_min_C_Mean)) +
  labs(x = "Tmin difference (C)",
       subtitle = "Mean Tmin difference")

g +
  geom_point(aes(PRECIP_ppt_SD)) +
  labs(x = "difference in SD of annual ppt (cm)",
       subtitle = "Differences in the mean of standard deviation of annual precip")

climate_perc_diffs %>%
  filter(intensity != "ambient") %>%
ggplot(aes(x = PRECIP_ppt_SD)) +
  theme(legend.position = "top") +
  facet_wrap(~intensity, ncol = 1) +
  geom_histogram() +
  geom_label(data = filter(sd_means, intensity != "ambient"),
             aes(x = 75, y = 500, label = label )) +
  labs(x = "% Change in precip SD",
       subtitle = "% Change in SD of inter-annual precip, relative to ambient (simulated) intensity")

dev.off()


