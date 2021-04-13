# Martin Holdrege

# script started 2/23/21

# For compiling the site level climate info. Used in downstream scripts

# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/fig_params.R")
theme_set(theme_classic())

# load data ---------------------------------------------------------------

# means by site/intensity/soil of sw2_yearly table
sw2_yrly1 <- readr::read_csv("data-processed/site_means/sw2_yr_means_v1.csv")

# process -----------------------------------------------------------------

cols <- str_subset(names(sw2_yrly1), "TEMP_(min|avg|max)|PRECIP_ppt|PET_pet_cm")

# * temp/ppt differences --------------------------------------------------

# means of ambient runs
amb_means <- sw2_yrly1 %>%
  filter(intensity == "ambient") %>%
  group_by(site) %>%
  select(site, all_of(cols)) %>%
  summarise_all(.funs = mean)

groups_df <- sw2_yrly1[, c("site", "intensity", "SoilTreatment")]

# want to have just the ambient means, but with rows repeated for all
# treatments/soils so can take difference
amb_means2 <-  left_join(groups_df, amb_means, by = "site")

climate_diffs <- sw2_yrly1 %>%
  select( c("site", "intensity", "SoilTreatment"), all_of(cols))

# difference between a given soil/intensity and the mean value for ambient intensity
climate_diffs[, cols] <- sw2_yrly1[, cols] - amb_means2[, cols]

# * aridity index/climate -------------------------------------------------

aridity1 <- amb_means %>%
    mutate(aridity_index = PRECIP_ppt_Mean/PET_pet_cm_Mean)

write_csv(aridity1, "data-processed/aridity_by_site.csv")

# figures -----------------------------------------------------------------

# * climate differences ---------------------------------------------------

pdf("figures/climate/climate_differences_by_trmt_v1.pdf")
g <- ggplot(climate_diffs,
            aes(y = site, color = intensity, group = SoilTreatment)) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(legend.position = "top") +
  scale_color_manual(values = cols2) +
  labs(caption = paste("Difference shown between mean for each intensity/soil",
                      "\nand the mean for ambient intensity (across all soils).",
                      "\nIndividual points shown for each of 4 soils."))

g +
  geom_point(aes(PRECIP_ppt_Mean)) +
  labs(x = "MAP difference (cm)",
       subtitle = "Mean annual precipitation difference")

g +
  geom_point(aes(TEMP_max_C_Mean)) +
  labs(x = "Tmax difference (C)",
       subtitle = "Mean Tmax difference")

g +
  geom_point(aes(TEMP_min_C_Mean)) +
  labs(x = "Tmin difference (C)",
       subtitle = "Mean Tmin difference")

g +
  geom_point(aes(PRECIP_ppt_SD)) +
  labs(x = "difference in SD of annual ppt (cm)",
       subtitle = "Differences in the mean of standard deviation of annual precip")

dev.off()


