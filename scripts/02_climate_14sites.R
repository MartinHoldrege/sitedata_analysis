# Martin Holdrege

# script started 2/23/21

# For compiling the site level climate info. Used in downstream scripts

# dependencies ------------------------------------------------------------

library(dplyr)

# load data ---------------------------------------------------------------

# sw2_yearly table annual means
sw2_yrly1 <- readr::read_csv("data-processed/14sites/sw2_yr_means_14sites.csv")

# process -----------------------------------------------------------------

# * aridity index/climate -------------------------------------------------

climate1 <- sw2_yrly1 %>%
  # these values should be the same for both trmts
  # however, worth confirming this at some point
  filter(intensity == "ambient", SoilTreatment == "soils_fixed1") %>%
  select(site, matches("TEMP_(min|avg|max)|PRECIP_ppt|PET_pet_cm")) %>%
  mutate(aridity_index = PRECIP_ppt_Mean/PET_pet_cm_Mean)


