# Martin Holdrege

# script started May 24, 2021

# Purpose is to create figures of soil water availability from soilwat2 output
# Examines water use etc. across treatments (intensity and warming), depth,

# This is based on averages for each day of the year (i.e. the sw2_daily
# and sw2_daily_slyrs tables)

# I'm most interested in looking changing in transpiration, drainage, and

# dependencies ------------------------------------------------------------

source("scripts/03_SM_daily_summarize.R")
source("scripts/fig_params.R")

# fig themes -------------------------------------------------------------


# fig params --------------------------------------------------------------


# sm across layers and PFTS -----------------------------------------------

# * ambient levels -------------------------------------------------------

g1 <- dly_tot_transp %>%
  filter(warm == "ambient" & intensity == "ambient") %>%
  ggplot(aes(g = as.factor(day))) +
  lemon::facet_rep_wrap(~ aridity_group)

g1 +
  geom_histogram(aes(x = TRANSP)) +
  coord_flip()

#  * diffs -------------------------------------------------------------------

# ** transpiration -------------------------------------------------------
