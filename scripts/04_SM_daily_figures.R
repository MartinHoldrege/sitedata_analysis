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

# line and ribbon plot
line_rib <- function(y_string) {
  y_mean = paste0(y_string, "_mean")
  y_upr = paste0(y_string, "_upr")
  y_lwr = paste0(y_string, "_lwr")

  list(
    geom_line(aes_string(y = y_mean)),
    geom_ribbon(aes_string(ymin = y_lwr, ymax = y_upr), alpha = 0.5,
                color = NA)
  )
}

# sm across layers and PFTS -----------------------------------------------

# CONTINUE HERE
# * ambient levels -------------------------------------------------------

g1 <- dly_tot_mean %>%
  # just controls
  filter(warm == "ambient" & intensity == "ambient") %>%
  ggplot(aes(x = day)) +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1)

g1 +
  geom_line(aes(y = TRANSP_mean)) +
  geom_ribbon(aes(ymin = TRANSP_lwr, ymax = TRANSP_upr), alpha = 0.5)

g1 +
  geom_line(aes(y = EVAPTOT_mean)) +
  geom_ribbon(aes(ymin = EVAPTOT_lwr, ymax = EVAPTOT_upr), alpha = 0.5)


#  * diffs -------------------------------------------------------------------

# ** transpiration -------------------------------------------------------

g2 <- dly_tot_diff_means %>%
  ggplot(aes(x = day, color = aridity_group,
             fill = aridity_group)) +
  lemon::facet_rep_grid(intensity~warm)

g2 +
  line_rib("TRANSP_diff")

g2 +
  line_rib("TRANSP_perc_diff")

# NOTE: Check why evaptot has missing values!
g2 +
  line_rib("EVAPTOT_diff")
