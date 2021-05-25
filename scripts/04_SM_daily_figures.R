# Martin Holdrege

# script started May 24, 2021

# Purpose is to create figures of soil water availability from soilwat2 output
# Examines water use etc. across treatments (intensity and warming), depth,

# This is based on averages for each day of the year (i.e. the sw2_daily
# and sw2_daily_slyrs tables)

# I'm most interested in looking changing in transpiration, drainage, and

# dependencies ------------------------------------------------------------

library(lubridate)
source("scripts/03_SM_daily_summarize.R")
source("scripts/fig_params.R")

# fig themes -------------------------------------------------------------

theme_set(theme_classic())
theme_update(strip.background = element_blank(),
             legend.title = element_blank(),
             legend.position = "top")

# fig params --------------------------------------------------------------

cap1 <- paste(
  "STEPWAT2 run for 200 sites. Only loam soils shown.\n",
  "Lines are means across sites, shaded regions are 5th and 95th percentiles.\n",
  "Control is ambient intensity and ambient warming"
  )
cap2 <- paste(
  "STEPWAT2 run for 200 sites. Only loam soils shown.\n",
  "Lines are means across sites"
)



# line and ribbon plot
line_rib <- function(y_string, add_ribbon = TRUE) {
  y_mean = paste0(y_string, "_mean")
  y_upr = paste0(y_string, "_upr")
  y_lwr = paste0(y_string, "_lwr")

  out <- list(
    geom_line(aes_string(y = y_mean)),
    geom_ribbon(aes_string(ymin = y_lwr, ymax = y_upr), alpha = 0.4,
                color = NA)
  )
  if(!add_ribbon) {
    out[[2]] <- NULL
  }
  out
}

aridity_legend <- function() {
  list(scale_color_manual(values = cols_aridity),
       scale_fill_manual(values = cols_aridity)
  )
}

trmt_free <- function() {
  lemon::facet_rep_grid(warm ~ intensity, scales = "free_y")
}

# want to add month labels to day of year
break_doys <- paste0(1:12, "-01-2020") %>%
  mdy() %>%
  yday()
break_labels <- rep("", 12)
# only add labels to some months
break_labels[c(1, 5, 9)] <-  c("Jan", "May", "Sept")

doy_axis <- function(){
  list(scale_x_continuous(breaks = break_doys,
                          labels = break_labels),
       labs(x = "Day")
  )
}

line_plots <- function(g1, add_ribbon = TRUE, name_extension = "") {
  # outputs line plots vs doy by aridity
  # g1--ggplot object
  # add_ribbon --logical whether to add ribbon to plot
  # name_extension (e.g. _diff), to comlete name for line_rib function

  o1 <- g1 +
    line_rib(paste0("TRANSP", name_extension),
             add_ribbon = add_ribbon) +
    labs(y = transp_lab0,
         title = "Transpiration")

  o2 <- g1 +
    line_rib(paste0("EVAPTOT", name_extension),
             add_ribbon = add_ribbon) +
    labs(y = evap_lab0,
         title = "Evaporation")

  o3 <- g1 +
    line_rib(paste0("drain", name_extension),
             add_ribbon = add_ribbon) +
    labs(y = drain_lab0,
         title = "Drainage")

  list(TRANSP = o1, EVAPTOT = o2, drain = o3)
}

# sm across layers and PFTS -----------------------------------------------

pdf("figures/soil_moisture/SM_vs_doy_v1.pdf")
# * absolute values-------------------------------------------------------

# actual transpiration, evaporation etc (not differences from control)

# ** ambient only-------------------------------------------------------

g1 <- dly_tot_mean %>%
  # just controls
  filter(warm == "ambient" & intensity == "ambient") %>%
  ggplot(aes(x = day)) +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1) +
  labs(caption = cap1,
       subtitle = "Only data from control treatments shown") +
  doy_axis()

plots <- line_plots(g1)

plots

# also want free axis for one of the plots
plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")

# ** by intensity ------------------------------------------------------
g1 <- dly_tot_mean %>%
  # just ambient temp
  filter(warm == "ambient") %>%
  ggplot(aes(x = day, color = intensity)) +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1) +
  labs(caption = cap2,
       subtitle = "Only ambient warming data shown") +
  doy_axis() +
  scale_color_manual(values = cols_intensity)


(plots <- line_plots(g1, add_ribbon = FALSE))

# also want free axis for one of the plots
plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")

# cumulative metrics
g1 <- g1 +
  labs(subtitle = "Year to date cumulative values. Only ambient warming data shown")

(plots <- line_plots(g1, add_ribbon = FALSE, name_extension = "_cum"))

# also want free axis for one of the plots
plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")

 # ** by warming ------------------------------------------------------

g1 <- dly_tot_mean %>%
  # just ambient temp
  filter(intensity == "ambient") %>%
  ggplot(aes(x = day, color = warm)) +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1) +
  labs(caption = cap2,
       subtitle = "Only ambient intensity data shown") +
  doy_axis() +
  scale_color_manual(values = cols_warm)


(plots <- line_plots(g1, add_ribbon = FALSE))

# also want free axis for one of the plots
plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")

# cumulative metrics
g1 <- g1 +
  labs(subtitle = "Year to date cumulative values. Only ambient intensity data shown")

(plots <- line_plots(g1, add_ribbon = FALSE, name_extension = "_cum"))

# also want free axis for one of the plots
plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")

#  * diffs -------------------------------------------------------------------

# trmt - control differences

g2 <- dly_tot_diff_means %>%
  # reversing for better over plotting
  mutate(aridity_group = fct_rev(aridity_group)) %>%
  ggplot(aes(x = day, color = aridity_group,
             fill = aridity_group)) +
  lemon::facet_rep_grid(warm ~ intensity) +
  labs(caption = cap1,
       subtitle = "Change relative to control") +
  aridity_legend() +
  doy_axis()

# transp/evap/drain
plots <- line_plots(g2, name_extension = "_diff")

# plotting each plot with and without free y axis
map2(plots, c(transp_lab1, evap_lab1, drain_lab1), function(g, ylab) {
  p <- g +
    # replacing ylab
    labs(y = ylab)
  p2 <- p + trmt_free()
  list(p, p2)
})

dev.off()
