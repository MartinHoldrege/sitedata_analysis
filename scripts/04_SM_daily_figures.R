# Martin Holdrege

# script started May 24, 2021

# Purpose is to create figures of soil water availability from soilwat2 output
# Examines water use etc. across treatments (intensity and warming), depth,

# This is based on averages for each day of the year (i.e. the sw2_daily
# and sw2_daily_slyrs tables)

# I'm most interested in looking changing in transpiration, drainage, and
# n wet days

# notes--the run_dly_all/run_dly_lyr_all functionality can now be removed
# b/ the 03_SM_daily_summarize.R now uses dtplyr and doesn't crash.

# code params -------------------------------------------------------------

# these logicals affect which objects are created in
# 03_SM_daily_summarize.R and then which figures are created here
# lets this script work on a subset of objects as desired when memory
# limitations are a problem.

# create figures of daily values across depths and pfts
run_dly_all <- TRUE

# create figures of daily values by depth category, across pfts
run_dly_lyr_all <- TRUE

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
  "Lines show means across sites for each day of year"
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

line_plots <- function(g1, add_ribbon = TRUE, name_extension = "",
                       aet_plots = FALSE) {
  # outputs line plots vs doy by aridity
  # g1--ggplot object
  # add_ribbon --logical whether to add ribbon to plot
  # name_extension (e.g. _diff), to comlete name for line_rib function
  # aet

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

  # create figures of AET
  if (aet_plots) {
    o4 <- g1 +
      line_rib(paste0("AET", name_extension),
               add_ribbon = add_ribbon) +
      labs(y = aet_lab0,
           title = "AET")

    o5 <- g1 +
      line_rib(paste0("T_AET", name_extension),
               add_ribbon = add_ribbon) +
      labs(y = t_aet_lab0,
           title = "T/AET")
    out <- list(TRANSP = o1, EVAPTOT = o2, AET = o4, T_AET = o5, drain = o3)
  } else {
    out <- list(TRANSP = o1, EVAPTOT = o2, drain = o3)
  }
  out
}

depth_by_arid <- function(data) {
  ggplot(data = data, aes(x = day)) +
    lemon::facet_rep_grid(depth_group ~ aridity_group) +
    labs(caption = cap2) +
    doy_axis()
}

# sm across layers and PFTS -----------------------------------------------

# if true then the sourced file should have created the necessary
# objects

if (run_dly_all) {
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

plots <- line_plots(g1, aet_plots = TRUE)

map(plots, print)

# also want free axis for one of the plots
p <- plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")
print(p)
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


plots <- line_plots(g1, add_ribbon = FALSE, aet_plots = TRUE)
map(plots, print)

# also want free axis for one of the plots
p <- plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")
print(p)
# cumulative metrics
g1 <- g1 +
  labs(subtitle = "Year to date cumulative values. Only ambient warming data shown")

# didn't calculate cumulative values for AET (cumulative T/AET doesn't make
# sense)
plots <- line_plots(g1, add_ribbon = FALSE, name_extension = "_cum",
                    aet_plots = FALSE)
map(plots, print)

# also want free axis for one of the plots
p <- plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")
print(p)
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


plots <- line_plots(g1, add_ribbon = FALSE, aet_plots = TRUE)
map(plots, print)

# also want free axis for one of the plots
p <- plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")
print(p)
# cumulative metrics
g1 <- g1 +
  labs(subtitle = "Year to date cumulative values. Only ambient intensity data shown")

plots <- line_plots(g1, add_ribbon = FALSE, name_extension = "_cum",
                     aet_plots = FALSE)
map(plots, print)

# also want free axis for one of the plots
p <- plots$drain +
  lemon::facet_rep_wrap(~ aridity_group, ncol = 1, scales = "free_y")
print(p)
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
plots <- line_plots(g2, name_extension = "_diff", aet_plots = TRUE)
map(plots, print)
# plotting each plot with and without free y axis
map2(plots, c(transp_lab1, evap_lab1, aet_lab1, t_aet_lab1,
              drain_lab1), function(g, ylab) {
  p <- g +
    # replacing ylab
    labs(y = ylab)
  p2 <- p + trmt_free()
  out <- list(p, p2)
  walk(out, print)
})

dev.off()
}


# sm by layer across PFTs -------------------------------------------------


if (run_dly_lyr_all) {

pdf("figures/soil_moisture/SM_vs_doy_by_lyr_v1.pdf")

# * by intensity ----------------------------------------------------------

p <- list()
g1 <- dly_lyr_means %>%
  filter(warm == "ambient")


g1 <- dly_lyr_means %>%
  filter(warm == "ambient") %>%
  depth_by_arid() +
  scale_color_manual(values = cols_intensity) +
  labs(subtitle = "Only ambient warming shown")

# transp
p[[1]] <- g1  +
  geom_line(aes(y = TRANSP, color = intensity)) +
  labs(y = transp_lab0,
       title = "Transpiration by depth"
  )

# VWC
p[[2]] <- g1  +
  geom_line(aes(y = VWCMATRIC, color = intensity)) +
  labs(y = vwc_lab0,
       title = "VWC by depth")

p[[3]] <- g1  +
  geom_line(aes(y = WETDAY, color = intensity)) +
  labs(y = wetday_prop_lab0,
       title = "Proportion wet days by depth")


# * by warming ----------------------------------------------------------

g1 <- dly_lyr_means %>%
  filter(intensity == "ambient") %>%
  depth_by_arid() +
  scale_color_manual(values = cols_warm) +
  labs(subtitle = "Only ambient intensity shown")

# transp
p[[4]] <- g1  +
  geom_line(aes(y = TRANSP, color = warm)) +
  labs(y = transp_lab0,
       title = "Transpiration by depth"
  )

# VWC
p[[5]] <- g1  +
  geom_line(aes(y = VWCMATRIC, color = warm)) +
  labs(y = vwc_lab0,
       title = "VWC by depth")

# wet days
p[[6]] <- g1  +
  geom_line(aes(y = WETDAY, color = warm)) +
  labs(y = wetday_prop_lab0,
       title = "Proportion wet days by depth")

# have to explicity print inside if statement
map(p, print)

dev.off()
}
