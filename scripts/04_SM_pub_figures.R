# Martin Holdrege

# Script Started July 7, 2021

# Purpose--to make publication quality figures of soil moisture data

# Next: consider adding tags (a), (b)... etc. to all multipanel figures


# dependencies ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gridExtra)
source("scripts/fig_params.R")
source("scripts/03_SM_summarize.R")

# read in files -----------------------------------------------------------

# mean transpiration and wet days for each day of year across sites
# also 5th and 95th percentiles
dly_lyr_means_all <- readRDS("data-processed/dly_lyr_means_all.RDS")


# summary df's ------------------------------------------------------------

dly_lyr_means_2x <- dly_lyr_means_all %>%
  filter(intensity %in% c("2x intensity", "ambient"))

# 0 warming, loam soil
lyr_all_diff_0l <- lyr_all_diff1 %>%
  filter(SoilTreatment == "loam",
         warm == "ambient")

tot_transp_diff_0l <- tot_transp_diff %>%
  filter(SoilTreatment == "loam",
         warm == "ambient")



# fig themes -------------------------------------------------------------

theme_set(theme_classic())
theme_update(strip.background = element_blank(),
             #strip.text = element_text(size = 12),
             #axis.title = element_text(size = 13),
             # increasing right margin so numbers not cutoff
             plot.margin = unit(c(5.5, 10, 5.5, 5.5), "points"))



# depth boxplot ---------------------------------------------------------
# transpiration vs depth by intensity treatment

breaks <- c(0, -50, -100)
break_labels <- as.character(-breaks)

jpeg("figures/soil_moisture/pub_qual/TBOX_transp_v_depth.jpg",
     res = 600,
     height = 5,
     width = 2.5,
     units = 'in')
g <- ggplot(lyr_all_diff_0l,
       aes(x = -depth, y = TRANSP_diff)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_boxplot(aes(group = -depth, color = intensity),
               outlier.size = 0.4, size = 0.8) +
  stat_summary(fun = mean, geom = "line", color = "black", alpha = 0.7,
               size = 0.8) +
  stat_summary(fun = mean, geom = "point", color = "black", alpha = 0.7,
               size = 0.8) +
  coord_flip() +
  lemon::facet_rep_wrap(~intensity, ncol = 1) +
  scale_x_continuous(breaks = breaks, labels = break_labels) +
  labs(x = depth_lab,
       y = "Transpiration change (cm)") +
  scale_color_manual(values = cols_intensity) +
  theme(legend.position = "none")
g
dev.off()

jpeg("figures/soil_moisture/pub_qual/TBOX_transp_v_depth_wide.jpg",
     res = 600,
     height = 2,
     width = 5,
     units = 'in')

g +lemon::facet_rep_wrap(~intensity, nrow = 1)
dev.off()

# DOY vs transp and wetday ----------------------------------------------

# combined figure of both transpiration and proportion wet days by depth
# catagory. Two versions of figure made with and without 5th to 95th percentile
# ribbon

break_doys <- paste0(1:12, "-01-2020") %>%
  mdy() %>%
  yday()
break_labels <- rep("", 12)
# only add labels to some months
break_labels[c(1, 5, 9)] <-  c("Jan", "May", "Sept")

# just ambient and 2x intensity
p <- ggplot(data = dly_lyr_means_2x,
            aes(x = day, color = intensity)) +
  lemon::facet_rep_wrap(~ depth_group, ncol = 1) +
  scale_color_manual(values = cols_intensity) +
  scale_x_continuous(breaks = break_doys,
                     labels = break_labels) +
  labs(y = transp_lab0,
       x = "Day of year")

# all intensity levels
p_all <- ggplot(data = dly_lyr_means_all,
                aes(x = day, color = intensity)) +
  lemon::facet_rep_wrap(~ depth_group, ncol = 1) +
  scale_color_manual(values = cols_intensity) +
  scale_x_continuous(breaks = break_doys,
                     labels = break_labels) +
  labs(y = transp_lab0,
       x = "Day of year")

# * DOY vs transp ---------------------------------------------------------
# mean trasnpiration by treatment vs day of year

g_line <- p_all +
  labs(y = transp_lab0) +
  theme(legend.position = "none") +
  geom_line(aes(y = TRANSP_mean))

g_line

g_rib <- p +
  labs(y = transp_lab0) +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin = TRANSP_lwr, ymax = TRANSP_upr,
                         fill = intensity), alpha = 0.2,
              color = NA) +
  geom_line(aes(y = TRANSP_mean)) +
 scale_fill_manual(values = cols_intensity)

g_rib


# * DOY vs WETDAY ---------------------------------------------------------

w_line <- p_all +
  labs(y = wetday_prop_lab0) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  geom_line(aes(y = WETDAY_mean))

w_line

w_rib <- p +
  labs(y =wetday_prop_lab0) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  geom_ribbon(aes(ymin = WETDAY_lwr, ymax = WETDAY_upr,
                  fill = intensity), alpha = 0.2,
              color = NA) +
  geom_line(aes(y = WETDAY_mean)) +
  scale_fill_manual(values = cols_intensity)

w_rib


# * combine figures -------------------------------------------------------

lay = rbind(c(1, 1),
            c(2, 3))
# line only
jpeg("figures/soil_moisture/pub_qual/TDOY_line.jpeg", res = 600,
     height = 5, width = 4, units = 'in')
grid.arrange(
  ggpubr::get_legend(w_line + guides(color = guide_legend(ncol = 2))),
  g_line, w_line + theme(legend.position = "none"),
             layout_matrix = lay,
  heights = c(1, 10))
dev.off()

# lines and ribbons
jpeg("figures/soil_moisture/pub_qual/TDOY_ribbon.jpeg", res = 600,
     height = 5, width = 4, units = 'in')
grid.arrange(
  ggpubr::get_legend(w_rib),
  g_rib, w_rib + theme(legend.position = "none"),
  layout_matrix = lay,
  heights = c(1, 10))
dev.off()

# aridity vs 3x SM ------------------------------------------------------
#  3 panels, transpiration, evaporation, deep drainage, all vs aridity

range <- with(tot_transp_diff_0l,
              range(c(TRANSP_diff, EVAPTOT_diff, drain_diff)))

psize <- 0.5 # point size

g <-  ggplot(tot_transp_diff_0l,
             aes(x = aridity_index, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  labs(x = aridity_lab) +
  geom_hline(yintercept = 0, linetype = 2,
             alpha = 0.7) +
  theme(legend.title = element_blank()) +
  coord_cartesian(ylim = range) +
  scale_y_continuous(breaks = c(-3, 0, 3))

g2 <- g +
  theme(legend.position = "none")

# transpiration
g_transp0 <- g +
  geom_point(aes(y = TRANSP_diff), size = psize) +
  geom_smooth(aes(y = TRANSP_diff), se = FALSE) +
  labs(y = "Transpiration change (cm)",
       tag = "(a)") +
  theme(legend.position = "top")

legend_intensity <- ggpubr::get_legend(g_transp0)
g_transp <- g_transp0 +
  theme(legend.position = "none")
g_transp

# evaporation
g_evap <- g2 +
  geom_point(aes(y = EVAPTOT_diff), size = psize) +
  geom_smooth(aes(y = EVAPTOT_diff), se = FALSE) +
  labs(y = "Evaporation change (cm)",
       tag = "(b)")
g_evap

# drainage
g_drain <- g2 +
  geom_point(aes(y = drain_diff), size = psize) +
  geom_smooth(aes(y = drain_diff), se = FALSE) +
  labs(y = "Drainage change (cm)",
       tag = "(c)")

g_drain

# combine figures

lay <- rbind(c(1, 1),
             c(2, 3),
             c(4, NA))

jpeg("figures/soil_moisture/pub_qual/ETDRAIN_E-T-and-drain_vs_arid.jpeg", res = 600,
     height = 5, width = 5, units = 'in')
grid.arrange(legend_intensity,
             g_transp,
             g_evap,
             g_drain,
             layout_matrix = lay,
             heights = c(2, 10, 10))
dev.off()

jpeg("figures/soil_moisture/pub_qual/ETDRAIN_E-T-and-drain_vs_arid_wide.jpeg", res = 600,
     height = 3, width = 6.5, units = 'in')

lay_wide <- lay <- rbind(c(1, 1, 1),
                         c(2, 3, 4))
grid.arrange(legend_intensity,
             g_transp,
             g_evap,
             g_drain,
             layout_matrix = lay_wide,
             heights = c(3, 10))
dev.off()

# aridity vs PFT transp -------------------------------------------------

# 3 panels, transpiration for shrubs, grasses and forbs
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid.jpeg", res = 600,
     height = 4, width = 4, units = 'in')
g <- tot_transp_pft_diff %>%
  filter(SoilTreatment == "loam",
         warm == "ambient",
         PFT != "total") %>%
  mutate(PFT = factor(PFT, levels = c("shrub", "grass", "forbs"),
                      labels = c("shrub", "grass", "forb"))) %>%
  ggplot(aes(x = aridity_index, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  labs(x = aridity_lab,
       y = "Transpiration change (cm)") +
  geom_hline(yintercept = 0, linetype = 2,
             alpha = 0.7) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  lemon::facet_rep_wrap(~PFT, scales = "free_y", ncol = 2) +
  geom_point(aes(y = TRANSP_diff), size = 0.5) +
  geom_smooth(aes(y = TRANSP_diff), se = FALSE)
g
dev.off()

# wide format--for powerpoint
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid_wide.jpeg", res = 600,
     height = 3, width = 6.5, units = 'in')
g +
  lemon::facet_rep_wrap(~PFT, scales = "free_y", nrow = 1)


dev.off()


# drain vs evap -----------------------------------------------------------

ggplot(tot_transp_diff_0l, aes(drain_diff, EVAPTOT_diff, color = TRANSP_diff > 0)) +
  geom_point() +
  geom_abline(slope = -1, intercept = 0) +
  facet_wrap(~intensity, nrow = 2)

ggplot(tot_transp_diff_0l, aes(aridity_index, drain_diff  + EVAPTOT_diff)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  #geom_abline(slope = -1, intercept = 0) +
  facet_wrap(~intensity, nrow = 2)

ggplot(tot_transp_diff_0l, aes(TRANSP_diff, drain_diff  + EVAPTOT_diff)) +
  geom_point() +
  geom_abline(slope = -1, intercept = 0) +
  facet_wrap(~intensity, nrow = 2)
