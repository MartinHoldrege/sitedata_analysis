# Martin Holdrege

# Script Started July 7, 2021

# Purpose--to make publication quality figures of soil moisture data

# Next: consider adding tags (a), (b)... etc. to all multipanel figures


# dependencies ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gridExtra)
source("scripts/functions.R")
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

lyr_pft_diff_0l <- lyr_pft_diff1 %>%
  filter(warm == "ambient", SoilTreatment == "loam") %>%
  mutate(PFT = str_replace(PFT, "forbs", "forb"),
         PFT = factor(PFT, levels = c("total", "shrub", "grass", "forb")))


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


# * only loam -------------------------------------------------------------

jpeg("figures/soil_moisture/pub_qual/TBOX_transp_v_depth.jpg",
     res = 600,
     height = 5,
     width = 2.5,
     units = 'in')


g <- lyr_all_diff_0l %>%
  mutate(intensity_lab = add_letters(x = intensity)) %>%
  ggplot(aes(x = -depth, y = TRANSP_diff)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_boxplot(aes(group = -depth, color = intensity),
               outlier.size = 0.4, size = 0.8) +
  stat_summary(fun = mean, geom = "line", color = "black", alpha = 0.7,
               size = 0.8) +
  stat_summary(fun = mean, geom = "point", color = "black", alpha = 0.7,
               size = 0.8) +
  coord_flip() +
  lemon::facet_rep_wrap(~intensity_lab, ncol = 1) +
  scale_x_continuous(breaks = breaks, labels = break_labels) +
  labs(x = depth_lab,
       y = "Transpiration change (cm)") +
  scale_color_manual(values = cols_intensity) +
  theme(legend.position = "none",
        # allows text to render as markdown
        strip.text = ggtext::element_markdown(hjust = 0))


g
dev.off()

jpeg("figures/soil_moisture/pub_qual/TBOX_transp_v_depth_wide.jpg",
     res = 600,
     height = 2,
     width = 5,
     units = 'in')

g +lemon::facet_rep_wrap(~intensity, nrow = 1)
dev.off()


# * by soil texture -------------------------------------------------------

jpeg("figures/soil_moisture/pub_qual/TBOXSOIL_transp_v_depth.jpg",
     res = 600,
     height = 5,
     width = 6,
     units = 'in')

lyr_all_diff1 %>%
  filter(warm == "ambient") %>%
  ggplot(aes(x = -depth, y = TRANSP_diff)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_boxplot(aes(group = -depth, color = intensity),
               outlier.size = 0.4, size = 0.8) +
  stat_summary(fun = mean, geom = "line", color = "black", alpha = 0.7,
               size = 0.8) +
  stat_summary(fun = mean, geom = "point", color = "black", alpha = 0.7,
               size = 0.8) +
  coord_flip() +
  lemon::facet_rep_grid(intensity~SoilTreatment) +
  scale_x_continuous(breaks = breaks, labels = break_labels) +
  labs(x = depth_lab,
       y = "Transpiration change (cm)") +
  scale_color_manual(values = cols_intensity) +
  theme(legend.position = "none")

dev.off()


# * by PFT ----------------------------------------------------------------

jpeg("figures/soil_moisture/pub_qual/TBOX_transp_v_depth_pft.jpg",
     res = 600,
     height = 4.5,
     width = 5.5,
     units = 'in')

tag_df <- expand_grid(intensity = unique(lyr_pft_diff_0l$intensity),
                      PFT = unique(lyr_pft_diff_0l$PFT)) %>%
  arrange(PFT, intensity)
# tags for corner of plots
tag_df$tag <- paste("(", letters[1:nrow(tag_df)], ")", sep = "")

g <- lyr_pft_diff_0l %>%
  left_join(tag_df, by = c("intensity", "PFT")) %>%
  group_by(PFT) %>%
  # culculating min for placement of facets
  mutate(diff_min = min(TRANSP_diff),
         range = max(TRANSP_diff) - diff_min) %>%
  ungroup() %>%
  # adjusting so total/shrub and grass/forb panels have same scales
  mutate(diff_min = ifelse(PFT == "shrub",
                           min(diff_min[PFT == "total"]),
                           ifelse(PFT == "forb",
                                  min(diff_min[PFT == "grass"]),
                                  diff_min)),
         range =ifelse(PFT == "shrub",
                       range[PFT == "total"],
                       ifelse(PFT == "forb",
                              range[PFT == "grass"],
                              range))) %>%
  ggplot(aes(x = -depth, y = TRANSP_diff)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_boxplot(aes(group = -depth, color = intensity),
               outlier.size = 0.4, size = 0.8) +
  stat_summary(fun = mean, geom = "line", color = "black", alpha = 0.7,
               size = 0.8) +
  stat_summary(fun = mean, geom = "point", color = "black", alpha = 0.7,
               size = 0.8) +
  coord_flip() +
  lemon::facet_rep_grid(intensity~PFT,
                        scales = "free_x") +
  scale_x_continuous(breaks = breaks, labels = break_labels) +
  labs(x = depth_lab,
       y = "Transpiration change (cm)") +
  scale_color_manual(values = cols_intensity) +
  theme(legend.position = "none") +
  geom_text(aes(x = -15, y = diff_min + 0.1*range,
                               label = tag)) +
  # setting limits on horizontal axis
  geom_point(aes(y = diff_min, x= 0), color = "white") +
  geom_point(aes(y = diff_min + range, x= 0), color = "white")

g
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



# all intensity levels
p_all <-  dly_lyr_means_all %>%
  mutate(lab1 = add_letters(depth_group, letters = letters),
         lab2 = add_letters(depth_group, letters = letters[4:6])) %>%
  ggplot(aes(x = day, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  scale_x_continuous(breaks = break_doys,
                     labels = break_labels) +
  labs(y = transp_lab0,
       x = "Day of year")

# * DOY vs transp ---------------------------------------------------------
# mean trasnpiration by treatment vs day of year

g_line <- p_all +
  labs(y = transp_lab0) +
  theme(legend.position = "none",
        strip.text = ggtext::element_markdown(hjust = 0)) +
  geom_line(aes(y = TRANSP_mean)) +
  lemon::facet_rep_wrap(~lab1, ncol = 1)


g_line


# * DOY vs WETDAY ---------------------------------------------------------

w_line <- p_all +
  labs(y = wetday_prop_lab0) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text = ggtext::element_markdown(hjust = 0)) +
  geom_line(aes(y = WETDAY_mean)) +
  lemon::facet_rep_wrap(~lab2, ncol = 1)

w_line


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


# aridity vs 3x SM ------------------------------------------------------
#  3 panels, transpiration, evaporation, deep drainage, all vs aridity

range <- with(tot_transp_diff_0l,
              range(c(TRANSP_diff, EVAPTOT_diff, drain_diff)))

psize <- 0.5 # point size

g <-  ggplot(tot_transp_diff_0l,
             aes(x = aridity_index, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  labs(x = NULL) +
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
  theme(legend.position = "top") +
  guides(color = guide_legend(ncol = 1))

legend_intensity <- ggpubr::get_legend(g_transp0)
legend_intensity_wide <- ggpubr::get_legend(
  g_transp0 +
    guides(color = guide_legend(ncol = 3)))

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
       tag = "(c)",
       x = aridity_lab)

g_drain

# combine figures


jpeg("figures/soil_moisture/pub_qual/ETDRAIN_E-T-and-drain_vs_arid.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
grid.arrange(legend_intensity,
             g_transp,
             g_evap,
             g_drain,
             layout_matrix = matrix(c(1, 2, 3, 4), ncol = 1),
             heights = c(4, 10, 10, 10))
dev.off()

jpeg("figures/soil_moisture/pub_qual/ETDRAIN_E-T-and-drain_vs_arid_wide.jpeg", res = 600,
     height = 3, width = 6.5, units = 'in')

lay_wide <- rbind(c(1, 1, 1),
                         c(2, 3, 4))
grid.arrange(legend_intensity_wide,
             g_transp + labs(x = aridity_lab),
             g_evap + labs(x = aridity_lab),
             g_drain,
             layout_matrix = lay_wide,
             heights = c(3, 10))
dev.off()

# aridity vs PFT transp -------------------------------------------------

# 3 panels, transpiration for shrubs, grasses and forbs
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
g <- tot_transp_pft_diff %>%
  filter(SoilTreatment == "loam",
         warm == "ambient",
         PFT != "total") %>%
  mutate(PFT = factor(PFT, levels = c("shrub", "grass", "forbs"),
                      labels = c("shrub", "grass", "forb")),
         PFT_lab = add_letters(PFT)) %>%
  ggplot(aes(x = aridity_index, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  labs(x = aridity_lab,
       y = "Transpiration change (cm)") +
  geom_hline(yintercept = 0, linetype = 2,
             alpha = 0.7) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        # allows text to render as markdown
        strip.text = ggtext::element_markdown(hjust = 0)) +
  lemon::facet_rep_wrap(~PFT_lab, scales = "free_y", ncol = 1) +
  geom_point(aes(y = TRANSP_diff), size = 0.5) +
  geom_smooth(aes(y = TRANSP_diff), se = FALSE) +
  guides(color = guide_legend(ncol = 1))
g
dev.off()

# wide format--for powerpoint
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid_wide.jpeg", res = 600,
     height = 3, width = 6.5, units = 'in')
g +
  lemon::facet_rep_wrap(~PFT, scales = "free_y", nrow = 1)


dev.off()


# drain vs evap -----------------------------------------------------------

jpeg("figures/soil_moisture/pub_qual/DRAIN-EVAP.jpeg", res = 600,
     height = 5, width = 5, units = 'in')
tot_transp_diff_0l %>%
  mutate(direction = ifelse(TRANSP_diff > 0,
                            "Transpiration increased",
                            "Transpiration decreased")) %>%
  ggplot(aes(drain_diff, EVAPTOT_diff, shape = direction, color = direction)) +
  geom_point() +
  geom_abline(slope = -1, intercept = 0) +
  lemon::facet_rep_wrap(~intensity, nrow = 2) +
  labs(x = "Change in drainage (cm)",
       y = "Change in evaporation (cm)") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c("Transpiration increased" = "dark blue",
                                "Transpiration decreased" = "dark red"))

dev.off()
# tot_transp_diff_0l %>%
#   ungroup() %>%
#   mutate(loss = drain_diff + EVAPTOT_diff,
#          diff = TRANSP_diff - loss) %>%
#   pull(diff) %>%
#   hist()
# ggplot(tot_transp_diff_0l,
#        aes(TRANSP_diff, EVAPTOT_diff + drain_diff)) +
#   geom_point() +
#   geom_abline(slope = -1, intercept = 0)
#
# ggplot(tot_transp_diff_0l,
#        aes(PRECIP_ppt_Mean, TRANSP + EVAPTOT + drain)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0)
