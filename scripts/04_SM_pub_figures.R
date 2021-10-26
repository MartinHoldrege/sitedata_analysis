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


# depth dotplot -----------------------------------------------------------
# simplified presentation of transpiration with depth response.

# depth vs transpiration for grasses and shrubs, 2x intensity, loam soil

jpeg("figures/soil_moisture/pub_qual/TDOT_transp_v_depth.jpg",
     res = 800,
     height = 3,
     width = 2.2,
     units = 'in')


lyr_pft_diff_0l %>%
  filter(intensity == "2x intensity",
         PFT %in% c("grass", "shrub")) %>%
  group_by(depth, PFT) %>%
  summarize(TRANSP_diff_se = plotrix::std.error(TRANSP_diff),
            TRANSP_diff = mean(TRANSP_diff),
            .groups = "drop") %>%
  mutate(PFT_lab = add_letters(factor(PFT, levels = c("shrub", "grass")))) %>%
  ggplot(aes(x = -depth, y = TRANSP_diff)) +
  geom_line(alpha = 0.8) +
  geom_point(color = cols_intensity["2x intensity"],
             size = 1) +
  geom_errorbar(aes(ymin = TRANSP_diff - TRANSP_diff_se,
                    ymax = TRANSP_diff + TRANSP_diff_se),
                color = cols_intensity["2x intensity"]) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  coord_flip() +
  lemon::facet_rep_wrap(~PFT_lab, ncol = 1) +
  scale_x_continuous(breaks = breaks, labels = break_labels) +
  labs(x = depth_lab,
       y = "Transpiration change (cm)") +
  theme(strip.text = ggtext::element_markdown(hjust = 0))

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
  mutate(lab1 = add_letters(depth_group, letters = c("a", "c")),
         lab2 = add_letters(depth_group, letters = c("b", "d"))) %>%
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
     height = 4, width = 4, units = 'in')
grid.arrange(
  ggpubr::get_legend(w_line + guides(color = guide_legend(ncol = 2))),
  g_line, w_line + theme(legend.position = "none"),
             layout_matrix = lay,
  heights = c(1, 7))
dev.off()


# aridity vs 3x SM ------------------------------------------------------
#  3 panels, transpiration, evaporation, deep drainage, all vs aridity

range <- with(tot_transp_diff_0l,
              range(c(TRANSP_diff, EVAPTOT_diff, drain_diff)))

psize <- 0.5 # point size

# not putting x variable here so figure base can be re-used for MAP
g <-  ggplot(tot_transp_diff_0l,
             aes(color = intensity)) +
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
  labs(y = "Transpiration change (cm)",
       tag = "(a)") +
  theme(legend.position = "top") +
  guides(color = guide_legend(ncol = 1))

g_transp1 <- g_transp0 +
  geom_point(aes(x = aridity_index, y = TRANSP_diff), size = psize) +
  geom_smooth(aes(x = aridity_index, y = TRANSP_diff), se = FALSE)

# extract legetnds
legend_intensity <- ggpubr::get_legend(g_transp1)
legend_intensity_wide <- ggpubr::get_legend(
  g_transp1 +
    guides(color = guide_legend(ncol = 3)))

g_transp <- g_transp1 + theme(legend.position = "none")

g_transp

# evaporation
g_evap0 <- g2 +
  labs(y = "Evaporation change (cm)",
       tag = "(b)")

g_evap <- g_evap0 +
  geom_point(aes(x = aridity_index, y = EVAPTOT_diff), size = psize) +
  geom_smooth(aes(x = aridity_index, y = EVAPTOT_diff), se = FALSE)
g_evap

# drainage
g_drain0 <- g2 +
  labs(y = "Drainage change (cm)",
       tag = "(c)")

g_drain <- g_drain0 +
  geom_point(aes(x = aridity_index, y = drain_diff), size = psize) +
  geom_smooth(aes(x = aridity_index, y = drain_diff), se = FALSE) +
  labs(x = aridity_lab)

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


# * MAP -------------------------------------------------------------------
# MAP instead of aridity

# transpiration
g_transp2 <- g_transp0 +
  theme(legend.position = "none") +
  geom_point(aes(x = PRECIP_ppt_Mean, y = TRANSP_diff), size = psize) +
  geom_smooth(aes(x = PRECIP_ppt_Mean, y = TRANSP_diff), se = FALSE)

g_transp2

# evaporation
g_evap2 <- g_evap0 +
  geom_point(aes(x = PRECIP_ppt_Mean, y = EVAPTOT_diff), size = psize) +
  geom_smooth(aes(x = PRECIP_ppt_Mean, y = EVAPTOT_diff), se = FALSE)
g_evap2

# drainage
g_drain2 <- g_drain0 +
  geom_point(aes(x = PRECIP_ppt_Mean, y = drain_diff), size = psize) +
  geom_smooth(aes(x = PRECIP_ppt_Mean, y = drain_diff), se = FALSE) +
  labs(x = map_lab)

g_drain2

# combine figures


jpeg("figures/soil_moisture/pub_qual/ETDRAIN_E-T-and-drain_vs_map.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
grid.arrange(legend_intensity,
             g_transp2,
             g_evap2,
             g_drain2,
             layout_matrix = matrix(c(1, 2, 3, 4), ncol = 1),
             heights = c(4, 10, 10, 10))
dev.off()


# ardity vs T by soiltype -------------------------------------------------
jpeg("figures/soil_moisture/pub_qual/T_vs_arid_by_soil.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
tot_transp_diff %>%
  filter(warm == "ambient") %>%
  ggplot(aes(x = aridity_index, y = TRANSP_diff, color = SoilTreatment)) +
  scale_color_manual(values = cols_text) +
  labs(x = aridity_lab,
       y = "Transpiration change (cm)") +
  geom_hline(yintercept = 0, linetype = 2,
             alpha = 0.7) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  geom_point(size = psize) +
  geom_smooth(se = FALSE) +
  lemon::facet_rep_wrap(~intensity, ncol = 1) +
  guides(color = guide_legend(nrow = 2))
dev.off()

# aridity vs PFT transp -------------------------------------------------

# 3 panels, transpiration for shrubs, grasses and forbs
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid_fixed-y.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
g0 <- tot_transp_pft_diff %>%
  filter(SoilTreatment == "loam",
         warm == "ambient",
         PFT != "total") %>%
  mutate(PFT = factor(PFT, levels = c("shrub", "grass", "forbs"),
                      labels = c("shrub", "grass", "forb")),
         PFT_lab = add_letters(PFT)) %>%
  ggplot(aes(x = aridity_index, color = intensity)) +
  scale_color_manual(values = cols_intensity) +
  labs(y = "Transpiration change (cm)") +
  geom_hline(yintercept = 0, linetype = 2,
             alpha = 0.7) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        # allows text to render as markdown
        strip.text = ggtext::element_markdown(hjust = 0)) +
  lemon::facet_rep_wrap(~PFT_lab, ncol = 1) +

  guides(color = guide_legend(ncol = 1))
g <- g0 +
  geom_point(aes(x = aridity_index, y = TRANSP_diff), size = 0.5) +
  geom_smooth(aes(x = aridity_index,y = TRANSP_diff), se = FALSE) +
  labs(x = aridity_lab)
g
dev.off()

# wide format--for powerpoint
jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_arid_wide.jpeg", res = 600,
     height = 3, width = 6.5, units = 'in')
g +
  lemon::facet_rep_wrap(~PFT, scales = "free_y", nrow = 1)


dev.off()

# * MAP vs PFT transp -----------------------------------------------------

jpeg("figures/soil_moisture/pub_qual/TPFTARID_T_vs_map.jpeg", res = 600,
     height = 7, width = 2.5, units = 'in')
g0 +
  geom_point(aes(x = PRECIP_ppt_Mean, y = TRANSP_diff), size = 0.5) +
  geom_smooth(aes(x = PRECIP_ppt_Mean,y = TRANSP_diff), se = FALSE) +
  labs(x = map_lab)

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
