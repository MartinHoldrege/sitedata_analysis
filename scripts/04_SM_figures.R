# Martin Holdrege

# script started 5/11/21 (figures previously in 03_analyze_SM_200sites.R)

# Purpose is to create figures of soil water availability soilwat2 output
# Examines water use etc. across treatments (intensity and warming), depth,
# soil type, and functional type.

# dependencies ------------------------------------------------------------

source("scripts/03_SM_summarize.R")
source("scripts/fig_params.R")

# fig themes ---------------------------------------------------------------

theme_set(theme_classic())
theme_update(strip.background = element_blank(),
             strip.text = element_text(size = 12),
             axis.title = element_text(size = 13),
             # increasing right margin so numbers not cutoff
             plot.margin = unit(c(5.5, 10, 5.5, 5.5), "points"))

# fig params ------------------------------------------------------------
# dimensions of pdf pages (in)
width = 8
height = 7

caption <- paste("STEPWAT2 run for 200 sites.",
                 "PPT intensity and warming manipulated by adjusting",
                 "markov coefficients.",
                 "\nControl is ambient intensity and ambient warming")


se = FALSE # confidence interval

perc_sub <-"% change in transpiration (trmt vs control) across soil layers"

lab_fun1 <- function(x) {
  paste("% change in", x, "(trmt vs ambient)",
        "vs aridity")
}
fgs <- c("forbs", "grass", "shrub")
names(fgs) <- fgs

# base of boxplot and lineplot with depth figures
lyr_base <- function() {
  list(lemon::facet_rep_grid(intensity~warm),
    geom_hline(yintercept = 0, alpha = 0.7, linetype = 2),
    coord_flip(),
    texture_legend(),
    labs(x = depth_lab)
  )
}


# adds mean line (across plots) with depth
mean_line <- function() {
  list(stat_summary(mapping = aes(x = -depth, color = SoilTreatment),
                    fun = mean, geom = "line"),
       stat_summary(mapping = aes(x = -depth, color = SoilTreatment),
                    fun = mean, geom = "point"),
       labs(subtitle = "Points are means across sites")
  )
}

boxplot_base <- function() {
  list(geom_boxplot(aes(x = as.factor(-depth),  fill = SoilTreatment,
                        color = SoilTreatment), position = "dodge2"))
}


aridity_base <- function(scales = "fixed", strip_size = 9) {
  list(geom_hline(yintercept = 0, alpha = 0.7, linetype = 2),
       geom_point(aes(x = aridity_index, color = SoilTreatment),
                  alpha = 0.3, size = 0.5),
       geom_smooth(aes(x = aridity_index, color = SoilTreatment),
                   method = "loess", se = se,
                   size = 0.5),
       facet_grid(as.factor(depth) ~ warm + intensity, scales = scales,
                  labeller = labeller(`as.factor(depth)` = depth2cm())),
       texture_legend(),
       # fewer breaks (small figs)
       scale_x_continuous(breaks = c(0.4, 0.8)),
       # small text to fit all plot in
       theme(strip.text = element_text(size = strip_size)),
       labs(x = aridity_lab,
            caption = caption,
            subtitle = "Soil depths plotted seperately")
  )
}


# create both boxplot and line graphs
box_and_line <- function(g) {
  g1 <- g + boxplot_base() + lyr_base() +
    lemon::facet_rep_wrap(~SoilTreatment)
  g2 <- g + line_base() + lyr_base() +
    lemon::facet_rep_wrap(~SoilTreatment)
  list(g1, g2)
}

texture_legend <- function() {
  list(scale_color_manual(values = cols_text),
       scale_fill_manual(values = cols_text),
       theme(legend.title = element_blank(),
             legend.position = "top")
  )
}


# change to TRUE if want to run all non pub qual figures
if (FALSE){
# sm sum across lyrs ----------------------------------------------------

# soil moisture across layers (ie transpiration or other cumulative metrics)
pdf("figures/soil_moisture/SM_across_lyrs_v3.pdf",
    width = width,
    height = height)

# * transpiration -------------------------------------------------------


ggplot(tot_transp_diff, aes(x = SoilTreatment, y = TRANSP_diff,
                            color = SoilTreatment)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  #  geom_rug(color = "red") +
  labs(y = transp_lab1,
       title = "Intensity and warming effect on total transpiration across soil layers",
       caption = caption) +
  lemon::facet_rep_grid(intensity ~ warm) +
  scale_color_manual(values = cols_text) +
  theme(legend.position = "none")

g <- ggplot(tot_transp_diff,
            aes(color = SoilTreatment, group = SoilTreatment)) +
  geom_hline(yintercept = 0, linetype = 2) +
  lemon::facet_rep_grid(intensity ~ warm) +
  labs(x = aridity_lab,
       caption = caption) +
  texture_legend()

# difference
g +
  geom_point(aes(x = aridity_index, y = TRANSP_diff)) +
  geom_smooth(aes(x = aridity_index, y = TRANSP_diff), method = "loess",
              se = se) +
  labs(y = transp_lab1,
       title = "Total transpiration difference across soil layers vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = TRANSP_perc_diff)) +
  geom_smooth(aes(aridity_index, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       subtitle = paste(perc_sub, "vs aridity"))

# % change vs MAP
g +
  geom_point(aes(PRECIP_ppt_Mean, y = TRANSP_perc_diff)) +
  geom_smooth(aes(PRECIP_ppt_Mean, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       x = "Mean annual precipitation (cm)",
       subtitle = paste(perc_sub, "vs MAP"))

# % change vs Tmax
g +
  geom_point(aes(TEMP_max_C_Mean, y = TRANSP_perc_diff)) +
  geom_smooth(aes(TEMP_max_C_Mean, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2,
       x = "Mean daily Tmax (C)",
       subtitle = paste(perc_sub, "vs Tmax"))

# % change vs MAP by PFT
g2 <- function(data) {
  ggplot(data, aes(x = aridity_index, color = SoilTreatment)) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x = aridity_lab,
         y = transp_lab2,
         caption = caption) +
    texture_legend()
}


# transp--all pfts same page
g2(tot_transp_pft_diff) +
  geom_point(aes(y = TRANSP_perc_diff)) +
  geom_smooth(aes(y = TRANSP_perc_diff),
              method = "loess", se = se) +
  labs(subtitle = paste(perc_sub, "vs aridity, by PFT")) +
  facet_grid(warm + intensity ~ PFT) +
  theme(strip.text.y.right = element_text(angle = 0))

# transp--pfts by page
map(fgs, function(x) {
  print(x)
  tot_transp_pft_diff %>%
    filter(PFT == x) %>%
    g2()  +
    geom_point(aes(y = TRANSP_perc_diff)) +
    geom_smooth(aes(y = TRANSP_perc_diff),
                method = "loess", se = se) +
    labs(subtitle = paste(perc_sub, "vs aridity"),
         title = x) +
    lemon::facet_rep_grid(intensity~warm)
})


# * drainage --------------------------------------------------------------
g +
  geom_point(aes(x = aridity_index, y = drain_diff)) +
  geom_smooth(aes(x = aridity_index, y = drain_diff), method = "loess",
              se = se) +
  labs(y = drain_lab1,
       title = "Deep drainage difference vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = drain_perc_diff)) +
  geom_smooth(aes(aridity_index, y = drain_perc_diff), method = "loess",
              se = se) +
  labs(y = drain_lab2,
       subtitle =lab_fun1("deep drainage"))

# * evaporation ----------------------------------------------------------
g +
  geom_point(aes(x = aridity_index, y = EVAPTOT_diff)) +
  geom_smooth(aes(x = aridity_index, y = EVAPTOT_diff), method = "loess",
              se = se) +
  labs(y = evap_lab1,
       title = "Total evaporation difference  vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = EVAPTOT_perc_diff)) +
  geom_smooth(aes(aridity_index, y = EVAPTOT_perc_diff), method = "loess",
              se = se) +
  labs(y = evap_lab2,
       subtitle =lab_fun1("total evaporation"))

# * AET ----------------------------------------------------------
g +
  geom_point(aes(x = aridity_index, y = AET_diff)) +
  geom_smooth(aes(x = aridity_index, y = AET_diff), method = "loess",
              se = se) +
  labs(y = aet_lab1,
       title = "Actual evapotranspiration difference vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = AET_perc_diff)) +
  geom_smooth(aes(aridity_index, y = AET_perc_diff), method = "loess",
              se = se) +
  labs(y = aet_lab2,
       subtitle =lab_fun1("AET"))

# * AET ----------------------------------------------------------
g +
  geom_point(aes(x = aridity_index, y = T_AET_diff)) +
  geom_smooth(aes(x = aridity_index, y = T_AET_diff), method = "loess",
              se = se) +
  labs(y = t_aet_lab1,
       title = "Difference in T/AET ratio vs aridity")

# % change
g +
  geom_point(aes(aridity_index, y = T_AET_perc_diff)) +
  geom_smooth(aes(aridity_index, y = T_AET_perc_diff), method = "loess",
              se = se) +
  labs(y = t_aet_lab2,
       subtitle =lab_fun1("T/AET"))

dev.off()


# * jpegs ----------------------------------------------------------------
jpeg2 <- function(x,...){
  jpeg(filename = x, height = 8, width = 10, res = 600, units = "in",...)
}

g <- ggplot(filter(tot_transp_diff, SoilTreatment == "loam")) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = aridity_lab)

# % change
jpeg2("figures/soil_moisture/tot_transp_diff_v2.jpg")

g +
  geom_point(aes(aridity_index, y = TRANSP_perc_diff)) +
  geom_smooth(aes(aridity_index, y = TRANSP_perc_diff), method = "loess",
              se = se) +
  labs(y = transp_lab2) +
  lemon::facet_rep_wrap(~intensity + warm)

dev.off()

# SM by lyrs across PFT ---------------------------------------------------

pdf("figures/soil_moisture/SM_by_lyr_across_PFT_v3.pdf",
    width = width,
    height = height)

# * VWC ------------------------------------------------------------------

g <- ggplot(lyr_all_diff1, aes(y = VWC_diff)) +
  lyr_base() +
  labs(y = vwc_lab1,
       title = "Treatment effects on volumetric water content")
g +
  boxplot_base()

g +
  mean_line()

# trmt*depth panels
ggplot(lyr_all_diff1, aes(y = VWC_diff)) +
  aridity_base(strip_size = 7) +
  labs(y = vwc_lab1,
       title = "Change in volumetric water content vs. aridity") +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02))


# * wetdays ---------------------------------------------------------------

g <- ggplot(lyr_all_diff1, aes(y = WETDAY_diff)) +
  lyr_base() +
  labs(y = wetday_lab1,
       title = "Treatment effects on number of wet days")

g +
  boxplot_base()

g +
  mean_line()

ggplot(lyr_all_diff1, aes(y = WETDAY_diff)) +
  labs(y = wetday_lab1,
       title = "Change in number of wet days vs. aridity") +
  aridity_base(strip_size = 7) +
  scale_y_continuous(breaks = c(-50, 0, 50))


# * total transpiration ---------------------------------------------------

g <- ggplot(lyr_all_diff1, aes(y = TRANSP_diff)) +
  lyr_base() +
  labs(y = transp_lab1,
       title = "Treatment effects on total transpiration from each layer")

g +
  boxplot_base()

g +
  mean_line()

# each trmt*depth comb
g2 <- ggplot(lyr_all_diff1, aes(y = TRANSP_diff)) +
  labs(y = transp_lab1,
       title = "Change in total transpiration by layer vs. aridity")

g2 +
  aridity_base(strip_size = 7)

g2 +
  aridity_base(scales = "free_y", strip_size = 7)

# * % change transpiration ------------------------------------------------

g <- ggplot(lyr_all_diff1, aes(y = TRANSP_perc_diff)) +
  lyr_base() +
  labs(y = transp_lab2,
       title = "Treatment effects on % transpiration change from each layer")

g +
  boxplot_base()

g +
  mean_line()

# each trmt*depth comb
g2 <- ggplot(lyr_all_diff1, aes(y = TRANSP_perc_diff)) +
  labs(y = transp_lab2,
       title = "Change in % transpiration by layer vs. aridity")

g2 +
  aridity_base(strip_size = 7)

g2 +
  aridity_base(scales = "free_y", strip_size = 7)

dev.off()


# transp by lyrs and PFT ------------------------------------------------

pdf("figures/soil_moisture/transp_by_lyr-PFT_v3.pdf",
    width = width,
    height = height)

# * transpiration change -------------------------------------------------

# each pft on separate page

transp_l1 <- map(fgs, function(x) {

  df <- filter(lyr_pft_diff1, PFT == x)

  # transp vs depth (each trmt in separate panel)
  g <- ggplot(df, aes(y = TRANSP_diff)) +
    lyr_base() +
    labs(y = transp_lab1,
         title = paste0(x, "--change in transpiration vs. depth"))

  out1 <- g + boxplot_base()

  out2 <- g + mean_line()

  # transp vs aridity (each trmt*depth combo in separate panel)
  g2 <- ggplot(df, aes(y = TRANSP_perc_diff)) +
    labs(y = transp_lab1,
         title = paste0(x, "--change in transpiration vs. aridity"))

  out3 <- g2 +
    aridity_base(strip_size = 7)

  out4 <- g2 +
    aridity_base(scales = "free_y", strip_size = 7)

  list(out1, out2, out3, out4)
})

# number of figures for specific functional type
n_figs <- length(transp_l1[[1]])
fig_df <- expand_grid(figs = 1:n_figs, fgs = fgs)

# printing figures in a better order so PFTs can be compared on
# adjacent pages of pdf
for (i in 1:nrow(fig_df)) {
  print(transp_l1[[fig_df$fgs[i]]][[fig_df$figs[i]]])
}

# * % transpiration ------------------------------------------------------

# each pft on separate page

transp_l2 <- map(fgs, function(x) {

  df <- filter(lyr_pft_diff1, PFT == x)

  # transp vs depth (each trmt in separate panel)
  g <- ggplot(df, aes(y = TRANSP_perc_diff)) +
    lyr_base() +
    labs(y = transp_lab2,
         title = paste0(x, "--% transpiration change vs. depth"))

  out1 <- g + boxplot_base()

  out2 <- g + mean_line()

  # transp vs aridity (each trmt*depth combo in separate panel)
  g2 <- ggplot(df, aes(y = TRANSP_perc_diff)) +
    labs(y = transp_lab2,
         title = paste0(x, "--% transpiration change vs. aridity"))

  out3 <- g2 +
    aridity_base(strip_size = 7)

  out4 <- g2 +
    aridity_base(scales = "free_y", strip_size = 7)

  list(out1, out2, out3, out4)
})


# printing figures in a better order so PFTs can be compared on
# adjacent pages of pdf
for (i in 1:nrow(fig_df)) {
  print(transp_l2[[fig_df$fgs[i]]][[fig_df$figs[i]]])
}

dev.off()

}

