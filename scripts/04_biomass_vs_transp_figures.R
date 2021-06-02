# Martin Holdrege

# Script started May 25, 2021

# Purpose of is to compare variables, to help explain why transpiration can
# go up with warming but that often still leads to a decrease in biomass.
# Examinations include regressing biomass and transpiration against each
# other. As well as regressing changes in response to warming against
# each other


# dependencies ------------------------------------------------------------

source("scripts/03_SM_summarize.R")
source("scripts/03_biomass_summarize.R")
source("scripts/fig_params.R")

# combine dfs -------------------------------------------------------------

tot_transp_pft$PFT %>%
  unique()

join_vars <- c("PFT", "site", "intensity", "warm", "SoilTreatment")
match_vars <- c("Temp", "PRECIP", "PET", "aridity")

# combining shrub/grass biomass and differences
bio_transp1 <- bio_SG1 %>%
  mutate(PFT = str_replace(SG, "total_", "")) %>%
  # removing cols that also included joining df
  select(-matches(match_vars)) %>%
  left_join(tot_transp_pft, by = join_vars) %>%
  # removing additional cols before join, note, left join is necessary
  # because right df is missing control intensity/warm combinations
  left_join(tot_transp_pft_diff %>%
              select(-matches(match_vars), -TRANSP),
            by = join_vars) %>%
  left_join(bio_SG_diff1 %>%
              select(-matches(match_vars)),
            by = c("SG", "site", "intensity", "warm", "SoilTreatment")
  )

bio_transp2 <- bio_transp1 %>%
  mutate(MAT_actual = ifelse(warm == "3C warming", TEMP_avg_C_Mean + 3.07,
                             ifelse(warm == "5C warming",
                                    TEMP_avg_C_Mean + 5.4,
                                    TEMP_avg_C_Mean)),
         MAT_thresh = ifelse(MAT_actual > 9.5 , "MAT > 9.5C", "MAT < 9.5C"),
         # so default colors are intuitive
         MAT_thresh = fct_rev(factor(MAT_thresh)))

y_lookup <- c(900, 1000)
# number of sites above threshold
n_thresh <- bio_transp2 %>%
  filter(intensity == "ambient") %>%
  group_by(warm, MAT_thresh) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(label = paste0("n = ", n),
         # location to plot labels
         y = y_lookup[as.numeric(MAT_thresh)])

# min temp for treatment to cross the 9.5C temp threshold, that is
# described in appendix 2, of palmquiest et al 2018.
min_thresh <- c("3C warming" = 9.5 - 3.07,
                "5C warming" = 9.5 - 5.4)


bio_transp_diff1 <- bio_transp1 %>%
  filter(!(intensity == "ambient" & warm == "ambient")) %>%
  mutate(
    thresh = min_thresh[as.character(warm)],
    # did treatment cause threshold to be crossed
    thresh_crossed = ifelse((warm %in% names(min_thresh) &
                              TEMP_avg_C_Mean > thresh & TEMP_avg_C_Mean < 9.5),
                            "9.5C threshold crossed", "threshold not crossed")) %>%
  select(-thresh)


# fig themes --------------------------------------------------------------

theme_set(theme_classic())
theme_update(strip.background = element_blank(),
             legend.title = element_blank(),
             legend.position = "top")

# fig params --------------------------------------------------------------

cap1 <- paste(
  "STEPWAT2 run for 200 sites. Only loam soils shown.\n"
)
cap2 <- paste(
  "STEPWAT2 run for 200 sites. Only loam soils shown.",
  "Control is ambient intensity and ambient warming",
  "\nThreshold crossed when warming treatment raised MAT from below 9.5 to above 9.5"
)

diff_fig_base <- function(df) {
  list(
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.3),
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.3),
    geom_point(size = 0.5),
    facet_grid(warm~intensity),
    geom_smooth(method = "lm", se = FALSE),
    labs(caption = cap2)
  )
}

# biomass vs transp -------------------------------------------------------

pdf("figures/biomass/bio_vs_transp_v1.pdf")

# * absolute values -------------------------------------------------------
# biomass and transpiration, under ambient conditions

bio_transp2 %>%
  filter(warm == "ambient", intensity == "ambient",
         SoilTreatment == "loam") %>%
  ggplot(aes(TRANSP, biomass, color = MAT_thresh)) +
  geom_point(size = 0.7) +
  facet_wrap(~PFT, scales = "free") +
  geom_smooth(method = "lm", se = FALSE, size = 0.7) +
  labs(x = transp_lab0, y = bio_lab0,
       subtitle = "Biomass vs transpiration for shrubs and grasses. Only control treatment shown.",
       caption = cap1)

bio_transp2 %>%
  filter(intensity == "ambient",
         SoilTreatment == "loam", SG == "total_shrub") %>%
  ggplot(aes(TRANSP, biomass, color = MAT_thresh)) +
  geom_point(size = 0.7) +
  facet_wrap(~warm, ncol = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 0.7) +
  # showing sample size on each panel
  geom_text(data = n_thresh, aes(x = 12, y = y, label = label),
            show.legend = FALSE) +
  labs(x = transp_lab0, y = bio_lab0,
       subtitle = "Biomass vs transpiration for shrubs. Only ambient intensity shown.",
       caption = cap1)

# diffs -------------------------------------------------------------------

# change in biomass vs change in transpiration, by treatment
# for shrubs and grasses

# * absolute diff ---------------------------------------------------------

g <- bio_transp_diff1 %>%
  filter(SoilTreatment == "loam", PFT == "shrub") %>%
  ggplot(aes(TRANSP_diff, bio_diff, color = thresh_crossed))  +
  diff_fig_base() +
  labs(x = transp_lab1, y = bio_lab1,
       subtitle = "Shrub biomass vs transpiration change")
g
g + facet_grid(warm~intensity, scales = "free")

g <- bio_transp_diff1 %>%
  filter(SoilTreatment == "loam", PFT == "grass") %>%
  ggplot(aes(TRANSP_diff, bio_diff, color = thresh_crossed))  +
  diff_fig_base() +
  labs(x = transp_lab1, y = bio_lab1,
       subtitle = "Grass biomass vs transpiration change")

g
g + facet_grid(warm~intensity, scales = "free")

# * % diff --------------------------------------------------------------

bio_transp_diff1 %>%
  filter(SoilTreatment == "loam", PFT == "shrub") %>%
  ggplot(aes(TRANSP_perc_diff, bio_perc_diff, color = thresh_crossed))  +
  diff_fig_base() +
  labs(x = transp_lab2, y = bio_lab2,
       subtitle = "Shrub % biomass change vs % transpiration change")

bio_transp_diff1 %>%
  filter(SoilTreatment == "loam", PFT == "grass") %>%
  ggplot(aes(TRANSP_perc_diff, bio_perc_diff, color = thresh_crossed))  +
  diff_fig_base() +
  labs(x = transp_lab2, y = bio_lab2,
       subtitle = "Grass % biomass change vs % transpiration change")

dev.off()
