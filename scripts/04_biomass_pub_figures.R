

# dependencies ------------------------------------------------------------

source("scripts/03_biomass_summarize.R")
source("scripts/fig_params.R")

# fig themes ---------------------------------------------------------------
theme_set(theme_classic())

theme_update(strip.background = element_blank())


# functions ---------------------------------------------------------------

create_trmt_labels <- function(df) {
  # remove control, and create unique name for each treatment and treatment
  # group


  trmt_group_levs <- c("intensity manipulation", "warming manipulation",
                       "intensity & warming manipulation")

  out <- df %>%
    filter(SoilTreatment == "loam",
         # remove ctrl
         !(warm == "ambient" & intensity == "ambient")) %>%
    # creating new variable to group treatments into
    mutate(trmt_group = ifelse(warm == "ambient",
                               "intensity manipulation",
                               ifelse(intensity == "ambient",
                                      "warming manipulation",
                                      "intensity & warming manipulation")),
           trmt_lab = ifelse(trmt_group == "intensity manipulation",
                             as.character(intensity),
                             ifelse(trmt_group == "warming manipulation",
                                    as.character(warm),
                                    paste(intensity, warm, sep = "/"))),
           trmt_lab = str_replace_all(trmt_lab, " intensity| warming", ""),
           trmt_group = factor(trmt_group,
                               levels = trmt_group_levs))
  trmt_levs <- out %>%
    arrange(trmt_group, warm, intensity) %>%
    pull(trmt_lab) %>%
    unique()

  out$trmt_lab <- factor(out$trmt_lab, levels = trmt_levs)

  out
}

cols_group <- c("#377eb8", "#e41a1c", "#984ea3")
box_base <- function() {
  list(
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.7),
    geom_boxplot(),
    scale_fill_manual(values = cols_group),
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5)),
    guides(fill = guide_legend(ncol = 2)),
    labs(x = "Treatment")
  )
}

# boxplot--by pft and trmt ------------------------------------------------


bio_prime_PFT_diff2 <- bio_prime_PFT_diff1 %>%
  rename(PFT = prime_PFT)


df_list <- map(list(pft_prime = bio_prime_PFT_diff2,
                    pft4 = bio_pft4_diff,
                    pft3 = bio_pft3_diff),
               .f = create_trmt_labels)

# creating seperate figures for each PFT grouping
figs <- map(df_list, function(df) {
  # absolute difference
  g1 <- ggplot(df, aes(x = trmt_lab, y = bio_diff, fill = trmt_group)) +
    box_base() +
    lemon::facet_rep_wrap(~ PFT, ncol = 1, scales = "free_y") +
    labs(y = bio_lab1)
  # percent difference
  g2 <- ggplot(df, aes(x = trmt_lab, y = bio_perc_diff, fill = trmt_group)) +
    box_base() +
    lemon::facet_rep_wrap(~ PFT, ncol = 1, scales = "free_y") +
    labs(y = bio_lab2)

  list(g1, g2)
})

pdf("figures/biomass/bio_boxplots_by_pft_v1.pdf",
    width = 5, height = 7)
figs
dev.off()


# biomass change vs aridity -----------------------------------------------

# choosing 1 intensity and 1 warming level
jpeg("figures/biomass/pub_figs/BIOARID_bio_vs_aridity.jpeg",
     res = 600,  height = 5,  width = 5, units = 'in')

df_list$pft4 %>%
  filter(trmt_lab %in% c("2x", "3C", "2x/3C")) %>%
  ggplot(aes(x = aridity_index, y = bio_diff, color = trmt_lab)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.7) +
  geom_point(size = 0.5) +
  geom_smooth(se = FALSE) +
  lemon::facet_rep_grid(PFT ~ trmt_lab, scales = "free_y") +
  scale_color_manual(values = cols_group) +
  labs(x = aridity_lab,
       y = bio_lab1) +
  theme(legend.position = "none")

dev.off()



