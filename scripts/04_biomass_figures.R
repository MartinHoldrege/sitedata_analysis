# Martin Holdrege

# script started May 18, 2021

# purpose is to create a complete set of figures of the biomass data


# dependencies ------------------------------------------------------------

source("scripts/03_biomass_summarize.R")
source("scripts/fig_params.R")

# fig themes ---------------------------------------------------------------
theme_set(theme_classic())

theme_update(strip.background = element_blank(),
             strip.text = element_text(size = 12),
             axis.title = element_text(size = 13),
             # increasing right margin so numbers not cutoff
             plot.margin = unit(c(5.5, 10, 5.5, 5.5), "points"))

# fig params ------------------------------------------------------------

caption <- paste("STEPWAT2 run for 200 sites.",
                 "\nControl is ambient intensity and ambient warming")


SGr_diff_lab1 <- expression("shrub:grass difference ("*S*":"*G[trmt]~-~S*":"*G[control]*")")
PFT_lab <- "Plant functional type"

pfts <- levels(bio_prime_PFT1$prime_PFT)

clear_x <- function(){
  list(theme(axis.ticks.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.x = element_blank()))
}

base <- function(strip_size = 12){
  list(labs(caption = caption),
       geom_hline(yintercept = 0, alpha = 0.4, linetype = 2),
       theme(strip.text = element_text(size = strip_size)))
}

texture_legend <- function() {
  list(scale_color_manual(values = cols_text),
       scale_fill_manual(values = cols_text),
       theme(legend.title = element_blank(),
             legend.position = "top")
  )
}

pdf("figures/biomass/bio_sensitivity_v2.pdf")

# SG --------------------------------------------------------------------
# shrubs and grass

# Shrub and grass diff
g <- ggplot(bio_SG_diff1) +
  base() +
  texture_legend()

# s and g diff boxplot
g2 <- g +
  lemon::facet_rep_grid(warm~intensity) +
  labs(x = PFT_lab) +
  theme(axis.text.x = element_text(size = 7))

g2 +
  geom_boxplot(aes(x = SG, y = bio_diff,
                      fill= SoilTreatment)) +
  labs(y = bio_lab1,
       title = "Change in shrub and grass biomass")

# % change boxplot s and g

g3 <- g2 +
  geom_boxplot(aes(x = SG, y = bio_perc_diff,
                   fill= SoilTreatment)) +
  labs(y = bio_lab2,
       title = "% Change in shrub and grass biomass")
g3

g3 + lemon::facet_rep_grid(warm~intensity, scales = "free_y")

# S and G diff across aridity

p <- ggplot(bio_SG_diff1, aes(x = aridity_index, color = SoilTreatment)) +
  base(strip_size = 8) +
  texture_legend() +
  labs(x = aridity_lab)

p2 <- p +
  geom_point(aes(y =  bio_diff)) +
  geom_smooth(aes(y = bio_diff), method = "loess", se = FALSE) +
  labs(x = aridity_lab,
       y = bio_lab1,
       title = "Shrub and grass sensitivity with aridity")

p2 + facet_grid(SG + warm ~ intensity)

p2 + facet_grid(SG + warm ~ intensity, scales = "free_y")

# S and G % diff across aridity
p2 <- p +
  geom_point(aes(y =  bio_perc_diff)) +
  geom_smooth(aes(y = bio_perc_diff), method = "loess", se = FALSE) +
  labs(x = aridity_lab,
       y = bio_lab2,
       title = "Shrub and grass % change with aridity")

p2 + facet_grid(SG + warm ~ intensity)

p2 + facet_grid(SG + warm ~ intensity, scales = "free_y")

# SGr -------------------------------------------------------------------
# shrub grass ratio

g <- ggplot(bio_SGr_diff1, aes(y = SGr_diff)) +
  labs(y = SGr_diff_lab1) +
  base() +
  texture_legend() +
  lemon::facet_rep_grid(warm~intensity)

# SGr diff boxplot

g2 <- g +
  geom_boxplot(aes(x = SoilTreatment, fill= SoilTreatment)) +
  labs(title = "Change in shrub:grass ratio by treatment",
       x = NULL)
g2

g2 + lemon::facet_rep_grid(warm~intensity, scales = "free_y")


# SGr diff across aridity
p <- g +
  geom_point(aes(x = aridity_index, color = SoilTreatment)) +
  geom_smooth(aes(x = aridity_index, color = SoilTreatment), method = "loess",
              se = FALSE) +
  labs(title = "Shrub:grass ratio sensitivity across aridity",
       x = aridity_lab)

p

p +  lemon::facet_rep_grid(warm~intensity, scales = "free_y")

# primary PFT diffs ------------------------------------------------------

# Shrub and grass diff
g <- ggplot(bio_prime_PFT_diff1, aes(fill = SoilTreatment)) +
  base() +
  texture_legend() +
  facet_grid(warm~intensity)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.2, hjust=0.2)) +
  labs(x = PFT_lab)

# PFT diff boxplots

g + geom_boxplot(aes(x = prime_PFT, y = bio_diff)) +
  labs(title = "Biomass change by PFT",
       y = bio_lab1)

# PFT % diff boxplots

# limits for the boxplots
range_df <- bio_prime_PFT_diff1 %>%
  filter(is.finite(bio_perc_diff)) %>%
  group_by(prime_PFT, SoilTreatment, warm, intensity) %>%
  summarize(IQR = IQR(bio_perc_diff),
            q1 = quantile(bio_perc_diff, 0.25),
            q3 = quantile(bio_perc_diff, 0.75)) %>%
  mutate(min = q1 - 1.5*IQR,
         max = q3 + 1.5*IQR)

ylim <- c(min(range_df$min), max(range_df$max))

g + geom_boxplot(aes(x = prime_PFT, y = bio_perc_diff),
                 outlier.shape = NA) +
  labs(title = "% Biomass change by PFT",
       subtitle = "outliers not shown",
       y = bio_lab2,
       x = NULL) +
  ylim(ylim =ylim)


# PFT diff across aridity
# each pft on own page
map(pfts, function(x) {
  df <- filter(bio_prime_PFT_diff1, prime_PFT == x)
  g <- ggplot(df, aes(x = aridity_index, color = SoilTreatment)) +
    base() +
    texture_legend() +
    geom_point(aes(y = bio_diff), alpha = 0.5) +
    geom_smooth(aes(y = bio_diff), method = "loess", se = FALSE) +
    labs(x = aridity_lab,
         y = bio_lab1,
         title = paste(x, "biomass change with aridity"))

  g1 <- g + facet_grid(warm~intensity)

  g2 <- g +
    facet_grid(warm~intensity, scales = "free_y") +
    labs(subtitle = "Note: scales differ")

  list(g1, g2)
})

# PFT % diff across aridity
# each pft on own page

# removing outliers
bio_prime_PFT_diff2 <- bio_prime_PFT_diff1 %>%
  left_join(range_df, by = c("SoilTreatment", "prime_PFT", "warm", "intensity")) %>%
  filter(bio_perc_diff >= min & bio_perc_diff < max)

sub <- "Outliers removed."

map(pfts, function(x) {
  df <- filter(bio_prime_PFT_diff2, prime_PFT == x)

  g <- ggplot(df, aes(x = aridity_index, y = bio_perc_diff,
                      color = SoilTreatment)) +
    base() +
    texture_legend() +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = aridity_lab,
         y = bio_lab2,
         title = paste(x, "% biomass change with aridity"))

  g1 <- g + facet_grid(warm~intensity) +
    labs(subtitle = sub)

  g2 <- g +
    facet_grid(warm~intensity, scales = "free_y") +
    labs(subtitle = paste(sub, "Scales differ"))

  list(g1, g2)
})

dev.off()


