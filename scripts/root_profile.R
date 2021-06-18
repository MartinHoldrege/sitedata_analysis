# Martin Holdrege

# Script started June 8, 2021

# This is data from Andrew, that he used in stepwat for his 2020 Journal of
# Ecology tracer. It provides root uptake for a few species.

# Within PFTs should then take weighted averages based on abundances from andrew


# dependencies ------------------------------------------------------------

library(tidyverse)
source("scripts/fig_params.R")

# read in data ------------------------------------------------------------

# rSFSTEP2 file  that includes transpiration proportion by functional type
# as was used for my standard runs (i.e. mh_develop branch)
git_path <- "https://raw.githubusercontent.com/MartinHoldrege/rSFSTEP2/mh_develop/inputs/InputData_SoilLayers.csv"

sw_slyrs <- read_csv(git_path)

# tracer uptake from Kulmatiski et al. 2020 journal of ecology
trace1 <- readxl::read_xlsx("data-raw/root and tracer uptake distribution_ak.xlsx",
                            na = c("", "."))

# parse data --------------------------------------------------------------

# clean column names
trace2 <- trace1 %>%
  janitor::clean_names()

names(trace2) <- names(trace2) %>%
  str_replace("_tracer_uptake_proportion_per_cm", "")

# lookup vector
pft_lookup <- c("artr" = "shrub",
                "basa" = "forb",
                "pssp" = "grass",
                "agcr" = "grass",
                "pose" = "grass")

# weights for weighte means (just placeholder values for artr and basa
# because they are only sp in the functional group. For grasses
# these are the percent covers from the paper)
wt_lookup <- c("artr" = 1,
               "basa" = 1,
               "pssp" =  5,
               "agcr" = 4.3,
               "pose" = 2)

depths <- sw_slyrs$depth %>%
  unique() %>%
  sort()

# locations to 'cut' depths.
depth_breaks <- c(0, depths[-8], 200)

# long form
trace3 <- trace2 %>%
  select(-root_biomass_proportion_per_cm, -community) %>%
  pivot_longer(cols = agcr:pssp,
               names_to = "species",
               values_to = "tracer_prop") %>%
  mutate(pft = pft_lookup[species])


means1 <- trace3 %>%
  mutate(depth_group = cut(depth_cm,
                           breaks = depth_breaks,
                           labels = depths
  ),
  depth_group = as.numeric(as.character(depth_group))) %>%
  # means across sampling months
  group_by(species, pft, depth_group) %>%
  summarise(tracer_prop = mean(tracer_prop), .groups = "drop") %>%
  mutate(wt = wt_lookup[species]) %>%
  group_by(pft, depth_group) %>%
  summarise(tracer_prop = weighted.mean(tracer_prop, wt, na.rm = TRUE),
            .groups = "drop") %>%
  # dividing so sums to 1, (as currently in rSFSTEP2)
  group_by(pft) %>%
  mutate(tracer_prop = tracer_prop/sum(tracer_prop))


means1

ggplot(trace3, aes(x = -depth_cm, y = tracer_prop, color = species)) +
  geom_line()  +
  facet_wrap(~sampling_month) +
  coord_flip()

# long form version of root profile in STEPWAT2
slyrs_long <- sw_slyrs %>%
  filter(soil_treatment == "loam") %>%
  select(depth, matches("trco")) %>%
  pivot_longer(cols = -depth,
               names_to = "pft",
               values_to = "roots") %>%
  mutate(pft = str_replace(pft, "trco_", "")) %>%
  filter(pft != "tree")

# figures -----------------------------------------------------------------

pdf("figures/root_profiles.pdf")

ggplot(means1, aes(x = -depth_group, y = tracer_prop, color = pft)) +
  geom_line()  +
  coord_flip() +
  labs(x = depth_lab,
       y = "proportion tracer uptake",
       subtitle = "Root profile based on tracer data (Kulmatiski et al. 2020)")

ggplot(slyrs_long, aes(x = -depth, y = roots, color = pft)) +
  geom_line()  +
  coord_flip()+
  labs(x = depth_lab,
       y = "proportion roots",
       subtitle = "Root profile currently used in rSFSTEP2, (forbs and grasses are same)")

dev.off()
