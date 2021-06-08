# Martin Holdrege

# Script started June 8, 2021

# WIP

# NEXT steps--determine if I'm using the correct data from andrew
# then take weighted averages based on abundances from andrew


# dependencies ------------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------

# rSFSTEP2 file  that includes transpiration proportion by functional type
# as was used for my standard runs (i.e. mh_develop branch)
git_path <- "https://raw.githubusercontent.com/MartinHoldrege/rSFSTEP2/mh_develop/inputs/InputData_SoilLayers.csv"

sw_slyrs <- read_csv(path)

# tracer uptake from Kulmatiski et al. 2020 journal of ecology
trace1 <- readxl::read_xlsx("data-raw/root and tracer uptake distribution_ak.xlsx",
                            na = c("", "."))



# parse data --------------------------------------------------------------

# clean column names
trace2 <- trace1 %>%
  janitor::clean_names()

names(trace2) <- names(trace2) %>%
  str_replace("_tracer_uptake_proportion_per_cm", "")

pft_lookup <- c("artr" = "shrub",
                "")
# long form
trace3 <- trace2 %>%
  select(-root_biomass_proportion_per_cm, -community) %>%
  pivot_longer(cols = agcr:pssp,
               names_to = "species",
               values_to = "tracer_prop")

ggplot(trace3, aes(x = -depth_cm, y = tracer_prop, color = species)) +
  geom_line()  +
  facet_wrap(~sampling_month) +
  coord_flip()

trace3 %>%
  filter(sampling_month == "july") %>%
  ggplot(aes(x = -depth_cm, y = tracer_prop, color = species)) +
  geom_line()  +
  facet_wrap(~sampling_month) +
  coord_flip()
