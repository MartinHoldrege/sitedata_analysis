# Martin Holdrege

# Script started Jan 9, 2021

# for starters,  purpose is to explore output of stepwat 2 runs on 14
# representative sites


# dependencies ------------------------------------------------------------

library(tidyverse)
library('DBI')
theme_set(theme_classic())

# file paths --------------------------------------------------------------

# kg1 directory should mounted

# this code added so that it will but on cluster and pc

sitedata_dir <- "/media/grad/kulmatiski-group1/stepwat/sitedata/"
chpc_dir <- "/uufs/chpc.utah.edu/common/home/kulmatiski-group1/stepwat/site_data"
# code should work both on cluster and locally if dir mounted
sitedata_dir <- if(dir.exists(sitedata_dir)) {
  sitedata_dir
} else if (dir.exists(chpc_dir)) {
  chpc_dir
} else if (dir.exists("../sitedata")) {
  "../sitedata"
} else {
  error("no sitedata directory")
}

# connect to db's ---------------------------------------------------------

db_amb_path <- file.path(sitedata_dir,
                         "20201229_ambient_14sites/Output_Compiled.sqlite")

db_2x_path <- file.path(sitedata_dir,
                         "20210108_2x-intensity_14sites/Output_Compiled.sqlite")

db_amb <- dbConnect(RSQLite::SQLite(), db_amb_path)
db_2x <- dbConnect(RSQLite::SQLite(), db_2x_path)


# understand structure ----------------------------------------------------

tables <- dbListTables(db_amb)
names(tables) <- tables
# list columns
map(tables, function(table) dbListFields(db_amb, table))

# db queries --------------------------------------------------------------

# sw2_yearly_slyrs (yearly output for each soil layer)
q1 <- paste("SELECT *",
            "FROM sw2_yearly_slyrs",
            "WHERE GCM = 'Current' AND Year >100;")
sw2_yearly_slyrs_amb <- dbGetQuery(db_amb, q1) %>%
  mutate(intensity = "ambient")
sw2_yearly_slyrs_2x <- dbGetQuery(db_2x, q1) %>%
  mutate(intensity = "event 2x intensity")

# Biomass output from stepwat2
q2 <- paste("SELECT *",
            "FROM Biomass",
            "WHERE GCM = 'Current' AND Year >100;")

bio_amb <- dbGetQuery(db_amb, q2) %>%
  mutate(intensity = "ambient")
bio_2x <- dbGetQuery(db_2x, q2) %>%
  mutate(intensity = "event 2x intensity")

# sw2_yearly_slyrs summaries ----------------------------------------------


# means across years for each layer
soil_mean0 <- bind_rows(sw2_yearly_slyrs_amb, sw2_yearly_slyrs_2x) %>%
  # discard first 100 years
  filter(Year > 100) %>%
  as_tibble() %>%
  group_by(site, intensity) %>%
  summarise_at(.vars = vars(matches("_Lyr_\\d_Mean")),
               .funs = mean)


# long format
soil_long0 <- soil_mean0 %>%
  select(site, intensity, matches("_Mean")) %>%
  pivot_longer(cols = matches("_Mean")) %>%
  mutate(layer = str_extract(name, "(?<=Lyr_)\\d"), # extracting layer
         name = str_replace(name, "_Lyr_\\d_Mean", ""), # removing layer from name
         name = str_replace(name, "_transp", ""), # shorting names duplicated names
         name = str_replace(name, "_swa", ""), #
         PFT = str_extract(name, "(?<=_)[A-z]+$"),  # extracting plant functional type
         name = str_replace(name, "_[A-z]+$", "")) # remove PFT from name

# dataframe with values broken down by plant functional types
lyr_yr_PFT1 <- soil_long0 %>%
  filter(!is.na(PFT))%>%
  pivot_wider(id_cols = c("site", "intensity", "layer", "PFT"),
              names_from = "name",
              values_from = "value")

# metrics that don't use PFT or total pft
lyr_yr_all1 <- soil_long0 %>%
  filter(is.na(PFT)| PFT == "total") %>%
  select(-PFT)%>%
  pivot_wider(id_cols = c("site", "intensity", "layer"),
              names_from = "name",
              values_from = "value")

# sw2_yearly summaries ----------------------------------------------------

# Note: group by SoilTreatment, grazing etc as needed depending on the model

# What is the STdDev column?

#CONTINUE HERE: NEXT CREATE MEANS OF BIOMASS VARIABLES
# PRINT OUT COLUMN NAMES.
sw2_yr_mean0 <- bind_rows(bio_amb, bio_2x) %>%
  as_tibble() %>%
  group_by(site, intensity, SoilTreatment) %>%
  summarise_at(.vars = vars(matches("_Mean$")),
               .funs = mean)

# longer
sw2_yr_mean0 %>%
  pivot_longer(cols = matches("_Mean")) %>%
  mutate(name = str_replace(name, "_Mean$", "")) %>%
  pull(name) %>% unique() %>% sort()

# old code below ----------------------------------------------------------



unique(yr_mean_long0$name) %>% sort()
# difference between ambient and 2x intensity
yr_diff <- yr_mean_long0 %>%
  pivot_wider(names_from = "intensity", values_from = "value") %>%
  mutate(diff = `event 2x intensity` - ambient) %>%
  select(-`event 2x intensity`, -ambient) %>%
  mutate(variable = str_extract(name, "^[A-Za-z_]*(?=_Lyr)"),
         layer = str_extract(name, "\\d")) %>%
  select(-name) %>%
  pivot_wider(names_from = variable, values_from = diff)

yr_mean_long <- yr_mean_long0 %>%
  mutate(variable = str_extract(name, "^[A-Za-z_]*(?=_Lyr)"),
         layer = str_extract(name, "\\d")) %>%
  select(-name) %>%
  pivot_wider(names_from = variable, values_from = value)

# Figures -----------------------------------------------------------------

# dir.create("figures")
# dir.create("figures/14sites")
# * across sites ----------------------------------------------------------

pdf("figures/14sites/ambient_vs_2x_14sites.pdf")
# vwc by rcp and layer
yr_mean_long %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = VWCBULK, y = intensity)) +
    geom_boxplot()+
    coord_flip() +
    facet_grid(layer~RCP) +
    labs(subtitle = "Mean site level VWC by depth and RCP")

yr_diff %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = VWCBULK, y = RCP)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(~layer) +
  labs(subtitle = "mean ambient and 2x intensity vwc difference at site by Layer and RCP",
       x = "VWCBULK diff (intensity - ambient)")

yr_mean_long %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = TRANSP_transp_total, y = intensity)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(layer~RCP) +
  labs(subtitle = "Mean site level transpiration by depth and RCP")

yr_diff %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = TRANSP_transp_total, y = RCP)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(~layer) +
  labs(subtitle = "mean ambient and 2x intensity transp_total difference at site by Layer and RCP",
       x = "total transp diff (intensity - ambient)")

dev.off()
dbDisconnect(db_amb)
dbDisconnect(db_2x)
