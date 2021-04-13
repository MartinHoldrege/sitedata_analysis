# Martin Holdrege

# Script started April 12, 2021

# purpose is to create summary csv files of the weather database info
# from full runs for all 200 sites

# dependencies ------------------------------------------------------------

library(tidyverse)
library(readr)
library('DBI')
source("scripts/functions.R")
theme_set(theme_classic())

# file paths --------------------------------------------------------------

# kg1 directory should mounted

# this code added so that it will but on cluster and pc

sitedata_dir <- "../sitedata"

# connect to db's ---------------------------------------------------------

db_amb_path <- file.path(sitedata_dir,
                         "20210323_ambient_200sites/Output_Compiled_ambient.sqlite")

db_2x_path <- file.path(sitedata_dir,
                        "20210412_2x_200sites/Output_Compiled_2x.sqlite")

db_amb <- dbConnect(RSQLite::SQLite(), db_amb_path)
db_2x <- dbConnect(RSQLite::SQLite(), db_2x_path)


# understand structure ----------------------------------------------------

tables <- dbListTables(db_amb)
names(tables) <- tables
# list columns
map(tables, function(table) dbListFields(db_amb, table))

# db queries --------------------------------------------------------------

# # sw2_yearly_slyrs (yearly output for each soil layer)
q1 <- paste("SELECT *",
            "FROM sw2_yearly_slyrs",
            "WHERE GCM = 'Current' AND Year >100;")
sw2_yearly_slyrs_amb <- dbGetQuery(db_amb, q1) %>%
  mutate(intensity = "ambient")
sw2_yearly_slyrs_2x <- dbGetQuery(db_2x, q1) %>%
  mutate(intensity = "2x intensity")

# Biomass output from stepwat2
q2 <- paste("SELECT *",
            "FROM Biomass",
            "WHERE GCM = 'Current' AND Year >100;")

bio_amb <- dbGetQuery(db_amb, q2) %>%
  mutate(intensity = "ambient")
bio_2x <- dbGetQuery(db_2x, q2) %>%
  mutate(intensity = "2x intensity")

# sw2_yearly
q3 <- paste("SELECT *",
            "FROM sw2_yearly",
            "WHERE GCM = 'Current' AND Year >100;")
sw2_yearly_amb <- dbGetQuery(db_amb, q3) %>%
  # note the db has a column called intensity, but I'm overwriting it here
  # (it just has the value 24)
  mutate(intensity = "ambient")
sw2_yearly_2x <- dbGetQuery(db_2x, q3) %>%
  mutate(intensity = "2x intensity")

dbDisconnect(db_amb)
dbDisconnect(db_2x)

# sw2_yearly_slyrs summaries ----------------------------------------------


# means across years for each layer
soil_mean0 <- bind_rows(sw2_yearly_slyrs_amb, sw2_yearly_slyrs_2x) %>%
  as_tibble() %>%
  group_by(site, intensity) %>%
  summarise_at(.vars = vars(matches("_Lyr_\\d_Mean")),
               .funs = mean)


# long format
soil_long0 <- soil_mean0 %>%
  select(site, intensity, SoilTreatment, matches("_Mean")) %>%
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
  pivot_wider(id_cols = c("site", "intensity", "layer", "PFT", "SoilTreatment"),
              names_from = "name",
              values_from = "value")

# metrics that don't use PFT or total pft
lyr_yr_all1 <- soil_long0 %>%
  filter(is.na(PFT)| PFT == "total") %>%
  select(-PFT)%>%
  pivot_wider(id_cols = c("site", "intensity", "layer", "SoilTreatment"),
              names_from = "name",
              values_from = "value")

# sw2_yearly summaries ---------------------------------------------------

# means across years
sw2_yrly1 <- bind_rows(sw2_yearly_amb, sw2_yearly_2x) %>%
  # discard first 100 years
  as_tibble() %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  group_by(site, intensity, SoilTreatment) %>%
  # i'm also interested in sd of ppt and temp
  summarise_at(.vars = vars(matches("_Mean$|^PRECIP_ppt_|^Temp_(max|min)_C")),
               .funs = mean)


# BIOMASS summaries ----------------------------------------------------

# yearly means of biomass and abiotic variables. Wide foremat
bio0 <-bind_rows(bio_amb, bio_2x) %>%
  as_tibble() %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  # only 1 level SpeciesTreatment, dst, grazing and RGroupTreatment, WildFire,
  # so removing here
  # note group by those variables if varied in model runs
  select(-matches("_(std)|(Indivs)|(Pfire)|(PRstd)|(PR)|(RSize)$"),
         -c(years, RCP, SpeciesTreatment, Year, RGroupTreatment, dst, grazing,
            WildFire, GCM)) %>%
  group_by(site, intensity, SoilTreatment) %>%
  summarise_all(.funs = mean) %>%
  ungroup()

# biomass long format
bio1 <-  bio0 %>%
  select(-c(PPT, StdDev, Temp, StdDev.1)) %>%
  pivot_longer(cols = -c(site, intensity, SoilTreatment),
               names_to = "PFT",
               values_to = "biomass")


# save files --------------------------------------------------------------

# soil lyr files
write_csv(lyr_yr_PFT1, "data-processed/site_means/yr_mean_by_lyr-PFT_v1.csv")

write_csv(lyr_yr_all1, "data-processed/site_means/yr_mean_by_lyr-all_v1.csv")

# soilwat2 yearly
write_csv(sw2_yrly1, "data-processed/site_means/sw2_yr_means_v1.csv")

# biomass files

write_csv(bio1, "data-processed/site_means/biomass_mean_v1.csv")




