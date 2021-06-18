# Martin Holdrege

# Script started Jan 9, 2021

# for starters,  purpose is to explore output of stepwat 2 runs on 14
# representative sites


# dependencies ------------------------------------------------------------

library(tidyverse)
library(readr)
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

# sw2_yearly_slyrs (yearly output for each soil layer)
q3 <- paste("SELECT *",
            "FROM sw2_yearly",
            "WHERE GCM = 'Current' AND Year >100;")
sw2_yearly_amb <- dbGetQuery(db_amb, q3) %>%
  mutate(intensity = "ambient")
sw2_yearly_2x <- dbGetQuery(db_2x, q3) %>%
  mutate(intensity = "event 2x intensity")

dbDisconnect(db_amb)
dbDisconnect(db_2x)

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

# sw2_yearly summaries ---------------------------------------------------

# means across years
sw2_yrly1 <- bind_rows(sw2_yearly_amb, sw2_yearly_2x) %>%
  # discard first 100 years
  as_tibble() %>%
  group_by(site, intensity, SoilTreatment) %>%
  summarise_at(.vars = vars(matches("_Mean$")),
               .funs = mean)


# BIOMASS summaries ----------------------------------------------------

# yearly means of biomass and abiotic variables. Wide foremat
sw2_yr_mean0 <-bind_rows(bio_amb, bio_2x) %>%
  as_tibble() %>%
  # only 1 level SpeciesTreatment, dst, grazing and RGroupTreatment, WildFire,
  # so removing here
  # note group by those variables if varied in model runs
  select(-matches("_(std)|(Indivs)|(Pfire)|(PRstd)|(PR)|(RSize)$"),
         -c(years, RCP, SpeciesTreatment, Year, RGroupTreatment, dst, grazing,
            WildFire)) %>%
  group_by(site, intensity, SoilTreatment, GCM) %>%
  summarise_all(.funs = mean) %>%
  ungroup()

# just abiotic variables (these are already in "long" format)
# What is the STdDev column?
sw2_yr_abiotic1 <- sw2_yr_mean0 %>%
  select(site, intensity, SoilTreatment, GCM, PPT, StdDev, Temp, StdDev.1)

# biomass long format
sw2_yr_bio1 <-  sw2_yr_mean0 %>%
  select(-c(PPT, StdDev, Temp, StdDev.1)) %>%
  pivot_longer(cols = -c(GCM, site, intensity, SoilTreatment),
               names_to = "PFT",
               values_to = "biomass")


# save files --------------------------------------------------------------

# soil lyr files
write_csv(lyr_yr_PFT1, "data-processed/14sites/yr_mean_SM_by_lyr-PFT_14sites.csv")

write_csv(lyr_yr_all1, "data-processed/14sites/yr_mean_SM_by_lyr-all_14sites.csv")

# soilwat2 yearly
write_csv(sw2_yrly1, "data-processed/14sites/sw2_yr_means_14sites.csv")

# biomass files
write_csv(sw2_yr_abiotic1, "data-processed/14sites/biomass_mean_abiotic_14sites.csv")

write_csv(sw2_yr_bio1, "data-processed/14sites/biomass_mean_14sites.csv")




