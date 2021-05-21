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

sitedata_dir <- "../sitedata"

# connect to db's ---------------------------------------------------------

# names are intensity_warming treatment (parsed later)
db_paths <- c(
  "ambient_ambient" = "20210323_ambient_200sites/Output_Compiled_ambient.sqlite",
  "2x intensity_ambient" = "20210412_2x_200sites/Output_Compiled_2x.sqlite",
  "1.5x intensity_ambient" = "20210428_1.5x_200sites/Output_Compiled._1.5x.sqlite",
  "1.25x intensity_ambient" = "20210429_1.25x_200sites/Output_Compiled_1.25x.sqlite",
  "ambient_3C" = "20210506_ambient_3C_200sites/Output_Compiled_ambient_3C.sqlite",
  "2x intensity_3C" = "20210507_2x_3C_200sites/Output_Compiled_2x_3C.sqlite",
  "1.5x intensity_3C" = "20210507_1.5x_3C_200sites/Output_Compiled_1.5x_3C.sqlite",
  "1.25x intensity_3C" = "20210509_1.25x_3C_200sites/Output_Compiled_1.25_3C.sqlite",
  "ambient_5C" = "20210512_ambient_5C_200sites/Output_Compiled_ambient_5C.sqlite",
  "2x intensity_5C" = "20210510_2x_5C_200sites/Output_Compiled_2x_5C.sqlite",
  "1.5x intensity_5C" = "20210511_1.5x_5C_200sites/Output_Compiled_1.5x_5C.sqlite",
  "1.25x intensity_5C" = "20210511_1.25x_5C_200sites/Output_Compiled_1.25_5C.sqlite"
)

db_connects <- map(db_paths, function(x) {
  dbConnect(RSQLite::SQLite(), file.path(sitedata_dir, x))
})

# understand structure ----------------------------------------------------

tables <- dbListTables(db_connects[[1]])
names(tables) <- tables
# list columns
map(tables, function(table) dbListFields(db_connects[[1]], table))

# db queries --------------------------------------------------------------

# # sw2_yearly_slyrs (yearly output for each soil layer)
q1 <- paste("SELECT *",
            "FROM sw2_yearly_slyrs",
            "WHERE GCM = 'Current' AND Year >100;")


sw2_yearly_slyrs <- query_add_trmt(db_connects, q1)
nrow(sw2_yearly_slyrs)


# Biomass output from stepwat2
q2 <- paste("SELECT *",
            "FROM Biomass",
            "WHERE GCM = 'Current' AND Year >100;")

bio <- query_add_trmt(db_connects, q2)
nrow(bio)

# sw2_yearly
q3 <- paste("SELECT *",
            "FROM sw2_yearly",
            "WHERE GCM = 'Current' AND Year >100;")

# note the db has a column called intensity, but I'm overwriting it here
# (it just has the value 24)
sw2_yrly0 <- query_add_trmt(db_connects, q3)

# sw2_daily
# mean values for each day of year across years and iterations
q4 <- paste("SELECT *",
            "FROM sw2_daily",
            "WHERE GCM = 'Current'")

sw2_dly0 <- query_add_trmt(db_connects, q4)

walk(db_connects, dbDisconnect) # disconnect from databases

# sw2_yearly_slyrs summaries ----------------------------------------------


# means across years for each layer
soil_mean0 <- sw2_yearly_slyrs %>%
  as_tibble() %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  summarise_at(.vars = vars(matches("_Lyr_\\d_Mean")),
               .funs = mean)


# long format
soil_long0 <- soil_mean0 %>%
  select(site, intensity, warm, SoilTreatment, matches("_Mean")) %>%
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
  pivot_wider(id_cols = c("site", "intensity", "warm", "layer", "PFT",
                          "SoilTreatment"),
              names_from = "name",
              values_from = "value")

# metrics that don't use PFT or total pft
lyr_yr_all1 <- soil_long0 %>%
  filter(is.na(PFT)| PFT == "total") %>%
  select(-PFT)%>%
  pivot_wider(id_cols = c("site", "intensity", "warm", "layer", "SoilTreatment"),
              names_from = "name",
              values_from = "value")

# sw2_yearly summaries ---------------------------------------------------

# means across years
sw2_yrly1 <- sw2_yrly0 %>%
  as_tibble() %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  # i'm also interested in sd of ppt and temp
  summarise_at(.vars = vars(matches("_Mean$|^PRECIP_ppt_|^Temp_(max|min)_C")),
               .funs = mean)


# sw2_daily summaries -----------------------------------------------------

# already means across iterations and years so no actually summarizing
sw2_dly1 <- sw2_dly0 %>%
  as_tibble() %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  select(site, intensity, warm, SoilTreatment, Day,
         matches("_Mean$|^PRECIP_ppt_|^Temp_(max|min)_C"))

# BIOMASS summaries ----------------------------------------------------

# yearly means of biomass and abiotic variables. Wide foremat
bio0 <- bio %>%
  as_tibble() %>%
  mutate(SoilTreatment = soil_name(SoilTreatment)) %>%
  # only 1 level SpeciesTreatment, dst, grazing and RGroupTreatment, WildFire,
  # so removing here
  # note group by those variables if varied in model runs
  select(-matches("_(std)|(Indivs)|(Pfire)|(PRstd)|(PR)|(RSize)$"),
         -c(years, RCP, SpeciesTreatment, Year, RGroupTreatment, dst, grazing,
            WildFire, GCM)) %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  summarise_all(.funs = mean) %>%
  ungroup()

# biomass long format
bio1 <-  bio0 %>%
  select(-c(PPT, StdDev, Temp, StdDev.1)) %>%
  pivot_longer(cols = -c(site, intensity, warm, SoilTreatment),
               names_to = "PFT",
               values_to = "biomass")

# save files --------------------------------------------------------------

# soil lyr files
write_csv(lyr_yr_PFT1, "data-processed/site_means/yr_mean_by_lyr-PFT_v1.csv")

write_csv(lyr_yr_all1, "data-processed/site_means/yr_mean_by_lyr-all_v1.csv")

# soilwat2 yearly
write_csv(sw2_yrly1, "data-processed/site_means/sw2_yr_means_v1.csv")

# soilwat2 daily
write_csv(sw2_dly1, "data-processed/site_means/sw2_dly_means_v1.csv")

# biomass files

write_csv(bio1, "data-processed/site_means/biomass_mean_v1.csv")




