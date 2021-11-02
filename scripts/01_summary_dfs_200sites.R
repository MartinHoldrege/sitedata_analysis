# Martin Holdrege

# Script started April 12, 2021

# purpose is to create summary csv files of database output
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
cols <- map(tables, function(table) dbListFields(db_connects[[1]], table))
cols
# db queries --------------------------------------------------------------

# Note the sql queries here that group by variables, assume wildfire,
# and grazing were held constant across runs (this is true here)

# * sw2_yearly_slyrs -----------------------------------------------------

#(yearly output for each soil layer)
q1 <- paste("SELECT *",
            "FROM sw2_yearly_slyrs",
            "WHERE GCM = 'Current' AND Year >100;")


sw2_yearly_slyrs <- query_add_trmt(db_connects, q1)
nrow(sw2_yearly_slyrs)


# * Biomass output from stepwat2 -----------------------------------------
q2 <- paste("SELECT *",
            "FROM Biomass",
            "WHERE GCM = 'Current' AND Year >100;")

bio <- query_add_trmt(db_connects, q2)
nrow(bio)

# * sw2_yearly ------------------------------------------------------------
# yearly output from soilwat2
q3 <- paste("SELECT *",
            "FROM sw2_yearly",
            "WHERE GCM = 'Current' AND Year >100;")

# note the db has a column called intensity, but I'm overwriting it here
# (it just has the value 24)
sw2_yrly0 <- query_add_trmt(db_connects, q3)

# * sw2_monthly_slyrs -----------------------------------------------------
# generating the sql statement

# this querry takes means of variables, by grouping variables

# columns to aggregate
mean_vars <- str_subset(cols$sw2_monthly_slyrs, "Mean$")

# cols to group by--at the moment all other potential grouping variables
# (GCM etc) were held constant so don't need to include them
group_vars <- c("site", "SoilTreatment", "Month")

# the select state ment
select <- paste(
  paste(group_vars, collapse = ", "),
  paste(
    paste0(", AVG(", mean_vars, ") AS ", mean_vars),
    collapse = ""
  ),
  collapse = ", "
)

# only loam for now to keep table size down
q5 <- paste("SELECT", select,
            "FROM sw2_monthly_slyrs",
            "WHERE SoilTreatment = 'soils_loam' AND YEAR > 100",
            "GROUP BY", paste(group_vars, collapse = ", "))

# for testing:
# df <- dbGetQuery(db_connects[[1]], q5)

# slow query!
sw2_mly_lyrs0 <- query_add_trmt(db_connects, q5)
nrow(sw2_mly_lyrs0)

# * sw2_monthly --------------------------------------------------------------
# generating the sql statement

# this querry takes means of variables, by grouping variables

# columns to aggregate
# also interested in sd of precip
mean_vars <- str_subset(cols$sw2_monthly,
                        "_Mean$|^PRECIP_ppt_|^Temp_(max|min)_C") %>%
  # for some reason the PET columns (which have .2 in the name where
  # causing the query to fail)
  # no tree data included
  str_subset(".2_Mean$|tree", negate = TRUE)

# the select state ment
select <- paste(
  paste(group_vars, collapse = ", "),
  paste(
    paste0(", AVG(", mean_vars, ") AS ", mean_vars),
    collapse = ""
  ),
  collapse = ", "
)

# here data size not too big, so keeping all soil types
q6 <- paste("SELECT", select,
            "FROM sw2_monthly",
            "WHERE YEAR > 100",
            "GROUP BY", paste(group_vars, collapse = ", "))

# for testing:
# df <- dbGetQuery(db_connects[[1]], q6)

# slow query!
sw2_mly0 <- query_add_trmt(db_connects, q6)
dim(sw2_mly0)

# * sw2_daily ------------------------------------------------------------

# non grouping variables to select
vars1 <- cols$sw2_daily %>%
  str_subset("_Mean$") %>%
  str_subset("CO2EFFECTS|tree|.2_Mean", negate = TRUE)

select <- paste(c("site", "Day", "SoilTreatment", vars1), collapse = ", ")

q7 <- paste("SELECT ", select,
            "FROM sw2_daily",
            "WHERE GCM = 'Current' AND SoilTreatment = 'soils_loam'")

sw2_dly0 <- query_add_trmt(db_connects, q7)

# * sw2_daily_slyrs ------------------------------------------------------------

vars1 <- cols$sw2_daily_slyrs %>%
  str_subset("_Mean$") %>%
  # columns I can do without--dataframe very large otherwise
  str_subset("tree|VWCBULK|SWABULK|SOILTEMP|SWA_|SWAMATRIC|HYDRED|LYRDRAIN|SWCBULK",
             negate = TRUE)

select <- paste(c("site", "Day", "SoilTreatment", vars1), collapse = ", ")

q8 <- paste("SELECT ", select,
            "FROM sw2_daily_slyrs",
            "WHERE GCM = 'Current' AND SoilTreatment = 'soils_loam'")

sw2_dly_lyrs0 <- query_add_trmt(db_connects, q8)
dim(sw2_dly_lyrs0)

# sw2_yearly_slyrs summaries ----------------------------------------------


# means across years for each layer
soil_mean0 <- sw2_yearly_slyrs %>%
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

# sw2_monthly_slyrs summaries ----------------------------------------------


# long format
sw2_mly_lyrs1 <- sw2_mly_lyrs0 %>%
  rename(month = Month) %>%
  select(site, month, intensity, warm, SoilTreatment, matches("_Mean")) %>%
  pivot_longer(cols = matches("_Mean")) %>%
  mutate_pft_cols()

# dataframe with values broken down by plant functional types
lyr_month_PFT1 <- sw2_mly_lyrs1 %>%
  filter(!is.na(PFT))%>%
  pivot_wider(id_cols = c("site", "month", "intensity", "warm", "layer", "PFT",
                          "SoilTreatment"),
              names_from = "name",
              values_from = "value") %>%
  filter(PFT != "tree") # no trees in dataset

# metrics that don't use PFT or total pft
lyr_month_all1 <- sw2_mly_lyrs1 %>%
  filter(is.na(PFT)| PFT == "total") %>%
  select(-PFT)%>%
  pivot_wider(id_cols = c("site", "month", "intensity", "warm", "layer",
                          "SoilTreatment"),
              names_from = "name",
              values_from = "value") %>%
  select(-VWCBULK, -SWABULK) # using Vwc matric instead

# sw2_yearly summaries ---------------------------------------------------

# means across years
sw2_yrly1 <- sw2_yrly0 %>%
  group_by(site, intensity, warm, SoilTreatment) %>%
  # i'm also interested in sd of ppt and temp
  summarise_at(.vars = vars(matches("_Mean$|^PRECIP_ppt_|^Temp_(max|min)_C")),
               .funs = mean)

# sw2_monthly summaries --------------------------------------------------

# no further summarizing done at the moment
sw2_mly1 <- sw2_mly0


# sw2_daily summaries -----------------------------------------------------

# already means across iterations and years so no actually summarizing
sw2_dly1 <- sw2_dly0

# sw2_daily_slyrs summaries -------------------------------------------------

# long format
sw2_dly_lyrs1 <- sw2_dly_lyrs0 %>%
  rename(day = Day)


# long dataframe with values broken down by plant functional types
lyr_dly_PFT1 <- sw2_dly_lyrs1 %>%
  # selecting before pivot to reduce memory problems
  select(site, day, intensity, warm, SoilTreatment,
         matches("_(grass|forbs|shrub)_")) %>%
  pivot_longer(cols = matches("_Mean")) %>%
  mutate_pft_cols() %>%
  pivot_wider(id_cols = c("site", "day", "intensity", "warm", "layer", "PFT",
                          "SoilTreatment"),
              names_from = "name",
              values_from = "value")
sum(is.na(lyr_dly_PFT1$PFT))

# metrics that don't use PFT
cols_non_pft <- names(sw2_dly_lyrs1) %>%
  str_subset("_(grass|forbs|shrub)_", negate = TRUE)

lyr_dly_all1 <- sw2_dly_lyrs1 %>%
  select(all_of(cols_non_pft)) %>%
  pivot_longer(cols = matches("_Mean")) %>%
  mutate_pft_cols() %>%
  select(-PFT) %>%
  pivot_wider(id_cols = c("site", "day", "intensity", "warm", "layer",
                          "SoilTreatment"),
              names_from = "name",
              values_from = "value")

# BIOMASS summaries ----------------------------------------------------

# yearly means of biomass and abiotic variables. Wide foremat
bio0 <- bio %>%
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

# sw2_monthly_slyrs
write_csv(lyr_month_PFT1, "data-processed/site_means/mo_mean_by_lyr-PFT_v1.csv")

write_csv(lyr_month_all1, "data-processed/site_means/mo_mean_by_lyr-all_v1.csv")

# sw2_monthly
write_csv(sw2_mly1, "data-processed/site_means/sw2_mo_means_v1.csv")

# sw2_yearly
write_csv(sw2_yrly1, "data-processed/site_means/sw2_yr_means_v1.csv")

# sw2_daily
write_csv(sw2_dly1, "data-processed/site_means/sw2_dly_means_v1.csv")

# sw2_daily_slyrs
write_csv(lyr_dly_PFT1, "data-processed/site_means/dly_mean_by_lyr-PFT_v1.csv")

write_csv(lyr_dly_all1, "data-processed/site_means/dly_mean_by_lyr-all_v1.csv")

# biomass files

write_csv(bio1, "data-processed/site_means/biomass_mean_v1.csv")


# disconnect db --------------------------------------------------------

walk(db_connects, dbDisconnect) # disconnect from databases


