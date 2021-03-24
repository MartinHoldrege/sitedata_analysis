# Martin Holdrege

# Script started March 24, 20

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
                         "20210323_ambient_200sites/Output_Compiled_ambient.sqlite")

db_amb <- dbConnect(RSQLite::SQLite(), db_amb_path)


# understand structure ----------------------------------------------------

tables <- dbListTables(db_amb)
names(tables) <- tables
# list columns
map(tables, function(table) dbListFields(db_amb, table))

# db queries --------------------------------------------------------------

# sw2_yearly_slyrs (yearly output for each soil layer)
q1 <- paste("SELECT site, PRECIP_ppt_Mean",
            "FROM sw2_yearly",
            "WHERE GCM = 'Current';")
sw2_yearly_amb <- dbGetQuery(db_amb, q1) %>%
  mutate(intensity = "ambient")


# calculate MAP ----------------------------------------------------------

map <- sw2_yearly_amb %>%
  group_by(site) %>%
  summarize(MAP = mean(PRECIP_ppt_Mean))

write_csv(map, "../sitedata/ppt/map_fromwgen_ambient_200sites.csv")

