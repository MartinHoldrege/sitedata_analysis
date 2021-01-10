# Martin Holdrege

# Script started Jan 9, 2021

# for starters,  purpose is to explore output of stepwat 2 runs on 14
# representative sites


# dependencies ------------------------------------------------------------

library(tidyverse)
library('DBI')


# file paths --------------------------------------------------------------

# kg1 directory should mounted

sitedata_dir <- "/media/grad/kulmatiski-group1/stepwat/sitedata/"

# code should work both on cluster and locally if dir mounted
sitedata_dir <- if(dir.exists(sitedata_dir)) {
  sitedata_dir
} else if (dir.exits("../sitedata")) {
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

dbListTables(db_amb)
dbListFields(db_amb, "sw2_monthly")
dbListFields(db_amb, "sw2_yearly_slyrs")
# db queries --------------------------------------------------------------

# SWCBULK
q1 <- paste("SELECT site, Year, VWCBULK_Lyr_1_Mean, VWCBULK_Lyr_3_Mean",
            "FROM sw2_yearly_slyrs;")

df1_amb <- dbGetQuery(db_amb, q1)
dim(df)
