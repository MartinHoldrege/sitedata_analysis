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

sitedata_dir <- "/media/grad/kulmatiski-group1/stepwat/sitedata/"

# code should work both on cluster and locally if dir mounted
sitedata_dir <- if(dir.exists(sitedata_dir)) {
  sitedata_dir
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

dbListTables(db_amb)
dbListFields(db_amb, "sw2_monthly")
dbListFields(db_amb, "sw2_yearly_slyrs")
# db queries --------------------------------------------------------------

# SWCBULK
q1 <- paste("SELECT *",
            "FROM sw2_yearly_slyrs;")

df1_amb <- dbGetQuery(db_amb, q1) %>%
  mutate(intensity = "ambient")
df1_2x <- dbGetQuery(db_2x, q1) %>%
  mutate(intensity = "event 2x intensity")

# means across years
yr_mean <- bind_rows(df1_amb, df1_2x) %>%
  as_tibble() %>%
  group_by(site, intensity, RCP) %>%
  summarise_at(.vars = vars(matches("_Lyr_\\d_Mean")),
               .funs = mean) %>%
  mutate(RCP = ifelse(is.na(RCP), "Current", RCP))

yr_mean_long0 <- yr_mean %>%
  select(site, intensity, RCP, matches("VWCBULK|TRANSP_total")) %>%
  pivot_longer(cols = matches("Lyr_\\d_Mean"))

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
