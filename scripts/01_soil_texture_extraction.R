# Martin Holdrege

# Script started 3/1/21

# Purpose is to extract soil texture data for each site


# load site locations -----------------------------------------------------

# info on the 200 sites simulations will be run for
site1 <- readxl::read_xlsx("../dbWeather/200sites.xlsx")

# parse site locations ----------------------------------------------------

# just need coordinates
site2 <- site1[ , c("site_id", "X_WGS84", "Y_WGS84")]


# get soil data -----------------------------------------------------------

# this takes maybe 20 minutes
sgrids1 <- soilDB::fetchSoilGrids(site2, loc.names = c("site_id", "Y_WGS84", "X_WGS84"))

saveRDS(sgrids1, "data-raw/soil_profiles_200sites.RDS")
