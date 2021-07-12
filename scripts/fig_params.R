# Martin Holdrege

# script started April 13, 2021

# Purpose is to provide parameters (colors etc) for figure making
# that are reused in multiple scripts

# colors ------------------------------------------------------------------

# colors for soil texture
cols_text <- c("sand" = "#e41a1c",
               "silt" = "#377eb8",
               "clay" =  "#4daf4a",
               "loam" = "black")

# colors for precipitation intensity
cols_intensity <- c("ambient" = "black",
                    "1.25x intensity" = "#fdcc8a",
                    "1.5x intensity" = "#fc8d59",
                    "2x intensity" = "#d7301f")

# aridity
cols_aridity <- c("aridity < 0.3" = "#d53e4f",
                  "aridity 0.3 - 0.5" = "#f46d43",
                  "aridity > 0.5" = "#2c7bb6")

# warming
cols_warm <- c("ambient" = "black",
               "3C warming" = "#fdcc8a",
               "5C warming" = "#d7301f")


# axis labels -------------------------------------------------------------

# transp
transp_lab0 <- "Transpiration (cm)"
transp_lab1 <- "Transpiration difference (trmt - control; cm)"
transp_lab2 <- "% Transpiration change"

# evap
evap_lab0 <- "Evaporation (cm)"
evap_lab1 <- "Evaporation difference (trmt - control; cm)"
evap_lab2 <- "% Evaporation change"

# drainage
drain_lab0 <- "Deep drainage (cm)"
drain_lab1 <- "Deep drainage difference (trmt - control; cm)"
drain_lab2 <- "% Deep drainage change"

# AET
aet_lab0 <- "Actual evapotranspiration (cm)"
aet_lab1 <- "AET difference (trmt - control; cm)"
aet_lab2 <- "% AET change"

# T/AET
t_aet_lab0 <- "T/AET"
t_aet_lab1 <- "T/AET difference (trmt - control; cm)"
t_aet_lab2 <- "% T/AET change"

# vwc
vwc_lab0 <- "Volumetric water content; cm/cm)"
vwc_lab1 <- "VWC difference (trmt - control; cm/cm)"

# wetday
wetday_lab1 <- "Wet day difference (trmt - control; # days > -1.5 MPa)"

# proportion wet days
wetday_prop_lab0 <- "Proportion wet days (days > -1.5 MPa)"

# biomass
bio_lab0 <- expression("Biomass ("*gm^-2*")")
bio_lab1 <- expression(Biomass[trmt]~-~Biomass[control]~"("*gm^-2*")")
bio_lab1_change <- expression("Biomass change ("*g~m^-2*")")
bio_lab2 <- expression("%"~Biomass~change~"("*frac("trmt - ctrl","ctrl")~"*100)")

# misc.
aridity_lab <- "Aridity index (MAP/PET)"
depth_lab  <- "Soil depth (cm)"
