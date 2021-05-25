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

cols_aridity <- c("aridity < 0.3" = "#d53e4f",
                  "aridity 0.3 - 0.5" = "#f46d43",
                  "aridity > 0.5" = "#2c7bb6")

cols_warm <- c("ambient" = "black",
               "3C warming" = "#fdcc8a",
               "5C warming" = "#d7301f")
