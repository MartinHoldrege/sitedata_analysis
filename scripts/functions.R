# Martin Holdrege

# functions used in this projects
# some may be migrated to the precipr package later on


# lyr2depth ---------------------------------------------------------------

#' convert layer to depth
#'
#' @param x numeric vector (soil layer in SOILWAT2)
#'
#' @return numeric vector of depth (cm)
#' @export
#'
#' @examples
#' lyr2depth(c(1, 1, 2:8))
lyr2depth <- function(x) {
  stopifnot(x %in% 1:8)
  depth_lookup <- c(10, 20, 30, 40, 60, 80, 100, 150)
  out <- depth_lookup[x]
  out
}



# lookup vectors ----------------------------------------------------------


intensity_lookup <- c("ambient" = "ambient",
                      "event 2x intensity" = "2x_intensity")



# I want to collapse to the major groups I'm interested in


#' primary PFTs
#'
#' @param x vector of plant functional types, as defined in the Biomass
#' rSFSTEP2 table
#'
#' @return factor of aggregated PFTS
#' @export
#'
#' @examples
#' prime_PFT(c("a.cool.forb", "blah", "sagebrush"))
prime_PFT <- function(x) {

  PFT_lookup <- c("sagebrush" = "sagebrush",
                  "shrub" = "other shrub",
                  "a.cool.forb" = "forb",
                  "a.warm.forb" = "forb",
                  "p.cool.forb" = "forb",
                  "p.warm.forb" = "forb",
                  "a.cool.grass" = "a.cool.grass",
                  "p.cool.grass" = "p.cool.grass",
                  "p.warm.grass" = "p.warm.grass")

  stopifnot(is.character(x),
            # if FALSE unlikely input is what is wanted
            any(x %in% names(PFT_lookup))
  )
  out <- factor(PFT_lookup[x],
                levels = c("sagebrush",
                           "other shrub",
                           "a.cool.grass",
                           "p.cool.grass",
                           "p.warm.grass",
                           "forb")
  )
  out
}


# "shrub" category only includes chvi not arttri so combining
SG_lookup <-  c("sagebrush" = "total_shrub",
                "shrub" = "total_shrub",
               "a.cool.grass" = "total_grass",
               "p.cool.grass" = "total_grass",
               "p.warm.grass" = "total_grass")

