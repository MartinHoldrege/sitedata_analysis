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


# intensity renaming ------------------------------------------------------

intensity_lookup <- c("ambient" = "ambient",
                      "event 2x intensity" = "2x_intensity")
