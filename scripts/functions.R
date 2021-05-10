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


# conditional_texture --------------------------------------------------------


#' expected percent of one ofsand silt or clay given value of sand silt or clay
#'
#' This function is somewhat unreliable due to the occasionally surprising
#' output of th emltools::empirical_cdf function, depending on inputs.
#' Purpose is to find say the appropriate percent sand to use given
#' the say the 95th percentile of silt (uses empirical joint cdf). It seems
#' that if the the x_conditional value occurs in the data set a lot
#' then the emperical_cdf function breaks down.
#'
#' @param dt # data table (two columns)
#' @param x_conditional # the value of the x variable that you want
#' to get the conditional mean of y for
#'
#' @return expected value of y texture class given x_conditional
#' @export
#'
#' @examples
conditional_texture <- function(dt, x_conditional, y_by = 0.1, x_step  = 1,
                                y_step = 1) {
  names(dt) <- c("x", "y")
  stopifnot(is.numeric(x_conditional),
            is.data.table(dt),
            names(dt) == c("x", "y"))
  if(x_conditional %in% dt$x) {
    warning("x_conditional value is an observed value of x, adjusting to avoid failure")
    # adding a trivial amount so that cdf function does fail
    x_conditional <- x_conditional + 0.0001
  }
  y_seq <- seq(from = 1, to = 100, by = y_by)
  bounds <- list()
  bounds$a2b2 <- data.table(x = x_conditional,
                            y = y_seq)

  bounds$a1b2 <- data.table(x = x_conditional,
                            y = y_seq - y_step)

  # to reduce variability I'm using a delta of 1 for the x value
  # (not sure if that actually improves things)
  bounds$a2b1 <- data.table(x = x_conditional - x_step,
                            y = y_seq)

  bounds$a1b1 <- data.table(x = x_conditional - x_step,
                            y = y_seq - y_step)

  Fs <- map(bounds, function(bound) {
    # emperical joint cdf
    mltools::empirical_cdf(dt, ubounds = bound)
  })

  out <- bounds$a2b2
  # this is proportional to the conditional probability density at each y
  # note that I'm not actually integrating, but taking small enough
  # steps that this sould be fine
  out$pdf <- Fs$a2b2$CDF - Fs$a1b2$CDF - Fs$a2b1$CDF + Fs$a1b1$CDF

  expected <- weighted.mean(out$y, w = out$pdf)
  expected
}


# soil_name ---------------------------------------------------------------

# for shortening the string provided in SoilTreatment column in stepwat
# databases
soil_name <- function(x) {
  stringr::str_replace(x, "soils_", "")
}


# calculate difference ----------------------------------------------------

# meant to be used inside a grouped mutate statement

calc_diff <- function(col, intensity) {
  # difference between value and the value when intensity is ambient
  col - col[intensity == "ambient"]
}

# calculating percent difference
calc_perc_diff <- function(col, intensity) {
  # difference between value and the value when intensity is ambient
  (col - col[intensity == "ambient"])/col[intensity == "ambient"]*100
}



# query function ----------------------------------------------------------


#' queries
#'
#' @description query multiple identical databases and combine, with new
#' trmt columns added
#'
#' @param connections named list of db connections
#' @param query string, sql query
#' @return dataframe
#' @export
#'
query_add_trmt <- function(connections, query) {

  out <- map2_dfr(connections, names(connections), function(x, name) {

    stopifnot(str_detect(name, "_"))

    trmts <- unlist(str_split(name, "_")) # trmt labels extracted from name

    dbGetQuery(x, query) %>%
      mutate(intensity = trmts[1], # intensity trmt
             # warming trmt
             warm = trmts[2])
  })
  out
}
