# Martin Holdrege

# functions used in this projects
# some may be migrated to the precipr package later on


# lyr2depth ---------------------------------------------------------------

#' convert layer to depth
#'
#' @param x numeric vector (soil layer in SOILWAT2)
#'
#' @return numeric vector of depth (cm) (midpoint of layer)
#' @export
#'
#' @examples
#' lyr2depth(c(1, 1, 2:8))
lyr2depth <- function(x) {
  stopifnot(x %in% 1:8)
  # bottom of layers
  #depth_lookup <- c(10, 20, 30, 40, 60, 80, 100, 150)
  # midpoint of layer:
  depth_lookup <- c(5,  15,  25,  35,  50,  70,  90, 125)
  out <- depth_lookup[x]
  out
}


# depth2mid ---------------------------------------------------------------

# replacing depth (i.e. actually bottom of layer), with middle of layer
depth2mid <- function(x) {
  bottom <- lyr2depth(1:8) # bottom of layer

}


# cut depth ---------------------------------------------------------------

# cut depth into 3 categories
cut_depth <- function(x) {
  # x--numeric vector, depth of the layer
  stopifnot(x %in% lyr2depth(1:8))
  cut(x,
      breaks = c(0, 10, 40, 200),
      labels = c("0-10 cm", "10-40 cm", "40-150 cm"))
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


#' Four main PFTs
#'
#' @param x vector of original 10 PFTs
#'
#' @return vector with 4 PFTs (C3 and C4 grasses seperate)
#' @export
PFT_four <- function(x) {
  pft4_lookup <- c("sagebrush" = "shrub",
                   "other shrub" = "shrub",
                   "a.cool.grass" = "C3 grass",
                   "p.coo.grass" = "C3 grass",
                   "p.warm.grass" = "C4 grass",
                   "forb" = "forb")
  pft_prime <- prime_PFT(x)
  out <- pft4_lookup[pft_prime]
  out <- factor(out, levels = unique(pft4_lookup))

  #stopifnot(all(!is.na(out)))
  out
}

#' Three main PFTs
#'
#' @param x vector of original 10 PFTs
#'
#' @return vector with 4 PFTs (C3 and C4 grasses combined)
#' @export
PFT_three <- function(x) {
  pft3_lookup <- c("shrub" = "shrub",
                   "C3 grass" = "grass",
                   "C4 grass" = "grass",
                   "forb" = "forb")
  pft4 <- PFT_four(x)
  out <- pft3_lookup[pft4]
  out <- factor(out, levels = unique(pft3_lookup))
  #stopifnot(all(!is.na(out)))
  out
}


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

calc_diff <- function(col, intensity, warm) {
  # difference between value and the value when intensity and temp is ambient
  col - col[intensity == "ambient" & warm == "ambient"]
}

# calculating percent difference
calc_perc_diff <- function(col, intensity, warm) {
  # difference between value and the value when intensity is ambient
  (col - col[intensity == "ambient" & warm == "ambient"])/
    col[intensity == "ambient" & warm == "ambient"]*100
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
             warm = trmts[2],
             SoilTreatment = soil_name(.data$SoilTreatment)) %>%
      as_tibble()
  })
  out
}



# trmts2factors -----------------------------------------------------------

#' treatment character columns into factors
#'
#' @param df dataframe with warm, intensity and soiltreatment columns
#'
#' @return dataframe with treatment columns converted to ordered factors
#' @export
trmts2factors <- function(df) {
  stopifnot(
    c("SoilTreatment", "warm", "intensity") %in% names(df)
  )

  warm_levels <- c("ambient", "3C")
  warm_labels = c("ambient", "3C warming")

  # adjusting function so it can adapt to new simulations with a 5C treatment
  if("5C" %in% unique(df$warm)) {
    warm_levels <- c(warm_levels, "5C")
    warm_labels <- c(warm_labels, "5C warming")
  }

  out <- df %>%
    mutate(SoilTreatment = factor(.data$SoilTreatment,
                                  levels = c("sand", "silt", "clay", "loam")),
           intensity = factor(.data$intensity,
                             levels = c("ambient", "1.25x intensity",
                                        "1.5x intensity", "2x intensity")),

           warm = factor(.data$warm,
                         levels = warm_levels,
                         labels = warm_labels)
           )
  out
}


# mutate_pft_cols ---------------------------------------------------------

# parsing PFT based columns when converting to long form in
# 01_summary_dfs...script
mutate_pft_cols <- function(df) {
  stopifnot(is.data.frame(df),
            "name" %in% names(df))

  out <- df %>%
    mutate(layer = str_extract(name, "(?<=Lyr_)\\d"), # extracting layer
           name = str_replace(name, "_Lyr_\\d_Mean", ""), # removing layer from name
           name = str_replace(name, "_transp", ""), # shortening names duplicated names
           name = str_replace(name, "_swa", ""), # shortening swa name
           PFT = str_extract(name, "(?<=_)[A-z]+$"),  # extracting plant functional type
           name = str_replace(name, "_[A-z]+$", "")) # remove PFT from name
  out
}


# quantiles ---------------------------------------------------------------

q1 <- function(x) quantile(x, 0.05, na.rm = TRUE)
q2 <- function(x) quantile(x, 0.95, na.rm = TRUE)
