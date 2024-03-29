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


# cut depth ---------------------------------------------------------------

# cut depth into 3 categories
cut_depth <- function(x, two_depths = FALSE) {
  # x--numeric vector, depth of the layer
  # two_depths --logical of whether to create two depth categories
  # (default is 3--for back compatibility)
  stopifnot(x %in% lyr2depth(1:8))
  out <- if (two_depths) {
    cut(x,
        breaks = c(0, 10, 200),
        labels = c("0-10 cm", "10-150 cm"))
  } else {
    cut(x,
        breaks = c(0, 10, 40, 200),
        labels = c("0-10 cm", "10-40 cm", "40-150 cm"))
  }
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
#' @param combine_shrubs logical whether to group sagebrush and 'other shrub'
#' into one category
#'
#' @return factor of aggregated PFTS
#' @export
#'
#' @examples
#' prime_PFT(c("a.cool.forb", "blah","shrub", "sagebrush"))
#' prime_PFT(c("shrub", "sagebrush"), combine_shrubs = TRUE)
prime_PFT <- function(x, combine_shrubs = FALSE) {

  PFT_lookup <- c("sagebrush" = "sagebrush",
                  "shrub" = "other shrub",
                  "a.cool.forb" = "forb",
                  "a.warm.forb" = "forb",
                  "p.cool.forb" = "forb",
                  "p.warm.forb" = "forb",
                  "a.cool.grass" = "a.cool.grass",
                  "p.cool.grass" = "p.cool.grass",
                  "p.warm.grass" = "p.warm.grass")

  # combine both shrubs
  if (combine_shrubs) {
    PFT_lookup[c("sagebrush", "shrub")] <- "shrub"
  }

  stopifnot(is.character(x),
            # if FALSE unlikely input is what is wanted
            any(x %in% names(PFT_lookup))
  )

  levels <- if (combine_shrubs) {
    c("shrub", "a.cool.grass", "p.cool.grass", "p.warm.grass", "forb")
  } else {
    c("sagebrush", "other shrub", "a.cool.grass", "p.cool.grass",
      "p.warm.grass", "forb")
  }

  out <- factor(PFT_lookup[x], levels = levels)
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
#' @description Currently, this function is by default
#' # only including perennials
#' in the C3 grass category, b/ the manuscript focuses on just those mose
#' dominant PFTs
#'
#' @param x vector of original 10 PFTs
#' @param exclude_annualC3 logical--whether to exclude annuals from the
#' C3 grass category.
#'
#' @return vector with 4 PFTs (C3 and C4 grasses seperate)
#' @export
PFT_four <- function(x, exclude_annualC3 = TRUE) {
  pft4_lookup <- c("sagebrush" = "shrub",
                   "other shrub" = "shrub",
                   "a.cool.grass" = NA,
                   "p.cool.grass" = "C3 grass",
                   "p.warm.grass" = "C4 grass",
                   "forb" = "forb")
  if(!exclude_annualC3) {
    pft4_lookup["a.cool.grass"] <- "C3 grass"
  }


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

  # here want all grasses--so not excluding any annual grasses
  pft4 <- PFT_four(x, exclude_annualC3 = FALSE)
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

# difference of differences
# i.e. differences between responses between soil types

#' calculate differences between soils
#'
#' @param col Vector, response variable of interest
#' @param soil Character/factor vector of soil type
#' @param ref_soil reference soil type that want to take the difference against
#'
#' @return difference in col between soil types
#' @export
#'
#' @examples
calc_soil_diff <- function(col, soil, ref_soil = "silt") {
  col - col[soil == ref_soil]
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


# sgf2factor --------------------------------------------------------------

# convert shrubs, grasses, and forbs to an ordered factor

sgf2factor <- function(x) {
  factor(x, levels = c("shrub", "grass", "forbs"),
         labels = c("shrub", "grass", "forb"))
}


# split aridity -----------------------------------------------------------

# function to create a factor that groups aridity into two levels
arid2levels <- function(x, cut_point = 0.54) {
  # x--aridity index
  # cut_point--point to split aridity (0.54 is the transpiration transition
  # point)
  low <- paste("aridity <", cut_point)
  high <- paste("aridity >", cut_point)
  out <- ifelse(x < cut_point, low, high)
  out <- factor(out, levels = c(low, high))
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


# predict -----------------------------------------------------------------


# return newdata dataframe along with prediction
predict_newdata <- function(mod, newdata) {
  newdata$yhat <- predict(mod, newdata = newdata)
  return(newdata)
}


# add letters -------------------------------------------------------------

add_letters <- function(x, letters = base::letters) {
  # x--factor that you want to factor by
  # letters to paste to strings
  # returns--factor with letters pasted in front, so that individual
  # facets in a ggplot have unique letters
  stopifnot(is.factor(x))

  levels_old <- levels(x)
  n <- length(levels_old)
  levels_new <- paste0("**(", letters[1:n], ")** ", levels_old)
  out <- factor(x, levels = levels_old, labels = levels_new)
  out
}
