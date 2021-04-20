# Martin Holdrege

# script started April 19, 2021

# purpose is to to output the markov files
# for sites where errors occured.


# dependencies ------------------------------------------------------------

library(tidyverse)
library(rSOILWAT2)
# read in data ------------------------------------------------------------

txt1 <- read_lines("data-processed/site_means/logfile_compiled_2x_20210412.txt")


# extract problem rows ----------------------------------------------------

txt2 <- str_subset(txt1, "^$|Zero growing season precip", negate = TRUE)
txt2

prog_lines <- which(str_detect(txt2, "R_program"))

prob_lines <- prog_lines[-length(prog_lines)][which(diff(prog_lines) > 1)]

# sites with problems (this code won't include the last site if it
# has a problem)
prob_sites <- txt2[prob_lines]

df <- tibble(site = prob_sites)

# warnings for each
txt_l <- map(prob_lines, function(x) {
  start <- x + 1
  end <- min(prog_lines[prog_lines > x]) - 1
  txt2[start:end]
})

# counting number of warnings of each type
df$n_warn_res_avail <- map_dbl(txt_l, function(x) {
  sum(str_detect(x, "res_avail is Zero"))
})

df$n_warn_stat_collect<- map_dbl(txt_l, function(x) {
  sum(str_detect(x, "stat_Collect"))
})

# one site only had the cf error (which I have corrected)
out <- df %>%
  filter(n_warn_stat_collect != 0 | n_warn_res_avail != 0) %>%
  rename(program = site) %>%
  mutate(site = as.numeric(str_extract(program, "\\d+$")))


# create markov files -----------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)


coeff_l <- map(out$site, function(site) {
  wdata <- dbW_getWeatherData(Site_id = site)
  wdata_df <- wdata %>%
    dbW_weatherData_to_dataframe() %>%
    as.data.frame()
  # markov files
  coeffs_ambient <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                            imputation_type = "mean")

  # 2x markov files
  coeffs_2x <- precipr::adjust_coeffs(coeffs = coeffs_ambient,
                                      data = wdata_df,
                                      mean_mult = 2,
                                      adjust_sd = TRUE)
  coeffs_2x
})


# * create folders --------------------------------------------------------

# each mkv file needs to be in diff folder because cannot give them unique
# names
paths <- file.path("data-processed/logfile_issues", out$program)
walk(paths, dir.create)


# * save markov files -----------------------------------------------------

walk2(coeff_l, paths, function(x, path) {
  print_mkv_files(mkv_doy = x$mkv_doy,
                  mkv_woy = x$mkv_woy,
                  path = path)
})

# write csv -------------------------------------------------------------

write_csv(out, "data-processed/logfile_issues/logfile_n_warns_2x_20210412.csv")

