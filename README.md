# sitedata_analysis

Analysis of data from STEPWAT2 simulations done for study on the effects of increased precipitation intensity on sagebrush dominated ecosystems. 
Simulations conducted for 200 sites across the western U.S. 

# Description of important scripts.

R scripts are found in the `scripts` folder. Other folders are empty 
(i.e. because their contents are data, figures etc. which I don't track with git). 

"SM" stands for soil moisture

Note about script names, leading numbers denote the order in which scripts are run. (e.g. 01_script.R would be run before 02_script.R). 
Usually this means an earlier script is sourced by a later script, or the output from one script is used by the next script.
Exceptions always exist. All dependencies and files loaded, are at the top of scripts, so examine what upstream scripts are used (i.e. `source()`),
and what csv files are read in. 


Scripts in bold are needed to recreate the main analyses in the manuscript, using summarized data that are found in the
version of this repository found on zenodo (data files are not put on github.)
(Many of the data files are output from the `01_summary_dfs_200sites.R` script). 
To run these scripts in bold, the following data files should be
in the `data-processed>site_means` folder: biomass_mean_v1.csv, dly_mean_by_lyr-all_v1.csv, dly_mean_by_lyr-PFT_v1.csv, 
sw2_dly_means_v1.csv, sw2_yr_means_v1.csv, yr_mean_by_lyr-all_v1.csv, yr_mean_by_lyr-PFT_v1.csv, sw2_mo_means_v1.csv. Alternatively, just
adjust the file paths to where ever you put those files. 

The version of this repository hosted on zenodo contains the necessary data
to run the scripts that are in bold (which as mentioned above, are the ones
necessary to recreate the results (and figures) in the manuscript).

Contact me (martinholdrege at gmail dot com) if there are questions about this repository. 

On the version of this repository put on Zenodo, empty folders are kept, these
are the empty folders that get populated by the output the scripts. 

## Summarizing/analysis/etc. scripts

`01_soil_texture_extraction.R`--Purpose is to summarize soil texture, and select soil texture classes to run STEPWAT2 for. 
Also makes figure of soil texture triangle.
This sources publicly available data (kindly provided by J. Bradford), that I did not put on the Zenodo repository.

`01_summary_dfs_200sites.R`--this script takes the raw output (SQL databases) from STEPWAT2, and creates summary dataframes (output as csv's). 
The the raw data is large (>100 Gb) so are not put on Zenodo. But the summary files created by these scripts are on Zenodo. 
This scripts averages over years, and iterations for each plot. All downstream analysis of model output is based on the csv's created here. 
Summary files of monthly values (e.g. mean monthly transpiration) are also created by this scrpt, but files which are not used in subsequent
analyses aren't put on Zenodo. 

**`02_climate_200sites.R`**--extracts the average climate data for each site (based on files created in `01_summary_dfs_200sites.R`) script. Calculates aridity index. 
The climate csv created in this script is used in subsequent summarizing scripts. 

**`03_biomass_summarize.R`**--summarize biomass data. This includes summaries by site, plant functional type, soil type. Also calculates 
some summary statistics included in the manuscript. 

**`03_SM_daily_summariz.R`**--summarize daily soil moisture data. This is average data on soil moisture, transpiration, evaporation, etc. for each day of the year. 

**`03_SM_summarize.R`**--summary soil moisture data. This is data on a yearly time scale. Summary dataframes are averages across years for a site, for each treatment combination. 
Includes multiple ecohydrological variables, such as E, T, drainage, etc. 
Also calculates some summary statistics included in the manuscript. 

## Figure creation scripts

`04_biomass_figures.R`--comprehensive set of figures that explore the biomass response data. Sources the `03_biomass_summarize.R` script. 
These are not "publication quality". 

**`04_biomass_pub_figures.R`**--figures of biomass responses. More limited set of figures, but are higher (publication) quality. 
Some of these figures used in the final manuscript.

`04_biomass_vs_transp_figures.R`--creates exploritory figures examining the realationship between changes in biomass and transpiration. 

`04_SM_daily_figures.R`--figures that explore responses of daily (average for each day of year), soil moisture variables. 
Fairly comprehensive, but not publication quality.

`04_SM_figures.R` --figures of soil moisture data (figures of averages of yearly data). 
Provides figures of responses by depth, and each treatment combination, and
plant functional types. Not puplication quality. 

**`04_SM_pub_figures.R`**--more restricted set of figures of soil moisture (and other ecohydrological) responses. 
Higher quality, some of these included in manuscript. 
This script uses output dataframes generated in `03_SM_daily_summarize.R` and `03_SM_summarize.R`

## Misc utility scripts:

**`functions.R`**--sourced by other scripts, contains custom functions. 

**`fig_params.R`**--contains parameters for plotting, colors, axis labels etc. sourced by scripts that create figures. 

## Other misc.

`sagebrush_extent.R`--create raster of the approximate  extent of big sagebrush ecosystems
based on GAP landcover dataset. The raster created here is used in the `site_map.R`
script.

`site_map.R`--maps of the site locations (some of these maps are only reproducible with all underlying geographic data--not included here). 

`logfile_issues.R`--examination of some logfile warnings etc. from STEPWAT2 simulations. 

`root_profile.R`--comparison of how root profiles used in STEPWAT2 compare to ones estimated in a tracer study by A. Kulmatiski. (data
not included here--profiles look very similar). 


# Session info

On June 9, 2022 I successfully re-ran the main analysis scripts (i.e. those in bold),
with the following session info in R:

`sessionInfo()`
R version 4.1.3 (2022-03-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] dtplyr_1.2.1    forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
[10] tidyverse_1.3.1

loaded via a namespace (and not attached):
 [1] cellranger_1.1.0  pillar_1.7.0      compiler_4.1.3    dbplyr_2.1.1      tools_4.1.3       bit_4.0.4         jsonlite_1.8.0    lubridate_1.8.0  
 [9] lifecycle_1.0.1   gtable_0.3.0      pkgconfig_2.0.3   rlang_1.0.2       reprex_2.0.1      rstudioapi_0.13   DBI_1.1.2         cli_3.2.0        
[17] parallel_4.1.3    haven_2.4.3       xml2_1.3.3        withr_2.5.0       httr_1.4.2        fs_1.5.2          generics_0.1.2    vctrs_0.3.8      
[25] hms_1.1.1         bit64_4.0.5       grid_4.1.3        tidyselect_1.1.2  data.table_1.14.2 glue_1.6.2        R6_2.5.1          plotrix_3.8-2    
[33] fansi_1.0.3       readxl_1.4.0      vroom_1.5.7       tzdb_0.3.0        modelr_0.1.8      magrittr_2.0.2    backports_1.4.1   scales_1.1.1     
[41] ellipsis_0.3.2    rvest_1.0.2       assertthat_0.2.1  colorspace_2.0-3  utf8_1.2.2        stringi_1.7.6     munsell_0.5.0     broom_0.7.12     
[49] crayon_1.5.1     
