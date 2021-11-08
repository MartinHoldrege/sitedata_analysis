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


Scripts in bold are needed to recreate the main analyses in the manuscript, using summarized data provided on Zenodo 
(which includes the output from the `01_summary_dfs_200sites.R` script). 
To run these scripts in bold, the following data files from the associated zenodo data repository should be put
in the `data-processed>site_means` folder: biomass_mean_v1.csv, dly_mean_by_lyr-all_v1.csv, dly_mean_by_lyr-PFT_v1.csv, 
sw2_dly_means_v1.csv, sw2_yr_means_v1.csv, yr_mean_by_lyr-all_v1.csv, yr_mean_by_lyr-PFT_v1.csv, sw2_mo_means_v1.csv. Alternatively, just
adjust the file paths to where ever you put those files. 

Contact me (martinholdrege at gmail dot com) if there are questions about this repository. 

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


