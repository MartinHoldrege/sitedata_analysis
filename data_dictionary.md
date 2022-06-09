# Data Dictionary

This documents describes data files in this folder (note these
data files are not on github because they are large).
The data files described are the ones that are needed to reproduce the main results
presented in the manuscript. For each file there is a brief description,
followed by a list of all the columns present in the table. 

All the data files described here that are in the `data-processed/site-means/` folder are files
created by the `01_summary_dfs_200sites.R` script, and are various site level summaries
of biomass and ecohydrological responses. That is, they are summarized values of
the 'raw' STEPWAT2 simulation output. 

Do no edit these data files, they are read in 'as is' in other R scripts.

## `data-processed/site-means/biomass_mean_v1.csv`:

Biomass data simulated in STEPWAT2. This data is simulated in the "STEPPE" 
submodule, where biomass of 10 functional types was simulated. This is the
best biomass data to use for analyses. (Other tables also include biomass data
but that is simulated in the SOILWAT2 submodule).
These are mean biomass values, for each site, plant functional type, and treatment
combination. Values are means, across years and iterations of the simulations.

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x 
increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `PFT`: Plant functional type (character), includes biomass of each plant
functional type that was simulated by STEPWAT2, as well as the individual species
(four letter codes of species given), but those lines with species codes  can be removed b/ only
one species was simulated for each of the 10 functional types, so the functional
type and species data are the same. 
* `biomass`: aboveground biomass (g/m^2, annual mean values). This is the biomass 
variable used in analyses.


## `data-processed/site-means/dly_mean_by_lyr-all_v1.csv`:

Mean of various ecohydrological variables for each site, treatment, soil layer,
and treatment combination. This includes total transpiration (from a given
layer), but not transpiration by each plant functional type. 


* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x 
increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `day`: day of year
* `layer`: soil layers (values from 1 to 8, corresponding to eight soil depths 
(0 - 10, 10 - 20, 20 - 30, 30 - 40, 40 - 60, 60 - 80, 80 - 100, 100 - 150 cm).
* `VWCMATRIC`: matric volumetric soilwater (cm / cm)
* `SWPMATRIC`: bulk volumetric soilwater (cm / cm)
* `TRANSP`: water extracted for transpiration (cm) ) (in this table, this
is total transpiration from the given soil layer across all plant functional
types).
* `EVAPSOIL`: bare-soil evaporation (cm)
* `WETDAY`: Number of days above -1.5 MPa (or if corresponds to day of year, then 
proportion of days that are above -1.5 MPa)


## `data-processed/site-means/sw2_dly_means_v1.csv`:


Mean of various ecohydrological variables for each site, treatment,
and treatment combination. Values are means for each day of the year. 
These are variables that do not pertain to specific soil layers.
Importantly this includes deep drainage 
(DEEPSWC_lowLayerDrain_cm_Mean), and evaporation from the surface (but
not soil evaporation which is found in the dly_mean_by_lyr-all_v1.csv file)
`BIOMASS_...` variables in this table were not used. 
This is based on data simulated in the SOILWAT2 submodule.

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `Day`: day of year
* `TEMP_max_C_Mean`: Daily max air temperature (mean) ( C )
* `TEMP_min_C_Mean`: Daily min air temperature (mean) ( C )
* `TEMP_avg_C_Mean`: Daily average air temperature (mean) ( C )
* `TEMP_surfaceTemp_C_Mean`: surface temperature ( C )
* `PRECIP_ppt_Mean`: Total  precipitation (cm)
* `PRECIP_rain_Mean`: Rainfall (cm)
* `PRECIP_snow_fall_Mean`: snowfall (cm)
* `PRECIP_snowmelt_Mean`: snowmel (cm)
* `PRECIP_snowloss_Mean`: snowloss (cm)
* `SOILINFILT_soil_inf_Mean`: soil infiltration of water (cm)
* `RUNOFF_net_Mean`: net runoff of water (cm)
* `RUNOFF_ponded_runoff_Mean`: NA
* `RUNOFF_snowmelt_runoff_Mean`: runoff of snowmelt water (cm)
* `RUNOFF_ponded_runon_Mean`: NA
* `SURFACEWATER_surfaceWater_cm_Mean`: surface water (cm)
* `EVAPSURFACE_evap_total_Mean`: total surface evaporation (cm)
* `EVAPSURFACE_evap_shrub_Mean`: evaporation from shrubs (cm)
* `EVAPSURFACE_evap_forbs_Mean`: evaporation from forbs (cm)
* `EVAPSURFACE_evap_grass_Mean`: evaporation from grasses (cm)
* `EVAPSURFACE_evap_litter_Mean`: evaporation from litter (cm)
* `EVAPSURFACE_evap_surfaceWater_Mean`: evaporation from surface water (cm)
* `INTERCEPTION_int_total_Mean`: total intercepted rain (cm)
* `INTERCEPTION_int_shrub_Mean`: rain intercepted by shrubs (cm)
* `INTERCEPTION_int_forbs_Mean`: rain intercepted by forbs (cm)
* `INTERCEPTION_int_grass_Mean`: rain intercepted by grasses (cm)
* `INTERCEPTION_int_litter_Mean`: rain inercepted by litter (cm)
* `AET_evapotr_cm_Mean`: actual evapotranspiration (cm)
* `PET_pet_cm_Mean`: potential evapotranspiration (cm)
* `SNOWPACK_snowpackWaterEquivalent_cm_Mean`: amount of water in snowpack (cm)
* `SNOWPACK_snowdepth_cm_Mean`: depth of snow (cm)
* `DEEPSWC_lowLayerDrain_cm_Mean`: deep drainage of water (cm) = diffuse recharge (cm)
* `BIOMASS_fCover_BareGround_Mean`: % bareground
* `BIOMASS_fCover_shrub_Mean`: % cover of shrubs
* `BIOMASS_fCover_forbs_Mean`: % cover of forbs
* `BIOMASS_fCover_grass_Mean`: % cover of grasses
* `BIOMASS_Biomass_total_Mean`: total biomass (g /m^2)
* `BIOMASS_Biomass_shrub_Mean`: shrub biomass (g/m^2)
* `BIOMASS_Biomass_forbs_Mean`: forb biomass (g/m^2)
* `BIOMASS_Biomass_grass_Mean`: grass biomass (g/m^2)
* `BIOMASS_Biomass_litter_Mean`: mass of litter (g/m^2)
* `BIOMASS_Biolive_total_Mean`: NA
* `BIOMASS_Biolive_shrub_Mean`: shrub biomass as component of total
* `BIOMASS_Biolive_forbs_Mean`: forbbiomass as component of total
* `BIOMASS_Biolive_grass_Mean`: grass biomass as component of total


## `data-processed/site-means/sw2_mo_means_v1.csv`:

Means of ecohydrological variables, for each month of the year, site, and 
treatment. These are not variables that pertain to specific soil layers. 
This file was used to confirm that treatments did not affect seasonality 
(they did not), but was not used in further analyses. 
This is based on data simulated in the SOILWAT2 submodule.

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `TEMP_max_C_Mean`: Daily max air temperature (mean) ( C )
* `TEMP_min_C_Mean`: Daily min air temperature (mean) ( C )
* `TEMP_avg_C_Mean`: Daily average air temperature (mean) ( C )
* `TEMP_surfaceTemp_C_Mean`: surface temperature ( C )
* `PRECIP_ppt_Mean`: Total  precipitation (cm)
* `PRECIP_rain_Mean`: Rainfall (cm)
* `PRECIP_snow_fall_Mean`: snowfall (cm)
* `PRECIP_snowmelt_Mean`: snowmel (cm)
* `PRECIP_snowloss_Mean`: snowloss (cm)
* `SOILINFILT_soil_inf_Mean`: soil infiltration of water (cm)
* `RUNOFF_net_Mean`: net runoff of water (cm)
* `RUNOFF_ponded_runoff_Mean`: NA
* `RUNOFF_snowmelt_runoff_Mean`: runoff of snowmelt water (cm)
* `RUNOFF_ponded_runon_Mean`: NA
* `SURFACEWATER_surfaceWater_cm_Mean`: surface water (cm)
* `EVAPSURFACE_evap_total_Mean`: total surface evaporation (cm)
* `EVAPSURFACE_evap_shrub_Mean`: evaporation from shrubs (cm)
* `EVAPSURFACE_evap_forbs_Mean`: evaporation from forbs (cm)
* `EVAPSURFACE_evap_grass_Mean`: evaporation from grasses (cm)
* `EVAPSURFACE_evap_litter_Mean`: evaporation from litter (cm)
* `EVAPSURFACE_evap_surfaceWater_Mean`: evaporation from surface water (cm)
* `INTERCEPTION_int_total_Mean`: total intercepted rain (cm)
* `INTERCEPTION_int_shrub_Mean`: rain intercepted by shrubs (cm)
* `INTERCEPTION_int_forbs_Mean`: rain intercepted by forbs (cm)
* `INTERCEPTION_int_grass_Mean`: rain intercepted by grasses (cm)
* `INTERCEPTION_int_litter_Mean`: rain inercepted by litter (cm)
* `AET_evapotr_cm_Mean`: actual evapotranspiration (cm)
* `PET_pet_cm_Mean`: potential evapotranspiration (cm)
* `SNOWPACK_snowpackWaterEquivalent_cm_Mean`: amount of water in snowpack (cm)
* `SNOWPACK_snowdepth_cm_Mean`: depth of snow (cm)
* `DEEPSWC_lowLayerDrain_cm_Mean`: deep drainage of water (cm) = diffuse recharge (cm)
* `BIOMASS_fCover_BareGround_Mean`: % bareground
* `BIOMASS_fCover_shrub_Mean`: % cover of shrubs
* `BIOMASS_fCover_forbs_Mean`: % cover of forbs
* `BIOMASS_fCover_grass_Mean`: % cover of grasses
* `BIOMASS_Biomass_total_Mean`: total biomass (g /m^2)
* `BIOMASS_Biomass_shrub_Mean`: shrub biomass (g/m^2)
* `BIOMASS_Biomass_forbs_Mean`: forb biomass (g/m^2)
* `BIOMASS_Biomass_grass_Mean`: grass biomass (g/m^2)
* `BIOMASS_Biomass_litter_Mean`: mass of litter (g/m^2)
* `BIOMASS_Biolive_total_Mean`: NA
* `BIOMASS_Biolive_shrub_Mean`: shrub biomass as component of total
* `BIOMASS_Biolive_forbs_Mean`: forbbiomass as component of total
* `BIOMASS_Biolive_grass_Mean`: grass biomass as component of total
* `Month`: Month (1-12)
* `PRECIP_ppt_SD`: interannual precipitation variability (standard deviation)
* `CO2EFFECTS_BioMult_shrub_Mean`: C02 effect multiplier for shrubs
* `CO2EFFECTS_BioMult_forbs_Mean`: C02 effect multiplier for forbs
* `CO2EFFECTS_BioMult_grass_Mean`: C02 effect multiplier for grasses
* `CO2EFFECTS_WUEMult_shrub_Mean`: water use efficiency C02 effect multiplier for shrubs
* `CO2EFFECTS_WUEMult_forbs_Mean`: water use efficiency C02 effect multiplier for forbs
* `CO2EFFECTS_WUEMult_grass_Mean`: water use efficiency C02 effect multiplier for grasses


## `data-processed/site-means/sw2_yr_means_v1.csv`:

Means of ecohydrological variables for each site, and 
treatment. These are all means of yearly data. 
These are not variables that pertain to specific soil layers. 
This is based on data simulated in the SOILWAT2 submodule.

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `TEMP_max_C_Mean`: Daily max air temperature (mean) ( C )
* `TEMP_min_C_Mean`: Daily min air temperature (mean) ( C )
* `TEMP_avg_C_Mean`: Daily average air temperature (mean) ( C )
* `TEMP_surfaceTemp_C_Mean`: surface temperature ( C )
* `PRECIP_ppt_Mean`: Total  precipitation (cm)
* `PRECIP_rain_Mean`: Rainfall (cm)
* `PRECIP_snow_fall_Mean`: snowfall (cm)
* `PRECIP_snowmelt_Mean`: snowmel (cm)
* `PRECIP_snowloss_Mean`: snowloss (cm)
* `SOILINFILT_soil_inf_Mean`: soil infiltration of water (cm)
* `RUNOFF_net_Mean`: net runoff of water (cm)
* `RUNOFF_ponded_runoff_Mean`: NA
* `RUNOFF_snowmelt_runoff_Mean`: runoff of snowmelt water (cm)
* `RUNOFF_ponded_runon_Mean`: NA
* `SURFACEWATER_surfaceWater_cm_Mean`: surface water (cm)
* `EVAPSURFACE_evap_total_Mean`: total surface evaporation (cm)
* `EVAPSURFACE_evap_shrub_Mean`: evaporation from shrubs (cm)
* `EVAPSURFACE_evap_forbs_Mean`: evaporation from forbs (cm)
* `EVAPSURFACE_evap_grass_Mean`: evaporation from grasses (cm)
* `EVAPSURFACE_evap_litter_Mean`: evaporation from litter (cm)
* `EVAPSURFACE_evap_surfaceWater_Mean`: evaporation from surface water (cm)
* `INTERCEPTION_int_total_Mean`: total intercepted rain (cm)
* `INTERCEPTION_int_shrub_Mean`: rain intercepted by shrubs (cm)
* `INTERCEPTION_int_forbs_Mean`: rain intercepted by forbs (cm)
* `INTERCEPTION_int_grass_Mean`: rain intercepted by grasses (cm)
* `INTERCEPTION_int_litter_Mean`: rain inercepted by litter (cm)
* `AET_evapotr_cm_Mean`: actual evapotranspiration (cm)
* `PET_pet_cm_Mean`: potential evapotranspiration (cm)
* `SNOWPACK_snowpackWaterEquivalent_cm_Mean`: amount of water in snowpack (cm)
* `SNOWPACK_snowdepth_cm_Mean`: depth of snow (cm)
* `DEEPSWC_lowLayerDrain_cm_Mean`: deep drainage of water (cm) = diffuse recharge (cm)
* `BIOMASS_fCover_BareGround_Mean`: % bareground
* `BIOMASS_fCover_shrub_Mean`: % cover of shrubs
* `BIOMASS_fCover_forbs_Mean`: % cover of forbs
* `BIOMASS_fCover_grass_Mean`: % cover of grasses
* `BIOMASS_Biomass_total_Mean`: total biomass (g /m^2)
* `BIOMASS_Biomass_shrub_Mean`: shrub biomass (g/m^2)
* `BIOMASS_Biomass_forbs_Mean`: forb biomass (g/m^2)
* `BIOMASS_Biomass_grass_Mean`: grass biomass (g/m^2)
* `BIOMASS_Biomass_litter_Mean`: mass of litter (g/m^2)
* `BIOMASS_Biolive_total_Mean`: NA
* `BIOMASS_Biolive_shrub_Mean`: shrub biomass as component of total
* `BIOMASS_Biolive_forbs_Mean`: forbbiomass as component of total
* `BIOMASS_Biolive_grass_Mean`: grass biomass as component of total
* `PRECIP_ppt_SD`: interannual precipitation variability (standard deviation)
* `CO2EFFECTS_BioMult_shrub_Mean`: C02 effect multiplier for shrubs
* `CO2EFFECTS_BioMult_forbs_Mean`: C02 effect multiplier for forbs
* `CO2EFFECTS_BioMult_grass_Mean`: C02 effect multiplier for grasses
* `CO2EFFECTS_WUEMult_shrub_Mean`: water use efficiency C02 effect multiplier for shrubs
* `CO2EFFECTS_WUEMult_forbs_Mean`: water use efficiency C02 effect multiplier for forbs
* `CO2EFFECTS_WUEMult_grass_Mean`: water use efficiency C02 effect multiplier for grasses
* `TEMP_max_C_SD`: Air temperature (maximum), standard deviation
* `TEMP_min_C_SD`: Air temperature (minimum), standard deviation.
* `EVAPSURFACE_evap_tree_Mean`: evaporation from trees (cm)
* `INTERCEPTION_int_tree_Mean`: rain intercepted by trees (cm)
* `PET_H_oh_MJm.2_Mean`: extraterrestrial horizontal solar irradiation [MJ/m2]
* `PET_H_ot_MJm.2_Mean`: -extraterrestrial tilted solar irradiation [MJ/m2
* `PET_H_gh_MJm.2_Mean`: global horizontal irradiation [MJ/m2]
* `PET_H_gt_MJm.2_Mean`: global tilted irradiation [MJ/m2]
* `CO2EFFECTS_BioMult_tree_Mean`: C02 effect multiplier for trees
* `CO2EFFECTS_WUEMult_tree_Mean`: NA
* `BIOMASS_fCover_tree_Mean`: % tree cover
* `BIOMASS_Biomass_tree_Mean`: tree biomass (g /^2)
* `BIOMASS_Biolive_tree_Mean`: tree biomass as component of total.


## `data-processed/site-means/yr_mean_by_lyr-all_v1.csv`:

Mean of various ecohydrological variables for each site, treatment, soil layer,
and treatment combination. This includes total transpiration (from a given
layer), but not transpiration by each plant functional type (i.e. this table
doesn't include any values specific to a given functional type).

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `layer`: soil layers (values from 1 to 8, corresponding to eight soil depths (0 - 10, 10 - 20, 20 - 30, 30 - 40, 40 - 60, 60 - 80, 80 - 100, 100 - 150 cm),
* `VWCMATRIC`: matric volumetric soilwater (cm / cm)
* `SWPMATRIC`: bulk volumetric soilwater (cm / cm)
* `TRANSP`: water extracted for transpiration (cm) (can refer to total tranpiration, or transpiration of shrubs, grasses or forbs).
* `EVAPSOIL`: bare-soil evaporation (cm)
* `WETDAY`: Number of days above -1.5 MPa (or if corresponds to day of year, then proportion of days that are above -1.5 MPa)
* `VWCBULK`: bulk volumetric soilwater (cm / cm)
* `SWCBULK`: bulk soil water content (cm)
* `SWABULK`: plant available soil water (cm)
* `SWAMATRIC`: plant available soil water (cm)
* `LYRDRAIN`: amount of water draining to to the next soil layer (cm)
* `HYDRED`: hydraulic redistribution from each layer (cm)
* `SOILTEMP`: soil temperature


## `data-processed/site-means/yr_mean_by_lyr-PFT_v1.csv`:

Means ecohydrological variables for each site, treatment, soil layer, plant
functional type and treatment combination. 
These are means of yearly variables. Importantly, this table includes the amount
of water transpired from each soil depth by each plant functional type. 

* `site`: unique site code (numeric). Separate model simulations run for each site.
* `intensity`: precipitation intensity treatment (control or 1.25x, 1.5x, or 2x increases in mean precipitation event size. Total precipitation remains unchanged).
* `warm`: warming treatment (control, or 3 or 5 C warming)
* `SoilTreatment`: Soil type treatment  (loam, silt, sand, or clay--see manuscript for details).
* `PFT`: Plant functional type (character)
* `layer`: soil layers (values from 1 to 8, corresponding to eight soil depths (0 - 10, 10 - 20, 20 - 30, 30 - 40, 40 - 60, 60 - 80, 80 - 100, 100 - 150 cm),
* `TRANSP`: water extracted for transpiration (cm) (can refer to total tranpiration, or transpiration of shrubs, grasses or forbs).
* `HYDRED`: hydraulic redistribution from each layer (cm)
* `SWA`: plant available soil water (cm)

## `data-raw/site_locations.csv`:

Locations for which simulations were run. And some other information about
the sites.

* `site_id`: unique site code (numeric), corresponds with the `site` column
in other tables.
* `X_WGS84`: longitude
* `Y_WGS84`: latitude
* `ELEV_m`: eleveation (m)
* `MAT`: mean annual temperature (C)
* `MAP`: mean annual precipitation (cm)
* `dailyweather_source`: source (gridded data product), from which the daily
weather data (1981-2010) for the site was pulled.
