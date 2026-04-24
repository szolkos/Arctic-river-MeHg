# Arctic Great Rivers - Methylmercury Concentrations and Fluxes
## Introduction
Source code for the manuscript: Methylmercury Export from Arctic Great Rivers: Magnitude, Controls, and Implications for Arctic Budgets. Supporting data can be downloaded from [Zolkos et al. 2026](https://doi.org/).

## Authors
- [Scott Zolkos](https://www.researchgate.net/profile/Scott-Zolkos)
- Colin P. Thackray
- Prentiss H. Balcom
- Suzanne E. Tank
- James W. McClelland
- Robert G.M. Spencer
- Anya Suslova
- Alexander Shiklomanov
- Edda A. Mutter
- Alexander V. Zhulidov
- Tatiana Gurtovaya
- Nikita Zimov
- Sergey Zimov
- Edwin Amos
- David P. Krabbenhoft
- Robert M. Holmes 
- Elsie M. Sunderland

## Data
### The following data associated with this manuscript can be downloaded from [Zolkos et al. 2026](https://doi.org/):
#### Data used in generation of results, including Load Estimator (LOADEST) modeling
- *Ob_Salekhard_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Ob' river.  

- *Yenisei_Igarka_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Yenisey river.  

- *Lena_Kyusyur_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Lena river.  

- *Kolyma_Kolymskoe_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Kolyma river.  

- *Yukon_PilotStation_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Yukon river.  

- *Mackenzie_ArcticRedRiver_Version_20251208.xlsx* (called in script '2_discharge_data.R'): Discharge data for Mackenzie river.  

- *Hg_2020_2024_QCd.xlsx* (called in script '3_hg_data.R'): Methyl and total mercury data.  

- *ArcticGRO-water-quality.xlsx* (called in script '4_arcticgro_chem_data.R'): Ancillary water chemistry data measured by ArcticGRO.  

- *watershed_maat_tap_sd_2020_2024.xlsx* (called in script '7_watershed_data.R'): Watershed mean annual air temperature (MAAT) and total annual precipitation (TAP) and uncertainty (SD) for each.  

- *watershed_pf_peat_coverage.xlsx* (called in script '7_watershed_data.R'): Permafrost distribution in each watershed.

- *AO shelf regional NPP.xlsx* (called in script '9_hg_npp_timing.R'): Mean monthly Arctic Ocean shelf region net primary production (NPP). See reference in manuscript for details. 

## Scripts
#### *Note: These scripts process the data summarized above, which was compiled as detailed in the Methods of the manuscript. Users of these scripts will need to update the working directory pathway within scripts.*  
- *1_pkgs_and_funcs.R*: Prepare working environment.  

- *2_discharge_data.R*: Load and prepare discharge data.  

- *3_hg_data.R*: Load and prepare mercury concentration data.  

- *4_arcticgro_chem_data.R*: Load and prepare ArcticGRO chemistry data.  

- *5_compile.R*: 1) Build working dataframe for data analyses; (2) Compile concentration and flux data.  

- *6_flux_data.R*: Load and prepare constituent flux data.

- *6b_flux_stats.R*: Calculate seasonal and annual constituent concentrations and fluxes from LOADEST data on MeHg, HgT, TSS, DOC.  

- *7_watershed_data.R*: Load and prepare data on watershed environmental characteristics.  

- *8_stats_rda.R*: Redundancy analysis for multivariate assessment of MeHg export patterns and drivers.  

- *9_hg_npp_timing.R*: Compiles monthly Arctic riverine Hg export and NPP in shelf regions, to assess the timing and implications of Hg export / methylation for coastal ecosystem productivity.  

- *10_plots.R*: Figures in manuscript.  

- *.js*: Google Earth Engine script for deriving environmental conditions from satellite remote sensing data and extracting the environmental data for use in development of predictive models of burn depth.  
