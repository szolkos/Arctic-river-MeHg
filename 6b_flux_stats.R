#================================================================================================================================#
# Filename: 6b_flux_stats.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Calculate seasonal and annual constituent concentrations and fluxes from LOADEST data on MeHg, HgT, TSS, DOC
#================================================================================================================================#

# Depends: functions: 'loadest_seasonal_data'

#-----------------#
# Source files ####
#-----------------#
source(file.path(root_path, "..", "6_flux_data.R"))

#---------------------------------------#
# Annual constituent conc and fluxes ####
#---------------------------------------#
# Define parameter & column name for flux variable
param_hg <- "MeHg" # HgT or MeHg
flux_col <- sym(paste0(tolower(param_hg), "_flux"))

# Mean (of mean) annual fluxes for each river
annl <- daily_flux %>%
  group_by(river, year) %>%
  # Mean annual fluxes for each river
  summarise(
    annual_flux = sum(!!flux_col, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Mean of mean
  group_by(river) %>%
  summarise(
    mean_annual_flux = mean(annual_flux, na.rm = TRUE),
    sd_annual_flux = sd(annual_flux, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(river = factor(river, levels = rivs)) %>%
  arrange(river)

# Format annual fluxes
annl_fmt <- annl %>%
  mutate(
    flux_fmt = paste0(round(mean_annual_flux, 1), " (", round(sd_annual_flux, 1), ")"),
    season = "Annual"
  ) %>%
  select(river, season, flux_fmt, mean_flux = mean_annual_flux, sd_flux = sd_annual_flux)

#-----------------------------------------#
# Seasonal constituent conc and fluxes ####
#-----------------------------------------#
# Compile seasonal data for all rivers
ssnl <- map_dfr(rivs, ~ loadest_seasonal_data(.x, param_hg)) %>%
  # Summarise by river × season
  group_by(river, param, season) %>%
  summarise(
    mean_q_km3 = mean(q_km3, na.rm = TRUE),
    sd_q_km3 = sd(q_km3, na.rm = TRUE),
    mean_conc = mean(conc, na.rm = TRUE),
    sd_conc = sd(conc, na.rm = TRUE),
    mean_flux = mean(flux, na.rm = TRUE),
    sd_flux = sd(flux, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(
    river = factor(river, levels = rivs),
    season = factor(season, levels = c("win", "spr", "smr"))
  ) %>%
  arrange(river, season)

# Format seasonal fluxes
ssnl_fmt <- ssnl %>%
  mutate(
    flux_fmt = paste0(round(mean_flux, 1), " (", round(sd_flux, 2), ")"),
    season = recode(season, win = "Winter", spr = "Spring", smr = "Summer")
  ) %>%
  select(river, season, flux_fmt, mean_flux, sd_flux) # keep numeric for totals

#--------------------------------------------------------------------#
# Combine annual & seasons constituent conc and fluxes for export ####
#--------------------------------------------------------------------#
# Combine seasonal + annual for individual rivers
annl_ssnl <- bind_rows(annl_fmt, ssnl_fmt) %>%
  mutate(
    cv_flux = sd_flux / mean_flux
  ) %>%
  arrange(river, factor(season, levels = c("Annual", "Spring", "Summer", "Winter")))

if(param_hg == "MeHg"){
  write_xlsx(annl_ssnl, paste0(tables_export_dir, "/", "Table 1-annual seasonal MeHg summary.xlsx"))
} else if(param_hg == "HgT"){
  write_xlsx(annl_ssnl, paste0(tables_export_dir, "/", "Table S1-annual seasonal HgT summary.xlsx"))
}

#-------------------------------------------------------------#
# Seasonal totals across rivers and upscaled to pan-Arctic ####
#-------------------------------------------------------------#
# Seasonal totals across rivers
seasonal_totals <- annl_ssnl %>%
  group_by(season) %>%
  summarise(
    river = "ArcticGRO (all rivers)",
    total_mean_flux = sum(mean_flux, na.rm = TRUE),
    total_sd_flux   = sqrt(sum(sd_flux^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    flux_fmt = paste0(round(total_mean_flux, 2), " (", round(total_sd_flux, 2), ")")
  ) %>%
  select(river, season, flux_fmt)

# Upscale summed totals to pan-Arctic watershed
upscaled_totals <- seasonal_totals %>%
  # Extract numeric values from flux_fmt (optional) or use stored numeric columns if available
  mutate(
    # First, parse flux_fmt into numeric mean and SD
    mean_flux = as.numeric(sub(" .*", "", flux_fmt)), # extract mean
    sd_flux   = as.numeric(gsub(".*\\((.*)\\)", "\\1", flux_fmt)), # extract SD
    # Scale by 0.53 (ArcticGRO rivers cover 53% of pan-Arctic watershed including Hudson Bay; Holmes et al. 2012 Est. & Coasts, Fig. 1)
    mean_flux_scaled = mean_flux / 0.53,
    sd_flux_scaled   = sd_flux / 0.53,
    # Reformat as string
    flux_fmt = paste0(round(mean_flux_scaled, 2), " (", round(sd_flux_scaled, 2), ")"),
    river = "pan-Arctic (upscaled)"
  ) %>%
  select(river, season, flux_fmt)

annl_ssnl_agsheds_upscaled <- bind_rows(seasonal_totals, upscaled_totals) %>%
  arrange(factor(season, levels = c("Annual", "Spring", "Summer", "Winter"))) %>%
  arrange(river)

# Export
#write_xlsx(annl_ssnl_agsheds_upscaled, file.path(tables_export_dir, paste0(param_hg, " mean annual and seasonal fluxes.xlsx")))

#--------------------------#
# Annual yield by river ####
#--------------------------#
shed_area_km2 <- c(2.99, 2.40, 2.43, 0.53, 0.83, 1.68) * 1e6 # Watershed areas (km²) in same order as rivs
names(shed_area_km2) <- rivs
shed_df <- tibble(river = rivs, shed_area_km2 = shed_area_km2)
# Note pan-Arctic watershed w/out Hudson Bay = 16.8 Mkm2 (https://arctic.noaa.gov/report-card/report-card-2021/river-discharge/)

# Filter only Annual values and add watershed area
annual_yield <- annl_ssnl %>%
  filter(season == "Annual") %>%
  left_join(shed_df, by = "river") %>%
  mutate(
    mean_yield = if (param_hg %in% c("HgT", "MeHg")) {
      (mean_flux / shed_area_km2) * 1e6
    } else {
      mean_flux / shed_area_km2
    },
    sd_yield = if (param_hg %in% c("HgT", "MeHg")) {
      (sd_flux / shed_area_km2) * 1e6
    } else {
      sd_flux / shed_area_km2
    },
    yield_fmt = paste0(round(mean_yield, 2), " (", round(sd_yield, 2), ")")
  ) %>%
  select(river, season, yield_fmt, mean_yield, sd_yield)

# Output units: kg/Mkm2/y = mg/km2/y
#write_xlsx(annual_yield, file.path(tables_export_dir, paste0(param_hg, " mean annual and seasonal yields.xlsx")))