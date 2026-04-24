#================================================================================================================================#
# Filename: 6_flux_data.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Load and prepare constituent flux data
#================================================================================================================================#
#------------------------------------------------#
# Calculate daily constituent conc and fluxes ####
#------------------------------------------------#
# Depends: functions: 'loadest_daily_data'
# Define multiple constituents
params <- c("mehg", "hgt", "doc", "no3", "tss")

# Merge daily data for all rivers and parameters; Hg vars in kg/day, NO3 in Gg/day, others in Tg/day
daily_flux <- map_dfr(rivers, function(riv) {
  param_data <- map(params, ~loadest_daily_data(riv, .x))
  reduce(param_data, left_join, by = c("date", "river", "year", "q_km3d"))
}) %>% 
  select(date, river, year, q_km3d, 
         mehg_conc, mehg_flux, 
         hgt_conc, hgt_flux, 
         doc_conc, doc_flux,
         no3_conc, no3_flux, 
         tss_conc, tss_flux) %>%
  mutate(river = factor(river, rivs))

dim(daily_flux); head(daily_flux); summary(daily_flux)

#------------------------------------------------------#
# Calculate annual yields and runoff for each river ####
#------------------------------------------------------#
ylds <- daily_flux %>%
  group_by(river, year) %>%
  summarise(
    annual_q_km3 = sum(q_km3d, na.rm = TRUE),
    annual_mehg_kg = sum(mehg_flux, na.rm = TRUE),
    annual_hg_kg = sum(hgt_flux, na.rm = TRUE),
    annual_doc_tg = sum(doc_flux, na.rm = TRUE),
    annual_no3_gg = sum(no3_flux, na.rm = TRUE),
    annual_tss_tg = sum(tss_flux, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    shed_km2 = shed_area_km2[river],
    runoff_cm = annual_q_km3 / shed_km2 * 1e5, # cm/y
    mehg_yield_mgkm2 = annual_mehg_kg / shed_km2 * 1e6, # mg/km²/y
    hg_yield_mgkm2 = annual_hg_kg / shed_km2 * 1e6, #
    doc_yield_tgkm2 = annual_doc_tg / shed_km2 * 1e6, #
    no3_yield_ggkm2 = annual_no3_gg / shed_km2 * 1e6, #
    tss_yield_tgkm2 = annual_tss_tg / shed_km2 * 1e6 #
  ) %>%
  ungroup() %>%
  select(river, year, runoff_cm, mehg_yield_mgkm2, hg_yield_mgkm2, doc_yield_tgkm2, no3_yield_ggkm2, tss_yield_tgkm2)

dim(ylds); head(ylds); summary(ylds)