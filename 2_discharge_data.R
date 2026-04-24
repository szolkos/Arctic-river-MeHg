#================================================================================================================================#
# Filename: 2_discharge_data.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Load and prepare discharge data
# References
## Holmes et al. 2012 Est. & Coasts
#================================================================================================================================#

# Load and prepare discharge data, applying correction for spatiotemporal offset in concentration and Q data (Holmes et al. 2012 p5-6)
q_dat <- list.files(path = paste0(root_path, "/Discharge"), pattern = "\\.xlsx$", full.names = TRUE) %>% # List all Excel files in root directory
  keep(grepl("Ob_Salekhard|Yenisei_Igarka|Lena_Kyusyur|Kolyma_Kolymskoe|Yukon_PilotStation|Mackenzie_ArcticRedRiver", .)) %>%
  map_df(~ read_excel(.)) %>%
  mutate(
    date = as.Date(date),
    discharge = as.numeric(discharge),
    river = factor(river, levels=rivs),
    agency = factor(agency),
    station_name = factor(station_name)
    ) %>%
  rename(q_cms = discharge) %>%
  # Offset correction: index chemistry data to the date of Q measurement (e.g., add 4 days to date of Lena chemistry mmnts)
  mutate(
    date = case_when(
      river == "Yenisey" ~ date - 2, # Yenisey (-2 days)
      river == "Lena" ~ date + 4, # Lena (+4 days)
      river == "Kolyma" ~ date - 1, # Kolyma (-1 day)
      TRUE ~ date
      )
  ) %>%
  select(river, date, q_cms)