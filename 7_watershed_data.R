#================================================================================================================================#
# Filename: 7_watershed_data.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Load and prepare data on watershed environmental characteristics
#================================================================================================================================#

# Permafrost and peatland coverage
shed_env <- read_excel(file.path(root_path, "Watershed", "watershed_pf_peat_coverage.xlsx"), sheet="watershed_stats_for_R") %>%
  mutate(
    river = factor(river, levels=rivs),
    pf_ex = factor(pf_ex, levels=c("c","d","i","s"))
    )

# Mean annual air temperature (MAAT) and total annual precipitation (TAP)
shed_clim <- read_excel(file.path(root_path, "Watershed", "watershed_maat_tap_2020_2024.xlsx")) %>%
  mutate(
    river = str_to_title(river),
    river = factor(river, levels=rivs)
  )

# Combine coverage and climate data
shed_stats <- shed_clim %>%
  left_join(
    shed_env %>%
      filter(pf_ex == "c") %>%
      # transmute() both creates pf_cont_frac and drops unwanted columns in one step
      transmute(
        river,
        pf_frac_all = pf_frac,
        pf_frac_cont = pf_ex_frac,
        peat_frac,
        slope
      ) %>%
      na.omit(),
    by = "river"
  ) %>%
  # Filter for relevant years by river group
  filter(
    (river %in% c("Ob", "Yenisey", "Lena", "Kolyma") & year >= 2020 & year < 2022) |
      (river %in% c("Yukon", "Mackenzie") & year >= 2020 & year <= 2024)
  ) %>%
  # Summarize climate data
  group_by(river)

shed_stats_smry <- shed_stats %>%
  summarise(
    maat_mean = mean(maat_c, na.rm = TRUE),
    maat_sd = sd(maat_c, na.rm = TRUE),
    tap_mean = mean(tap_mm, na.rm = TRUE),
    tap_sd = sd(tap_mm, na.rm = TRUE),
    pf_frac_all = round(first(pf_frac_all) * 100, 1),
    pf_frac_cont = round(first(pf_frac_cont) * 100, 3),
    peat_frac = round(first(peat_frac) * 100, 1),
    slope = round(first(slope), 1),
    .groups = "drop"   # ensure grouping is dropped
  )

# Compile data in table for export
shed_stats_fmt <- shed_stats_smry %>%
  mutate(
    maat_fmt = paste0(round(maat_mean, 1), " (", round(maat_sd, 1), ")"),
    tap_fmt = paste0(round(tap_mean, 0), " (", round(tap_sd, 0), ")"),
    pf_frac_all = pf_frac_all,
    peat_frac = peat_frac,
    slope = slope
  ) %>%
  select(river, maat_fmt, tap_fmt, pf_frac_all, peat_frac, slope)

# Export
#write_xlsx(shed_stats_fmt, paste0(tables_export_dir,"shed_stats.xlsx"))
