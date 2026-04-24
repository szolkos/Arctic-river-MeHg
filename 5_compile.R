#================================================================================================================================#
# Filename: 5_compile.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: (1) Build working dataframe for data analyses; (2) Compile concentration and flux data
#================================================================================================================================#

# Source scripts
source(file.path(root_path, "..", "1_pkgs_and_funcs.R")) # Libraries, definitions of variables, & custom functions
source(file.path(root_path, "..", "2_discharge_data.R"))
source(file.path(root_path, "..", "3_hg_data.R"))
source(file.path(root_path, "..", "4_arcticgro_chem_data.R")) # DOC, NO3, TSS data

# Merge Hg and discharge data
hg_q_df <- merge(x=q_dat, y=hg_dat %>% select(river, date, hgt, mehg), by=c("river", "date"), all.x=T) %>%
  mutate(
    date = as.Date(date),
    river = factor(river, levels=rivs)
  ) %>%
  select(river, date, q_cms, hgt, mehg)

# Merge Hg and DOC/TSS data
hg_df <- merge(x=hg_q_df, y=ag_chem, by=c("river", "date"), all.x=T) %>%
  mutate(hgt = coalesce(hgt.x, hgt.y)) %>%
  select(-hgt.x, -hgt.y) %>%
  #mutate(
  #  river = factor(river, levels=rivs),
  #  date = as.Date(date),
  #  year = format(date, "%Y") %>% as.numeric()
  #) %>%
  filter(year(date) >= 2012) %>%
  mutate(
    lab = case_when(
    year(date) >= 2012 & year(date) <= 2018 ~ "usgs",
    year(date) >= 2020 ~ "bgc",
    TRUE ~ NA_character_ # Assign NA for other years
    ),
    # Set lab factor levels
    lab = factor(lab, levels=c("usgs","bgc"))
    ) %>%
  # Add Yukon 2024 TSS data
  left_join(yuma_2024_tss, by = c("river", "date")) %>%
  mutate(tss = coalesce(tss.y, tss.x)) %>%  # Prioritize new TSS values
  select(-tss.x, -tss.y) %>%  # Remove duplicate columns
  # Add seasons to dataframe, following season definitions in Holmes et al. 2012
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(11:12, 1:4) ~ "win",  # Winter
      month %in% 5:6 ~ "spr",   # Spring
      month %in% 7:10 ~ "sum"    # Summer
    ),
    season = factor(season, levels = c("win", "spr", "sum"))
  ) %>%
  select(-month) %>%
  mutate(
    #q_cms = q_cms, # Rename discharge to reflect units (m³/s)
    year = year(date), # Add year as variable
    mehgpcnt = mehg/hgt*100 # Calculate percent MeHg
    ) %>%
  # Select relevant columns
  select(river, date, year, season, q_cms, hgt, mehg, mehgpcnt, tss, doc, lab)

rm(hg_q_df)