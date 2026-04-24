#================================================================================================================================#
# Filename: 4_arcticgro_chem_data.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Load and prepare ArcticGRO chemistry data
#================================================================================================================================#

# Compile ArcticGRO chemistry data
ag_chem <- read_excel(file.path(root_path, "Chemistry", "ArcticGRO-water-quality.xlsx")) %>%
  filter(parameter %in% c("TDN", "NH4", "NO3",
                          "DOC", "A254",
                          "Alkalinity", "Ca", "Mg", "Na", "K", "SO4", "Cl",
                          "TSS", "POC", "PN",
                          "UTHg",
                          "H2O-18O", "H2O-2H")) %>%
  filter(!(parameter %in% c("TSS","POC","PN") & depth != 0)) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  group_by(project, river, date) %>%
  summarise(across(c(TDN, NH4, NO3,
                     DOC, A254,
                     Alkalinity, Ca, Mg, Na, K, SO4, Cl,
                     TSS, POC, PN,
                     UTHg,
                     `H2O-18O`, `H2O-2H`),
                   ~ ifelse(all(is.na(.)), NA, mean(.x, na.rm = TRUE)),
                   .names = "mean_{.col}"),
            .groups = "drop") %>%
  rename(
    tdn = mean_TDN,
    nh4 = mean_NH4,
    no3 = mean_NO3,
    doc = mean_DOC,
    a254 = mean_A254,
    alk = mean_Alkalinity,
    ca = mean_Ca,
    mg = mean_Mg,
    na = mean_Na,
    k = mean_K,
    so4 = mean_SO4,
    cl = mean_Cl,
    tss = mean_TSS,
    poc = mean_POC,
    pn = mean_PN,
    hgt = mean_UTHg,
    d18o = `mean_H2O-18O`,
    d2h  = `mean_H2O-2H`
  ) %>%
  mutate(
    date = as.Date(date),
    river = str_replace(river, "Ob'", "Ob"),
    river = factor(river, levels = rivs),
    suva254 = a254/doc*100, # Calculate SUVA254 (L/mgC/m),
    dex = d2h-(8*d18o)
  ) %>%
  select(river, date, everything()) %>% 
  select(-project) %>% #, -a254
  filter(between(date, as.Date("2012-01-01"), as.Date("2024-12-31"))) %>%
  as.data.frame()
