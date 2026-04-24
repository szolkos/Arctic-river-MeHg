#================================================================================================================================#
# Filename: 3_hg_data.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Load and prepare mercury concentration data
#================================================================================================================================#

# HgT, 2020-2024 (ng/L)
hgt_2020_2024 <- read_excel(file.path(root_path, "Chemistry", "Hg_2020_2024_QCd.xlsx"), sheet="HgT") %>%
  select(River, Date, Conc) %>%
  rename(
    river = River,
    date = Date,
    hgt = Conc
  ) %>%
  mutate(date = as.Date(date)) # Convert "date" to date

# MeHg, 2020-2024 (ng/L)
mehg_2020_2024 <- read_excel(file.path(root_path, "Chemistry", "Hg_2020_2024_QCd.xlsx"), sheet="MeHgT") %>%
  select(River, Date, Conc) %>%
  rename(
    river = River,
    date = Date,
    mehgt = Conc
  ) %>%
  mutate(
    date = as.Date(date), # Convert "date" to date
    mehg = mehgt
    ) %>%
  select(!mehgt)

# Merge
hg_dat <- merge(x=hgt_2020_2024, y=mehg_2020_2024, by=c("river", "date"), all.x=T) %>%
  mutate(
    river = str_replace(river, "Ob'", "Ob"),
    river = factor(river, levels=rivs)
  )

rm(hgt_2020_2024)
rm(mehg_2020_2024)