#================================================================================================================================#
# Filename: 9_hg_npp_timing.R ####
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: This code compiles monthly Arctic riverine Hg export and NPP in shelf regions, to assess the timing and 
## implications of Hg export / methylation for coastal ecosystem productivity.
# References
## Vonk et al. 2025: https://www.nature.com/articles/s43017-024-00627-w
#================================================================================================================================#

#----------------#
# Definitions ####
#----------------#
months_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
shelves <- c("Kara Sea", "Laptev Sea", "East Siberian Sea", "Bering Sea", "Beaufort Sea")
shelf_rivs <- c("Ob_Yenisey","Lena","Kolyma","Yukon","Mackenzie")
# Define your mapping of rivers to shelf seas
riv_shelves <- c(
  "Ob_Yenisey" = "Kara Sea",
  "Lena" = "Laptev Sea",
  "Kolyma" = "East Siberian Sea",
  "Yukon" = "Bering Sea",
  "Mackenzie" = "Beaufort Sea"
)

#-------------------------------#
# Monthly NPP in shelf areas ####
#-------------------------------#
# Shelf region areas (Vonk et al. 2025, Table 1)
shelf_df <- tibble(
  river = shelf_rivs,
  shelf_area = c(941, 504, 999, 1177, 184) # shelf area in 10^3 km2
)

# NPP (TgC/mo.) by shelf region, from remote sensing (Vonk et al. 2025, Supplementary Table 4)
npp <- read_excel(file.path(paste0(root_path, "/Ocean"), "AO shelf regional NPP.xlsx")) %>%
  # Step 0: Clean and factorize columns
  mutate(
    river = factor(river),
    shelf_area = factor(shelf_area),
    month = factor(month, levels = month.abb), # keep month as ordered factor for plotting
    river_group = ifelse(river %in% c("Ob", "Yenisey"), "Ob_Yenisey", as.character(river)),
    month_num = as.numeric(month) # numeric month for cumulative calculations
  ) %>%
  filter(river_group %in% shelf_rivs) %>%
  # Step 1: Aggregate monthly NPP per river (sum if multiple shelf areas)
  group_by(river_group, month_num) %>%
  summarise(
    npp_tgc_mo = sum(npp_TgC_remsens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 2: Compute cumulative fraction of annual NPP for each river
  group_by(river_group) %>%
  arrange(month_num) %>%
  mutate(
    annual_npp = sum(npp_tgc_mo, na.rm = TRUE),
    npp_monthly_frac = npp_tgc_mo / annual_npp,
    cum_frac_npp = cumsum(npp_tgc_mo) / annual_npp
  ) %>%
  ungroup() %>%
  # Step 3: Summarize across years (if you had multiple years; here mean = value itself)
  group_by(river_group, month = month_num) %>%
  summarise(
    npp_tgc_mo = npp_tgc_mo,
    npp_monthly_frac = npp_monthly_frac,
    npp_cum_frac = mean(cum_frac_npp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 4: Convert month back to factor for plotting
  mutate(
    river_group = factor(river_group),
    month = factor(month.abb[month], levels = month.abb)
  )

head(npp); summary(npp)

#-----------------------#
# Riverine Hg fluxes ####
#-----------------------#
# Compile mean ± SD of monthly riverine Hg fluxes
monthly_flux <- daily_flux %>%
  mutate(
    river = as.character(river),  # <— this prevents numeric coercion
    river_group = ifelse(river %in% c("Ob", "Yenisey"), "Ob_Yenisey", river),
    month = floor_date(date, "month"),
    mehg_flux = mehg_flux * 0.889, # Fraction MeHg not lost in delta (Emmerton et al. 2013 Table S1)
    hgt_flux = hgt_flux * 0.807 # Fraction HgT not lost in delta (Emmerton et al. 2013 Table S1)
  ) %>%
  group_by(river_group, year, month) %>%
  summarise(
    mehg_monthly_flux = sum(mehg_flux, na.rm = TRUE),
    hgt_monthly_flux = sum(hgt_flux, na.rm = TRUE),
    .groups = "drop"
  )

head(monthly_flux); summary(monthly_flux)

# Summarize across years: mean and SD of total and fractional monthly Hg fluxes for each river group
monthly_summary <- daily_flux %>%
  # Combine Ob + Yenisey into one group
  mutate(
    river_group = ifelse(river %in% c("Ob", "Yenisey"), "Ob_Yenisey", as.character(river)),
    month = month(date),  # numeric month for aggregation
    mehg_flux = mehg_flux * 0.889, # Fraction MeHg not lost in delta (Emmerton et al. 2013 Table S1)
    hgt_flux = hgt_flux * 0.807 # Fraction HgT not lost in delta (Emmerton et al. 2013 Table S1)
  ) %>%
  # Step 1: Calculate monthly flux per river-year
  group_by(river_group, year, month) %>%
  summarise(
    mehg_monthly_flux = sum(mehg_flux, na.rm = TRUE),
    hgt_monthly_flux = sum(hgt_flux, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  # Step 2: Compute cumulative fraction for each river-year
  group_by(river_group, year) %>%
  arrange(month) %>%
  mutate(
    mehg_annual_flux = sum(mehg_monthly_flux, na.rm = TRUE),
    mehg_monthly_frac = mehg_monthly_flux / mehg_annual_flux,
    mehg_cum_frac = cumsum(mehg_monthly_flux) / mehg_annual_flux,
    hgt_annual_flux = sum(hgt_monthly_flux, na.rm = TRUE),
    hgt_monthly_frac = hgt_monthly_flux / hgt_annual_flux,
    hgt_cum_frac = cumsum(hgt_monthly_flux) / hgt_annual_flux
  ) %>%
  ungroup() %>%
  # Step 3: Summarize across years: mean and SD of monthly flux and cumulative fraction
  group_by(river_group, month) %>%
  summarise(
    mehg_flux_mean = mean(mehg_monthly_flux, na.rm = TRUE),
    mehg_flux_sd = sd(mehg_monthly_flux, na.rm = TRUE),
    mehg_flux_monthly_frac_mean = mean(mehg_monthly_frac, na.rm = TRUE),
    mehg_flux_monthly_frac_sd = sd(mehg_monthly_frac, na.rm = TRUE),
    mehg_flux_cum_frac_mean = mean(mehg_cum_frac, na.rm = TRUE),
    mehg_flux_cum_frac_sd = sd(mehg_cum_frac, na.rm = TRUE),
    hgt_flux_mean = mean(hgt_monthly_flux, na.rm = TRUE),
    hgt_flux_sd = sd(hgt_monthly_flux, na.rm = TRUE),
    hgt_flux_monthly_frac_mean = mean(hgt_monthly_frac, na.rm = TRUE),
    hgt_flux_monthly_frac_sd = sd(hgt_monthly_frac, na.rm = TRUE),
    hgt_flux_cum_frac_mean = mean(hgt_cum_frac, na.rm = TRUE),
    hgt_flux_cum_frac_sd = sd(hgt_cum_frac, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 4: Convert numeric month to factor for plotting
  mutate(
    river_group = factor(river_group),
    month = factor(month.abb[month], levels = month.abb)
  )

head(monthly_summary); summary(monthly_summary)

#-------------------------------#
# Combine monthly Hg and NPP ####
#-------------------------------#
dpm <- data.frame(
  month = months_list,
  month_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
) %>%
  mutate(
    month = factor(month, levels=months_list)
  )

hg_npp <- inner_join(
  monthly_summary,
  npp,
  by = c("river_group", "month")
) %>%
  rename(river = river_group) %>%
  select(river, month, npp_tgc_mo, npp_monthly_frac, npp_cum_frac,
         mehg_flux_mean, mehg_flux_sd,
         mehg_flux_monthly_frac_mean, mehg_flux_monthly_frac_sd,
         mehg_flux_cum_frac_mean, mehg_flux_cum_frac_sd,
         hgt_flux_mean, hgt_flux_sd,
         hgt_flux_monthly_frac_mean, hgt_flux_monthly_frac_sd,
         hgt_flux_cum_frac_mean, hgt_flux_cum_frac_sd) %>%
  # Add seasons to dataframe, following season definitions in Holmes et al. 2012
  mutate(
    season = case_when(
      month %in% c("Jan", "Feb", "Mar", "Apr", "Nov", "Dec") ~ "win",  # Winter
      month %in% c("May", "Jun") ~ "spr",   # Spring
      month %in% c("Jul", "Aug", "Sep", "Oct") ~ "sum"    # Summer
    ),
    season = factor(season, levels = c("win", "spr", "sum"))
  ) %>%
  # Merge data on area of shelf regions
  left_join(shelf_df, by = "river") %>%
  left_join(dpm, by = "month") %>%
  mutate(
    # Convert NPP (TgC/mo/shelf) to gC/m²/day
    npp_gC_m2_d = (npp_tgc_mo / shelf_area) * 1e3 / month_days,
    # Approximate inorganic total Hg as: Hg(II) = HgT - MeHg
    hgii_flux = hgt_flux_mean - mehg_flux_mean,
    river = factor(river, levels = c("Ob_Yenisey","Lena","Kolyma","Yukon","Mackenzie"))
  ) %>%
  arrange(river, month) %>%
  select(river, season, month, month_days, shelf_area, npp_tgc_mo, npp_gC_m2_d, everything())

head(hg_npp); summary(hg_npp)

#-----------------------------#
# Clean Global Environment ####
#-----------------------------#
rm(list=c("months_list", "shelves", "shelf_rivs", "riv_shelves", "shelf_df", 
          "npp", "monthly_flux", "monthly_summary", "dpm"))

#----------#
# Plotting #
#----------#
# Named labels for the right-side y-axis
riv_labels <- c(
  "Ob_Yenisey" = "Kara Sea\n(Ob', Yenisey)",
  "Lena"       = "Laptev Sea\n(Lena)",
  "Kolyma"     = "East Siberian Sea\n(Kolyma)",
  "Yukon"      = "Bering Sea\n(Yukon)",
  "Mackenzie"  = "Beaufort Sea\n(Mackenzie)"
)

params <- names(riv_labels)

plots <- purrr::map2(
  params,
  seq_along(params),
  ~{
    df_sub <- hg_npp %>% 
      filter(river == .x) %>%
      mutate(month = factor(month, levels = month.abb))
    
    p <- ggplot(df_sub, aes(x = month)) +
      # y-axis: cumulative fractions (0–1)
      scale_y_continuous(
        limits = c(0, 0.5),
        breaks = seq(0, 0.5, 0.1),
        labels = c(0, "", "0.2", "", "0.4", ""),
        name = NULL,
        sec.axis = dup_axis(name = riv_labels[.x], labels = NULL)
      ) +
      theme_bw(base_size = 10) +
      theme(
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_text(size = 7, color = "black"),
        axis.line.y.left = element_line(),
        axis.ticks.y.left = element_line(),
        axis.title.y.right = element_text(size = 8, angle = 270, vjust = 0.5),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = if (.y < length(params)) element_blank() else element_text(size = 7, color = "black"),
        axis.ticks.x = if (.y < length(params)) element_blank() else element_line(),
        legend.position = "none",
        plot.margin = margin(t = 0.5, r = 10, b = 0.5, l = 25)
      ) +
      
      # NPP
      geom_line(aes(y = npp_monthly_frac, group = 1), color = "black", linewidth = 0.9) +
      geom_point(aes(y = npp_monthly_frac, group = 1),
                 color = "black", shape = 16, size = 1.6) +
      
      # HgT fluxes ± SD
      geom_ribbon(aes(ymin = hgt_flux_monthly_frac_mean - hgt_flux_monthly_frac_sd,
                      ymax = hgt_flux_monthly_frac_mean + hgt_flux_monthly_frac_sd,
                      group = 1), fill = "gray70", alpha = 0.4) +
      geom_line(aes(y = hgt_flux_monthly_frac_mean, group = 1), color = "gray70", linewidth = 0.9) +
      geom_point(aes(y = hgt_flux_monthly_frac_mean, group = 1),
                 color = "gray70", shape = 15, size = 1.4) +
      
      # MeHg fluxes ± SD
      geom_ribbon(aes(ymin = mehg_flux_monthly_frac_mean - mehg_flux_monthly_frac_sd,
                      ymax = mehg_flux_monthly_frac_mean + mehg_flux_monthly_frac_sd,
                      group = 1), fill = "dodgerblue3", alpha = 0.4) +
      geom_line(aes(y = mehg_flux_monthly_frac_mean, group = 1), color = "dodgerblue3", linewidth = 0.9) +
      geom_point(aes(y = mehg_flux_monthly_frac_mean, group = 1),
                 color = "dodgerblue3", shape = 17, size = 2.1) +
      
      labs(x = NULL)
    
    p
  }
)

# Stack vertically
stacked <- wrap_plots(plots, ncol = 1, heights = rep(1, length(plots)))

# Create a mini legend manually
legend_data <- data.frame(
  x = 1,
  y = 1:3,
  label = rev(c("HgT", "MeHg", "NPP")),
  color = rev(c("gray70", "dodgerblue3", "black")),
  shape = rev(c(15, 17, 16))
)

legend_plot <- ggplot(legend_data, aes(x, y)) +
  geom_point(aes(color = color, shape = shape), size = 2, show.legend = FALSE) +
  scale_color_identity() +
  scale_shape_identity() +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.2, size = 2.3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

# Convert to grob
legend_grob <- ggplotGrob(legend_plot)

fig_hg_npp_stacked_with_legend <- ggdraw() +
  draw_plot(stacked) +
  draw_label(
    "Riverine Hg flux and shelf region NPP\n(fraction of annual total)",
    x = 0.04, y = 0.5, angle = 90,
    vjust = 0.75, hjust = 0.5,
    fontface = "plain", color = "black", size = 10
  ) +
  draw_plot(legend_grob, x = 0.21, y = 0.895, width = 0.025, height = 0.07, hjust = 0, vjust = 0)

# Display the figure
fig_hg_npp_stacked_with_legend

# Optional: save
ggsave(paste0(fig_export_dir, "/", "Figure S#- monthly fractions", " ", Sys.Date(), ".", dev_form), 
       plot = fig_hg_npp_stacked_with_legend, device = dev_form,
       width = 3.33, height = 4.329, dpi = 350)
