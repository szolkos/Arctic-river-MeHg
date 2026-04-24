#================================================================================================================================#
# Filename: 10_plots.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Figures for manuscript
#================================================================================================================================#

# EST graphics formatting requirements
# Single-column: ≤ 240 points wide (3.33")
# Double-column: 300-504 points wide (4.167-7")

# For each river, what is the first and last date with Hg data?
hg_df %>%
  filter(year >= 2020, year <= 2024, !is.na(mehg)) %>%
  group_by(river) %>%
  summarise(
    first_date = min(date),
    last_date  = max(date)
  )

#-------------------------------------------#
# Compare measured [MeHg] between rivers ####
#-------------------------------------------#

# Mean and median [MeHg] amd [HgT] by river
write_xlsx(
  hg_df %>%
  filter(year >= 2020, year <= 2024) %>%
  group_by(river) %>%
  summarise(
    mehg_mean = mean(mehg, na.rm=T),
    mehg_med = median(mehg, na.rm=T),
    mehg_sd = sd(mehg, na.rm=T),
    hgt_mean = mean(hgt, na.rm=T),
    hgt_med = median(hgt, na.rm=T),
    hgt_sd = sd(hgt, na.rm=T)
  ),
  paste0(tables_export_dir,"hg_concs.xlsx")
)

# Mean and median [MeHg] and MeHg% by season ####
aov_dat <- hg_df %>%
  filter(year >= 2020, year <= 2024) %>%
  # Manually reassign season for the pre-freshet May samples
  mutate(season = case_when(
    river == "Lena" & date == "2021-05-01" ~ "win",
    river == "Yenisey" & date == "2021-05-02" ~ "win",
    TRUE ~ season 
  )) %>% 
  select(river, season, hgt, mehg, mehgpcnt) %>% 
  na.omit() %>%
  mutate(river = factor(river, levels = c("Ob", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")))

aov_dat %>%
  group_by(season) %>%
  summarise(
    # HgT
    n_hgt = sum(!is.na(hgt)), # n (number of non-NA observations)
    hgt_mean = mean(hgt, na.rm=T),
    hgt_sd = sd(hgt, na.rm=T),
    # MeHg
    n_mehg = sum(!is.na(mehg)),
    mehg_mean = mean(mehg, na.rm=T),
    mehg_sd = sd(mehg, na.rm=T),
    # MeHg%
    n_mehgpcnt = sum(!is.na(mehgpcnt)),
    mehgpcnt_mean = mean(mehgpcnt, na.rm=T),
    mehgpcnt_sd = sd(mehgpcnt, na.rm=T),
    # Medians
    hgt_med = median(hgt, na.rm=T),
    mehg_med = median(mehg, na.rm=T),
    mehgpcnt_med = median(mehgpcnt, na.rm=T)
  )

# ANOVA ####
# How many obs per river and season?
table(aov_dat$river, aov_dat$season)

summary(aov_dat)
boxplot(aov_dat$mehg ~ aov_dat$season)

# Store model
aov_formula <- formula(log(mehgpcnt) ~ season) # "river" or "season"
#aov_formula <- formula(log(mehgpcnt) ~ river*season) # MeHg% river-season interaction
aov_lm <- lm(aov_formula, data = aov_dat)
Anova(aov_lm, type = "III") # type III for unbalanced design

# QC ANOVA
# 1. Independence of observations
## Implicit, not tested
# 2. Check normality of residuals
## H0 = residuals are normally distributed; p ≤ 0.05 = reject H0
qqnorm(residuals(aov_lm)); qqline(residuals(aov_lm), col = "red") # QQ plot
shapiro.test(residuals(aov_lm))
## NOTE: given high n (90+) and small deviation at tail of QQ plot, ANOVA is deemed ok (ANOVA with moderate to large n because is fairly robust to mild departures from normality)
# 3. Check homogeneity of variances (homoscedasticity)
## H0 = variances equal across groups; Pr(>F) ≤ 0.05 => reject H0
plot(aov_lm, which = 1)  # Residuals vs Fitted
car::leveneTest(aov_formula, data = aov_dat, center = median) # Use the transformed response (log(mehg)) in Levene’s test, to match the model. Also, set center = median to make the test robust to non-normality.

# Summary & Tukey HSD
summary(aov(aov_lm))
TukeyHSD(aov(aov_lm))

# MeHg ANOVA results: generate letter labels for boxplot
mehg_aov_labs <- hg_df %>%
  group_by(river) %>%
  summarise(Q3 = quantile(mehg, 0.75, na.rm = TRUE)) %>% # Compute Q3 for each river
  mutate(
    y = Q3 + 0.01  # slightly above Q3
  ) %>%
  mutate(
    label = case_when(
      river %in% c("Ob", "Yukon") ~ "a",
      river %in% c("Yenisey", "Lena", "Mackenzie") ~ "a,b",
      river == "Kolyma" ~ "b"
    )
  )


# Measured MeHg conc by river ####
#fig_mehg_conc_river <- ggplot(hg_df, aes(y=mehg, x=river, fill="gray60", color="black")) +

# Define conversion factor for femtomolar
ng_to_fm <- 4985.293

# Calculate the y-coordinates in ng/L
target_ng  <- 130 / ng_to_fm
upper_ng   <- (130 + 110) / ng_to_fm
lower_ng   <- (130 - 110) / ng_to_fm

fig_mehg_conc_river <- ggplot(hg_df, aes(y=mehg, x=river, fill=river)) + 
  # 1. Shaded Region (Lowest Layer)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = lower_ng, ymax = upper_ng, 
           fill = "gray85", alpha = 0.5) +
  
  # 2. Dashed Line
  geom_hline(yintercept = target_ng, linetype = "dashed", color = "gray40", size = 0.5) +
  
  # 3. Label: MeHg (PML)
  # x = 0.5 puts it on the far left of the first category
  # vjust = 1.5 pushes it just below the line
  annotate("text", x = 0.5, y = target_ng, label = "MeHg (PML)", 
           hjust = 0, vjust = 1.5, size = 1.8, fontface = "italic", color = "gray30") +
  
  # 4. Boxplots
  geom_boxplot(outlier.shape = NA, size = 0.5, color = "black") + 
  
  scale_y_continuous(
    limits = c(0, 0.25), 
    breaks = seq(0, 0.25, 0.05),
    sec.axis = sec_axis(~ . * ng_to_fm, name = expression(MeHg~(fM)))
  ) +
  scale_x_discrete(labels = riv.labs) +
  scale_fill_manual(values = riv_cols) + 
  theme_bw() +
  theme(
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y.left = element_text(size = 8, margin = ggplot2::margin(r = 10)),
    axis.title.y.right = element_text(size = 8, margin = ggplot2::margin(l = 4)),
    axis.text.y = element_text(size = 6.5, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    legend.position = "none"
  ) +
  geom_point(
    position = position_jitter(width = 0.15), 
    size = 1, 
    alpha = 0.4,
    color = "black"
  ) +
  geom_text(
    data = mehg_aov_labs, 
    aes(x = river, y = y, label = label), 
    nudge_x = 0.3, 
    size = 2.5, 
    color = "black"
  ) +
  labs(y = expression(MeHg~(ng~L^-1)), x = NULL)

fig_mehg_conc_river

#ggsave(paste0(file.path(fig_export_dir, "/"), "Figure S# LOADEST ", param, " QAQC", " ", Sys.Date(), ".", dev_form)
ggsave(paste0(fig_export_dir, "/", "Figure 1", " ", Sys.Date(), ".", dev_form), 
       plot = fig_mehg_conc_river, device = dev_form,
       width = 3.333, height = 2.222, dpi = 350)


# MeHg ANOVA results: generate letter labels for boxplot
mehgpcnt_aov_labs <- hg_df %>%
  group_by(river) %>%
  summarise(Q3 = quantile(mehgpcnt, 0.75, na.rm = TRUE)) %>% # Compute Q3 for each river
  mutate(
    y = Q3 + 0.6  # slightly above Q3
  ) %>%
  mutate(
    label = case_when(
      river == "Ob" ~ "a,b",
      river == "Yenisey" ~ "b",
      river == "Lena" ~ "a",
      river %in% c("Kolyma", "Yukon", "Mackenzie") ~ "c"
    )
  )

# MeHg% by river
fig_mehg_pcnt_river <- ggplot(hg_df, aes(y=mehgpcnt, x=river, fill=river, color="black")) + # TURN ON FOR COLORED BOXES
  geom_boxplot(outlier.shape = NA, size = 0.5) +
  #scale_y_continuous(limits=c(0,0.25), breaks=seq(0,0.25,0.05)) +
  scale_x_discrete(labels=riv.labs) +
  #scale_fill_manual(values="gray60") +
  scale_fill_manual(values=riv_cols) + # TURN ON FOR COLORED BOXES
  scale_color_manual(values="black") +
  theme_bw() +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.border=element_rect(colour="black", fill=NA, size=1),
        plot.background=element_rect(colour="white", size=1),
        plot.title = element_text(size = 12),
        axis.title.y.left=element_text(size=10, margin=ggplot2::margin(t=0, r=10, b=0, l=0)),
        axis.text.y.left=element_text(size=8, angle=0, hjust=0.5, colour="black"),
        axis.title.x=element_text(size=10, margin=ggplot2::margin(t=0, r=0, b=10, l=0)),
        axis.text.x=element_text(size=7, angle=0, hjust=0.5, colour="black"),
        legend.position="none") +
  geom_point(aes(y=mehgpcnt, x=river), 
             position=position_jitterdodge(jitter.width = 0.1), 
             #position_jitter(width = 0.1, height = 0), 
             size=1, 
             alpha=0.7) +
  geom_text(data=mehgpcnt_aov_labs, aes(x=river, y=y, label=label), nudge_x = 0.15, size=3, color="black", hjust=0.25) +
  labs(y="MeHg (%)", x=NULL)

fig_mehg_pcnt_river

ggsave(paste0(fig_export_dir, "/", "Figure 1-MeHg pcnt", " ", Sys.Date(), ".", dev_form), 
       plot = fig_mehg_pcnt_river, device = dev_form,
       width = 3.333, height = 2.222, dpi = 350)


#----------------------------------------------#
# Modeled and measured MeHg fluxes by river ####
#----------------------------------------------#

# Create df
mehg_mdl <- purrr::map_dfr(rivers, function(riv) {
  loadest_daily_data(riv = riv, param = "mehg") %>%
    mutate(
      river = riv,
      year = year(date)
    )
}) %>%
  select(date, river, mehg_flux) %>%
  rename(mehg_flux_mdl = mehg_flux) %>% 
  mutate(river = factor(river, levels = rivs))

mehg_mdlmsr <- hg_df %>% 
  mutate(
    # Convert Q (m3/s) to L/d, multiply by [MeHg] converted to kg/L
    mehg_flux_msr = q_cms*1000*86400*mehg*1e-12,
    date = as.Date(date)
    ) %>%
  select(river, date, mehg_flux_msr) %>% 
  left_join(mehg_mdl, by = c("river", "date")) %>%
  filter(between(date, as.Date("2020-01-01"), as.Date("2024-12-31")))

# Stacked plots
params <- levels(mehg_mdlmsr$river)

# For y-axis scales, what's the range of values for each river? (i.e., min & max of measured and modeled)
mehg_mdlmsr %>%
  group_by(river) %>%
  summarise(
    flux_range = paste0(
      signif(min(c(mehg_flux_msr, mehg_flux_mdl), na.rm = TRUE), 3),
      " – ",
      signif(max(c(mehg_flux_msr, mehg_flux_mdl), na.rm = TRUE), 3)
    ),
    .groups = "drop"
  )

plots <- purrr::map2(
  params,
  seq_along(params),
  ~{
    df_sub <- filter(mehg_mdlmsr, river == .x)
    
    p <- ggplot(df_sub, aes(x = date, y = mehg_flux_mdl)) +
      # modeled flux (lines)
      geom_line(color = riv_cols[[.x]], linewidth = 0.8) + # TURN ON FOR COLORED LINES
      #geom_line(color = "grey20", linewidth = 0.8) +
      # measured flux (points)
      geom_point(
        aes(y = mehg_flux_msr),
        shape = 21, size = 2, stroke = 0.5,
        fill = riv_cols[[.x]], color = "black" # TURN ON FOR COLORED POINTS
        #fill = "white", color = "grey20"
      ) +
      scale_x_date(
        limits = as.Date(c("2020-01-01", "2025-01-01")),
        breaks = seq(as.Date("2020-01-01"), as.Date("2025-01-01"), by = "1 year"),
        date_labels = "%Y"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        
        axis.title.x = element_text(margin = margin(t = 4), color = "black"),
        axis.text.x  = if (.y < length(params)) element_blank() else element_text(size=8, color = "black"),
        axis.ticks.x = if (.y < length(params)) element_blank() else element_line(),
        #axis.line.x  = if (.y < length(params)) element_blank() else element_line(),
        #axis.line.x  = element_line(color = "black"),
        
        # Primary y-axis
        axis.text.y   = element_text(size=8, color = "black"),
        axis.title.y  = element_blank(),
        axis.line.y.left  = element_line(),
        axis.ticks.y.left = element_line(),
        
        # Secondary y-axis (river names)
        axis.title.y.right = element_text(
          color = "black", face = "plain", size = 9, angle = 270, vjust = 0.5
        ),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_line(color = "black", linewidth = 0.7),
        
        legend.position = "none",
        plot.margin = margin(t=0.5, r=10, b=0.5, l=20)
      ) +
      labs(
        #x = if (.y == length(params)) "Date" else NULL
        x=NULL
      )
    
    # Add custom y scales by parameter
    if (.x == "Ob") {
      p <- p + scale_y_log10(
        limits = c(0.01, 0.55),
        breaks = c(seq(0.01, 0.1, 0.01),0.2, 0.3, 0.4, 0.5),
        labels = c("0.01", "", "", "", "", "", "", "", "", "0.1", "", "", "", ""),
        name = NULL,
        sec.axis = dup_axis(
          name = "Ob'",
          labels = NULL
        )
      )
    } else if (.x == "Yenisey") {
      p <- p + scale_y_log10(
        limits = c(0.01, 2),
        breaks = c(0.01, 0.10, 1.00),
        name = NULL,
        sec.axis = dup_axis(
          name = .x,
          labels = NULL
        )
      )
    } else if (.x == "Lena") {
      p <- p + scale_y_log10(
        limits = c(0.003,3.7),
        breaks = c(0.01, 0.1, 0.1, 1.0),
        name = NULL,
        sec.axis = dup_axis(
          name = .x,
          labels = NULL
        )
      )
    } else if (.x == "Kolyma") {
      p <- p + scale_y_log10(
        limits = c(0.0001,0.2),
        breaks = c(0.001, 0.01, 0.01, 0.1),
        name = NULL,
        sec.axis = dup_axis(
          name = .x,
          labels = NULL
        )
      )
    } else if (.x == "Yukon") {
      p <- p + scale_y_log10(
        limits = c(0.003,0.22),
        breaks = c(seq(0.003, 0.01, 0.001), seq(0.01, 0.1, 0.01), 0.2),
        labels = c("","","","","","","","0.01","","","","","","","","","0.1","",""),
        name = NULL,
        sec.axis = dup_axis(
          name = .x,
          labels = NULL
        )
      )
    } else if (.x == "Mackenzie") {
      p <- p + scale_y_log10(
        limits = c(0.001, 0.35),
        breaks = c(0.001, 0.01, 0.1),
        name = NULL,
        sec.axis = dup_axis(
          name = .x,
          labels = NULL
        )
      )
    }
    
    # Return plot
    p
  }
)

stacked <- wrap_plots(plots, ncol = 1, heights = rep(1, length(plots)))

# Add one shared y-axis label
fig_mehg_flux_msr_mdl <- ggdraw() +
  draw_plot(stacked) +
  draw_label(
    expression(MeHg~flux~(kg~d^-1)),
    x = 0.04, y = 0.5, angle = 90,
    vjust = 0.5, hjust = 0.5,
    fontface = "plain", color = "black", size = 10
  )

fig_mehg_flux_msr_mdl

ggsave(paste0(fig_export_dir, "/", "Figure S3", " ", Sys.Date(), ".", dev_form), 
       plot = fig_mehg_flux_msr_mdl, device = dev_form,
       width = 3.33, height = 4.329, dpi = 350)


#----------------#
# MeHg yields ####
#----------------#

# Calculate annual MeHg yields and runoff for each river
mehg_yields <- daily_flux %>%
  group_by(river, year) %>%
  summarise(
    annual_q_km3 = sum(q_km3d, na.rm = TRUE),
    annual_mehg_kg = sum(mehg_flux, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    shed_km2 = shed_area_km2[river],
    water_yield_cm = annual_q_km3 / shed_km2 * 1e5, # cm/y
    mehg_yield_mgkm2 = annual_mehg_kg / shed_km2 * 1e6 # mg/km²/y
  ) %>%
  ungroup() %>%
  select(river, year, water_yield_cm, mehg_yield_mgkm2)

# Plot annual MeHg yields and runoff for each river
ggplot(mehg_yields, aes(x = water_yield_cm, y = mehg_yield_mgkm2, fill = river)) +
  scale_x_continuous(limits = c(0,35), breaks=seq(0,35,5)) +
  scale_y_continuous(limits = c(0,35), breaks=seq(0,35,5)) +
  scale_fill_manual(values = riv_cols) +  # <--- use color, not fill
  theme_bw() +
  theme(
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill='white'),
    panel.border = element_rect(colour="black", fill=NA, size=1),
    plot.background = element_rect(colour="white", size=1),
    plot.title = element_text(size = 16),
    axis.title.y.left = element_text(size=15, margin=ggplot2::margin(t=0, r=10, b=0, l=0)),
    axis.text.y.left = element_text(size=13.5, angle=0, hjust=0.5, colour="black"),
    axis.title.x = element_text(size=15, margin=ggplot2::margin(t=5, r=0, b=0, l=0)),
    axis.text.x = element_text(size=13.5, angle=0, hjust=0.5, colour="black"),
    legend.position = "none"
  ) +
  geom_point(shape = 21, color = "black", size = 5, stroke = 0.6) +
  labs(
    y = expression(MeHg~yield~(mg~km^-2*~y^-1)),
    x = expression(Runoff~(cm~y^-1)),
    color = "River"
  )

# Summarize ranges and mean ± SE of MeHg yield and runoff
mehg_summary <- mehg_yields %>%
  group_by(river) %>%
  summarise(
    mehg_mean = mean(mehg_yield_mgkm2, na.rm = TRUE),
    mehg_min = min(mehg_yield_mgkm2, na.rm = TRUE),
    mehg_max = max(mehg_yield_mgkm2, na.rm = TRUE),
    mehg_se = sd(mehg_yield_mgkm2, na.rm = TRUE)/sqrt(n()),
    runoff_mean = mean(water_yield_cm, na.rm = TRUE),
    runoff_min = min(water_yield_cm, na.rm = TRUE),
    runoff_max = max(water_yield_cm, na.rm = TRUE),
    runoff_se = sd(water_yield_cm, na.rm = TRUE)/sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    river = factor(river, levels = rivs),
    river_label = ifelse(as.character(river) == "Ob", "Ob'", as.character(river))
    ) %>%
  arrange(river)

# Plot mean ± range of MeHg yields and runoff for each river
fig_mehg_runoff_yields <- ggplot(mehg_summary, aes(x = runoff_mean, y = mehg_mean, fill = river)) +
  scale_fill_manual(values = riv_cols) +
  scale_color_manual(values = riv_cols) +
  scale_x_continuous(limits = c(0,35), breaks=seq(0,35,5)) +
  scale_y_continuous(limits = c(0,35), breaks=seq(0,35,5)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill='white'),
    panel.border = element_rect(colour="black", fill=NA, size=1),
    plot.title = element_text(size = 14),
    axis.title.y.left = element_text(size=10, margin=ggplot2::margin(t=0, r=5, b=0, l=0)),
    axis.text.y.left = element_text(size=8, angle=0, hjust=0.5, colour="black"),
    axis.title.x = element_text(size=10, margin=ggplot2::margin(t=5, r=0, b=0, l=0)),
    axis.text.x = element_text(size=8, angle=0, hjust=0.5, colour="black"),
    legend.position = "none"
  ) +
  geom_errorbar(aes(ymin = mehg_min, ymax = mehg_max, color = river), width = 0) +
  geom_errorbarh(aes(xmin = runoff_min, xmax = runoff_max, color = river), height = 0) +
  geom_point(shape = 21, color = "black", size = 4, stroke = 0.6) +
  labs(
    y = expression(MeHg~yield~(mg~km^-2*~y^-1)),
    x = expression(Runoff~(cm~y^-1)),
    fill = "River"
  ) +
  geom_text_repel(
    aes(label = river_label),
    nudge_x = 0.9, # shift right
    nudge_y = 1.2, # shift up
    size = 3, # text size
    color = "black", #riv_cols,
    hjust = 0, # left-justified
    segment.color = NA, # remove connecting lines
    show.legend = FALSE
  )

fig_mehg_runoff_yields

ggsave(paste0(fig_export_dir, "/", "Figure S4", " ", Sys.Date(), ".", dev_form), 
       plot = fig_mehg_runoff_yields, device = dev_form,
       width = 3.33, height = 2.664, dpi = 350)


#--------#
# HgT ####
#--------#

# Measured HgT conc by river
ggplot(hg_df %>% filter(year(date) >= 2020), aes(y=hgt, x=river, fill="gray60", color="black")) + # fill=river, color=river
  geom_boxplot(outlier.shape = NA, size = 0.8) +
  #scale_fill_manual(name="river", values=riv_cols) +
  #scale_color_manual(name="river", values=riv_cols) +
  scale_y_log10(limits=c(0.6,100), breaks=c(1,10,100)) +
  #annotation_logticks(sides = "l", outside=F) +
  annotation_logticks(sides = "l", outside=F, short=unit(1,"mm"), mid=unit(1,"mm"), long=unit(1.5,"mm")) + 
  scale_x_discrete(labels=riv.labs) +
  scale_fill_manual(values="gray60") +
  scale_color_manual(values="black") +
  theme_bw() +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.border=element_rect(colour="black", fill=NA, size=1),
        plot.background=element_rect(colour="white", size=1),
        plot.title = element_text(size = 16),
        axis.title.y.left=element_text(size=15, margin=ggplot2::margin(t=0, r=10, b=0, l=0)),
        axis.text.y.left=element_text(size=13.5, angle=0, hjust=0.5, colour="black"),
        axis.title.x=element_text(size=15, margin=ggplot2::margin(t=0, r=0, b=10, l=0)),
        axis.text.x=element_text(size=13.5, angle=0, hjust=0.5, colour="black"),
        legend.position="none") +
  geom_point(aes(y=hgt, x=river), position=position_jitterdodge(), size=2, alpha=0.7) +
  labs(y=expression(HgT~(ng~L^-1)), x=NULL)
