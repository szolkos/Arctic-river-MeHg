#================================================================================================================================#
# Filename: 8_stats_rda.R ####
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-04-23
# Background: Redundancy analysis for multivariate assessment of MeHg export patterns and drivers. 
# References
## 'ordistep' function: https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordistep
## scaling & centering: https://stats.stackexchange.com/questions/29781/when-conducting-multiple-regression-when-should-you-center-your-predictor-varia
#================================================================================================================================#

# Depends: functions: test_and_transform

#------------------------#
# Load & prepare data ####
#------------------------#
# water quality <- tbd
# nutrients <- no3, doc, tss # NO3, DOC, TSS yields
# organic matter <- doc, suva # DOC yield, SUVA254
# Carbon & OM properties
# particulates <- tss, poc, pon # TSS, POC, PON yields
# hydrology <- runoff
# landscape conditions <- slope, peat, pf # watershed slope, peatland coverage, permafrost coverage
# climate <- maat, map # MAAT, MAP
# Vegetation characteristics

rda_df <- ylds %>%
  inner_join(shed_stats, by = c("river", "year"))

# Select variables for RDA
# Site characteristics
site_chars <- rda_df %>% select(river)
# Hydrology
runoff <- rda_df %>% select(runoff_cm)
# Hg species
hg_spp <- rda_df %>% select(mehg_yield_mgkm2, hg_yield_mgkm2)
# Carbon
dom <- rda_df %>% select(doc_yield_tgkm2)
# Nutrients
nuts <- rda_df %>% select(no3_yield_ggkm2)
# Particulates
particles <- rda_df %>% select(tss_yield_tgkm2)
# Climate
clim <- rda_df %>% select(tap_mm, maat_c)
# Watershed characteristics
shed_chars <- rda_df %>% select(pf_frac_cont, peat_frac, slope) #pf_frac_all

# Prepare data for RDA
rda_df <- cbind(site_chars, runoff, hg_spp, dom, nuts, particles, clim, shed_chars)

# View transformations
test_and_transform(dft = rda_df)$transformation_results

# Conditionally test and transform variables for normality
rda_df <- test_and_transform(dft = rda_df)$transformed_df %>%
  # Center & scale
  mutate(across(2:ncol(.), scale)) %>%
  #select(-c(hgp_ngl, mehgp_ngl)) %>%
  rename_with(~ str_replace(., "\\.V1$", ""), ends_with(".V1")) %>%
  na.omit

# Summarize df
summary(rda_df); dim(rda_df); paste0(nrow(rda_df), " observations in dataframe")

# Create filepath for outputs
run_num <- "1"
out_path <- paste0(root_path, "/Modeling/", Sys.Date(), "_RDA", run_num)
dir.create(out_path, recursive = F)


#------------#
# Run RDA ####
#------------#

# Define response variables
rda_hg <- rda_df %>% 
  select(names(hg_spp))

# Define predictor variables
rda_preds <- rda_df %>% 
  select(-c(river, names(hg_spp)))

# Run RDA
hg_rda <- rda(rda_hg ~ . - runoff_cm + Condition(runoff_cm), rda_preds)

# Run ordistep (automated covariate selection)
ordistep(hg_rda, permutations=5000, direction="backward")
  
# Store trimmed RDA
rda_fin <- rda(formula = rda_hg ~ doc_yield_tgkm2 + no3_yield_ggkm2 + tss_yield_tgkm2 + peat_frac + slope + Condition(runoff_cm),
               data = rda_preds)

# Check VIF
#vif.cca(rda_fin)

#----------------------#
# Check assumptions ####
#----------------------#
# 1. Linearity: Plot observed vs. predicted values
predicted_values <- predict(rda_fin, type = "response") %>% as.data.frame()

ys <- predicted_values$mehg_yield_mgkm2
xs <- rda_hg$mehg_yield_mgkm2

qc_linearity <- ggplot(rda_hg[1], aes(y = ys, x = xs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "predicted", x = "observed") +
  ggtitle("Linearity Check")

qc_linearity

ggsave(
  filename = paste0(out_path, "/", "QC-linearity ", Sys.Date(), ".", dev_form),
  plot = qc_linearity,
  device = 'pdf',
  width = 5, 
  height = 4,
  dpi = 350)

# 2. Independence: No specific RDA function for independence, as it assumes independence by default.

# 3. Homoscedasticity: Plot residuals vs. fitted values
residuals <- residuals(rda_fin) %>% as.data.frame(); residuals <- residuals$mehg_yield_mgkm2

qc_hom <- ggplot(rda_df, aes(x = residuals, y = ys)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "fitted", x = "residuals") +
  ggtitle("Homoscedasticity Check")

qc_hom

ggsave(
  filename = paste0(out_path, "/", "QC-homoscedasticity ", Sys.Date(), ".", dev_form),
  plot = qc_hom,
  device = 'pdf',
  width = 5, 
  height = 4,
  dpi = 350)

# 4. Normality of Residuals: Plot a histogram or a Q-Q plot of residuals
# Histogram
qc_normality <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(
    bins = 15,
    fill = "lightblue",
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Frequency"
  )

plot(qc_normality)

ggsave(
  filename = paste0(out_path, "/", "QC-normality ", Sys.Date(), ".", dev_form),
  plot = qc_normality,
  device = 'pdf',
  width = 5, 
  height = 4,
  dpi = 350)

# Q-Q plot
qqnorm(residuals); qqline(residuals)
      

#### Summarize ####

# Determine significance of all explanatory terms in RDA, export results
rda_fin %>%
  anova(by = "term") %>%
  as.data.frame() %>%
  # Add rownames as a column for the variable names
  rownames_to_column(var = "Variable") %>%
  # Remove the Residual row (if present)
  #filter(Variable != "Residual") %>%
  # Arrange by F statistic in descending order
  arrange(desc(F)) %>%
  # Print results
  #print() %>%
  # Export
  write_xlsx(paste0(out_path,"/1-Significance of explanatory vars.xlsx"))

# Determine significance of marginal effects in RDA (i.e., what is the single best explanatory variable, if only one is used? (Zuur et al. 2007)), export results
#anova(rda_fin, by="mar")#, permutations=500) 
rda_fin %>%
  anova(by = "mar") %>%
  as.data.frame() %>%
  # Add rownames as a column for the variable names
  rownames_to_column(var = "Variable") %>%
  # Remove the Residual row (if present)
  #filter(Variable != "Residual") %>%
  # Arrange by F statistic in descending order
  arrange(desc(F)) %>%
  # Print results
  #print() %>%
  # Export
  write_xlsx(paste0(out_path,"/2-Significance of marginal effects.xlsx"))

# Determine significance of each RDA axis, export results
rda_fin %>%
  anova(by = "axis") %>%
  as.data.frame() %>%
  # Add rownames as a column for the variable names
  rownames_to_column(var = "Variable") %>%
  # Remove the Residual row (if present)
  #filter(Variable != "Residual") %>%
  # Arrange by F statistic in descending order
  arrange(desc(F)) %>%
  # Print results
  #print() %>%
  # Export
  write_xlsx(paste0(out_path,"/3-Significance of RDA axes.xlsx"))

# Summary of final RDA
# NOTE: this gives proportions of variance that are conditioned (unique to [i.e., explained by] runoff) versus constrained (unique to others after removing runoff), but NOT the fraction of variance shared between runoff and other variables
summary(rda_fin)

# Run variance partitioning using varpart() to get variance unique to runoff, unique to others variables, shared, and unexplained
hg_varpart <- varpart(
  rda_hg,
  ~ runoff_cm,
  ~ doc_yield_tgkm2 + no3_yield_ggkm2 + tss_yield_tgkm2 + pf_frac_cont + peat_frac + slope,
  data = rda_df
)

# Review varpart() results
hg_varpart
# [a+c] = X1 is Total variation explained by runoff alone (without controlling for X2); Runoff alone explains ~54% (see adj. R2) of the variance in MeHg composition.
# [b+c] = X2 is Total variation explained by the other predictors alone (without controlling for runoff); The other predictors explain ~97% of the variance (massive overlap with runoff).
# [a+b+c] = X1 + X2 is Combined model with all predictors; Together, runoff and others explain ~97% of variance — barely more than the others alone.

plot(hg_varpart, Xnames = c("Runoff", "Other predictors"))


# Extract scores (Scaling 2 for correlation-focused interpretation)
sp_scores <- scores(rda_fin, display = "sp", scaling = 2)
bp_scores <- scores(rda_fin, display = "bp", scaling = 2)

# Define variables
pred_vars <- c("doc_yield_tgkm2", "peat_frac", "no3_yield_ggkm2", "tss_yield_tgkm2", "slope")
resp_vars <- c("mehg_yield_mgkm2", "hg_yield_mgkm2")

# Calculate "impact" as the cosine similarity between peat and MeHg (i.e., correlation in RDA space- how much two vectors are "pointing in the same direction.")
# This gives you a value between -1 and 1; values near 1 = Strong Positive, near -1 = Strong Negative, near 0 = No effect
# This creates a matrix where rows are predictors and columns are responses
impact_matrix <- sapply(resp_vars, function(resp) {
  sapply(pred_vars, function(pred) {
    sum(sp_scores[resp, ] * bp_scores[pred, ])
  })
})

impact_matrix

# Clean and pivot the matrix into a tidy dataframe
impact_df <- as.data.frame(impact_matrix) %>%
  # Rename columns immediately so pivot_longer works correctly
  rename('MeHg cor' = mehg_yield_mgkm2, 'HgT cor' = hg_yield_mgkm2) %>%
  # Move the row names (doc_yield, etc.) into a real column called "covariate"
  rownames_to_column(var = "covariate") %>%
  # Pivot the columns (MeHg cor, HgT cor) into one column called "estimate"
  # and put the names into a column called "response"
  pivot_longer(
    cols = c("MeHg cor", "HgT cor"),
    names_to = "response",
    values_to = "estimate"
  )

# Determine factor order (based on decreasing absolute MeHg correlation)
me_hg_order <- impact_df %>%
  filter(response == "MeHg cor") %>%
  mutate(abs_est = abs(estimate)) %>%
  arrange(desc(abs_est)) %>%
  pull(covariate)

impact_df$covariate <- factor(impact_df$covariate, levels = rev(me_hg_order))

# View the result to see the new columns
print(impact_df)

# Create labels
covariate_labs <- c(
  "doc_yield_tgkm2" = "DOC yield",
  "peat_frac"       = "Peat fraction",
  "no3_yield_ggkm2" = expression(NO[3]^"-"~yield),
  "tss_yield_tgkm2" = "TSS yield",
  "slope"           = "Watershed slope"
)

# Define shapes and colors for response vars
response_shapes <- c("HgT cor" = 22, "MeHg cor" = 21)
response_colors <- c("HgT cor" = "black", "MeHg cor" = "white")
response_labs <- c("MeHg cor" = "MeHg", "HgT cor" = "HgT")

# Create a sequence for the background gradient
grad_df <- data.frame(
  x_start = seq(-1, 0.98, by = 0.02),
  x_end = seq(-0.98, 1, by = 0.02)
)

# Define the color mapping: Blue (Negative) -> White (Zero) -> Red (Positive)
# Using muted colors so the points remain legible
back_colors <- colorRampPalette(c("#0571b0", "white", "#ca0020"))(nrow(grad_df))
grad_df$fill_col <- back_colors

# Plot
rda_cors <- ggplot() +
  
  # Scales
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0,0)) +
  scale_y_discrete(labels = covariate_labs) + # Predictor labels for Y-axis
  
  # Themes
  theme_bw() +
  theme(
    plot.margin=unit(c(0.2,0.2,0.1,0.1), "in"),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.background=element_rect(fill='white'),
    panel.border=element_rect(colour="black", fill=NA, size=1),
    plot.background=element_rect(colour="white", size=1),
    plot.title = element_text(size = 12),
    axis.title.y.left=element_text(size=10, margin=ggplot2::margin(t=0, r=10, b=0, l=0)),
    axis.text.y.left=element_text(size=8, angle=0, hjust=0.5, colour="black"),
    axis.title.x=element_text(size=10, margin=ggplot2::margin(t=10, r=0, b=0, l=0)),
    axis.text.x=element_text(size=8, angle=0, hjust=0.5, colour="black"),
    # Internal legend styling
    legend.position = c(0.02, 0.98), # Top-left coordinates (x, y)
    legend.spacing.x = unit(0.05, 'cm'),
    legend.key.size = unit(0.5, "cm"),
    legend.justification = c("left", "top"), # Anchors legend to its top-left corner
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = NA), # Background of the legend
    legend.box.background = element_rect(color = "black", linewidth = 0.5), # The border
    legend.margin = margin(1, 1, 1, 1) # Padding inside the legend box
  ) +
  
  # Background gradient
  geom_rect(data = grad_df, 
            aes(xmin = x_start, xmax = x_end, ymin = -Inf, ymax = Inf, fill = fill_col),
            alpha = 0.7) +
  scale_fill_identity() + # Tells ggplot to use the literal color strings in grad_df
  
  # Reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  
  # Data (add manually)
  geom_point(data = subset(impact_df, response == "MeHg cor"),
             aes(x = estimate, y = covariate, color = response, shape = response),
             fill = "white", size = 4, stroke = 1,
             position = position_nudge(y = 0.0)) +
  geom_point(data = subset(impact_df, response == "HgT cor"),
             aes(x = estimate, y = covariate, color = response, shape = response),
             fill = "black", size = 4, stroke = 1,
             position = position_nudge(y = 0.0)) +

  # Correcting the Legend: Order, Shapes, and Labels
  scale_color_manual(
    values = c("MeHg cor" = "black", "HgT cor" = "black"),
    breaks = c("MeHg cor", "HgT cor"), # Forces MeHg to the top
    labels = c("MeHg cor" = "MeHg", "HgT cor" = "HgT")
  ) +
  scale_shape_manual(
    values = c("MeHg cor" = 21, "HgT cor" = 22),
    breaks = c("MeHg cor", "HgT cor"), # Forces MeHg to the top
    labels = c("MeHg cor" = "MeHg", "HgT cor" = "HgT")
  ) +
  
  # Use guides to force the internal fill colors in the legend
  guides(
    color = guide_legend(override.aes = list(fill = c("white", "black")),
                         label.theme = element_text(margin = margin(l = 4, unit = "pt"))),
    shape = guide_legend(label.theme = element_text(margin = margin(l = 4, unit = "pt")))
  ) +
  labs(
    y = NULL,
    x = "Correlation in RDA space",
    title = NULL
  )

rda_cors

ggsave(
  filename = paste0(fig_export_dir, "/", "Figure 2b", " ", Sys.Date(), ".", dev_form),
  plot = rda_cors,
  device = 'pdf',
  width = 3.333*1.25, 
  height = 2.75*1.25, #2.222
  dpi = 350)





#### Plot RDA ####

# Extract biplot scores for RDA1 and RDA2 as matrix
biplot_scores <- scores(rda_fin, display = "bp", choices = c(1, 2)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "parameter") %>%
  select(parameter, RDA1, RDA2)
species_scores <- scores(rda_fin, display = "sp", choices = c(1, 2))

# Define custom labels for Hg spp
hg_spp_labs <- c("MeHg", "HgT")

# Write labels for variables that do not need special formatting
rownames(biplot_scores) # View variables for which labels will be created
biplot_scores_noparse <- biplot_scores %>%
  filter(
    parameter %in% c("doc_yield_tgkm2", "tss_yield_tgkm2", "peat_frac", "slope")
  )
pred_labs_noparse <- c("DOC", "TSS", "peatland", "slope")

# Write labels for variables that need special formatting
biplot_scores_parse <- biplot_scores %>%
  filter(
    parameter %in% c("no3_yield_ggkm2")
    )
pred_labs_parse <- expression("NO"[3]^"-")

# Get RDA scores
rda_scores <- scores(rda_fin, display = "sites", choices = c(1, 2)) %>%
  as.data.frame() %>%
  mutate(river = rda_df$river) %>% # Add the river names from your original data
  select(river, rda1 = RDA1, rda2 = RDA2) # Rename columns to lowercase to match your goal

# Summarize ranges and mean ± SE of MeHg yield and runoff
rda_scores_smry <- rda_scores %>%
  group_by(river) %>%
  summarise(
    rda1_mean = mean(rda1, na.rm = TRUE),
    rda1_min = min(rda1, na.rm = TRUE),
    rda1_max = max(rda1, na.rm = TRUE),
    rda2_mean = mean(rda2, na.rm = TRUE),
    rda2_min = min(rda2, na.rm = TRUE),
    rda2_max = max(rda2, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    river = factor(river, levels = rivs),
    river_label = ifelse(as.character(river) == "Ob", "Ob'", as.character(river))
  ) %>%
  arrange(river)

hg_rda_plot <- ggplot() +
  # Set axis limits
  scale_y_continuous(
    limits = c(-1.3, 1.7),
    breaks = seq(-1, 1.5, 0.5),
    labels = c("-1.0", "", "0.0", "", "1.0", "")
    ) +
  scale_x_continuous(
    limits = c(-1.25, 1.2),
    breaks = seq(-1, 1, 0.5),
    labels = c("-1.0", "", "0.0", "", "1.0")
  ) +
  # Themes
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
        axis.title.x=element_text(size=10, margin=ggplot2::margin(t=10, r=0, b=0, l=0)),
        axis.text.x=element_text(size=7, angle=0, hjust=0.5, colour="black"),
        legend.position="none") +
  # Add horizontal and vertical reference lines
  geom_hline(yintercept=0, lty=2, lwd=0.5, col="darkgray") +
  geom_vline(xintercept=0, lty=2, lwd=0.5, col="darkgray") +
  # Add data points
  #geom_point(data=rda_df, aes(x=rda_scores$rda1, y=rda_scores$rda2), shape=21, size=3, stroke=0.75, col="black", fill=riv_cols[rda_df$river]) +
  # Add data points (mean ± min/max)
  geom_errorbarh(data=rda_scores_smry, aes(y = rda2_mean, xmin = rda1_min, xmax = rda1_max), color = riv_cols[rda_scores_smry$river], height = 0) +
  geom_errorbar(data=rda_scores_smry, aes(x = rda1_mean, ymin = rda2_min, ymax = rda2_max), color = riv_cols[rda_scores_smry$river], width = 0) +
  geom_point(data=rda_scores_smry, aes(x=rda1_mean, y=rda2_mean), 
             shape=21, size=3, stroke=0.75, col="black", fill=riv_cols[rda_scores_smry$river]) +
  # Text
  #geom_text(aes(x=summary(rda_fin)[[1]][,1], y=summary(rda_fin)[[1]][,2], label=hg_spp_labs), size=3, col="black", fontface = "bold") +
  geom_text(data=species_scores, aes(x=RDA1, y=RDA2, label=hg_spp_labs), size=3, col="black", fontface = "bold") +
  annotate("text", x = biplot_scores_noparse$RDA1, y = biplot_scores_noparse$RDA2, label = pred_labs_noparse, size = 2.5, col = "blue", parse = TRUE) +
  annotate("text", x = biplot_scores_parse$RDA1, y = biplot_scores_parse$RDA2, label = pred_labs_parse, size = 2.5, col = "blue", parse = TRUE) +
  # Axis labels
  labs(y="Axis 2 (26.1% of total variance)", x="Axis 1 (68.4% of total variance)") +
  geom_text_repel(
    data = rda_scores_smry,
    aes(x = rda1_mean, y = rda2_mean, label = river_label),
    nudge_x = -0.04, # shift left
    nudge_y = 0.1, # shift up
    size = 2.2, # text size
    fontface = "italic",
    color = "black", #riv_cols,
    hjust = 1.12, # left-justified
    segment.color = NA, # remove connecting lines
    show.legend = FALSE
  )

ggsave(
  filename = paste0(fig_export_dir, "/", "Figure 2", " ", Sys.Date(), ".", dev_form),
  plot = hg_rda_plot,
  device = 'pdf',
  width = 3.333, 
  height = 2.75, #2.222
  dpi = 350)
