#================================================================================================================================#
# Filename: 11_responses_to_reviews.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-10-12
# Background: Code for responding to reviews
#================================================================================================================================#
# Load libraries
library(lme4)
library(lmerTest) # Satterthwaite-approximated p-values for mixed model coefficients

#------------------------------------------------------------------------------#
# RESPONSE #3 ####
#------------------------------------------------------------------------------#
loadest_qc_plot <- function(riv, param) {
  
  # Identify subfolder for river
  qc_folders <- list.dirs(file.path(root_path, paste0("Fluxes/", param)), full.names = FALSE, recursive = FALSE)
  qc_data_path <- qc_folders[grepl(riv, qc_folders, ignore.case = TRUE)]
  
  # Full file path
  res_file <- file.path(root_path, paste0("Fluxes/", param), qc_data_path, "loadest", "NoSiteGiven", paste0(param, ".res"))
  
  # Read .res file
  resids_riv <- read.table(
    file = res_file,
    header = FALSE,
    col.names = c("resid", "zscore", "estval", "lnflow", "dtime")
  )
  
  # Plotting
  label_text <- paste(riv, param)
  my_theme <- theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                    panel.grid.minor=element_blank(),
                    panel.grid.major=element_blank(),
                    plot.background=element_rect(colour="white", size=1),
                    panel.background=element_rect(fill='white'),
                    panel.border=element_rect(colour="black", fill=NA, size=1),
                    text=element_text(size=10),
                    axis.text.y=element_text(angle=0, hjust=0.5, colour="black"),
                    axis.title.y.left=element_text(margin=margin(t=0, r=5, b=0, l=0)),
                    axis.text.x=element_text(angle=0, hjust=0.5, colour="black"),
                    axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0)),
                    plot.title = element_text(hjust = 0.5, face = "bold"))
  my_point <- geom_point(shape = 21, fill = "black", color = "white", size = 4)
  my_ann <- annotate("text", x = -Inf, y = Inf, label = label_text, hjust = -0.05, vjust = 1.5, size = 3, fontface = "plain")
  
  # Plot A: Residual vs Estimated
  p1 <- ggplot(resids_riv, aes(x = estval, y = resid)) +
    my_theme +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    my_point + 
    my_ann +
    labs(x = "Estimated value", y = "Model residual")
  
  # Plot B: Residual vs Z-score
  p2 <- ggplot(resids_riv, aes(x = zscore, y = resid)) +
    my_theme +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    my_point +
    my_ann +
    labs(x = "Normal quantile (Z score)", y = "Model residual")
  
  # Plot C: Residual vs log(discharge) -- the actual high-flow bias diagnostic.
  # LOESS smooth shows whether the mean residual drifts away from 0 as flow increases.
  p3 <- ggplot(resids_riv, aes(x = lnflow, y = resid)) +
    my_theme +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    my_point +
    geom_smooth(method = "loess", se = TRUE, color = "firebrick", fill = "firebrick", alpha = 0.15, linewidth = 0.7) +
    my_ann +
    labs(x = "log(Discharge)", y = "Model residual")
  
  # Quantitative check: is there a significant relationship between
  # residual magnitude/sign and flow, and specifically at the high-flow tail
  # (top quartile of lnflow, where storm/snowmelt pulses concentrate)?
  resids_riv$flow_q <- ntile(resids_riv$lnflow, 4)
  
  overall_cor <- cor.test(resids_riv$resid, resids_riv$lnflow, method = "spearman")
  high_flow <- resids_riv[resids_riv$flow_q == 4, ]
  high_flow_t <- t.test(high_flow$resid, mu = 0)
  
  qc_stats <- list(
    n = nrow(resids_riv),
    overall_spearman_rho = unname(overall_cor$estimate),
    overall_spearman_p = overall_cor$p.value,
    high_flow_n = nrow(high_flow),
    high_flow_mean_resid = mean(high_flow$resid),
    high_flow_ttest_p = high_flow_t$p.value
  )
  
  return(list(p1 = p1, p2 = p2, p3_resid_vs_flow = p3, qc_stats = qc_stats))
}

# --- Batch runner across rivers/params, with a plain-language summary ---
loadest_highflow_bias_check <- function(rivers, params, alpha = 0.05) {
  results <- list()
  for (riv in rivers) {
    for (param in params) {
      key <- paste(riv, param, sep = "_")
      out <- tryCatch(loadest_qc_plot(riv, param), error = function(e) NULL)
      if (is.null(out)) next
      s <- out$qc_stats
      flag <- (s$overall_spearman_p < alpha) | (s$high_flow_ttest_p < alpha)
      results[[key]] <- data.frame(
        river = riv, param = param, n = s$n,
        spearman_rho = round(s$overall_spearman_rho, 3),
        spearman_p = round(s$overall_spearman_p, 4),
        high_flow_mean_resid = round(s$high_flow_mean_resid, 4),
        high_flow_p = round(s$high_flow_ttest_p, 4),
        flagged_high_flow_bias = flag
      )
    }
  }
  summary_df <- do.call(rbind, results)
  rownames(summary_df) <- NULL
  summary_df
}

# Example usage:
# rivers <- c("Kolyma", "Lena", "Mackenzie", "Ob", "Yenisey", "Yukon")
# params <- c("MeHg", "HgT", "DOC", "NO3", "TSS")
# bias_summary <- loadest_highflow_bias_check(rivers, params)
# print(bias_summary)
# any(bias_summary$flagged_high_flow_bias)  # TRUE if any river/param shows high-flow bias

bias_summary <- loadest_highflow_bias_check(
  c("Kolyma", "Lena", "Mackenzie", "Ob", "Yenisey", "Yukon"),
  c("MeHg", "HgT", "DOC", "NO3", "TSS")
)

stopifnot(nrow(bias_summary) == 30) # 6 rivers x 5 constituents; fails loudly if any run was skipped

print(bias_summary)
any(bias_summary$flagged_high_flow_bias)

#write_xlsx(bias_summary, file.path(tables_export_dir, "bias_summary.xlsx"))


#--------------------------------------------------------------#
# RESPONSE #15 ####
#--------------------------------------------------------------#
# Season definitions follow Methods Sec. 2.1:
#   winter = Nov-Apr, spring/freshet = May-Jun, summer = Jul-Oct
daily_flux_seas <- daily_flux %>%
  mutate(
    month = lubridate::month(date),
    season = case_when(
      month %in% c(5, 6)               ~ "spring",
      month %in% c(7, 8, 9, 10)        ~ "summer",
      month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
      TRUE ~ NA_character_
    )
  )

# Annual and spring-only flux sums per river-year, and spring export fractions
seas_flux <- daily_flux_seas %>%
  group_by(river, year) %>%
  summarise(
    annual_mehg_kg = sum(mehg_flux, na.rm = TRUE),
    annual_doc_tg  = sum(doc_flux,  na.rm = TRUE),
    annual_tss_tg  = sum(tss_flux,  na.rm = TRUE),
    spring_mehg_kg = sum(mehg_flux[season == "spring"], na.rm = TRUE),
    spring_doc_tg  = sum(doc_flux[season == "spring"],  na.rm = TRUE),
    spring_tss_tg  = sum(tss_flux[season == "spring"],  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mehg_spring_frac = spring_mehg_kg / annual_mehg_kg,
    doc_spring_frac  = spring_doc_tg  / annual_doc_tg,
    tss_spring_frac  = spring_tss_tg  / annual_tss_tg
  ) %>%
  filter(is.finite(mehg_spring_frac), is.finite(doc_spring_frac), is.finite(tss_spring_frac))

# Check replication per river before doing anything else -- this determines
# which rivers can support within-river inference (need n_years >= 2 at minimum,
# and meaningfully more than 2 to trust a within-river slope estimate)
seas_flux %>% count(river)

dim(seas_flux); head(seas_flux); summary(seas_flux)


# PRIMARY ANALYSIS: within-river covariation, well-replicated rivers only ####
# Only Yukon and Mackenzie have adequate within-river seasonal replication
# (n = 5 years each) to support a river-level random-intercept model.
# Ob, Yenisey, Lena, and Kolyma have only 2 years each and are excluded --
# with only 2 points per river, "within-river slope" is not meaningfully
# distinguishable from a line connecting two points (see diagnostic section below).

seas_flux_rep <- seas_flux %>%
  filter(river %in% c("Yukon", "Mackenzie")) %>%
  droplevels()

stopifnot(all(table(seas_flux_rep$river) == 5))  # sanity check on replication

# MeHg ~ DOC, river-level random intercept
m_doc_rep <- lmer(mehg_spring_frac ~ doc_spring_frac + (1 | river), data = seas_flux_rep)
summary(m_doc_rep)
isSingular(m_doc_rep)

# MeHg ~ TSS, river-level random intercept
m_tss_rep <- lmer(mehg_spring_frac ~ tss_spring_frac + (1 | river), data = seas_flux_rep)
summary(m_tss_rep)
isSingular(m_tss_rep)  # expect TRUE: river random-effect variance collapses to ~0
# for TSS, meaning within-river structure isn't
# distinguishable from a shared regression across the
# two rivers -- report this as a caveat on the TSS result


# DIAGNOSTIC / SUPPLEMENTARY: why the full six-river pooled test is not reported ####
# These steps informed the decision to restrict the primary analysis to
# Yukon/Mackenzie; kept here for transparency but NOT reported as results.

# Demeaned (within-river) values for all six rivers, for visual diagnosis
seas_flux_demeaned <- seas_flux %>%
  group_by(river) %>%
  mutate(
    mehg_spring_frac_dm = mehg_spring_frac - mean(mehg_spring_frac, na.rm = TRUE),
    doc_spring_frac_dm  = doc_spring_frac  - mean(doc_spring_frac,  na.rm = TRUE),
    tss_spring_frac_dm  = tss_spring_frac  - mean(tss_spring_frac,  na.rm = TRUE),
    n_years = n()
  ) %>%
  ungroup()

# Visual check: rivers with n_years = 2 (Ob, Yenisey, Lena, Kolyma) each
# contribute only a single line segment and have outsized leverage on any
# pooled within-river correlation -- see esp. Lena, which spans nearly the
# full range of both axes on just 2 points.
ggplot(seas_flux_demeaned, aes(x = doc_spring_frac_dm, y = mehg_spring_frac_dm, color = river)) +
  geom_point(size = 3) +
  geom_line(aes(group = river), alpha = 0.3) +
  theme_minimal()

# Naive pooled mixed model across all six rivers (for internal comparison only --
# inflated by low-replication rivers, not reported in response letter)
m_doc_all <- lmer(mehg_spring_frac ~ doc_spring_frac + (1 | river), data = seas_flux)
m_tss_all <- lmer(mehg_spring_frac ~ tss_spring_frac + (1 | river), data = seas_flux)
summary(m_doc_all); isSingular(m_doc_all)
summary(m_tss_all); isSingular(m_tss_all)

# Naive demeaned cor.test (invalid inference -- treats 18 river-years as
# independent despite only 6 river-level means removed and 4 rivers
# contributing just 2 points each; p-values are anticonservative)
cor.test(seas_flux_demeaned$mehg_spring_frac_dm, seas_flux_demeaned$doc_spring_frac_dm, method = "spearman")
cor.test(seas_flux_demeaned$mehg_spring_frac_dm, seas_flux_demeaned$tss_spring_frac_dm, method = "spearman")

# Between-river only (one averaged value per river, n = 6) -- weak/non-significant,
# not reported as primary result but useful context
seas_flux_byriver <- seas_flux %>%
  group_by(river) %>%
  summarise(
    mehg_spring_frac = mean(mehg_spring_frac, na.rm = TRUE),
    doc_spring_frac  = mean(doc_spring_frac,  na.rm = TRUE),
    tss_spring_frac  = mean(tss_spring_frac,  na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

cor.test(seas_flux_byriver$mehg_spring_frac, seas_flux_byriver$doc_spring_frac, method = "spearman")
cor.test(seas_flux_byriver$mehg_spring_frac, seas_flux_byriver$tss_spring_frac, method = "spearman")


#------------------------------------------------------------------------------#
# RESPONSE #38 ####
#------------------------------------------------------------------------------#
# Linear model
mehg_hgt_lm <- lm( log(mehg_yield_mgkm2) ~ log(hg_yield_mgkm2), data=ylds)
par(mfrow=c(2,2))
plot(mehg_hgt_lm)
summary(mehg_hgt_lm)

# From comparison, try LMER
mehg_hgt_lmer <- lmer( log(mehg_yield_mgkm2) ~ log(hg_yield_mgkm2) + (1|river), data=ylds)

# QC model
par(mfrow=c(1,1))
plot(mehg_hgt_lmer)

# Summarize model
summary(mehg_hgt_lmer)

# Extract observed values (log-transformed)
mehg_hgt_lmer_obs <- log(ylds$mehg_yield_mgkm2)

# Extract predicted values from the lmer model
mehg_hgt_lmer_pred <- predict(mehg_hgt_lmer)

# Fit linear model of observed vs. predicted and view summary/R-squared
mehg_hgt_obs_pred_lm <- lm(mehg_hgt_lmer_obs ~ mehg_hgt_lmer_pred)
summary(mehg_hgt_obs_pred_lm)$r.squared

# Create the scatterplot frame and points
par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
plot(mehg_hgt_lmer_obs, mehg_hgt_lmer_pred, 
     pch = 21, 
     bg = riv_cols, 
     col = "black", 
     cex = 1.8,
     xlab = "Observed log(MeHg yield)", 
     ylab = "Predicted log(MeHg yield)"
)

# Add a 1:1 reference line (it will be drawn underneath the points)
abline(a = 0, b = 1, col = "gray40", lty = 2, lwd = 1.5)

# Re-add the points so they sit cleanly on top of the line
points(mehg_hgt_lmer_obs, mehg_hgt_lmer_pred, 
       pch = 21, 
       bg = riv_cols, 
       col = "black", 
       cex = 1.8)
