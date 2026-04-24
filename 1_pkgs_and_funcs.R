#================================================================================================================================#
# Filename: 1_pkgs_and_funcs.R
# Author(s): Scott Zolkos (sgzolkos@gmail.com)
# Revised: 2026-01-29
# Background: Prepare working environment
#================================================================================================================================#

#----------------#
#### PACKAGES ####
#----------------#
# Install R package management tool (pacman), as needed
if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}

# Load necessary packages
pacman::p_load(broom, # functionality | 'tidy' up prediction outputs
               dplyr, # functionality
               foreign, # functionality | for 'read.dbf'
               here, # functionality | file referencing
               #purrr, # functionality
               janitor, # functionality | cleaning & examining data
               lubridate, # functionality | date manipulation
               outliers, # functionality | for outlier()
               readxl, # functionality | read files in .xls or .xlsx format
               readr, # functionality | 
               conflicted, # functionality | set conflicting packages
               reshape2, # functionality | data manipulation
               tidyverse, # functionality
               writexl, # functionality | write files in .xls or .xlsx format
               zoo, # functionality | time series structuring
               car, # stats | 'Anova' function
               dunn.test, # stats | pairwise multiple comparison for post-hoc Kruskal-Wallis (sig different medians)
               MASS, # stats | for boxcox()
               psych, # stats | for corr.test()
               vegan, # stats | for rda()
               cowplot, # plotting | Arranging plots
               ggrepel, # plotting | label placement
               grid,
               gridExtra, # plotting | Arrange plots in a grid (optional, for display purposes)
               patchwork # plotting | multi-panel
               ) 

# Conflicts (set which packages to use for specific functions in this session, if function appears across multiple packages)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("ungroup", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("tibble", "dplyr")
#conflict_prefer("lag", "dplyr")

#-----------------#
#### FILEPATHS ####
#-----------------#
# Set location of root filepath directory
root_path <- here("data")

# Set location of filepath for exporting tables
tables_export_dir <- file.path(root_path, "..", "..", "Manuscript", "tables")

# Set location of filepath for exporting figures
fig_export_dir <- file.path(root_path, "..", "..", "Manuscript", "figures")
dev_form <- "pdf"

#-------------------#
#### DEFINITIONS ####
#-------------------#
# Rivers & projects
rivs <- c("Ob", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")
ag_projects <- c("PARTNERS", "ArcticGRO I", "ArcticGRO II", "ArcticGRO III", "ArcticGRO IV")

# Seasons
winter <- c(11,12,1,2,3,4)
summer <- c(5:10)

# Graphics & plotting
riv_cols <- c("red2","yellow2","green3","purple3","orange2","blue")
riv_cols <- c("Ob" = "red2", "Yenisey" = "yellow2", "Lena" = "green3", 
              "Kolyma" = "purple3", "Yukon" = "orange2", "Mackenzie" = "blue")
riv.labs <- c("Ob'", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")

#-----------------#
#### FUNCTIONS ####
#-----------------#
# Function for numerical y-axis labels, no commas, no extraneous decimal places ####
## Source: https://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot
NotFancy <- function(l) {
  l <- format(l, scientific = FALSE)
  parse(text=l)
}

# Export concentration and daily discharge data in LOADEST format for selected river ####
# Define chem_var (one of: "HgT", "MeHg", "DOC, "TSS", "NO3", "SO4")
loadest_export <- function(chem_var){
  
  # Source compiled ArcticGRO ancillary chemistry data (e.g., DOC, TSS)
  source(paste0(getwd(),"/", "4_ag_chem.R"))
  #ag_chem %>% select(river, date, tss, doc, no3, so4)
  
  # LOADEST root filepath
  loadest_root <- file.path(root_path, "Fluxes/")
    
  # Define output directory path
  out_dir <- file.path(paste0(loadest_root, chem_var))
  
  # Create folder if it doesn't exist
  if (!dir.exists(out_dir)) {
    dir.create(paste0(out_dir,"/"), recursive = TRUE)
  }
  
  # Define variable- and code-specific info (https://help.waterdata.usgs.gov/codes-and-parameters/parameters)
  chem_map <- list(
    HgT   = list(chem_df = "hg_df", col = "hgt", code = "p50286"),
    MeHg = list(chem_df = "hg_df", col = "mehg", code = "p50284"),
    DOC = list(chem_df = "ag_chem", col = "doc", code = "p00681"),
    TSS = list(chem_df = "ag_chem", col = "tss", code = "p00530"),
    NO3 = list(chem_df = "ag_chem", col = "no3", code = "p00618"),
    SO4 = list(chem_df = "ag_chem", col = "so4", code = "p00550")
  )
  
  # Ensure chem_var is recognized
  if (!chem_var %in% names(chem_map)) {
    stop(paste("Variable ", chem_var, " not found in chem_map!"))
  }
  
  # Extract metadata
  df_name <- chem_map[[chem_var]]$chem_df
  col_name <- chem_map[[chem_var]]$col
  code_name <- chem_map[[chem_var]]$code
  
  # Get the actual dataframe by name
  chem_df <- get(df_name)
  
  # Loop over rivers
  for(i in rivs){
    
    # If the river is Mackenzie or Yukon, keep dates up to 2023-12-31; for all other rivers, keep dates up to 2021-12-31
    date_end <- if (i %in% c("Mackenzie", "Yukon")) as.Date("2024-12-31") else as.Date("2021-12-31")
    
    # Chemistry data
    df_out <- chem_df %>%
      filter(
        river == i,
        date >= as.Date("2020-01-01"),
        date <= date_end
      ) %>%
      select(date, !!sym(col_name)) %>%
      rename(
        sample_dt = date,
        !!code_name := !!sym(col_name)
      ) %>%
      na.omit()
    
    # Write output
    write.table(df_out,
                file.path(out_dir, paste0(i, "_", chem_var, ".txt")), 
                sep = "\t", 
                row.names = FALSE, 
                quote = FALSE)
    
    # Discharge data
    write.table(
      q_dat %>%
        mutate(
          q_cfs = round(q_cms*35.3147248276,5) # convert m3/s to ft3/s and round to n decimal places
        ) %>%
        # Filter range of Q data, depending on river
        filter(
          river == i,
          date >= as.Date("2020-01-01"),
          date <= date_end
        ) %>%
        select(date, q_cfs) %>%
        rename(datetime = date, '01_00060_00003' = q_cfs),
      file.path(out_dir, paste0(i, "_Q.txt")), 
      sep = "\t",
      row.names = FALSE, 
      quote = FALSE
    )
  }
}

# Import LOADEST modeled estimates of conc and flux ####
loadest_read_outputs <- function(chem_var) {
  
  # Setup and checks
  if (!exists("rivs")) stop("Object 'rivs' not found in environment.")
  
  # Base path to LOADEST outputs
  chem_dir <- file.path(root_path, paste0("Fluxes/", chem_var))
  
  if (!dir.exists(chem_dir)) stop(paste("No folder found for", chem_var))
  
  # Initialize results
  output_df <- tibble()
  
  # Loop through rivers
  for (i in rivs) {
    
    # List all subfolders and find the most recent model for this river and chem_var
    all_dirs <- list.dirs(path = chem_dir, recursive = FALSE, full.names = TRUE)
    river_dirs <- all_dirs[grepl(paste0(i, " ", chem_var, " model"), basename(all_dirs))]
    
    if (length(river_dirs) == 0) {
      warning(paste("No model folder found for", i, chem_var))
      next
    }
    
    # Identify the *most recent* run by date prefix
    model_folder <- river_dirs[which.max(as.numeric(substr(basename(river_dirs), 1, 8)))]
    filepath <- file.path(model_folder, "outputs", paste0("allsite_", chem_var, "_flux.txt"))
    
    if (!file.exists(filepath)) {
      warning(paste("Missing output file for", i, chem_var))
      next
    }
    
    # Read and process data
    temp_df <- read.delim(filepath, skip = 20, stringsAsFactors = FALSE) %>%
      setNames(c(
        "site", "date", "flow", "amle_flux", "mle_flux", "lad_flux",
        "amle_conc", "mle_conc", "lad_conc", "caveat"
      )) %>%
      select(date, flow, amle_flux, amle_conc) %>%
      mutate(
        river = i,
        date = as.Date(as.character(date), format = "%m/%d/%Y"),
        flow = as.numeric(gsub(",", "", flow)),
        q_ld = flow * 0.0283168 * 1000 * 86400, # convert ft³/s to L/d
        amle_flux = as.numeric(gsub(",", "", amle_flux)) / 1e6, # convert mg/d to kg/d
        conc = as.numeric(gsub(",", "", amle_conc)),
        flux = amle_flux
      ) %>%
      select(river, date, q_ld, conc, flux)
    
    # Append results
    output_df <- bind_rows(output_df, temp_df)
  }
  
  # Finalize
  output_df <- output_df %>%
    mutate(
      river = factor(river, levels = rivs)
    )
  
  return(output_df)
}

# Organize and plot LOADEST modeled estimates of defined constituent (parameter) ####
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
                    axis.title.y.left=element_text(margin=margin(t=0, r=5, b=0, l=0)), # adjust y-axis title distance from plot area
                    axis.text.x=element_text(angle=0, hjust=0.5, colour="black"),
                    axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0)), # adjust x-axis title distance from plot area
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
  
  return(list(p1, p2))
}

# Compile measured daily discharge and LOADEST modeled daily estimates of constituent concentration and flux ####
loadest_daily_data <- function(riv, param) {

  # Identify folder containing river name
  folders <- list.dirs(file.path(root_path, paste0("Fluxes/", param)), full.names = FALSE, recursive = FALSE)
  data_path <- folders[grepl(riv, folders, ignore.case = TRUE)]
  
  if (length(data_path) != 1) {
    stop(paste("Could not uniquely identify folder for river:", riv))
  }
  
  file_path <- file.path(root_path, paste0("Fluxes/", param), data_path, "outputs", paste0("NoSiteGiven_", param, "_flux.txt"))
  
  # Read and clean data
  flux_data <- read.delim(file_path, skip = 19) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    select(date = Date, Flow, AMLE.Load, AMLE.Conc) %>%
    mutate(
      AMLE.Conc = as.numeric(gsub(",", "", AMLE.Conc)), # Convert to numeric
      q_km3d = as.numeric(gsub(",", "", Flow)) * 86400 * 0.0283168 * 1e-9  # Convert ft³/s to km³/day
    )
  
  # Convert units based on parameter
  param_lower <- tolower(param)
  
  if (param_lower %in% c("mehg", "mehgd", "mehgp", "hgt", "hgd", "hgp")) {
    # Mercury: convert flux units from mg/day to kg/day
    flux_data <- flux_data %>%
      mutate(
        AMLE.Load = as.numeric(gsub(",", "", AMLE.Load)) * 1e-6
      )
  } else if (param_lower %in% c("tss", "doc", "so4", "no3")) {
    # TSS, DOC, SO4: convert flux units from kg/day to Tg/day
    # NO3: convert flux units from g/day to Gg/day
    flux_data <- flux_data %>%
      mutate(
        AMLE.Load = as.numeric(gsub(",", "", AMLE.Load)) * 1e-9
      )
  } else {
    stop(paste("Parameter not recognized for conversion:", param))
  }
  
  # Rename columns dynamically
  conc_name <- paste0(param_lower, "_conc")
  flux_name <- paste0(param_lower, "_flux")
  
  flux_data <- flux_data %>%
    select(date, q_km3d, AMLE.Conc, AMLE.Load) %>%
    rename(
      !!conc_name := AMLE.Conc,
      !!flux_name := AMLE.Load
    ) %>%
    mutate(
      year = year(date),
      river = riv
    ) %>%
    select(river, year, date, q_km3d, ends_with("_conc"), ends_with("_flux"))
  
  return(flux_data)
  
  # Clean Global Environment
  rm(riv, param, root_path, folders, data_path, file_path, flux_data, param_lower, conc_name, flux_name)
  
}

# Calculate total seasonal discharge and compile estimates of seasonal mean constituent concentration and total constituent flux ####
loadest_seasonal_data <- function(riv, param) {
  
  # Get daily data from your function
  daily_df <- loadest_daily_data(riv, param) %>%
    mutate(
      year = year(date),
      month = month(date),
      # Assign season
      season = case_when(
        month %in% 5:6  ~ "spr",
        month %in% 7:10 ~ "smr",
        month %in% c(11, 12) ~ "win",
        month %in% 1:4 ~ "win"
      ),
      # Shift winter to following year
      season_year = if_else(month %in% 11:12, year + 1, year)
    )
  
  # Dynamically identify column names
  conc_col <- grep("_conc$", names(daily_df), value = TRUE)
  flux_col <- grep("_flux$", names(daily_df), value = TRUE)
  
  # Summarise seasonally
  seasonal_df <- daily_df %>%
    group_by(season_year, season) %>%
    summarise(
      conc = mean(.data[[conc_col]], na.rm = TRUE),
      conc_sd = sd(.data[[conc_col]], na.rm = TRUE),
      flux = sum(.data[[flux_col]], na.rm = TRUE),
      flux_sd = sd(.data[[flux_col]], na.rm = TRUE),
      q_km3 = sum(q_km3d, na.rm = TRUE),
      q_km3_sd = sd(q_km3d, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(year = season_year) %>%
    mutate(
      param = param,
      river = riv,
      season = factor(season, levels = c("win", "spr", "smr"))
    ) %>%
    select(river, param, year, season, q_km3, q_km3_sd, everything())
  
  return(seasonal_df)
}

# 
mmnt_test <- function(riv, param) {
  # Identify folder containing river name
  folders <- list.dirs(file.path(root_path, paste0("Fluxes/", param)), full.names = FALSE, recursive = FALSE)
  data_path <- folders[grepl(riv, folders, ignore.case = TRUE)]
  file_path <- file.path(root_path, paste0("Fluxes/", param), data_path, "inputs", paste0(riv, "_", param, ".txt"))
}

# Function to test normality of each variable in a df and conditionally apply the best ####
## transformation (none, log, sqrt, box-cox) as defined by highest resulting p-value from Shapiro-Wilk test ('shapiro_test')
test_and_transform <- function(dft){
  
  num_cols <- dft %>%
    select(-river) %>%
    select(where(is.numeric))
  
  transformation_results <- data.frame(
    variable = character(),
    transformation = character(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (coln in colnames(num_cols)) {
    
    dat_orig <- num_cols[[coln]]
    
    # Test original data
    p_none <- ifelse(length(dat_orig) <= 5000,
                     shapiro.test(dat_orig)$p.value,
                     NA)
    
    p_values <- c(none = p_none)
    
    # Prepare shifted version ONLY for transforms that require positivity
    if (any(dat_orig <= 0, na.rm = TRUE)) {
      min_val <- min(dat_orig, na.rm = TRUE)
      dat_pos <- dat_orig + abs(min_val) + 0.001
    } else {
      dat_pos <- dat_orig
    }
    
    # Log transform
    log_transformed <- log(dat_pos)
    p_values["log"] <- shapiro.test(log_transformed)$p.value
    
    # Sqrt transform
    sqrt_transformed <- sqrt(dat_pos)
    p_values["sqrt"] <- shapiro.test(sqrt_transformed)$p.value
    
    # Choose transformation
    if (p_values["none"] <= 0.05) {
      best_transformation <- names(p_values)[which.max(p_values)]
    } else {
      best_transformation <- "none"
    }
    
    # Apply ONLY the chosen transformation
    best_data <- switch(
      best_transformation,
      none = dat_orig,
      log  = log_transformed,
      sqrt = sqrt_transformed
    )
    
    # Replace column
    dft[[coln]] <- best_data
    
    # Store transformation as attribute
    attr(dft[[coln]], "transformation") <- best_transformation
    
    # Save summary
    transformation_results <- rbind(
      transformation_results,
      data.frame(
        variable = coln,
        transformation = best_transformation,
        p_value = max(p_values, na.rm = TRUE)
      )
    )
  }
  
  list(
    transformed_df = dft,
    transformation_results = transformation_results
  )
}

test_and_transform_old <- function(dft){
  
  # Select numeric columns only, starting from the 4th column
  num_cols <- dft %>%
    select(-river) %>%
    select(where(is.numeric))
  
  # Initialize results
  transformation_results <- list()
  
  # Iterate through each numeric column
  for (coln in colnames(num_cols)) {
    
    # Store data
    dat2 <- num_cols[[coln]]
    
    # Check if the column has any negative values
    if (any(dat2 <= 0, na.rm = TRUE)) {
      # Add a small constant to ensure all values are positive
      min_val <- min(dat2, na.rm = TRUE)
      dat2 <- dat2 + abs(min_val) + 0.001
    }
    
    # Test original data
    p_values <- c(none = ifelse(length(dat2) <= 5000, shapiro.test(dat2)$p.value, NA))
    
    # Log transformation
    log_transformed <- log(dat2)
    p_values["log"] <- ifelse(length(log_transformed) <= 5000, shapiro.test(log_transformed)$p.value, NA)
    
    # Square root transformation
    sqrt_transformed <- sqrt(dat2)
    p_values["sqrt"] <- ifelse(length(sqrt_transformed) <= 5000, shapiro.test(sqrt_transformed)$p.value, NA)
    
    # Box-Cox transformation
    bc_lm <- lm(y ~ 1, data = data.frame(y = dat2))
    boxcox_lambda <- boxcox(bc_lm, lambda = seq(-2, 2, 0.1))
    best_lambda <- boxcox_lambda$x[which.max(boxcox_lambda$y)]
    if (!is.na(best_lambda)) {
      boxcox_transformed <- (dat2^best_lambda - 1) / best_lambda
      p_values["boxcox"] <- ifelse(length(boxcox_transformed) <= 5000, shapiro.test(boxcox_transformed)$p.value, NA)
    }
    
    # Compare p-values and select the highest only if the original p-value <= 0.05
    if (p_values["none"] <= 0.05) {
      best_transformation <- names(p_values)[which.max(p_values)]
      best_data <- list(none = dat2, log = log_transformed, sqrt = sqrt_transformed, boxcox = boxcox_transformed)[[best_transformation]]
      
      # Save results
      transformation_results[[coln]] <- list(
        transformation = best_transformation,
        p_value = max(p_values, na.rm = TRUE)
      )
      
      # Replace the column in the dataframe with the best-transformed data
      dft[[coln]] <- best_data
    } else {
      transformation_results[[coln]] <- list(
        transformation = "none",
        p_value = p_values["none"]
      )
      
      # No transformation needed, keep original data
      dft[[coln]] <- num_cols[[coln]]
    }
  }
  
  # Return transformed dataframe and results
  list(
    transformed_df = dft,
    transformation_results = transformation_results
  )
}