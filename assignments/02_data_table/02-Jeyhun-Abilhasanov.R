# ---------------------------------------
# Assignment 2: Hydrological Analysis using data.table
# Author: Jeyhun Abilhasanov (xabij001)
# Date: May 2025
# ---------------------------------------

# Load required library
library(data.table)

# --- Load the dataset ---
data <- fread("runoff _data.csv")  # Be cautious of the space in the filename

# --- PART 1: Hydrological Year and Runoff Coefficients ---

# Task 1: Assign Hydrological Year (HYR)
# Water year starts in October: Oct-Dec belong to next calendar year
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Task 2: Calculate Total Precipitation, Total Runoff, and Runoff Coefficient (RC) for each catchment
rc_summary <- data[, .(
  total_precipitation = sum(PRCP, na.rm = TRUE),
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
), by = ID][, RC := total_runoff / total_precipitation]

# Task 3: Classify Catchments by Runoff Coefficient

# Merge RC values back into main dataset
data <- merge(data, rc_summary[, .(ID, RC)], by = "ID", all.x = TRUE)

# Create RC classification using quantile-based bins
data[, RC_class := cut(
  RC,
  breaks = quantile(RC, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]

# Randomly select one representative catchment per RC class
selected_catchments <- data[, .SD[sample(.N, 1)], by = RC_class]
selected_ids <- unique(selected_catchments$ID)

# --- PART 2: Water Balance and Snowmelt Contribution ---

# Filter only selected catchments for further analysis
filtered_data <- data[ID %in% selected_ids]

# Task 4: Water Balance Calculations

# Monthly water balance
monthly_balance <- filtered_data[, .(
  PRCP = mean(PRCP, na.rm = TRUE),
  PET = mean(PET, na.rm = TRUE),
  OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

# Calculate water balance (Precipitation - Evapotranspiration)
monthly_balance[, WB := PRCP - PET]

# Identify deficit months (where PET > PRCP)
monthly_balance[, Deficit := WB < 0]

# Annual water balance per catchment
annual_balance <- monthly_balance[, .(
  PRCP = sum(PRCP, na.rm = TRUE),
  PET = sum(PET, na.rm = TRUE),
  OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
  WB = sum(WB, na.rm = TRUE)
), by = .(HYR, ID)]

# Task 5: Snowmelt Contribution to Runoff

# Step 1: Average SWE per month, year, and catchment
swe_summary <- filtered_data[, .(mean_SWE = mean(SWE, na.rm = TRUE)), by = .(HYR, ID, MNTH)]

# Step 2: Maximum SWE for each hydrological year and catchment
swe_max <- swe_summary[, .(max_SWE = max(mean_SWE, na.rm = TRUE)), by = .(HYR, ID)]

# Step 3: Calculate snowmelt as difference between max SWE and current monthly SWE
swe_merged <- merge(swe_summary, swe_max, by = c("HYR", "ID"))
swe_merged[, snowmelt := max_SWE - mean_SWE]

# Step 4: Merge with observed runoff for spring months (March–May)
spring_data <- merge(
  swe_merged[MNTH %in% 3:5],
  monthly_balance[MNTH %in% 3:5, .(HYR, ID, MNTH, OBS_RUN)],
  by = c("HYR", "ID", "MNTH")
)

# Step 5: Correlation analysis between snowmelt and runoff (per catchment)
snowmelt_correlation <- spring_data[, .(
  cor_snowmelt_runoff = cor(snowmelt, OBS_RUN, use = "complete.obs")
), by = ID]

# --- Optional: Save Output Files ---
# fwrite(rc_summary, "runoff_coefficients.csv")
# fwrite(selected_catchments, "selected_catchments.csv")
# fwrite(monthly_balance, "monthly_balance.csv")
# fwrite(annual_balance, "annual_balance.csv")
# fwrite(swe_merged, "snowmelt_data.csv")
# fwrite(snowmelt_correlation, "snowmelt_runoff_correlations.csv")

# --- Preview: Show Correlation Results ---
print(snowmelt_correlation)
