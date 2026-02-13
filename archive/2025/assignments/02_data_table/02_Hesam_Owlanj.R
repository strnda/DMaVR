# -------------------------------
# Assignment 2: Hydrological Analysis with data.table
# Author: Hesam Owlanj (xowlh001)
# Date: April 2025
# -------------------------------

# Load data.table
library(data.table)

# --- Loading dataset

dta <- fread("runoff _data.csv")

# --- PART 1: Hydrological Years and Runoff Coefficients ---

# Task 1: Assigning Hydrological Year (HYR)
dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Verify shift (optional)
# table(dta[MNTH %in% c(10,11,12), .(YR, HYR)])

# Task 2: Compute Overall Runoff Coefficient per Catchment
rc_table <- dta[, .(
  total_prcp = sum(PRCP, na.rm = TRUE),
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
), by = ID][, RC := total_runoff / total_prcp]

# Task 3: Classify Catchments Based on RC
# Add RC to main dataset
dta <- merge(dta, rc_table[, .(ID, RC)], by = "ID", all.x = TRUE)

# Create RC classes
dta[, RC_class := cut(
  RC,
  breaks = quantile(RC, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]

# Randomly select one catchment per class
selected_catchments <- dta[, .SD[sample(.N, 1)], by = RC_class]
selected_ids <- unique(selected_catchments$ID)

# --- PART 2: Water Balance and Snowmelt Contribution ---

# Filter only selected catchments
dta_selected <- dta[ID %in% selected_ids]

# Task 4: Monthly and Annual Water Balance
# Monthly water balance per catchment and year
monthly_balance <- dta_selected[, .(
  PRCP = mean(PRCP, na.rm = TRUE),
  PET = mean(PET, na.rm = TRUE),
  OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

monthly_balance[, WB := PRCP - PET]  # Water Balance

# Identify months in deficit
monthly_balance[, Deficit := WB < 0]

# Annual water balance
annual_balance <- monthly_balance[, .(
  PRCP = sum(PRCP, na.rm = TRUE),
  PET = sum(PET, na.rm = TRUE),
  OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
  WB = sum(WB, na.rm = TRUE)
), by = .(HYR, ID)]

# Task 5: Snowmelt Contribution to Runoff

# Step 1: Mean monthly SWE per HYR and ID
snow_data <- dta_selected[, .(mean_SWE = mean(SWE, na.rm = TRUE)), by = .(HYR, ID, MNTH)]

# Step 2: Find max SWE per HYR
max_swe <- snow_data[, .(max_SWE = max(mean_SWE, na.rm = TRUE)), by = .(HYR, ID)]

# Step 3: Merge and compute snowmelt: max SWE - current month SWE
snow_data <- merge(snow_data, max_swe, by = c("HYR", "ID"))
snow_data[, snowmelt := max_SWE - mean_SWE]

# Step 4: Merge with observed runoff
spring_data <- merge(
  snow_data[MNTH %in% 3:5], 
  monthly_balance[MNTH %in% 3:5, .(HYR, ID, MNTH, OBS_RUN)], 
  by = c("HYR", "ID", "MNTH")
)

# Step 5: Correlation analysis (by catchment)
cor_results <- spring_data[, .(cor_snowmelt_runoff = cor(snowmelt, OBS_RUN, use = "complete.obs")), by = ID]

# --- OUTPUTS (optional: write to file) ---
# fwrite(rc_table, "runoff_coefficients.csv")
# fwrite(selected_catchments, "selected_catchments.csv")
# fwrite(monthly_balance, "monthly_balance.csv")
# fwrite(annual_balance, "annual_balance.csv")
# fwrite(snow_data, "snowmelt_data.csv")
# fwrite(cor_results, "snowmelt_runoff_correlations.csv")

# --- Interpretation Preview ---
print(cor_results)
