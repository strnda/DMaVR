# ========================================
# Assignment 2: Hydrological Data Analysis
# Author: Novruzov Elvin
# Course: Advanced Programming
# Using: data.table
# Date: April 2025
# ========================================

library(data.table)

# Task 1: Load dataset
dt <- fread("runoff _data.csv")  # Make sure the file name is correct

# Task 2: Assign Hydrological Years
dt[, hydrological_year := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Task 3: Compute Overall Runoff Coefficients for each Catchment
rc_table <- dt[, .(
  total_precip = sum(PRCP, na.rm = TRUE),
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
), by = ID][, runoff_coefficient := total_runoff / total_precip]

# Task 4: Classify Catchments Based on Runoff Coefficient
dt <- merge(dt, rc_table[, .(ID, runoff_coefficient)], by = "ID", all.x = TRUE)

dt[, RC_group := cut(
  runoff_coefficient,
  breaks = quantile(runoff_coefficient, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]

# Task 5: Randomly select one catchment from each RC group
selected <- dt[, .SD[sample(.N, 1)], by = RC_group]
selected_ids <- unique(selected$ID)

# Task 6: Compute Monthly and Annual Water Balance for Selected Catchments
dt_selected <- dt[ID %in% selected_ids]

monthly_stats <- dt_selected[, .(
  PRCP = mean(PRCP, na.rm = TRUE),
  PET = mean(PET, na.rm = TRUE),
  OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(hydrological_year, ID, MNTH)]

monthly_stats[, water_balance := PRCP - PET]
monthly_stats[, deficit_month := water_balance < 0]

annual_stats <- monthly_stats[, .(
  total_PRCP = sum(PRCP, na.rm = TRUE),
  total_PET = sum(PET, na.rm = TRUE),
  total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
  total_WB = sum(water_balance, na.rm = TRUE)
), by = .(hydrological_year, ID)]

# Task 7: Estimate Snowmelt Contribution to Runoff
# Task 7.1: Calculate Mean Monthly SWE
swe_stats <- dt_selected[, .(mean_SWE = mean(SWE, na.rm = TRUE)), by = .(hydrological_year, ID, MNTH)]

# Task 7.2: Identify Maximum SWE per Year
max_swe <- swe_stats[, .(max_SWE = max(mean_SWE, na.rm = TRUE)), by = .(hydrological_year, ID)]

# Task 7.3: Compute Snowmelt
swe_stats <- merge(swe_stats, max_swe, by = c("hydrological_year", "ID"))
swe_stats[, snowmelt := max_SWE - mean_SWE]

# Task 7.4: Merge Spring Snowmelt with Runoff Data
spring_data <- merge(
  swe_stats[MNTH %in% 3:5],
  monthly_stats[MNTH %in% 3:5, .(hydrological_year, ID, MNTH, OBS_RUN)],
  by = c("hydrological_year", "ID", "MNTH")
)

# Task 7.5: Calculate Correlation between Snowmelt and Runoff
cor_table <- spring_data[, .(
  snowmelt_runoff_correlation = cor(snowmelt, OBS_RUN, use = "complete.obs")
), by = ID]

# Task 8: Print or Export Outputs (Optional)
# fwrite(rc_table, "runoff_coefficients.csv")
# fwrite(selected, "selected_catchments.csv")
# fwrite(monthly_stats, "monthly_water_balance.csv")
# fwrite(annual_stats, "annual_water_balance.csv")
# fwrite(swe_stats, "snowmelt_data.csv")
# fwrite(cor_table, "snowmelt_runoff_correlation.csv")

# Preview correlations
print(cor_table)
