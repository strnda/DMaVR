#Part 1: Hydrological Years and Catchment Selection
# Load required package
library(data.table)

# Task 1: Assigning Hydrological Years
# Create a dataset containing ID (catchment ID), YR (year), and MNTH (month)
data <- data.table(
  ID = c(1, 2, 3, 4, 5, 6),
  YR = c(2000, 2001, 2002, 2003, 2004, 2005),
  MNTH = c(9, 10, 11, 12, 1, 2)
)

# Assign hydrological year (HYR) by shifting October-December months to the next year
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Print results
print(data)

#Task 2: Compute Overall Runoff Coefficients per Catchment
# Create a sample dataset with hydrological year and runoff data
data <- data.table(
  ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3),   # Catchment ID
  YR = c(2000, 2000, 2000, 2001, 2001, 2001, 2002, 2002, 2002),  # Year
  MNTH = c(9, 10, 11, 12, 1, 2, 3, 4, 5), # Month
  PRCP = c(100, 150, 120, 80, 90, 85, 60, 70, 65),  # Precipitation (mm)
  OBS_RUN = c(50, 75, 60, 40, 45, 42, 30, 35, 32)   # Observed Runoff (mm)
)

# Assign Hydrological Year (HYR)
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Compute Runoff Coefficient (RC) per catchment
runoff_data <- data[, .(
  RC = sum(OBS_RUN, na.rm = TRUE) / sum(PRCP, na.rm = TRUE)  # RC = total runoff / total precipitation
), by = ID]

# Print results
print(data)
print(runoff_data)

#Task 3: Classify Catchments Based on Runoff Coefficients
# Compute quantiles for RC to define five runoff coefficient groups
rc_breaks <- quantile(runoff_data$RC, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Ensure breaks are unique
rc_breaks <- unique(rc_breaks)

# Assign catchments to one of five runoff coefficient categories
runoff_data[, RC_class := cut(RC, 
                              breaks = rc_breaks, 
                              labels = c("Very Low", "Low", "Moderate", "High", "Very High"), 
                              include.lowest = TRUE)]

# Randomly select one catchment from each runoff category
selected_catchments <- runoff_data[, .SD[sample(.N, 1)], by = RC_class]

# Print results
print(runoff_data)
print(selected_catchments)


#Part 2: Water Balance and Snowmelt Contribution to Runoff
# Load required package
library(data.table)

#Task 4: Compute Monthly and Annual Water Balance for Selected Catchments
# Create a dataset containing precipitation (PRCP), evapotranspiration (PET), and observed runoff (OBS_RUN)
data <- data.table(
  ID = rep(1:5, each = 6),  # Five selected catchments
  HYR = rep(2000:2001, each = 15),  # Hydrological year
  MNTH = rep(1:6, 5),  # Months
  PRCP = runif(30, 50, 150),  # Random precipitation values
  PET = runif(30, 30, 120),  # Random PET values
  OBS_RUN = runif(30, 20, 80)  # Random observed runoff values
)

# Compute mean monthly precipitation, potential evapotranspiration, and observed runoff per catchment
monthly_balance <- data[, .(
  Mean_PRCP = mean(PRCP, na.rm = TRUE),
  Mean_PET = mean(PET, na.rm = TRUE),
  Mean_OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

# Compute total annual values for PRCP, PET, and OBS_RUN
annual_balance <- data[, .(
  Total_PRCP = sum(PRCP, na.rm = TRUE),
  Total_PET = sum(PET, na.rm = TRUE),
  Total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID)]

# Compute water balance (WB = PRCP - PET) at both monthly and annual levels
monthly_balance[, WB := Mean_PRCP - Mean_PET]
annual_balance[, WB := Total_PRCP - Total_PET]

# Identify months where WB < 0 (indicating potential water deficit)
monthly_deficit <- monthly_balance[WB < 0]

# Print results
print(monthly_balance)
print(annual_balance)
print(monthly_deficit)

# Task 5: Snowmelt Contribution to Runoff

# Ensure dataset is in data.table format
setDT(data)  

# Assign example SWE values (Ensure correct length)
set.seed(123)  # For reproducibility
data[, SWE := sample(20:60, .N, replace = TRUE)]  

# Compute mean monthly SWE per catchment and hydrological year
monthly_swe <- data[, .(Mean_SWE = mean(SWE, na.rm = TRUE)), by = .(HYR, ID, MNTH)]

# Identify the maximum SWE within each hydrological year
max_swe <- monthly_swe[, .(Max_SWE = max(Mean_SWE, na.rm = TRUE)), by = .(HYR, ID)]

# Merge max SWE back to original dataset
data <- merge(data, max_swe, by = c("HYR", "ID"), all.x = TRUE)

# Compute snowmelt as the difference between Max SWE and SWE in later months
data[, Snowmelt := Max_SWE - SWE]

# Merge with observed runoff data from Task 4
data <- merge(data, monthly_balance[, .(HYR, ID, MNTH, Mean_OBS_RUN)], 
              by = c("HYR", "ID", "MNTH"), all.x = TRUE)

# Compute correlation between snowmelt and observed runoff for spring months (March-May)
cor_snowmelt_runoff <- cor(data[MNTH %in% c(3, 4, 5), .(Snowmelt, Mean_OBS_RUN)], 
                           use = "complete.obs")

# Print results
print(data)
print(cor_snowmelt_runoff)
