
library(data.table)
setwd("C:/Users/azato/PED")
# Load cleaned data
combined_data <- readRDS("camels_model_output_cleaned.rds")
setDT(combined_data)

# -------------------------------
# Part 1: Hydrological Years and Catchment Selection
# -------------------------------

# Task 1: Assign Hydrological Years
combined_data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Task 2: Compute Runoff Coefficients per Catchment
runoff_summary <- combined_data[, .(
  total_prcp = sum(PRCP, na.rm = TRUE),
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
), by = ID]
runoff_summary[, RC := total_runoff / total_prcp]

# Task 3: Classify Catchments by RC
breaks <- quantile(runoff_summary$RC, probs = seq(0, 1, 0.2), na.rm = TRUE)
runoff_summary[, RC_class := cut(
  RC,
  breaks = breaks,
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]

# Randomly select one catchment from each class
set.seed(42)
selected_catchments <- runoff_summary[,
                                      .SD[sample(.N, 1)],
                                      by = RC_class
][!is.na(RC)]

selected_ids <- selected_catchments$ID


# Part 2: Water Balance and Snowmelt Contribution


# Filter data to only the selected catchments
selected_data <- combined_data[ID %in% selected_ids]

# Task 4a: Monthly Water Balance
monthly_summary <- selected_data[, .(
  mean_prcp = mean(PRCP, na.rm = TRUE),
  mean_pet = mean(PET, na.rm = TRUE),
  mean_runoff = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

monthly_summary[, WB := mean_prcp - mean_pet]

# Task 4b: Annual Water Balance
annual_summary <- selected_data[, .(
  total_prcp = sum(PRCP, na.rm = TRUE),
  total_pet = sum(PET, na.rm = TRUE),
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID)]

annual_summary[, WB := total_prcp - total_pet]

# Task 5a: Compute Snowmelt (mean monthly SWE)
snowmelt_data <- selected_data[, .(
  mean_swe = mean(SWE, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

# Task 5b: Identify max SWE per HYR per catchment
max_swe <- snowmelt_data[, .(max_swe = max(mean_swe, na.rm = TRUE)), by = .(HYR, ID)]

# Task 5c: Compute Snowmelt = Max SWE - Monthly SWE
snowmelt_data <- merge(snowmelt_data, max_swe, by = c("HYR", "ID"))
snowmelt_data[, snowmelt := max_swe - mean_swe]

# Task 5d: Merge with March–May runoff
spring_runoff <- monthly_summary[MNTH %in% c(3, 4, 5), .(HYR, ID, MNTH, mean_runoff)]

spring_merged <- merge(
  snowmelt_data[MNTH %in% c(3, 4, 5), .(HYR, ID, MNTH, snowmelt)],
  spring_runoff,
  by = c("HYR", "ID", "MNTH")
)

# Task 5e: Correlation Analysis
cor_results <- spring_merged[
  , .(cor_snowmelt_runoff = cor(snowmelt, mean_runoff, use = "complete.obs")),
  by = ID
]


# OUTPUT 


cat("✅ Hydrological Year Assignment Verified:\n")
print(combined_data[MNTH %in% c(10, 11, 12)][1:5, .(YR, MNTH, HYR)])

cat("\n✅ Runoff Coefficients per Catchment:\n")
print(runoff_summary[1:5, .(ID, RC)])

cat("\n✅ Selected Catchments (one from each RC class):\n")
print(selected_catchments)

cat("\n✅ Monthly Water Balance Sample:\n")
print(monthly_summary[1:5])

cat("\n✅ Annual Water Balance Sample:\n")
print(annual_summary[1:5])

cat("\n✅ Snowmelt Estimates Sample:\n")
print(snowmelt_data[MNTH %in% c(3, 4, 5)][1:5])

cat("\n✅ Snowmelt-Runoff Correlation Results:\n")
print(cor_results)


