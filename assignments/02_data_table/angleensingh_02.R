library(data.table)

### Task 1: Create base dataset with synthetic hydrological data

set.seed(42)  # For reproducibility

# Define catchments, years, and months
catchment_ids <- paste0("C", 1:5)
years <- 2000:2002
months <- 1:12

# Create a data table with all combinations of catchment, year, and month
hydro_data <- CJ(Catchment = catchment_ids, Year = years, Month = months)

# Generate synthetic hydrological values
hydro_data[, Precipitation := round(runif(.N, 0, 300), 1)]
hydro_data[, Observed_Runoff := round(Precipitation * runif(.N, 0.2, 0.8), 1)]
hydro_data[, PET := round(runif(.N, 10, 200), 1)]  # Potential evapotranspiration
hydro_data[, SWE := round(runif(.N, 0, 100), 1)]   # Snow water equivalent

# Assign hydrological year (HYR): starts in October
hydro_data[, Hydro_Year := ifelse(Month %in% c(10, 11, 12), Year + 1, Year)]

# Sample 10 random records for inspection
hydro_data[sample(.N, 10)]

### Task 2: Calculate runoff coefficient (RC) per catchment

runoff_coeff_table <- hydro_data[, .(
  Total_Precip = sum(Precipitation, na.rm = TRUE),
  Total_Runoff = sum(Observed_Runoff, na.rm = TRUE)
), by = Catchment]

# Calculate runoff coefficient
runoff_coeff_table[, Runoff_Coeff := Total_Runoff / Total_Precip]

print(runoff_coeff_table)

### Task 3: Classify catchments by runoff coefficient and select one per class

# Define quantile breaks for classification
quantile_breaks <- quantile(runoff_coeff_table$Runoff_Coeff, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Classify each catchment based on its runoff coefficient
runoff_coeff_table[, RC_Class := cut(
  Runoff_Coeff,
  breaks = quantile_breaks,
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]

# Randomly select one catchment per runoff coefficient class
set.seed(123)
selected_catchments <- runoff_coeff_table[, .SD[sample(.N, 1)], by = RC_Class]

print(selected_catchments)

### Task 4: Monthly and annual summaries for selected catchments

# Filter data to include only selected catchments
filtered_data <- hydro_data[Catchment %in% selected_catchments$Catchment]

# Monthly summary: mean precipitation, PET, and runoff
monthly_summary <- filtered_data[, .(
  Mean_Precip = mean(Precipitation, na.rm = TRUE),
  Mean_PET = mean(PET, na.rm = TRUE),
  Mean_Runoff = mean(Observed_Runoff, na.rm = TRUE)
), by = .(Hydro_Year, Catchment, Month)]

# Calculate water balance (WB = precipitation - PET)
monthly_summary[, Water_Balance := Mean_Precip - Mean_PET]

# Identify deficit months (where PET > precipitation)
deficit_months <- monthly_summary[Water_Balance < 0]

# Annual summary: total precipitation, PET, and runoff
annual_summary <- filtered_data[, .(
  Total_Precip = sum(Precipitation, na.rm = TRUE),
  Total_PET = sum(PET, na.rm = TRUE),
  Total_Runoff = sum(Observed_Runoff, na.rm = TRUE)
), by = .(Hydro_Year, Catchment)]

# Annual water balance
annual_summary[, Water_Balance := Total_Precip - Total_PET]

# Preview the summaries
head(monthly_summary)
head(annual_summary)
head(deficit_months)

### Task 5: Snowmelt analysis and correlation with spring runoff

# Monthly SWE averages
swe_summary <- filtered_data[, .(
  Mean_SWE = mean(SWE, na.rm = TRUE)
), by = .(Hydro_Year, Catchment, Month)]

# Maximum SWE per hydrological year and catchment
max_swe_by_year <- swe_summary[, .(
  Max_SWE = max(Mean_SWE, na.rm = TRUE)
), by = .(Hydro_Year, Catchment)]

# Filter spring months (March, April, May) and calculate snowmelt
spring_swe <- merge(
  swe_summary[Month %in% c(3, 4, 5)],
  max_swe_by_year,
  by = c("Hydro_Year", "Catchment")
)

spring_swe[, Snowmelt := Max_SWE - Mean_SWE]

# Merge spring snowmelt data with runoff data
spring_combined <- merge(
  spring_swe,
  monthly_summary[Month %in% c(3, 4, 5)],
  by = c("Hydro_Year", "Catchment", "Month")
)

# Calculate correlation between snowmelt and runoff for each catchment
correlation_results <- spring_combined[, .(
  Snowmelt_Runoff_Correlation = cor(Snowmelt, Mean_Runoff, use = "complete.obs")
), by = Catchment]

print(correlation_results)
