# Task 1 

set.seed(42)

# Define basic IDs
catchment_ids <- paste0("BAS", 1:5)
year_seq <- 2000:2002
month_seq <- 1:12

library(data.table)
hydro_data <- CJ(CATCH_ID = catchment_ids, YEAR = year_seq, MONTH = month_seq)

# Generate synthetic data
hydro_data[, PRECIP := round(runif(.N, 0, 300), 1)]
hydro_data[, RUNOFF := round(PRECIP * runif(.N, 0.2, 0.8), 1)]
hydro_data[, EVAP := round(runif(.N, 10, 200), 1)]
hydro_data[, SNOW := round(runif(.N, 0, 100), 1)]

# Assign hydrological year
hydro_data[, HYDRO_YEAR := ifelse(MONTH %in% c(10, 11, 12), YEAR + 1, YEAR)]

hydro_data[sample(.N, 10)]

# Task 2

rc_summary <- hydro_data[, .(
  PRCP_TOTAL = sum(PRECIP, na.rm = TRUE),
  RUNOFF_TOTAL = sum(RUNOFF, na.rm = TRUE)
), by = CATCH_ID]

rc_summary[, RC := RUNOFF_TOTAL / PRCP_TOTAL]

print(rc_summary)

# Task 3 

rc_quants <- quantile(rc_summary$RC, probs = seq(0, 1, 0.2), na.rm = TRUE)

rc_summary[, RC_CLASS := cut(RC,
                             breaks = rc_quants,
                             labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                             include.lowest = TRUE)]

set.seed(123)
rep_sample <- rc_summary[, .SD[sample(.N, 1)], by = RC_CLASS]

print(rep_sample)

# Task 4 

filtered_data <- hydro_data[CATCH_ID %in% rep_sample$CATCH_ID]

monthly_stats <- filtered_data[, .(
  AVG_PRCP = mean(PRECIP, na.rm = TRUE),
  AVG_EVAP = mean(EVAP, na.rm = TRUE),
  AVG_RUNOFF = mean(RUNOFF, na.rm = TRUE)
), by = .(HYDRO_YEAR, CATCH_ID, MONTH)]

monthly_stats[, WATER_BAL := AVG_PRCP - AVG_EVAP]

deficit_months <- monthly_stats[WATER_BAL < 0]

annual_stats <- filtered_data[, .(
  TOTAL_PRCP = sum(PRECIP, na.rm = TRUE),
  TOTAL_EVAP = sum(EVAP, na.rm = TRUE),
  TOTAL_RUNOFF = sum(RUNOFF, na.rm = TRUE)
), by = .(HYDRO_YEAR, CATCH_ID)]

annual_stats[, ANNUAL_BAL := TOTAL_PRCP - TOTAL_EVAP]

head(monthly_stats)
head(annual_stats)
head(deficit_months)

# Task 5 

snow_summary <- filtered_data[, .(AVG_SNOW = mean(SNOW, na.rm = TRUE)),
                              by = .(HYDRO_YEAR, CATCH_ID, MONTH)]

max_snow_by_year <- snow_summary[, .(PEAK_SNOW = max(AVG_SNOW, na.rm = TRUE)),
                                 by = .(HYDRO_YEAR, CATCH_ID)]

# Focus on March to May
spring_snow <- merge(snow_summary[MONTH %in% 3:5], max_snow_by_year, by = c("HYDRO_YEAR", "CATCH_ID"))

spring_snow[, SNOWMELT := PEAK_SNOW - AVG_SNOW]

spring_merged <- merge(spring_snow,
                       monthly_stats[MONTH %in% 3:5],
                       by = c("HYDRO_YEAR", "CATCH_ID", "MONTH"))

snowmelt_runoff_cor <- spring_merged[, .(
  COR_SNOWMELT_RUNOFF = cor(SNOWMELT, AVG_RUNOFF, use = "complete.obs")
), by = CATCH_ID]

print(snowmelt_runoff_cor)
