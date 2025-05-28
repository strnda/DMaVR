library(data.table)

# ----------------------------
# Task 1: Create synthetic dataset
# ----------------------------

set.seed(42)

<<catchments>> <- paste0("C", 1:5)                # Catchment IDs
<<years>> <- 2000:2002                            # Years
<<months>> <- 1:12                                # Months

<<dta>> <- CJ(ID = catchments, YR = years, MNTH = months)  # Create all combinations

# Add random values for each parameter
dta[, PRCP := round(runif(.N, 0, 300), 1)]        # Precipitation
dta[, OBS_RUN := round(PRCP * runif(.N, 0.2, 0.8), 1)]  # Observed runoff
dta[, PET := round(runif(.N, 10, 200), 1)]        # Potential evapotranspiration
dta[, SWE := round(runif(.N, 0, 100), 1)]         # Snow water equivalent

# Add hydrological year (starts in October)
dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Preview 10 random rows
dta[sample(.N, 10)]

# ----------------------------
# Task 2: Runoff coefficient summary
# ----------------------------

<<rc_table>> <- dta[, .(
  total_PRCP = sum(PRCP, na.rm = TRUE),
  total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
), by = ID]

rc_table[, RC := total_OBS_RUN / total_PRCP]     # Runoff coefficient

print(rc_table)

# ----------------------------
# Task 3: Classify runoff and select random catchments
# ----------------------------

<<quantile_breaks>> <- quantile(rc_table$RC, probs = seq(0, 1, 0.2), na.rm = TRUE)

rc_table[, RC_class := cut(RC,
                           breaks = quantile_breaks,
                           labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                           include.lowest = TRUE)]

set.seed(123)

<<selected_catchments>> <- rc_table[, .SD[sample(.N, 1)], by = RC_class]

print(selected_catchments)

# ----------------------------
# Task 4: Monthly and annual summaries
# ----------------------------

<<selected_data>> <- dta[ID %in% selected_catchments$ID]  # Filter selected catchments

<<monthly_summary>> <- selected_data[, .(
  mean_PRCP = mean(PRCP, na.rm = TRUE),
  mean_PET = mean(PET, na.rm = TRUE),
  mean_OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

monthly_summary[, WB := mean_PRCP - mean_PET]     # Water balance

<<deficit_months>> <- monthly_summary[WB < 0]      # Identify deficit months

<<annual_summary>> <- selected_data[, .(
  total_PRCP = sum(PRCP, na.rm = TRUE),
  total_PET = sum(PET, na.rm = TRUE),
  total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID)]

annual_summary[, WB := total_PRCP - total_PET]     # Annual water balance

head(monthly_summary)
head(annual_summary)
head(deficit_months)

# ----------------------------
# Task 5: Snowmelt analysis and correlation
# ----------------------------

<<swe_summary>> <- selected_data[, .(mean_SWE = mean(SWE, na.rm = TRUE)),
                                 by = .(HYR, ID, MNTH)]

<<max_swe>> <- swe_summary[, .(max_SWE = max(mean_SWE, na.rm = TRUE)),
                           by = .(HYR, ID)]

<<swe_spring>> <- merge(swe_summary[MNTH %in% c(3, 4, 5)], max_swe, by = c("HYR", "ID"))

swe_spring[, snowmelt := max_SWE - mean_SWE]     # Estimate snowmelt

<<spring_merged>> <- merge(swe_spring, 
                           monthly_summary[MNTH %in% c(3, 4, 5)], 
                           by = c("HYR", "ID", "MNTH"))

<<cor_snowmelt_runoff>> <- spring_merged[, .(
  correlation = cor(snowmelt, mean_OBS_RUN, use = "complete.obs")
), by = ID]

print(cor_snowmelt_runoff)
