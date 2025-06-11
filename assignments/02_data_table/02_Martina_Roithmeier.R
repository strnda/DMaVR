library(data.table)
library(dplyr)

setwd("C:/Users/maroi/Desktop/CZU/Code VS")
data <- fread("complete_model_output.csv")

#Task 1: create new column
data$HYR <- ifelse(data$MNTH %in% c(10, 11, 12), data$YR + 1, data$YR)

## Task 2: Compute Overall Runoff Coefficients (RC) per Catchment
rc_dt <- data %>%
  group_by(ID) %>%
  summarise(RC = sum(OBS_RUN) / sum(PET))

## Task 3: Classify Catchments and Select One from Each Group
rc_dt$RC_class <- cut(rc_dt$RC,
                      breaks = quantile(rc_dt$RC, probs = seq(0, 1, 0.2), na.rm = TRUE),
                      labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                      include.lowest = TRUE)

# Select one catchment from each group randomly
set.seed(123)
selected_catchments <- rc_dt %>%
  group_by(RC_class) %>%
  sample_n(1) %>%
  select(ID)

selected_catchments

#Part 2: Water Balance and Snowmelt Contribution to Runoff --------
#Task 4: Compute Monthly and annual water balance for selected catchments

# Filter data and group for selected catchments
selected_data <- data %>%
  filter(ID %in% selected_catchments$ID)

# Compute monthly water balance
monthly_balance <- selected_data %>%
  group_by(HR, ID, MNTH) %>%
  summarise(meanPRCP = mean(PRCP),
            meanPET = mean(PET),
            meanOBS_RUN = mean(OBS_RUN))

# Compute annual water balance
annual_balance <- monthly_balance %>%
  group_by(HR) %>%
  summarise(totalPRCP = sum(meanPRCP),
            totalPET = sum(meanPET),
            totalOBS_RUN = sum(meanOBS_RUN))

head(annual_balance)

#calculate monthly and annual water balance W = P - PET
water_balance_monthly <- monthly_balance %>%
  mutate(W = meanPRCP - meanPET) %>%
  select(HR, ID, MNTH, W) %>%
  mutate(W_status = ifelse(W < 0, "potential deficit", "potential surplus"))

water_balance_annual <- annual_balance %>%
  mutate(W = totalPRCP - totalPET) %>%
  select(HR, W)

head(water_balance_monthly)

#Task 5: Snowmelt Contribution to Runoff
# Compute snowmelt contribution to runoff monthly, max SWE per year & SWE rate
snowmelt <- selected_data %>%
  group_by(HR, ID) %>%
  mutate(
    maxSWE = max(SWE, na.rm = TRUE),
    maxSWE = if_else(is.infinite(maxSWE), NA_real_, maxSWE)
  ) %>%
  group_by(HR, ID, MNTH) %>%
  reframe(
    meanSWE = mean(SWE, na.rm = TRUE),
    maxSWE = first(maxSWE),
    snowmelt = maxSWE - SWE,
    snowmelt_rate = snowmelt / sum(OBS_RUN, na.rm = TRUE)
  )

# Merge snowmelt data with monthly mean observed runoff
snowmelt_runoff <- snowmelt %>%
  left_join(water_balance_monthly, by = c("HR", "ID", "MNTH"))

# Filter for spring months (March-May) and compute correlation
spring_data <- snowmelt_runoff %>%
  filter(MNTH %in% c(3, 4, 5))

cor_swe_run <- cor(
  spring_data$snowmelt,
  spring_data$W,
  use = "complete.obs"
)

# Print the correlation
print(paste("Correlation between snowmelt & observed runoff (March-May):", cor_swe_run))
