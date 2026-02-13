#load some required libraries
library(data.table)
library(ggplot2)
library(plotly)
library(ggExtra)

# PART 1

# 1: culc hydrological years
catchment_dates <- data.table(
  CatchmentID = 1:6,
  Year = 2000:2005,
  Month = c(9, 10, 11, 12, 1, 2)
)
catchment_dates[, HydrologicalYear := ifelse(Month %in% c(10:12), Year + 1, Year)]

# 2: runoff coefficients
runoff_data <- data.table(
  CatchmentID = rep(1:3, each = 3),
  Precipitation = runif(9, 80, 150),
  ObservedRunoff = runif(9, 30, 90),
  Month = 1:3,
  Year = 2001
)
runoff_data[, HydrologicalYear := ifelse(Month %in% c(10:12), Year + 1, Year)]
runoff_coefficients <- runoff_data[, .(
  RunoffCoefficient = sum(ObservedRunoff) / sum(Precipitation)
), by = CatchmentID]

# 3: class catchments and select one per class
breaks <- quantile(runoff_coefficients$RunoffCoefficient, probs = seq(0, 1, 0.2), na.rm = TRUE)
runoff_coefficients[, RC_Class := cut(
  RunoffCoefficient, 
  breaks = breaks,
  labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)]
selected_catchments <- runoff_coefficients[, .SD[sample(.N, 1)], by = RC_Class]

# PART 2

# 4: WB month/annual
hydro_data <- data.table(
  CatchmentID = rep(1:5, each = 6),
  HydrologicalYear = rep(2001:2002, each = 15),
  Month = rep(1:6, 5),
  Precipitation = runif(30, 60, 150),
  SWE = runif(30, 10, 90),
  ObservedRunoff = runif(30, 20, 70)
)

monthly_water_balance <- hydro_data[, .(
  MeanPrecip = mean(Precipitation),
  MeanSWE = mean(SWE),
  MeanRunoff = mean(ObservedRunoff)
), by = .(HydrologicalYear, CatchmentID, Month)]

annual_water_balance <- hydro_data[, .(
  TotalPrecip = sum(Precipitation),
  TotalSWE = sum(SWE),
  TotalRunoff = sum(ObservedRunoff)
), by = .(HydrologicalYear, CatchmentID)]

monthly_water_balance[, WaterBalance := MeanPrecip - MeanSWE]
monthly_water_balance[, DeficitFlag := WaterBalance < 0]

# 5: estimate snowmelt with runoff
hydro_data[, Snowmelt := max(SWE) - SWE, by = .(HydrologicalYear, CatchmentID)]
spring_data <- hydro_data[Month %in% 3:5]

spring_correlation <- cor(
  spring_data$Snowmelt,
  spring_data$ObservedRunoff,
  use = "complete.obs"
)

cat("Correlation between snowmelt and runoff (March–May):", round(spring_correlation, 3), "\n")

# PART 3

# 6: timing of runoff by catchment
set.seed(42)
ts_data <- data.table(
  CatchmentID = rep(1:3, each = 36),
  HydrologicalYear = rep(rep(2001:2003, each = 12), 3),
  Month = rep(1:12, 9),
  ObservedRunoff = runif(108, 10, 100)
)
ts_data[, Anomaly := ObservedRunoff > quantile(ObservedRunoff, 0.95)]

p1 <- ggplot(ts_data, aes(x = Month, y = ObservedRunoff, group = HydrologicalYear, color = factor(HydrologicalYear))) +
  geom_line() +
  facet_wrap(~CatchmentID, scales = "free_y") +
  geom_point(data = ts_data[Anomaly == TRUE], color = "red", size = 2, shape = 17) +
  labs(title = "Monthly Observed Runoff with Anomalies", x = "Month", y = "Runoff (mm)", color = "Hydro Year") +
  theme_minimal()

ggplotly(p1)

# PART 4

p2 <- ggplot(monthly_water_balance, aes(x = MeanPrecip, y = MeanRunoff)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  facet_grid(CatchmentID ~ HydrologicalYear) +
  labs(title = "Precipitation vs Runoff", x = "Precipitation", y = "Runoff")

print(p2)

# PART 5 (SWE bby month)
swe_data <- data.table(
  CatchmentID = rep(1:3, each = 36),
  HydrologicalYear = rep(rep(2001:2003, each = 12), 3),
  Month = rep(1:12, 9),
  SWE = sample(10:100, 108, replace = TRUE)
)

p3 <- ggplot(swe_data, aes(x = factor(Month), y = SWE, fill = factor(HydrologicalYear))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  facet_wrap(~ CatchmentID) +
  labs(title = "SWE Distribution by Month", x = "Month", y = "SWE (mm)") +
  theme_minimal()

print(p3)

# PART 6 (new versions of SWE in WB)

#merge SWE with water balance data
snow_plot_data <- merge(swe_data, monthly_water_balance[, .(HydrologicalYear, CatchmentID, Month, MeanRunoff)], 
						by = c("HydrologicalYear", "CatchmentID", "Month"), all.x = TRUE)
snow_plot_data[, MaxSWE := max(SWE), by = .(HydrologicalYear, CatchmentID)]
snow_plot_data[, Snowmelt := MaxSWE - SWE]

p4 <- ggplot(snow_plot_data, aes(x = Snowmelt, y = MeanRunoff, color = Snowmelt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Snowmelt vs Runoff", x = "Snowmelt (mm)", y = "Runoff (mm)", color = "Snowmelt") +
  theme_minimal()

ggMarginal(p4, type = "histogram", fill = "gray")

# PART 7 (WB visualization)

balance_data <- data.table(
  CatchmentID = rep(1:3, each = 12),
  HydrologicalYear = rep(2001:2003, each = 12),
  Month = rep(1:12, 3),
  Precipitation = runif(36, 50, 150),
  SWE = runif(36, 20, 80)
)
balance_data[, WaterBalance := Precipitation - SWE]

p5 <- ggplot(balance_data, aes(x = factor(Month), y = WaterBalance, group = HydrologicalYear, color = WaterBalance)) +
  geom_bar(stat = "identity", aes(fill = WaterBalance), alpha = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = ifelse(WaterBalance > 20, "Surplus", ifelse(WaterBalance < -20, "Deficit", ""))),
			size = 3, vjust = -0.5) +
  facet_wrap(~ CatchmentID + HydrologicalYear) +
  labs(title = "Monthly Water Balance", x = "Month", y = "Balance (mm)") +
  theme_minimal()

print(p5)
