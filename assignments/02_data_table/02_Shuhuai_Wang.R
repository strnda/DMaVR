# Load required packages
library(data.table)
library(ggplot2)
library(plotly)
library(ggExtra)

# Part 1: Hydrological Years and Catchment Selection

# Task 1: Assign hydrological years
data <- data.table(
  ID = c(1, 2, 3, 4, 5, 6),
  YR = c(2000, 2001, 2002, 2003, 2004, 2005),
  MNTH = c(9, 10, 11, 12, 1, 2)
)
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Task 2: Compute Overall Runoff Coefficients per Catchment
data <- data.table(
  ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  YR = c(2000, 2000, 2000, 2001, 2001, 2001, 2002, 2002, 2002),
  MNTH = c(9, 10, 11, 12, 1, 2, 3, 4, 5),
  PRCP = c(100, 150, 120, 80, 90, 85, 60, 70, 65),
  OBS_RUN = c(50, 75, 60, 40, 45, 42, 30, 35, 32)
)
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]
runoff_data <- data[, .(RC = sum(OBS_RUN) / sum(PRCP)), by = ID]

# Task 3: Classify Catchments Based on Runoff Coefficients
rc_breaks <- unique(quantile(runoff_data$RC, probs = seq(0, 1, 0.2)))
runoff_data[, RC_class := cut(RC, breaks = rc_breaks, labels = c("Very Low", "Low", "Moderate", "High", "Very High"), include.lowest = TRUE)]
selected_catchments <- runoff_data[, .SD[sample(.N, 1)], by = RC_class]

# Part 2: Water Balance and Snowmelt Contribution to Runoff

# Task 4: Compute Monthly and Annual Water Balance for Selected Catchments
data <- data.table(
  ID = rep(1:5, each = 6),
  HYR = rep(2000:2001, each = 15),
  MNTH = rep(1:6, 5),
  PRCP = runif(30, 50, 150),
  SWE = runif(30, 10, 100),  # Replaced PET with SWE
  OBS_RUN = runif(30, 20, 80)
)
monthly_balance <- data[, .(Mean_PRCP = mean(PRCP), Mean_SWE = mean(SWE), Mean_OBS_RUN = mean(OBS_RUN)), by = .(HYR, ID, MNTH)]
annual_balance <- data[, .(Total_PRCP = sum(PRCP), Total_SWE = sum(SWE), Total_OBS_RUN = sum(OBS_RUN)), by = .(HYR, ID)]
monthly_balance[, WB := Mean_PRCP - Mean_SWE]
annual_balance[, WB := Total_PRCP - Total_SWE]
monthly_deficit <- monthly_balance[WB < 0]

# Task 5: Snowmelt contribution to runoff
set.seed(123)
data[, Snowmelt := max(SWE) - SWE]
data <- merge(data, monthly_balance[, .(HYR, ID, MNTH, Mean_OBS_RUN)], by = c("HYR", "ID", "MNTH"), all.x = TRUE)
cor_snowmelt_runoff <- cor(data[MNTH %in% c(3, 4, 5), .(Snowmelt, Mean_OBS_RUN)], use = "complete.obs")

# Part 3: Exploratory Data Analysis and Temporal Dynamics

# Task 1: Advanced time-series analysis of runoff
set.seed(42)
data <- data.table(
  ID = rep(1:3, each = 36),
  HYR = rep(rep(2001:2003, each = 12), 3),
  MNTH = rep(1:12, 9),
  OBS_RUN = runif(108, 10, 100)
)
p1 <- ggplot(data, aes(x = MNTH, y = OBS_RUN, group = HYR, color = factor(HYR))) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_y") +
  labs(title = "Monthly Observed Runoff by Hydrological Year", x = "Month", y = "Observed Runoff (mm)", color = "Hydrological Year") +
  theme_minimal()
threshold <- quantile(data$OBS_RUN, 0.95)
data[, anomaly := OBS_RUN > threshold]
p2 <- p1 + geom_point(data = data[anomaly == TRUE], aes(x = MNTH, y = OBS_RUN), color = "red", size = 2, shape = 17)
interactive_plot <- ggplotly(p2)
interactive_plot

# Task 2: Precipitation-Runoff Relationship and Non-linear Analysis
monthly_balance <- na.omit(monthly_balance)
p <- ggplot(monthly_balance, aes(x = Mean_PRCP, y = Mean_OBS_RUN)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  facet_grid(ID ~ HYR) +
  labs(title = "Precipitation vs Runoff", x = "Mean Monthly Precipitation (mm)", y = "Mean Observed Runoff (mm)")
print(p)

# Part 4: Complex Snowmelt and Runoff Dynamics

# Task 3: Comprehensive Visualization of SWE Dynamics
set.seed(123)
data <- data.table(
  ID = rep(1:3, each = 36),
  HYR = rep(rep(2001:2003, each = 12), 3),
  MNTH = rep(1:12, 9),
  SWE = sample(10:100, 108, replace = TRUE)
)
p_violin <- ggplot(data, aes(x = factor(MNTH), y = SWE, fill = factor(HYR))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black", size = 1) +
  facet_wrap(~ ID) +
  labs(title = "Monthly SWE Distribution by Catchment and Year", x = "Month", y = "SWE (mm)", fill = "Hydrological Year") +
  theme_minimal()
median_swe <- data[, .(Median_SWE = median(SWE)), by = .(ID, HYR, MNTH)]
p_violin <- p_violin + geom_point(data = median_swe, aes(x = factor(MNTH), y = Median_SWE), color = "red", shape = 18, size = 2)
print(p_violin)

# Task 4: Multivariate Snowmelt and Runoff Correlation Analysis

# First, prepare snowmelt and SWE data
merged_data <- merge(data, monthly_balance[, .(HYR, ID, MNTH, Mean_OBS_RUN)], by = c("HYR", "ID", "MNTH"), all.x = TRUE)
merged_data <- merge(merged_data, max_swe, by = c("HYR", "ID"), all.x = TRUE)
merged_data[, Snowmelt := Max_SWE - SWE]

# Prepare the multivariate data, including Snowmelt and Mean_OBS_RUN
multivar_data <- na.omit(merged_data[, .(ID, HYR, MNTH, Snowmelt, Mean_OBS_RUN)])

p_multivar <- ggplot(multivar_data, aes(x = Snowmelt, y = Mean_OBS_RUN, color = Snowmelt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Multivariate Relationship: Snowmelt vs Runoff (Colored by Snowmelt)",
       x = "Snowmelt (mm)", y = "Observed Runoff (mm)", color = "Snowmelt (mm)") +
  theme_minimal()

# Optionally add marginal histograms
p_marginal <- ggMarginal(p_multivar, type = "histogram", fill = "gray", color = "black")
print(p_marginal)


# Part 3: Advanced Statistical and Facet Analysis

# Task 5: Water Balance and Surplus-Deficit Dynamics

# Compute and visualize monthly water balance (WB = PRCP - SWE)
data <- data.table(
  ID = rep(1:3, each = 12),  # Simulate 3 catchments
  HYR = rep(2001:2003, each = 12),  # 3 hydrological years
  MNTH = rep(1:12, 3),  # 12 months
  PRCP = runif(36, 50, 150),  # Random precipitation values
  SWE = runif(36, 20, 80)  # Random SWE values
)

# Calculate Water Balance (WB = PRCP - SWE)
data[, WB := PRCP - SWE]

# Use color gradients to represent surplus or deficit
p1 <- ggplot(data, aes(x = factor(MNTH), y = WB, group = HYR, color = WB)) +
  geom_line() +
  geom_bar(stat = "identity", aes(fill = WB), alpha = 0.5) +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Monthly Water Balance by Catchment and Year",
       x = "Month", y = "Water Balance (mm)",
       color = "Water Balance") +
  facet_wrap(~ ID + HYR) +  # Facet by Catchment and Year
  theme_minimal()

# Annotate significant periods of surplus or deficit
# Add annotations for significant surplus or deficit periods
p1 <- p1 + geom_text(aes(label = ifelse(WB > 20, "Surplus", ifelse(WB < -20, "Deficit", ""))),
                     position = position_dodge(width = 0.7), size = 3, vjust = -0.5)

# Display plot
print(p1)
