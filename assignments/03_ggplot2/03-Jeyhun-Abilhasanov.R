# ---------------------------------------
# Assignment 3: ggplot2 Visualization
# Author: Jeyhun Abilhasanov (xabij001)
# Date: May 2025
# ---------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(readr)
library(tidyr)

# --- Load and preprocess hydrological data ---
df <- read_csv("hydro_data.csv", col_types = cols(
  DATE = col_date(format = "%Y-%m-%d"),
  PRCP = col_double(),
  OBS_RUN = col_double()
))

# Compute hydrological year and month label
df <- df %>%
  mutate(
    HYR = if_else(month(DATE) >= 10, year(DATE) + 1, year(DATE)),
    Month = month(DATE, label = TRUE, abbr = TRUE)
  )

# --- Task 1: Identify Top 5% Runoff Anomalies ---
threshold <- quantile(df$OBS_RUN, 0.95, na.rm = TRUE)
df <- df %>%
  mutate(Anomaly = OBS_RUN >= threshold)

# Time-series plot with anomalies highlighted
ts_plot <- ggplot(df, aes(x = DATE, y = OBS_RUN)) +
  geom_line(color = "#1f78b4") +
  geom_point(data = filter(df, Anomaly),
             aes(x = DATE, y = OBS_RUN),
             color = "#e31a1c", shape = 4, size = 2) +
  facet_wrap(~ HYR, scales = "free_x") +
  labs(
    title = "Monthly Observed Runoff Across Hydrological Years",
    x = "Date", y = "Observed Runoff (OBS_RUN)"
  ) +
  theme_minimal()

# Annotate a significant event (adjust date as needed)
ts_plot <- ts_plot +
  annotate(
    "text", x = as.Date("2010-05-15"),
    y = max(df$OBS_RUN, na.rm = TRUE),
    label = "Spring Flood", color = "darkgreen",
    angle = 45, hjust = 0
  )

# Convert to interactive plot
interactive_ts_plot <- ggplotly(ts_plot)
interactive_ts_plot

# --- Task 2: Monthly Precipitation vs. Runoff Relationship ---
monthly_df <- df %>%
  mutate(YearMonth = floor_date(DATE, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Monthly_PRCP = sum(PRCP, na.rm = TRUE),
    Monthly_OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
    .groups = "drop"
  )

# Scatter plot with LOESS smoother
prcp_plot <- ggplot(monthly_df, aes(x = Monthly_PRCP, y = Monthly_OBS_RUN)) +
  geom_point(color = "#1f78b4", alpha = 0.6) +
  geom_smooth(method = "loess", color = "#e31a1c", se = TRUE) +
  labs(
    title = "Monthly Precipitation vs. Runoff",
    x = "Monthly Precipitation (PRCP)",
    y = "Monthly Observed Runoff (OBS_RUN)"
  ) +
  theme_minimal()

# Log-transformed version
prcp_plot_log <- prcp_plot +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Log-Transformed: Precipitation vs. Runoff",
    x = "Log(Monthly Precipitation)",
    y = "Log(Monthly Observed Runoff)"
  )

# Display plots
print(prcp_plot)
print(prcp_plot_log)
