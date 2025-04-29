# Assignment 3: 03_ggplot2
# Author: Elvin Novruzov (xnove041)
# Date: April 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(readr)
library(tidyr)

# Load the hydrological data
hydro_data <- read_csv("hydro_data.csv")

# Data Preprocessing
hydro_data <- hydro_data %>%
  mutate(
    DATE = as.Date(DATE),
    HYR = ifelse(month(DATE) >= 10, year(DATE) + 1, year(DATE)),
    Month = month(DATE, label = TRUE, abbr = TRUE)
  )

# Identify Top 5% Runoff Values
runoff_threshold <- quantile(hydro_data$OBS_RUN, 0.95, na.rm = TRUE)

hydro_data <- hydro_data %>%
  mutate(Anomaly = ifelse(OBS_RUN >= runoff_threshold, "Yes", "No"))

# Create Multi-Panel Time-Series Plot
runoff_plot <- ggplot(hydro_data, aes(x = DATE, y = OBS_RUN)) +
  geom_line(color = "blue") +
  geom_point(data = filter(hydro_data, Anomaly == "Yes"),
             aes(x = DATE, y = OBS_RUN),
             color = "red", shape = 4, size = 2) +
  facet_wrap(~ HYR, scales = "free_x") +
  labs(
    title = "Monthly Observed Runoff Across Hydrological Years",
    x = "Date",
    y = "Observed Runoff (OBS_RUN)"
  ) +
  theme_minimal()

# Annotate a Significant Event (example date, adjust if necessary)
runoff_plot <- runoff_plot +
  annotate(
    "text",
    x = as.Date("2010-05-15"),
    y = max(hydro_data$OBS_RUN, na.rm = TRUE),
    label = "Spring Flood",
    color = "darkgreen",
    angle = 45,
    hjust = 0
  )

# Convert to Interactive Plot
interactive_runoff_plot <- ggplotly(runoff_plot)

# Display the interactive runoff plot
interactive_runoff_plot

# Precipitation-Runoff Relationship

# Aggregate Monthly Data
monthly_data <- hydro_data %>%
  mutate(YearMonth = floor_date(DATE, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Monthly_PRCP = sum(PRCP, na.rm = TRUE),
    Monthly_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
  )

# Create Scatterplot with Non-linear Regression Line
prcp_runoff_plot <- ggplot(monthly_data, aes(x = Monthly_PRCP, y = Monthly_OBS_RUN)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(
    title = "Monthly Precipitation vs. Runoff",
    x = "Monthly Precipitation (PRCP)",
    y = "Monthly Observed Runoff (OBS_RUN)"
  ) +
  theme_minimal()

# Apply Log Transformation
prcp_runoff_plot_log <- ggplot(monthly_data, aes(x = Monthly_PRCP, y = Monthly_OBS_RUN)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Log-Transformed Monthly Precipitation vs. Runoff",
    x = "Log(Monthly Precipitation)",
    y = "Log(Monthly Observed Runoff)"
  ) +
  theme_minimal()

# Display the original scatter plot
print(prcp_runoff_plot)

# Display the log-transformed scatter plot
print(prcp_runoff_plot_log)
