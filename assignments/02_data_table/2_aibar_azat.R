
library(data.table)
library(ggplot2)
library(plotly)
library(ggridges)
library(ggExtra)
library(patchwork)

# Load cleaned data
combined_data <- readRDS("camels_model_output_cleaned.rds")
setDT(combined_data)

# Ensure HYR is assigned
combined_data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# ===============================
# Part 1: Temporal Runoff and PRCP-Runoff Analysis
# ===============================

# Task 1: Advanced Time-Series Runoff Visualization
runoff_plot <- ggplot(combined_data, aes(x = as.Date(paste(YR, MNTH, "15", sep = "-")), y = OBS_RUN)) +
  geom_line(aes(group = ID), color = "steelblue", alpha = 0.5) +
  geom_point(data = combined_data[OBS_RUN > quantile(OBS_RUN, 0.95, na.rm = TRUE)],
             aes(x = as.Date(paste(YR, MNTH, "15", sep = "-")), y = OBS_RUN),
             color = "red", size = 2, shape = 21, fill = "yellow") +
  facet_wrap(~ ID, scales = "free_y") +
  labs(title = "Monthly Observed Runoff with Anomalies (Top 5%)",
       x = "Date", y = "Observed Runoff (mm/month)") +
  theme_minimal()

# Interactive version
interactive_runoff_plot <- ggplotly(runoff_plot)

# Task 2: Nonlinear PRCP-Runoff Relationship
prcp_runoff_plot <- ggplot(combined_data, aes(x = PRCP, y = OBS_RUN)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = TRUE) +
  stat_summary(fun = median, geom = "point", color = "black", shape = 18, size = 2) +
  facet_wrap(~ ID, scales = "free") +
  labs(title = "Precipitation vs Runoff with Nonlinear Fit",
       x = "Monthly Precipitation (mm)", y = "Observed Runoff (mm)") +
  theme_bw()

# Add correlation annotation manually (optional)


# Part 2: SWE and Snowmelt Dynamics


# Task 3: SWE Ridge Plot by HYR
swe_data <- combined_data[, .(mean_swe = mean(SWE, na.rm = TRUE)), by = .(HYR, ID, MNTH)]
swe_data[, mnth_factor := factor(MNTH, levels = 1:12, labels = month.abb)]

swe_ridge <- ggplot(swe_data, aes(x = mean_swe, y = mnth_factor, fill = mnth_factor)) +
  geom_density_ridges(alpha = 0.7, scale = 1.1, color = "gray40") +
  geom_jitter(height = 0.1, width = 0.1, size = 0.5, alpha = 0.4) +
  facet_wrap(~ ID, scales = "free") +
  labs(title = "SWE Distribution by Month and Catchment",
       x = "Mean SWE (mm)", y = "Month") +
  theme_minimal() +
  theme(legend.position = "none")

# Task 4: Snowmelt vs Runoff (Multivariate)
# Create dummy PET or TEMP for color if not present
if (!"TEMP" %in% names(combined_data)) {
  combined_data[, TEMP := runif(.N, min = -5, max = 25)]
}
# Join snowmelt and runoff
melt_data <- combined_data[, .(
  HYR, ID, MNTH, SWE, PRCP, OBS_RUN, PET, TEMP
)][, snowmelt := max(SWE, na.rm = TRUE) - SWE, by = .(HYR, ID)]

multi_plot <- ggplot(melt_data[MNTH %in% 3:5], aes(x = snowmelt, y = OBS_RUN, color = TEMP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Snowmelt vs Runoff (Colored by Temperature)",
       x = "Snowmelt (mm)", y = "Observed Runoff (mm)", color = "Temp (°C)") +
  theme_light()

# Add marginal histograms
multi_with_marginals <- ggMarginal(multi_plot, type = "density", groupColour = TRUE, groupFill = TRUE)


# Part 3: Water Balance Surplus/Deficit


# Monthly water balance
combined_data[, WB := PRCP - PET]

# Quarter assignment
combined_data[, quarter := factor(quarter(as.Date(paste(YR, MNTH, "15", sep = "-"))),
                                  labels = c("Winter", "Spring", "Summer", "Fall"))]

wb_plot <- ggplot(combined_data, aes(x = factor(MNTH), y = WB, fill = WB)) +
  geom_col() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(ID ~ quarter) +
  labs(title = "Monthly Water Balance (PRCP - PET)",
       x = "Month", y = "Water Balance (mm)") +
  theme_minimal()



# Display plots interactively or arrange using patchwork
print(interactive_runoff_plot)
print(prcp_runoff_plot)
print(swe_ridge)
print(multi_with_marginals)
print(wb_plot)
