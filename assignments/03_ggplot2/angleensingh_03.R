# Load required libraries
library(data.table)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(ggridges)
library(ggExtra)

### ───────────────────────────────────────
### Task 1: Extreme Event Detection by Date
### ───────────────────────────────────────

set.seed(42)

# Create synthetic dataset with catchments, years, and months
catchments <- paste0("C", 1:4)
years <- 2000:2002
months <- 1:12
hydro_data <- CJ(Catchment = catchments, Year = years, Month = months)

# Assign hydrological year (HYR)
hydro_data[, Hydro_Year := ifelse(Month %in% c(10, 11, 12), Year + 1, Year)]

# Simulate observed runoff (OBS_RUN)
hydro_data[, Observed_Runoff := round(runif(.N, 10, 250), 1)]

# Create a proper date column for time series plotting
hydro_data[, Date := as.Date(paste0(Hydro_Year, "-", Month, "-01"))]

# Identify extreme events as top 5% runoff values
threshold <- quantile(hydro_data$Observed_Runoff, 0.95, na.rm = TRUE)
hydro_data[, Extreme_Event := Observed_Runoff >= threshold]

# Plot time series with extreme events highlighted
p1 <- ggplot(hydro_data, aes(x = Date, y = Observed_Runoff)) +
  geom_line(color = "darkblue", linewidth = 0.4) +
  geom_point(data = hydro_data[Extreme_Event == TRUE],
             aes(color = "Extreme Event", shape = "Extreme Event"),
             size = 2, show.legend = TRUE) +
  facet_wrap(~ Catchment, scales = "free_y") +
  labs(title = "Monthly Observed Runoff by Hydrological Year",
       subtitle = "Anomalies (Top 5%) Highlighted",
       x = "Date", y = "Observed Runoff (OBS_RUN)") +
  scale_color_manual(values = c("Extreme Event" = "red")) +
  scale_shape_manual(values = c("Extreme Event" = 17)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Make the plot interactive
ggplotly(p1, tooltip = c("x", "y", "Catchment"))

### ─────────────────────────────
### Task 2: Correlation Analysis
### ─────────────────────────────

set.seed(43)
hydro_data[, Precip := round(runif(.N, 0, 300), 1)]

# Filter to valid values for correlation
filtered_data <- hydro_data[Precip > 0 & Observed_Runoff > 0]

# Compute correlation by Catchment and Year
correlation_table <- filtered_data[, .(
  Correlation = round(cor(Precip, Observed_Runoff, use = "complete.obs"), 2)
), by = .(Catchment, Hydro_Year)]

# Merge correlation values for annotation
plot_data <- merge(filtered_data, correlation_table, by = c("Catchment", "Hydro_Year"))

# Scatter plot with LOESS smoother and correlation labels
p2 <- ggplot(plot_data, aes(x = Precip, y = Observed_Runoff)) +
  geom_point(alpha = 0.3, size = 1.2, color = "#2c7fb8") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               linetype = "dashed", color = "darkgreen") +
  facet_grid(Catchment ~ Hydro_Year) +
  geom_text(aes(x = Inf, y = Inf, label = paste0("r = ", Correlation)),
            hjust = 1.2, vjust = 1.5, size = 3, color = "black") +
  labs(title = "Nonlinear Relationship: Precipitation vs Runoff",
       subtitle = "With LOESS smoother and correlation annotations",
       x = "Precipitation (PRCP)", y = "Observed Runoff (OBS_RUN)") +
  theme_minimal()

print(p2)

### ────────────────────────────────
### Task 3: SWE Distributions & Max
### ────────────────────────────────

set.seed(44)
hydro_data[, SWE := round(runif(.N, 0, 150), 1)]

# Get max and median SWE
max_SWE_annot <- hydro_data[, .SD[which.max(SWE)], by = .(Hydro_Year, Catchment)]
median_SWE <- hydro_data[, .(Median_SWE = median(SWE, na.rm = TRUE)), by = .(Hydro_Year, Catchment)]

# Violin plot of SWE with annotations
p3 <- ggplot(hydro_data, aes(x = factor(Month), y = SWE)) +
  geom_violin(fill = "#a6bddb", color = "black", alpha = 0.7) +
  geom_jitter(data = max_SWE_annot, aes(color = "Max SWE"), width = 0.2, size = 1.5) +
  geom_text(data = median_SWE,
            aes(x = 6.5, y = Median_SWE, label = paste("Median:", round(Median_SWE))),
            inherit.aes = FALSE, size = 2.7, color = "black", hjust = 0) +
  facet_grid(Catchment ~ Hydro_Year) +
  scale_color_manual(values = c("Max SWE" = "red")) +
  labs(title = "Monthly SWE Distributions by Catchment and Hydrological Year",
       subtitle = "Violin plots with max SWE (jittered) and annotated medians",
       x = "Month", y = "Snow Water Equivalent (SWE)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

### ─────────────────────────────
### Task 4: Snowmelt vs Runoff
### ─────────────────────────────

setDT(hydro_data)

# Remove existing max_SWE column if present
if ("max_SWE" %in% names(hydro_data)) hydro_data[, max_SWE := NULL]

# Recalculate max SWE
max_swe <- hydro_data[, .(max_SWE = max(SWE, na.rm = TRUE)), by = .(Hydro_Year, Catchment)]
hydro_data <- merge(hydro_data, max_swe, by = c("Hydro_Year", "Catchment"), all.x = TRUE)

# Calculate snowmelt (Max SWE - SWE)
hydro_data[, Snowmelt := max_SWE - SWE]

# Add PET values
set.seed(45)
hydro_data[, PET := round(runif(.N, 20, 200), 1)]

# Plot snowmelt vs runoff for spring months
spring_plot <- ggplot(hydro_data[Month %in% 3:5], aes(x = Snowmelt, y = Observed_Runoff, color = PET)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Snowmelt vs Runoff with PET Influence",
       subtitle = "Spring months only (March–May)",
       x = "Snowmelt (Max SWE - SWE)", y = "Observed Runoff (OBS_RUN)", color = "PET") +
  theme_minimal()

ggExtra::ggMarginal(spring_plot, type = "histogram", fill = "gray", bins = 20)

### ─────────────────────────────
### Task 5: Water Balance by Season
### ─────────────────────────────

set.seed(46)

# Ensure PRCP and PET are present
if (!"Precip" %in% names(hydro_data)) hydro_data[, Precip := round(runif(.N, 0, 300), 1)]
if (!"PET" %in% names(hydro_data)) hydro_data[, PET := round(runif(.N, 10, 200), 1)]

# Compute Water Balance
hydro_data[, Water_Balance := Precip - PET]

# Assign season based on month
hydro_data[, Season := fifelse(Month %in% c(12, 1, 2), "Winter",
                               fifelse(Month %in% c(3, 4, 5), "Spring",
                                       fifelse(Month %in% c(6, 7, 8), "Summer", "Autumn")))]

# Bar + line plot of Water Balance
p5 <- ggplot(hydro_data, aes(x = factor(Month), y = Water_Balance, fill = Water_Balance)) +
  geom_col() +
  geom_line(aes(group = 1), color = "black", linewidth = 0.4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(Catchment ~ Hydro_Year) +
  labs(title = "Monthly Water Balance (WB = PRCP - PET)",
       subtitle = "Surplus and Deficit Visualized with Seasonal Coloring",
       x = "Month", y = "Water Balance (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify prolonged deficit streaks (3+ months)
hydro_data[, Deficit_Flag := Water_Balance < 0]
hydro_data[, Deficit_Run_ID := rleid(Deficit_Flag), by = .(Hydro_Year, Catchment)]
deficit_streaks <- hydro_data[Deficit_Flag == TRUE, .N, by = .(Hydro_Year, Catchment, Deficit_Run_ID)][N >= 3]

# Flag months in prolonged deficit streaks
hydro_data[, Prolonged_Deficit := paste(Hydro_Year, Catchment, Deficit_Run_ID) %in%
             paste(deficit_streaks$Hydro_Year, deficit_streaks$Catchment, deficit_streaks$Deficit_Run_ID)]

# Annotate prolonged deficit
p5 + geom_text(data = hydro_data[Prolonged_Deficit == TRUE],
               aes(label = "Prolonged Deficit"), color = "red", size = 2.5, vjust = -0.8)
