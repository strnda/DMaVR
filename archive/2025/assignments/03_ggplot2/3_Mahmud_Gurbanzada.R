# Load required libraries
library(data.table)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(ggridges)
library(ggExtra)

#Task 1

set.seed(42)

basins <- paste0("BAS", 1:4)
years <- 2000:2002
months <- 1:12

hydro_dt <- CJ(BASIN = basins, YEAR = years, MONTH = months)
hydro_dt[, HYDRO_YEAR := ifelse(MONTH %in% c(10, 11, 12), YEAR + 1, YEAR)]
hydro_dt[, RUNOFF := round(runif(.N, 10, 250), 1)]
hydro_dt[, DATE := as.Date(paste0(HYDRO_YEAR, "-", MONTH, "-01"))]

extreme_cutoff <- quantile(hydro_dt$RUNOFF, 0.95, na.rm = TRUE)
hydro_dt[, EXTREME_EVENT := RUNOFF >= extreme_cutoff]

p1 <- ggplot(hydro_dt, aes(x = DATE, y = RUNOFF)) +
  geom_line(color = "darkblue", linewidth = 0.4) +
  geom_point(data = hydro_dt[EXTREME_EVENT == TRUE],
             aes(color = "Extreme Runoff", shape = "Extreme Runoff"),
             size = 2, show.legend = TRUE) +
  facet_wrap(~ BASIN, scales = "free_y") +
  labs(
    title = "Monthly Runoff by Hydrological Year",
    subtitle = "Top 5% Runoff Events Highlighted",
    x = "Date", y = "Runoff (mm)"
  ) +
  scale_color_manual(values = c("Extreme Runoff" = "red")) +
  scale_shape_manual(values = c("Extreme Runoff" = 17)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(p1, tooltip = c("x", "y", "BASIN"))

#Task 2

set.seed(43)
hydro_dt[, PRECIP := round(runif(.N, 0, 300), 1)]

valid_pairs <- hydro_dt[PRECIP > 0 & RUNOFF > 0]

cor_summary <- valid_pairs[, .(
  CORREL = round(cor(PRECIP, RUNOFF, use = "complete.obs"), 2)
), by = .(BASIN, HYDRO_YEAR)]

merged_dt <- merge(valid_pairs, cor_summary, by = c("BASIN", "HYDRO_YEAR"))

p2 <- ggplot(merged_dt, aes(x = PRECIP, y = RUNOFF)) +
  geom_point(alpha = 0.3, size = 1.2, color = "#2c7fb8") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               linetype = "dashed", color = "darkgreen") +
  geom_text(aes(x = Inf, y = Inf, label = paste0("r = ", CORREL)),
            hjust = 1.2, vjust = 1.5, size = 3, color = "black") +
  facet_grid(BASIN ~ HYDRO_YEAR) +
  labs(
    title = "Precipitation vs Runoff",
    subtitle = "LOESS Smoother and Correlation by Basin-Year",
    x = "Precipitation (mm)", y = "Runoff (mm)"
  ) +
  theme_minimal()

print(p2)

#Task 3

set.seed(44)
hydro_dt[, SNOW_EQ := round(runif(.N, 0, 150), 1)]

max_snow <- hydro_dt[, .SD[which.max(SNOW_EQ)], by = .(HYDRO_YEAR, BASIN)]
median_snow <- hydro_dt[, .(MED_SWE = median(SNOW_EQ, na.rm = TRUE)), by = .(HYDRO_YEAR, BASIN)]

p3 <- ggplot(hydro_dt, aes(x = factor(MONTH), y = SNOW_EQ)) +
  geom_violin(fill = "#a6bddb", color = "black", alpha = 0.7) +
  geom_jitter(data = max_snow, aes(color = "Max SWE"), width = 0.2, size = 1.5) +
  geom_text(data = median_snow, aes(x = 6.5, y = MED_SWE, label = paste("Median:", round(MED_SWE))),
            inherit.aes = FALSE, size = 2.7, color = "black", hjust = 0) +
  facet_grid(BASIN ~ HYDRO_YEAR) +
  scale_color_manual(values = c("Max SWE" = "red")) +
  labs(
    title = "Monthly SWE Distribution",
    subtitle = "Violin Plot with Max and Median Annotations",
    x = "Month", y = "Snow Water Equivalent (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

#Task 4: Snowmelt vs Runoff, Colored by PET

set.seed(45)
hydro_dt[, PET := round(runif(.N, 20, 200), 1)]

hydro_dt[, PEAK_SWE := max(SNOW_EQ, na.rm = TRUE), by = .(HYDRO_YEAR, BASIN)]
hydro_dt[, SNOWMELT := PEAK_SWE - SNOW_EQ]

spring_dt <- hydro_dt[MONTH %in% 3:5]

p4 <- ggplot(spring_dt, aes(x = SNOWMELT, y = RUNOFF, color = PET)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Snowmelt vs Runoff (Spring)",
    subtitle = "Colored by Potential Evapotranspiration (PET)",
    x = "Snowmelt (mm)", y = "Runoff (mm)", color = "PET (mm)"
  ) +
  theme_minimal()

ggMarginal(p4, type = "histogram", fill = "gray", bins = 20)

#Task 5

set.seed(46)
hydro_dt[, WATER_BAL := PRECIP - PET]

hydro_dt[, SEASON := fifelse(MONTH %in% c(12, 1, 2), "Winter",
                             fifelse(MONTH %in% c(3, 4, 5), "Spring",
                                     fifelse(MONTH %in% c(6, 7, 8), "Summer", "Autumn")))]

hydro_dt[, DEFICIT := WATER_BAL < 0]
hydro_dt[, DEFICIT_RUN := rleid(DEFICIT), by = .(HYDRO_YEAR, BASIN)]

deficit_streaks <- hydro_dt[DEFICIT == TRUE, .N, by = .(HYDRO_YEAR, BASIN, DEFICIT_RUN)][N >= 3]

hydro_dt[, PROLONGED := paste(HYDRO_YEAR, BASIN, DEFICIT_RUN) %in% 
           paste(deficit_streaks$HYDRO_YEAR, deficit_streaks$BASIN, deficit_streaks$DEFICIT_RUN)]

p5 <- ggplot(hydro_dt, aes(x = factor(MONTH), y = WATER_BAL, fill = WATER_BAL)) +
  geom_col() +
  geom_line(aes(group = 1), color = "black", linewidth = 0.4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(BASIN ~ HYDRO_YEAR) +
  geom_text(data = hydro_dt[PROLONGED == TRUE],
            aes(label = "Prolonged Deficit"), color = "red", size = 2.5, vjust = -0.8) +
  labs(
    title = "Monthly Water Balance per Basin-Year",
    subtitle = "Prolonged Deficit Periods (≥3 Months) Highlighted",
    x = "Month", y = "Water Balance (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)
