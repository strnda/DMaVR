# Required libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(plotly)

# Loading and preparing data ---------------------------------------------------
setwd("C:/Users/maroi/Desktop/CZU/Code VS")
data <- fread("complete_model_output.csv")
set.seed(143)
data <- data[sample(.N, size = round(.N * 0.05))] #using sample

# finding out group ID function
get_group_id <- function(id) {
  id_str <- as.character(id)
  len <- nchar(id_str)
  case_when(
    len == 7 ~ as.numeric(substr(id_str, 1, 1)),
    len == 8 ~ as.numeric(substr(id_str, 1, 2)),
    TRUE ~ NA_real_
  )
}

# Common data preparation
data[, HYR := fifelse(MNTH %in% 10:12, YR + 1, YR)]

data[, `:=`(
  group_ids = as.factor(get_group_id(ID)),
  OBS_RUN = fifelse(OBS_RUN == -999, NA_real_, OBS_RUN),
  date = as.Date(ISOdate(ifelse(MNTH %in% 10:12, YR, HYR), MNTH, 15)),
  Month = factor(month.abb[MNTH], levels = month.abb),
  HYR_period = cut(HYR,
                   breaks = c(1980, 1990, 2000, 2010, Inf),
                   labels = c("1980-1990", "1991-2000", "2001-2010", "2011+"),
                   right = FALSE),
  Season = fcase(
    MNTH %in% c(12, 1, 2), "Winter",
    MNTH %in% 3:5, "Spring",
    MNTH %in% 6:8, "Summer",
    MNTH %in% 9:11, "Fall"
  )
)]

# Created cleaned version for analysis
plot_data <- data[OBS_RUN >= 0 & PRCP >= 0]
plot_data[, `:=`(
  PRCP_plot = PRCP + 1,
  RUNOFF_plot = OBS_RUN + 1
)]

# Part 1: Exploratory Data Analysis and Temporal Dynamics -------------------

## Task 1: Advanced Time-Series Analysis of Runoff -------------------------
#create additional columns summarizing data for plotting
runoff_analysis <- data[, .(
  mean_runoff = mean(OBS_RUN, na.rm = TRUE),
  total_PRCP = sum(PRCP, na.rm = TRUE),
  mean_PRCP = mean(PRCP, na.rm = TRUE)
), by = .(date, HYR, MNTH = month(date))]

#creating threshold for extreme events
runoff_analysis[, is_anomaly := mean_runoff > quantile(mean_runoff, 0.95, na.rm = TRUE), by = MNTH]

#Line Plot with extreme events
runoff_plot <- ggplot(runoff_analysis, aes(x = date, y = mean_runoff)) +
  geom_line(aes(color = "Monthly Mean Runoff"), linewidth = 0.7) +
  geom_point(data = runoff_analysis[is_anomaly == TRUE],
             aes(shape = "Extreme Event"), color = "red", size = 2.5) +
  scale_x_date(date_labels = "%Y",date_breaks = "2 years", expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("Monthly Mean Runoff" = "steelblue")) +
  scale_shape_manual(values = c("Extreme Event" = 17)) +
  labs(title = "Runoff Time Series with Anomaly Detection",
       x = "Date", y = "Runoff (mm)",
       color = "Runoff", shape = "Anomalies") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

# Create interactive version of first plot
interactive_runoff_plot <- ggplotly(runoff_plot)
interactive_runoff_plot

runoff_plot

## Task 2: Precipitation-Runoff Relationship and Non-linear Analysis ------
#Preparing data - removing missing values, grouping and sumarizing
prcp_run_data <- data[!is.na(HYR_period),
                      .(
                        PRCP = sum(PRCP, na.rm = TRUE),
                        OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
                      ),
                      by = .(group_ids, HYR, MNTH, HYR_period)][
  complete.cases(PRCP, OBS_RUN)
]

#creating correlation labels
cor_labels <- prcp_run_data[, .(
  correlation = round(cor(PRCP, OBS_RUN, use = "complete.obs"), 2),
  label = paste("cor =", round(cor(PRCP, OBS_RUN, use = "complete.obs"), 2))
), by = HYR_period][!is.na(correlation)]

# Filter data and clean factor levels
valid_periods <- unique(cor_labels$HYR_period)
prcp_run_data <- prcp_run_data[HYR_period %in% valid_periods]
prcp_run_data[, HYR_period := factor(HYR_period, levels = levels(HYR_period)[levels(HYR_period) %in% valid_periods])]

#Scatterplot with summary stats
prcp_run_plot <- ggplot(prcp_run_data, aes(x = PRCP, y = OBS_RUN)) +
  geom_point(alpha = 0.5, aes(color = group_ids)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4),
              se = FALSE, color = "black") +
  stat_summary_bin(aes(linetype = "95% Confidence Interval"),
                   fun.data = mean_cl_normal, geom = "errorbar",
                   bins = 4, color = "green", linewidth = 0.8) +
  stat_summary_bin(aes(shape = "Bin Mean"),
                   fun = mean, geom = "point",
                   size = 4, color = "green", bins = 4) +
  # Correlation labels
  geom_text(data = cor_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = 0, vjust = 1.5, size = 5, fontface = "bold") +
  scale_color_viridis_d("Catchment Group", option = "plasma") +
  #legend for bin summary
  scale_linetype_manual(name = "Summary Statistics",
                        values = c("95% Confidence Interval" = 1)) +
  scale_shape_manual(name = "Summary Statistics",
                     values = c("Bin Mean" = 18)) +
  facet_wrap(~ HYR_period, nrow = 4) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Precipitation (mm)", y = "Runoff (mm)",
       title = "Precipitation-Runoff by Time Period",
       subtitle = "Green markers show binned summary statistics") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0.5, "lines"))

prcp_run_plot

# Part 2: Complex Snowmelt and Runoff Dynamics -----------------------------

## Task 3: Comprehensive Visualization of SWE Dynamics -------------------
#Removing NAs
swe_analysis <- data[!is.na(SWE) & SWE > 0]
#Grouping data
swe_plot_data <- swe_analysis[, .SD[.N >= 5], by = .(group_ids, Month)]
#Creating Peaks and Medians labeling
stats_data <- swe_plot_data[, .(
  Peak = max(SWE, na.rm = TRUE),
  Median = median(SWE, na.rm = TRUE)
), by = .(group_ids, Month, HYR_period)]
label_data <- stats_data[, .SD[which.max(Peak)], by = .(group_ids, Month)]

#Violin Plotting
swe_plot <- ggplot(swe_plot_data, aes(x = SWE, y = HYR_period)) +
  geom_violin(aes(fill = Month), alpha = 0.5) +
  geom_jitter(data = stats_data, aes(x = Peak), color = "red",
              size = 1.2, width = 0.2, height = 0.1) +
  geom_jitter(data = stats_data, aes(x = Median), color = "purple",
              size = 1.2, width = 0.2, height = 0.1) +
  geom_text(data = label_data, aes(x = Peak, label = paste0("Max: ", round(Peak))),
            color = "darkred", size = 2, nudge_y = 0.5) +
  geom_text(data = label_data, aes(x = Median, label = paste0("Med: ", round(Median))),
            color = "purple4", size = 2, nudge_y = -0.5) +
  facet_grid(group_ids ~ Month) +
  labs(title = "Monthly SWE Distribution by Catchment and Hydrological Year",
       x = "Snow Water Equivalent (mm)", y = "Hydrological Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

swe_plot

## Task 4: Multivariate Snowmelt and Runoff Correlation Analysis ----------
#Creating R2 labels
monthly_r2 <- plot_data[, .(
  lm_r2 = round(cor(PRCP, OBS_RUN, use = "complete.obs")^2, 2),
  x_pos = 10,
  y_pos = 100
), by = Month]

#Scatterplot with R2 labels
multivariate_plot <- ggplot(plot_data, aes(x = PRCP_plot, y = RUNOFF_plot)) +
  geom_point(aes(color = TAIR, size = SWE), alpha = 0.1) +
  geom_smooth(aes(linetype = "Non-linear (GAM)"),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              color = "black", se = FALSE) +
  geom_smooth(aes(linetype = "Linear (LM)"),
              method = "lm", color = "darkgreen", se = FALSE) +
  geom_text(data = monthly_r2,
            aes(x = x_pos, y = y_pos, label = paste("R² (lm) =", lm_r2)),
            color = "darkgreen", size = 4) +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("0", "10", "100", "1000+")) +
  scale_y_log10(breaks = c(1, 10, 100, 1000), labels = c("0", "10", "100", "1000+")) +
  scale_color_viridis_c("Temperature (°C)", option = "plasma") +
  scale_size_continuous("SWE", range = c(1, 4)) +
  facet_wrap(~ Month, ncol = 4) +
  labs(title = "Precipitation vs Runoff Analysis",
       x = expression("Precipitation (mm, log"["10"] * "(PRCP + 1))"),
       y = expression("Runoff (mm, log"["10"] * "(RUNOFF + 1))")) +
  theme_bw()

multivariate_plot

# Part 3: Advanced Statistical and Facet Analysis --------------------------

## Task 5: Water Balance and Surplus-Deficit Dynamics ---------------------
#Removing NAs
wb_data <- data[!is.na(PRCP) & !is.na(PET)]

#Creating WB column
wb_data[, `:=`(WB = PRCP - PET)]

#Calculating cumulative sum by group
wb_data[, Cum_WB := cumsum(WB), by = .(group_ids, HYR_period)]

#Preparing data for column plotting
wb_annual_summary <- wb_data[, .(
  Total_WB = sum(WB, na.rm = TRUE),
  Plot_Date = mean(date),
  Season = names(which.max(table(Season)))
), by = .(group_ids, HYR, HYR_period)][
  , Cum_Total_WB := cumsum(Total_WB), by = .(group_ids, HYR_period)
][, Surplus_Deficit := fifelse(Total_WB > 0, "Surplus", "Deficit")]

annual_plot <- ggplot() +
  geom_rect(
    data = data.frame(
      season = rep(c("Winter","Spring","Summer","Fall"), each = length(unique(year(wb_data$date)))),
      year = rep(unique(year(wb_data$date)), 4)
    ),
    aes(xmin = as.Date(paste(year - (season == "Winter"),
                             c(12,3,6,9)[match(season, c("Winter","Spring","Summer","Fall"))], "01", sep = "-")),
        xmax = as.Date(paste(year + (season == "Winter"),
                            c(2,5,8,11)[match(season, c("Winter","Spring","Summer","Fall"))], "01", sep = "-")),
        ymin = -Inf,
        ymax = Inf,
        fill = season),
    alpha = 0.2
  ) +
  geom_col(
    data = wb_annual_summary,
    aes(x = Plot_Date, y = Total_WB, fill = Surplus_Deficit),
    alpha = 0.8, width = 300
  ) +
  geom_line(
    data = wb_annual_summary,
    aes(x = Plot_Date, y = Cum_Total_WB),
    color = "black", linewidth = 0.7
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c(
      "Winter" = "lightblue", 
      "Spring" = "lightgreen", 
      "Summer" = "coral", 
      "Fall" = "yellow",
      "Surplus" = "#1E88E5", 
      "Deficit" = "#D81B60"
    ),
    breaks = c("Winter", "Spring", "Summer", "Fall", "Surplus", "Deficit"),
    labels = c("Winter (Dec-Feb)", "Spring (Mar-May)", "Summer (Jun-Aug)", "Fall (Sep-Nov)",
               "Surplus", "Deficit")
  ) +
  facet_wrap(~ group_ids, nrow = 4) +
  labs(
    title = "Annual Water Balance Trends by Catchment Group",
    subtitle = "Seasonal periods shaded in background | Bars: Annual water balance (surplus/deficit)",
    x = "Year",
    y = "Water Balance (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

annual_plot

# Create interactive versions of first plot
interactive_runoff_plot <- ggplotly(runoff_plot)
interactive_runoff_plot

# Display plots
runoff_plot
prcp_run_plot
swe_plot
multivariate_plot
annual_plot