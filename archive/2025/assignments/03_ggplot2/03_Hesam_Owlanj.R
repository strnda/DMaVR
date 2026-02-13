# -------------------------------
# Assignment 3: 03_ggplot2
# Author: Hesam Owlanj (xowlh001)
# Date: April 2025
# -------------------------------

# For this assignment, I used two data files: 02108000_elev_band_000_cida_forcing_leap.txt and 02046000_streamflow_qc.txt.
# These files are from the basin_timeseries_v1p2_metForcing_obsFlow.zip archive, which is accessible from the link https://gdex.ucar.edu/dataset/camels/file.html.
# As provided by the professor as the data source for this course.


# Load required libraries
library(ggplot2)

# ------------------------------
# 1. Load and preprocess data (using base R)
# ------------------------------
# Read streamflow data (columns: ID, Year, Month, Day, Flow, QC)

streamflow <- read.table("02046000_streamflow_qc.txt", header = FALSE,
                         col.names = c("ID", "Year", "Month", "Day", "Flow", "QC")
                         
)

# Parse dates manually 
streamflow$Date <- as.Date(paste(streamflow$Year, streamflow$Month, streamflow$Day, sep = "-"),
                           format = "%Y-%m-%d")
# Add hydrological year (HYR starts in October)
streamflow$HYR <- ifelse(streamflow$Month >= 10, streamflow$Year + 1, streamflow$Year)

# Read climate data 
# File is whitespace-separated 
climate <- read.table("02108000_elev_band_000_cida_forcing_leap.txt", skip = 5, header = FALSE,
                      col.names = c("Year", "Month", "Day", "Hour", "Elev", "PRCP", "SRAD",
                                    "SWE", "TMAX", "TMIN", "VP")
                      
)

# Parse dates and calculate mean temperature
climate$Date <- as.Date(paste(climate$Year, climate$Month, climate$Day, sep = "-"),
                        format = "%Y-%m-%d")
climate$Temp <- (climate$TMAX + climate$TMIN) / 2

# Merge datasets by date
hyd_data <- merge(streamflow, climate, by = "Date")
hyd_data <- hyd_data[!is.na(hyd_data$PRCP), ]  

# Add extreme flow events (top 5%)
extreme_threshold <- quantile(hyd_data$Flow, 0.95, na.rm = TRUE)
hyd_data$Extreme <- ifelse(hyd_data$Flow > extreme_threshold, "Extreme", "Normal")

# ------------------------------
# 2. Task 1: Time-series with extremes
# ------------------------------
p1 <- ggplot(hyd_data, aes(x = Date, y = Flow, color = factor(HYR))) +
  geom_line() +
  geom_point(data = subset(hyd_data, Extreme == "Extreme"),
             aes(shape = Extreme), color = "red", size = 2) +
  scale_shape_manual(values = c("Extreme" = 8)) +
  labs(title = "Daily Streamflow with Extreme Events (Top 5%)",
       y = "Flow (cfs)", color = "Hydrological Year") +
  theme_minimal()

# ------------------------------
# 3. Task 2: Precipitation-Flow Relationship
# ------------------------------
p2 <- ggplot(hyd_data, aes(x = PRCP, y = Flow)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue") +
  stat_summary(fun = "median", geom = "point", shape = 3, size = 3, color = "red") +
  annotate("text", x = max(hyd_data$PRCP, na.rm = TRUE) * 0.8,
           y = max(hyd_data$Flow, na.rm = TRUE),
           label = paste("r =", round(cor(hyd_data$PRCP, hyd_data$Flow, use = "pairwise.complete.obs"), 2)), 
           color = "darkgreen", size = 5) +
  labs(title = "Precipitation vs. Streamflow (GAM Smoothing)",
       x = "Precipitation (mm/day)", y = "Flow (cfs)") +
  theme_bw()

# ------------------------------
# 4. Task 3: SWE Distribution
# ------------------------------
climate_nonzero <- climate[climate$SWE > 0, ]


p3 <- ggplot(climate_nonzero, aes(x = factor(Year), y = SWE)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "SWE by Year",
       x = "Year", y = "SWE (mm)") +
  theme_minimal()

# ------------------------------
# 5. Task 4: Multivariate Analysis (Flow vs. Temp)
# ------------------------------
p4 <- ggplot(hyd_data, aes(x = Temp, y = Flow, color = PRCP)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Streamflow vs. Temperature (Colored by Precipitation)",
       x = "Temperature (°C)", y = "Flow (cfs)") +
  theme_minimal()

# ------------------------------
# 6. Task 5: Water Balance (PRCP vs. Flow)
# ------------------------------
hyd_data$WB_Status <- ifelse(hyd_data$PRCP > median(hyd_data$PRCP, na.rm = TRUE), "Wet", "Dry")

p5 <- ggplot(hyd_data, aes(x = Date)) +
  geom_col(aes(y = PRCP, fill = WB_Status), alpha = 0.6) +
  geom_line(aes(y = Flow / 10, color = "Streamflow"), linewidth = 1) +
  scale_fill_manual(values = c("Wet" = "blue", "Dry" = "red")) +
  scale_color_manual(values = c("Streamflow" = "black")) +
  scale_y_continuous(
    name = "Precipitation (mm/day)",
    sec.axis = sec_axis(~ . * 10, name = "Flow (cfs)")
  ) +
  labs(title = "Water Balance: Precipitation vs. Streamflow",
       fill = "PRCP Status", color = "Variable") +
  theme_bw()

# ------------------------------
# 7. Display all plots
# ------------------------------
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)