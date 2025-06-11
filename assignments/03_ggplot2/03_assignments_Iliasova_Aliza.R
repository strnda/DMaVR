# This project analyzes hydrological data using advanced data visualization techniques in R (mainly `ggplot2`).  
# PET was missing from the dataset, so SWE was used as a proxy in water balance (WB = PRCP - SWE * 0.3)...

# `02046000_streamflow_qc.txt`: Daily streamflow measurements
# `02108000_elev_band_000_cida_forcing_leap.txt`: Daily climate variables (PRCP, SWE, TMAX, TMIN, etc.)

# Data from: https://gdex.ucar.edu/dataset/camels/file.html

#lload packages
library(ggplot2)
library(plotly)
library(ggExtra)


#load and preprocess dta
streamflow <- read.table("02046000_streamflow_qc.txt", header = FALSE)
colnames(streamflow) <- c("ID", "Year", "Month", "Day", "Flow", "QC")
streamflow$Date <- as.Date(paste(streamflow$Year, streamflow$Month, streamflow$Day, sep = "-"))
streamflow$HYR <- ifelse(streamflow$Month >= 10, streamflow$Year + 1, streamflow$Year)

climate <- read.table("02108000_elev_band_000_cida_forcing_leap.txt", skip = 5, header = FALSE)
colnames(climate) <- c("Year", "Month", "Day", "Hour", "Elev", "PRCP", "SRAD", "SWE", "TMAX", "TMIN", "VP")
climate$Date <- as.Date(paste(climate$Year, climate$Month, climate$Day, sep = "-"))
climate$Temp <- (climate$TMAX + climate$TMIN) / 2

hydro <- merge(streamflow, climate, by = "Date")
hydro <- hydro[!is.na(hydro$PRCP), ]
hydro$Month <- hydro$Month.x  # fix column name after merge


#TASK1 - time-series with extremes
threshold <- quantile(hydro$Flow, 0.95, na.rm = TRUE)
hydro$Extreme <- hydro$Flow > threshold

plot1 <- ggplot(hydro, aes(x = Date, y = Flow, color = factor(HYR))) +
  geom_line() +
  geom_point(data = hydro[hydro$Extreme == TRUE, ], aes(shape = "Extreme"), color = "red", size = 1.5) +
  scale_shape_manual(values = c("Extreme" = 8)) +
  labs(title = "Daily Streamflow with Extreme Events", y = "Flow (cfs)", color = "HYR") +
  theme_minimal()


#connection
plot1_interactive <- ggplotly(plot1)


#TASK2: PRCP vs Flow
cor_val <- cor(hydro$PRCP, hydro$Flow, use = "complete.obs")

plot2 <- ggplot(hydro, aes(x = PRCP, y = Flow)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "loess", color = "red") +
  stat_summary(fun = median, geom = "point", shape = 3, color = "black", size = 2) +
  facet_wrap(~HYR) +
  annotate("text", x = max(hydro$PRCP) * 0.6, y = max(hydro$Flow), 
		   label = paste("cor =", round(cor_val, 2)), color = "darkgreen", size = 4) +
  labs(title = "Precipitation vs Flow (LOESS + median)", x = "Precipitation", y = "Flow (cfs)") +
  theme_classic()


#TASK3: SWE per year
swe_data <- climate[climate$SWE > 0, ]
swe_data$Year <- factor(swe_data$Year)

plot3 <- ggplot(swe_data, aes(x = factor(Month), y = SWE)) +
  geom_violin(aes(fill = Year), alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.7, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 18, color = "red", size = 2) +
  facet_wrap(~ Year) +
  labs(title = "Monthly SWE by Year", x = "Month", y = "SWE (mm)") +
  theme_minimal()


#TASK4: Flow vs Temp, color = PRCP + smooth + marginal histogram
plot4_base <- ggplot(hydro, aes(x = Temp, y = Flow, color = PRCP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  scale_color_gradient(low = "lightblue", high = "red") +
  labs(title = "Flow vs Temperature, colored by PRCP", x = "Temp (°C)", y = "Flow (cfs)", color = "PRCP") +
  theme_minimal()

plot4 <- ggMarginal(plot4_base, type = "histogram", fill = "grey", color = "black")


#TASK5: WB Visualization
hydro$WB <- hydro$PRCP - (hydro$SWE * 0.3)  # Assume PET unavailable, use SWE as proxy
hydro$WB_Status <- ifelse(hydro$WB >= 0, "Surplus", "Deficit")

plot5 <- ggplot(hydro, aes(x = Date)) +
  geom_col(aes(y = WB, fill = WB_Status), alpha = 0.6) +
  geom_line(aes(y = Flow / 10), color = "black", linewidth = 1) +
  scale_fill_manual(values = c("Surplus" = "blue", "Deficit" = "red")) +
  scale_y_continuous(name = "Water Balance", sec.axis = sec_axis(~.*10, name = "Flow (cfs)")) +
  facet_wrap(~HYR) +
  labs(title = "Water Balance and Flow", x = "Date", fill = "Status") +
  theme_minimal()

# DISPLAY
print(plot1_interactive)  #only one made interactive
print(plot2)
print(plot3)
print(plot4)
print(plot5)
