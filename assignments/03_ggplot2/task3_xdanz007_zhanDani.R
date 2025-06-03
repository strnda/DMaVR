library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)

#set directory
setwd("C:/Users/student/Documents/R")

#load dataset from previous task
data <- fread("camels_model_output.csv")

#remove unnecessary columns if they exist (not all files have them)
data[, c("HR", "RAIM", "TAIR", "ET", "MOD_RUN") := NULL]

#add hydrological yer column
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

#saample data (to make it faster)
sample_data <- data[sample(.N, 100000)]

#TASK 1 - time series of runoff with anomalies
sample_data <- sample_data %>% 
  group_by(ID, HYR) %>% 
  mutate(top5 = OBS_RUN > quantile(OBS_RUN, 0.95, na.rm = T)) %>%
  ungroup()

p1 <- ggplot(sample_data, aes(x = HYR, y = OBS_RUN)) +
  geom_line(color = "steelblue") +
  geom_point(data = sample_data[sample_data$top5 == TRUE, ], color = "red", size = 1) +
  facet_wrap(~ID, scales = "free_y") +
  labs(title = "Runoff by HYR with anomalies", x = "Hydrological Year", y = "Observed Runoff")

ggplotly(p1)

# Task 2 - Precipitation vs Runoff (nonlinear)
p2 <- ggplot(sample_data, aes(x = PRCP, y = OBS_RUN)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  facet_grid(HYR ~ ID) +
  labs(title = "Precipitation vs Runoff", x = "Precipitation", y = "Runoff")

ggplotly(p2)

#TASK 3 - SWE Distribution
p3 <- ggplot(sample_data, aes(x = MNTH, y = SWE)) +
  geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.5) +
  geom_jitter(data = sample_data %>% group_by(MNTH, ID) %>% filter(SWE == max(SWE, na.rm = T)),
              color = "red", size = 0.8, width = 0.2, alpha = 0.6) +
  facet_wrap(~HYR, nrow = 2, scales = "free_y") +
  labs(title = "SWE by Month", x = "Month", y = "SWE")

print(p3)

#TASK 4 - SWE vs Runoff with PET
p4 <- ggplot(sample_data, aes(x = SWE, y = OBS_RUN, color = PET)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "SWE vs Runoff (colored by PET)", x = "SWE", y = "Runoff")

print(p4)

#TASK 5 - water balance (Surplus/Deficit) WB
# Add season column
sample_data <- sample_data %>%
  mutate(
    Season = case_when(
      MNTH %in% c(12, 1, 2) ~ "Winter",
      MNTH %in% c(3, 4, 5) ~ "Spring",
      MNTH %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )

#add WB
sample_data$WB <- sample_data$PRCP - sample_data$PET
# Plot WB with fill color
p5 <- ggplot(sample_data, aes(x = MNTH, y = WB, fill = WB > 0)) +
  geom_col(show.legend = FALSE) +
  facet_grid(ID ~ Season) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  labs(title = "Water Balance per Month and Season", x = "Month", y = "Water Balance")

print(p5)
