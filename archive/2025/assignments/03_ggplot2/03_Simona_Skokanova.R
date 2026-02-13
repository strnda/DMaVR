library("fst")
library("ggplot2")
library("data.table")
library("plotly")
library("dplyr")

setwd("C:/Users/simca/Documents/czu/R/assignments/3")

#load data from previous assignment and convert it to data table
data <- fst(path = "data_snipet.fst")
data <- as.data.table(x = data)
sample_data <- data %>% sample_n(100000)

sample_data[, c("HR", "RAIM", "TAIR", "ET", "MOD_RUN")] <- NULL

sample_data[, HYR := ifelse(MNTH %in% c(10,11,12), YR + 1, YR)]

#creates new column "top5" and identifies if the values are above or below the 95th percentile
sample_data <- sample_data %>% group_by(ID, HYR) %>% mutate(top5 = OBS_RUN > quantile(OBS_RUN, 0.95, na.rm = TRUE))

#TASK 1 

p1 <- ggplot(sample_data, aes( x = HYR, y = OBS_RUN)) +
  geom_line(color = "steelblue") + 
  geom_point(data = sample_data[sample_data$top5 == TRUE, ],
             color = "red", size = 1, shape = 16) +
  facet_wrap(~ID, scales = "free_y") +
  labs(title = "Monthly observed runoff with anomalies", y = "runoff (OBS_RUN)", x = NULL)

#convert ggplot to plotly so the plot is interactive
ggplotly(p1)
#print(p1)

#TASK 2

p2 <- ggplot(sample_data, aes( x= PRCP, y = OBS_RUN)) +
  geom_point(color = "steelblue") + 
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  facet_grid(HYR ~ ID) +
  labs(title = "precipitation vs Runoff",
       x = "Precipitation", y = "runoff")

ggplotly(p2)
print(p2)

#TASK 3

p3 <- ggplot(sample_data, aes(x = MNTH, y = SWE)) +
  geom_violin(alpha = 0.5, color = "darkblue") + 
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "red",
              data = function(x){
                x %>% group_by(MNTH, ID) %>% filter(SWE == max(SWE))
              }) +
  facet_wrap(~HYR, scales = "free_y", nrow = 2)

print(p3)

#TASK 4

p4 <- ggplot(sample_data, aes(x = SWE, y = OBS_RUN, color = PET))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(title = "snowmelt vs runoff",
       x = "SWE", y = "Runoff", color = "PET")

print(p4)

#TASK 5 

#add season column
sample_data <- sample_data %>%
  mutate(
    Season = case_when(
      MNTH %in% c(12, 1, 2) ~ "winter",
      MNTH %in% c(3, 4, 5) ~ "spring",
      MNTH %in% c(6, 7, 8) ~ "summer",
      TRUE ~ "fall"
    )
  )

#add water balance column
sample_data$WB <- sample_data$PRCP - sample_data$PET

p5 <- ggplot(sample_data, aes(x = MNTH, y = WB, fill = WB > 0)) +
  geom_col(show.legend = FALSE) +
  facet_grid(ID ~ Season) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  labs(title = "monthly water balance",
       x = "month", y = "water balance")

print(p5)