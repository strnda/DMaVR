# path "D:/UNI/DMaVR"

## libraries

library(ggplot2)
library(data.table)

## import data

Precipitation <- readRDS(file = "D:/UNI/DMaVR/prec_data.rds")
View(Precipitation)
object.size(Precipitation)
class(Precipitation)
head(Precipitation)
colnames(Precipitation)
nrow(Precipitation)
range(Precipitation$DT)
unique(Precipitation$STATION)
unique(Precipitation$ELEMENT)

# Candidates for X value: "STATION" "DT"
# Candidates for Y Value: "VALUE"
# Candidates for:
# - Color: we could use "VALUE" to define a gradient
# - GROUP: "STATION" "DT" or a range of "VALUE"
# - FACET: "STATION" "DT"
# - Line type: "Solid" and based on "VALUE" over "DT" or "STATION"
# - SHAPE: not thinking of anything other than just lines
# - Alpha: No need, plots should not be overlapping
# - Size: no need.

# The variables "ELEMENT" "FLAG" "QUALITY" and "X" should not be mapped
# They do not provide any useful information and most of it is just empty 


# transform to data table

Prec_table <- as.data.table(Precipitation)

# Removing duplicates

Prec_table <- Prec_table[!duplicated(Prec_table)]

# classifying precipitation 

Prec_table <- Prec_table[, RATE := fcase(VALUE == 0, "No Precipitation",
                                 VALUE <= 10,"Moderate",
                                 VALUE <= 20, "Heavy",
                                 VALUE <= 50, "Very Heavy",
                                 VALUE <= 100, "Extreme",
                                 VALUE > 100, "Catastrophic")]
# This will help to filter the precipitation by the gravity of the event 

View(Prec_table)

# Renaming columns

setnames(Prec_table, old = "DT",
         new = "DATE")

# Removing columns

Prec_table <- Prec_table[,c("FLAG","X","QUALITY") := NULL]


# Adding helper columns to classify per day, month, year
# These 3 will help with group in the variables for plotting

Prec_table <- Prec_table[, DAY  := as.Date(format(DATE, "%Y-%m-%d"))]

Prec_table <- Prec_table[, MONTH  := format(DATE, "%Y-%m")]

Prec_table <- Prec_table[, YEAR  := year(DATE)]


# I'll be plotting the 5 stations with the highest standard deviation rates
# to compare them and see how big is the change over time and what 
# that represents in terms of risk of flooding.

St_dev <- Prec_table[,sd(VALUE), by = STATION]

setnames(St_dev, old = "V1",
         new = "Deviation")

View(St_dev)

# Filtering stations

high_station <- St_dev[order(St_dev$Deviation, decreasing = T),][1:5]

high_station <- high_station[[1]]

print(high_station)

# Creating separate data tables with filtered stations

top5_data <- Prec_table[STATION %in% high_station,]
View(top5_data)

# Temporal plots
# Geom_line is definitely better for the plot, faster and it's easier to read
# compare to other plots 


# plot of all the data by hour, facet-wrapped by station to keep it readable

ggplot(top5_data, 
       aes(x = DATE,
           y = VALUE))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~STATION)

## other geometries makes it really difficult to understand, the point doesn't
## really provide any additional information and the boxplot is not helpful at all
## as it doesn't even shows the points in time where the values increased
## with the geom line we can clearly see the instances in which the values 
## increased and how they are distributed along the time period

# Zeros are also important in the plot as we can understand properly the 
# variance of the precipitation



########## AGGREGATED TIME SERIES #####


day_agg <- top5_data[,sum(VALUE), by = .(STATION,DAY)]
View(day_agg)

setnames(day_agg, old = "V1",
         new = "VALUE")

ggplot(day_agg, 
       aes(x = DAY,
           y = VALUE))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~STATION)+
  labs(title = "Daily aggregated precipitation",
       y = "Precipitation in mm")

## once the data is aggregated by day it shows 2 things
# the first: does show the overall impact of the precipitation
# the second is, we loose data of the extreme cases per hour
# as they are mitigated by the hours where there is lower or no precipitation,
# therefore losing the ability to tell whether there was a risk of flooding
# or a storm.

# It could serve to understand better the impact on the land 


year_agg <- top5_data[,sum(VALUE), by = .(STATION, year(DATE))]
View(year_agg)

setnames(year_agg, old = "V1",
         new = "VALUE")

ggplot(year_agg, 
       aes(x = year,
           y = VALUE))+
  geom_point()+
  theme_minimal()+
  facet_grid(~STATION)+
  labs(title = "Precipitation aggregated by year",
       y = "Precipitation in mm")

## The aggregation by year helps to see there is a trend to precipitation increases
## We lost the individual observations but we can see a wider perspective

########## EXTREME EVENT VIEW ######


Extreme_events <- top5_data[RATE == "Very Heavy",]
View(Extreme_events)

ggplot(top5_data, 
       aes(x = DATE,
           y = VALUE))+
  geom_line(col = "deepskyblue")+
  geom_point(data = Extreme_events, 
             aes(x = DATE,
                 y = VALUE),
             shape = "☔",
             size = 5,
             color = "blue",
             alpha = 0.7)+
  facet_wrap(~STATION)+
  labs(title = "Extreme Rainfall Events",
       x = "Date by Station",
       y = "Precipitation in mm")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold",
                                  color = "darkblue",
                                  size = 12),
        axis.title.x = element_text(colour = "darkblue"),
        axis.title.y = element_text(colour = "darkblue"),
        panel.background = element_rect(fill = "lightgreen"),
        panel.grid.minor = element_line(linetype = "3313"))

## The plot accurately shows the strongest events in the time line, I went for 
## the design of an umbrella with rain to depict larger events, also it is very 
## difficult to ignore 




############### DISTRIBUTION PLOTS ##################

# For distribution plots I'll remove the zeroes from the data table
# as this plot time will be heavily influenced by the amount of 
# zero observations

#Counting zero observations
zerocount <- nrow(top5_data[VALUE == 0,])
zerocount

# creating table with just positive values

nozero <- top5_data[VALUE > 0,]
View(nozero)

## plotting zeros vs positive value

## Zero plot

zerotable <- data.frame(Type = c("Zero","Positive"),
                        Count = c(zerocount,nrow(nozero)))

View(zerotable)
plotzero <- ggplot(zerotable,
                   aes(x = Type,
                       y = Count))+
  geom_col(width = 0.5,
           fill = c("darksalmon","deepskyblue"))+
  labs(title = "A) Dry Vs. Wet hours", 
       y = "Frequency", 
       x = "")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold",
                                  colour = "darkblue"),
        panel.grid.major.y = element_line(linetype = "dashed",
                                  color = "darkblue"))

plotzero

# Positive values plot

plotrain <- ggplot(nozero, 
                   aes(x = VALUE)) +
  geom_histogram(bins = 20, 
                 fill = "deepskyblue",
                 color = "white")+
  scale_x_log10()+
  labs(title = "B) Positive Values only (Log Scale)",
       x = "Precipitation (mm, Log Scale)",
       y = "Frequency")+
  theme_minimal()+
  theme(plot.title = element_text(colour = "darkblue",
                                  face = "bold"),
        axis.title.x = element_text(colour = "darkblue"),
        axis.title.y = element_text(colour = "darkblue"),
        panel.grid.minor = element_line(linetype = "dashed",
                                        colour = "lightblue"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "darkblue"))


plotrain

# Combining plots 

library(patchwork)

prec_histogram <- plotzero + plotrain + 
  plot_annotation(title = "Precipitation Analysis: Two-part Visualization",
                  theme = theme(plot.title = element_text(face = "bold",
                                                          colour = "darkblue")))

prec_histogram


## Too many zeroes in the data makes it impossible to make the plot readable
## from the 5 stations selected only about 20% of the observations were positive
## numbers, for the distribution plot it is a must to remove all zeros from 
## the data.
## The histogram gives more information with a log-scaled x-axis as it augments
## the space between the actual values at smaller numbers and reduces the space
## between larger numbers
## Without the scaled axis the histogram shows almost nothing as the amount of
## observations are mostly values under 2mm


#### SECOND DISTRIBUTION PLOT


# prec_boxplot <- ggplot(nozero,
#                        aes(x = VALUE))+
#   geom_boxplot()
# 
# prec_boxplot
# 
# 
# Prec_dense <- ggplot(nozero,
#                      aes(x = VALUE))+
#   geom_density()
# 
# Prec_dense
## this geometries serve nothing due to the huge amounts of low precipitation rates

## Going for Q-Q plots to see where the data lies following the theoretical
## quantiles


Prec_qq <- ggplot(nozero,
                    aes(sample = VALUE))+
  stat_qq()+
  theme_minimal()+
  facet_grid(~STATION)

Prec_qq

## Q-Q plot is showing where the extreme cases lay, also showing that 97.7% of
## the observations are under 5mm, not only that but also 
## the theoretical quantiles stretch from -4 to +4 meaning there is data laying
## in extreme points very far from the expected normal values "massive outliers"



