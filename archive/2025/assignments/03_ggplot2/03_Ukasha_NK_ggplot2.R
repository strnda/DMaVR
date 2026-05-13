############################################################
## ggplot2 assignment — precipitation data
## Author: xkatu001 
############################################################

## Load packages
library(ggplot2)
library(data.table)
library(magrittr)
library(lubridate)
library(patchwork)
## Load Data
dta <- readRDS(file = "D:/CZU/sem 2/DMaVR/data/prec_data.rds")
## Convert to data.table
dta_t <- as.data.table(x = dta)

############################################################
## basic inspection
############################################################

## Inspecting the data
class (x= dta)
class (x= dta_t)

head(x = dta_t)

## column names and dimensions
colnames(x = dta_t)
dim(x = dta_t)

## DT range
class(x = dta_t$DT)
range(x = dta_t$DT)

## unique stations
length(x = unique(x = dta_t$STATION))

## unique elements
length(x = unique(x = dta_t$ELEMENT))

unique(x = dta_t$FLAG)
unique(x = dta_t$QUALITY)

## inspect NA
sum(is.na(x = dta_t$QUALITY))
sum(is.na(x = dta_t$X))


############################################################
## plotting discussion
############################################################

# Natural x candidates:
# DT, MONTH, YEAR

# Natural y candidates:
# VALUE, aggregated precipitation

# Useful colour/group/facet variables:
# STATION, QUALITY, FLAG, ELEMENT

# Useful for linetype/alpha/shape/size:
# aggregated precipitation, MONTH, YEAR, STATION, QUALITY

# Variables unsuitable for direct mapping:
# DT as shape or colour due to too many unique values

# Drop X as it is not useful for plotting and contains only NA values

############################################################
## data preparation
############################################################

## Keep Useful columns
dta_t <- dta_t[, .(
  STATION,
  DT,
  VALUE,
  ELEMENT,
  QUALITY,
  FLAG
)]

## create helper variables
dta_t[, YEAR := year(x = DT)]
dta_t[, MONTH := month(x = DT)]
dta_t[, DAY := day(x = DT)]

## precipitation indicator
dta_t[, VALUE_POS := VALUE > 0]

## missing quality indicator
dta_t[, QUALITY_MISSING := is.na(QUALITY)]

## suspicious precipitation indicator
dta_t[, EXTREME_VALUE := VALUE > 20]

## Uses of helper variables:
# YEAR and MONTH enable temporal aggregation.
# VALUE_POS helps distinguish dry from wet periods.
# QUALITY_MISSING helps visualise missing quality-control data.
# EXTREME_VALUE helps identify unusually large rainfall events.

############################################################
## station selection strategy
############################################################

## aggregate monthly precipitation
dta_monthly <- dta_t[, .(
  VALUE_MNT = sum(VALUE, na.rm = TRUE)
), by = .(STATION, YEAR, MONTH)]

## identify dry months
dta_monthly[, VALUE_POS := VALUE_MNT > 0]

## station summaries
dta_stations <- dta_monthly[, .(
  TOTAL_PRECIP = sum(VALUE_MNT, na.rm = TRUE),
  MONTHS_WITHOUT_PRECIP = sum(VALUE_POS == FALSE)
), by = STATION]

## select 6 stations
dry <- dta_stations[
  MONTHS_WITHOUT_PRECIP > 0
][order(-TOTAL_PRECIP)][1:6]

st_sel <- data.table(STATION = dry$STATION)

############################################################
## station selection discussion
############################################################

# Stations were selected based on:
# 1. at least one dry month
# 2. highest total precipitation

# This creates stations that experienced both:
# wet and dry conditions.

# What is lost:
# stations with consistently moderate precipitation
# are excluded from comparison.

############################################################
## element selection
############################################################

el_sel <- unique(dta_t$ELEMENT)

############################################################
## filter selected stations
############################################################

dta_t_selected <- dta_t[
  STATION %in% st_sel$STATION
]

############################################################
## reshape step
############################################################

dta_m <- melt(
  data = dta_t_selected[, .(
    STATION,
    DT,
    VALUE,
    QUALITY
  )],
  id.vars = c("STATION", "DT")
)

############################################################
## reshape explanation
############################################################

# melt() converts the data into long format,
# which makes faceting and grouped plotting easier.
# Without reshaping, multiple plotting calls
# would be needed for each variable.

############################################################
## temporal plots
############################################################

############################################################
## Plot A — raw time series
############################################################

p1 <- ggplot(
  data = dta_t_selected,
  mapping = aes(
    x = DT,
    y = VALUE,
    colour = STATION
  )
) +
  geom_line(alpha = 0.6) +
  facet_grid(STATION ~ .) +
  labs(
    title = "Raw Time Series of Precipitation",
    x = "Date",
    y = "Precipitation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p1


# Faceting was used instead of plotting all stations
# in one panel because severe overplotting made the combined plot unreadable.
# I observed that data from some stations starts almost 3 years later. 
# I observed distinct seasons of high precipitation, consistent acrross allstations.

############################################################
## Plot B — aggregated monthly series
############################################################
# Selected stations at monthly level
dta_monthly_selected <- dta_monthly[
  STATION %in% st_sel$STATION
]

# Create a date variable for the middle of each month
dta_monthly_selected[
  ,
  DT := as.Date(
    paste(YEAR, MONTH, "15", sep = "-")
  )
]

p2 <- ggplot(
  dta_monthly_selected,
  aes(
    x = DT,
    y = VALUE_MNT,
    colour = STATION
  )
) +
  geom_line() +
  facet_grid(STATION ~ .) +
  labs(
    title = "Monthly Aggregated Precipitation",
    x = "Date",
    y = "Monthly precipitation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p2


# Monthly aggregation smooths hourly noise
# and reveals seasonal patterns more clearly.
# However, short-term rainfall spikes are lost.

############################################################
## Plot C — extreme events
############################################################

p3 <- ggplot(
  dta_monthly_selected[
    STATION == unique(STATION)[1]
  ],
  aes(x = DT, y = VALUE_MNT)
) +geom_line(color = "grey40") +
  geom_point(aes(color = EXTREME_EVENT), size = 1.5) +
  # horizontal shaded regions
  geom_rect(
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = 30,
      ymax = 90
    ),
    fill = "green",
    alpha = 0.005,
    inherit.aes = FALSE
  )+
  geom_rect(
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = 30
    ),
    fill = "yellow",
    alpha = 0.005,
    inherit.aes = FALSE
  )+
  geom_rect(
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = 90,
      ymax = Inf
    ),
    fill = "red",
    alpha = 0.005,
    inherit.aes = FALSE
  )+
  annotate("text", x = Inf, y = 15, label = "Bellow Average", hjust = 1.1) +
  annotate("text", x = Inf, y = 170, label = "Above Average", hjust = 1.1) +
  labs(
    title = "Monthly Aggregated Time Series of Precipitation for Selected Stations",
    x = "Date",
    y = "Monthly Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


p3

# we can highlight months with more than  90 mm or less than 30 mm of precipitation as extreme events.
# I used a horizontal shaded region from 30 to 90 mm to indicate the region within averages for winter and summer months, 
# and colored the points based on whether they exceed the 90mm threshold.

############################################################
## distribution plots
############################################################

############################################################
## Histogram with zeros
############################################################
# ignoring comparisons between elements since only one unique element exists
p4 <- ggplot(
  dta_t_selected,
  aes(x = VALUE)
) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black"
  ) +
  scale_x_continuous(trans = "log1p") +
  labs(
    title = "Distribution of Rainfall Values",
    x = "VALUE (log1p scale)",
    y = "Frequency"
  ) +
  theme_minimal()

p4

# The distribution is highly right-skewed.
# A transformed scale helps reveal the structure
# of smaller nonzero rainfall values.


# In this plot we can see that rainfall values rarely exceed 15 mm, 
# this will be used to set the x-axis limits in the next plot 
# to focus on the more common range of rainfall values.

# The log transformation helps to visualize the distribution of nonzero values, 
# but it can be less intuitive for interpreting the actual rainfall amounts.

# This plot is misleading because
# the large spike at zero dominates the histogram.
# This makes it difficult to see the distribution of nonzero values.
# and small values are compressed, making it hard to interpret the frequency of different rainfall amounts.

############################################################
## Nonzero histogram
############################################################

nonzero_data <- dta_t_selected[
  VALUE > 0
]

p5 <- ggplot(
  nonzero_data,
  aes(x = VALUE)
) +
  geom_histogram(
    binwidth = 1,
    fill = "darkgreen",
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, 15)
  ) +
  coord_cartesian(
    ylim = c(0, 12000)
  ) +
  labs(
    title = "Distribution of Nonzero Rainfall Values",
    x = "VALUE",
    y = "Frequency"
  ) +
  theme_minimal()

p5
# This plot focuses on the distribution of nonzero rainfall values, excluding the large number of zero measurements.
# The distribution of nonzero values is still right-skewed, but we can see more detail about the range and frequency of different rainfall amounts.
# We also avoid log scaling to keep the plot more understandable, ie what is log1p scale and how to interpret it.
# we limit the x-axis to 15 mm to focus on the more common range of rainfall values, and we set the y-axis limits to avoid the tall bars dominating the plot.
# This gives a clearer picture of the distribution of nonzero rainfall values, while acknowledging that zero measurements are a significant part of the overall data.

############################################################
## boxplot across stations
############################################################

p6 <- ggplot(
  nonzero_data,
  aes(
    x = STATION,
    y = VALUE
  )
) +
  geom_boxplot(fill = "orange") +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Rainfall Distribution Across Stations",
    x = "Station",
    y = "VALUE (log1p scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

p6

# Boxplots can be misleading because
# heavy zero inflation compresses
# the interquartile structure.
# We can observe the large variation in outliers across stations.

############################################################
## quality and suspicious-data plots
############################################################

############################################################
## quality distribution
############################################################
dta_t_selected <- dta_t_selected[order(DT)] # order by date to ensure correct lag calculations

dta_t_selected[
  ,
  QUALITY := as.factor(QUALITY)
]


p7 <- ggplot(
  dta_t_selected,
  aes(x = QUALITY)
) +
  geom_bar(
    fill = "salmon",
    color = "black"
  ) +
  labs(
    title = "Distribution of Quality Flags",
    x = "QUALITY",
    y = "Count"
  ) +
  theme_minimal()

p7

# observe that 0 flag dominates the data.
# investigating 3 and 4 flags accross stations using facet wrap
# to see if there are any patterns in the distribution of these flags.

############################################################
## FLAG distribution
############################################################

p8 <- ggplot(dta_t_selected[QUALITY %in% c(3,4)], aes(x = QUALITY)) +
  geom_bar(color = "black", fill = "salmon") +
  labs(
    title = "Distribution of Quality Flags 3 and 4",
    x = "Quality Flag",
    y = "Count"
  ) +
  theme_minimal()+
  facet_wrap(~STATION)

p8

# observe that station 0-203-0-11637 significantly contributes to both 3 and 4 flags, 
# while stations 0-203-0-42108066001 and 0-203-0-10601029001 only contribute to flag 3,
# all remaining stations are negligible when comparing these two flags.

############################################################
## suspicious jump rule
############################################################

# RULE:
# suspicious jump =
# hourly precipitation change > 20 units

jump_data <- dta_t_selected %>%
  group_by(STATION) %>%
  arrange(DT) %>%
  mutate(
    jump = abs(VALUE - lag(VALUE))
  ) %>%
  ungroup()

suspicious_jumps <- jump_data %>%
  filter(jump > 20)

p9 <- ggplot(
  suspicious_jumps,
  aes(
    x = DT,
    y = jump,
    colour = STATION
  )
) +
  geom_point(alpha = 0.8) +
  labs(
    title = "Suspicious Precipitation Jumps",
    subtitle = "Hourly changes > 20",
    x = "Date",
    y = "Absolute jump"
  ) +
  theme_minimal()

p9


# we can also identify that station 0-203-010601029001 has suspicious jumps, in discrete jumps in the x axis. 
# These events might have occured on the same day. Further investigation is required, 
# (Maybe this specific day was extremely rainy or just isolated failure in sensors)

############################################################
## long zero run rule
############################################################

# RULE:
# suspicious zero run =
# 240 consecutive zero hours

dta_t_selected <- dta_t_selected[
  order(STATION, DT)
]

dta_t_selected[
  ,
  zero_run := {
    
    r <- rle(VALUE == 0)
    
    rep(
      ifelse(r$values, r$lengths, 0),
      r$lengths
    )
    
  },
  by = STATION
]

long_zero_runs <- dta_t_selected[
  zero_run >= 240
]

p10 <- ggplot(
  long_zero_runs,
  aes(
    x = DT,
    y = STATION,
    colour = zero_run
  )
) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Long Zero Runs",
    subtitle = "240+ consecutive zero hours",
    x = "Date",
    y = "Station"
  ) +
  theme_minimal()

p10

############################################################
## element comparison
############################################################

# Only one element exists in the dataset.
# True comparison between elements is impossible.

p11 <- ggplot(
  dta_t_selected,
  aes(
    x = ELEMENT,
    y = VALUE
  )
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribution by ELEMENT",
    x = "ELEMENT",
    y = "VALUE"
  ) +
  theme_minimal()

p11

p12 <- ggplot(
  dta_t_selected,
  aes(
    x = DT,
    y = VALUE
  )
) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ELEMENT) +
  labs(
    title = "Time Series by ELEMENT",
    x = "Date",
    y = "VALUE"
  ) +
  theme_minimal()

p12

############################################################
## Grammar of Graphics explanations
############################################################

# p1
# data: dta_t_selected
# mapping: DT -> x, VALUE -> y, STATION -> colour
# geom: geom_line()
# stat: default identity
# facet: facet_grid() separates stations
# scale: default datetime scale
# theme: legend removed for readability



# p9
# data: suspicious_jumps
# mapping: DT -> x, jump -> y, STATION -> colour
# geom: geom_point()
# stat: identity
# facet: none
# scale: default continuous scales
# theme: minimal theme


############################################################
## scale and faceting decisions
############################################################

# Decision 1:
# facet_grid() preferred over colour-only design
# because plotting all stations together caused
# severe overplotting.

# Decision 2:
# log1p transformation used because
# rainfall distribution is strongly right-skewed.

# Decision 3:
# fixed scales preferred in temporal plots
# to preserve comparability across stations.

############################################################
## final combined figure
############################################################

final_plot <- wrap_plots(
  p1 ,
  p9 + theme(legend.position = "none"),
  ncol = 1
)

final_plot

############################################################
## final figure discussion
############################################################

# The combined figure compares:
# 1. the raw precipitation time series,
# 2. and the detected suspicious rainfall jumps.

# The reader should first notice that
# the timestamps of the suspicious jumps
# align closely with major precipitation spikes
# in the raw time series.

# This suggests that many of the flagged jumps
# are likely real meteorological events
# rather than obvious sensor failures.

# However, some isolated jumps still appear unusual
# and may require further investigation together
# with quality flags and station metadata.

############################################################
## save figures
############################################################

ggsave(
  filename = "fig_timeseries.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "fig_quality.png",
  plot = p9,
  width = 10,
  height = 6,
  dpi = 300
)

############################################################
## final conclusions
############################################################

# The most informative visualisation
# was the aggregated monthly time series,
# because it revealed seasonal structure clearly.

# The hardest visualisation to design well
# was the rainfall histogram because
# extreme skewness and zero inflation
# made default histograms misleading.

# The most important readability choices were:
# faceting,
# transformed scales,
# restricting station selection.
# setting axis limits.
# removing legends.

# Suspicious data issues became visible after plotting:
# long zero runs,
# sudden jumps,
# and concentration of quality flags
# at particular stations.

# Station 0-203-010601029001 appeared
# particularly unusual due to both
# suspicious jumps and quality flags.

# ELEMENT comparison was limited because
# only one element existed in the dataset.

# Default ggplot2 choices that would
# have been misleading:
# plotting all stations together,
# using untransformed histograms,
# and ignoring zero inflation.

# Final conclusion
# The visualisation process revealed key patterns in the precipitation data,
# However since all visualisations are from the same dataset there is a risk of overfitting the narrative to the data.
# It would be helpful to investigate the precipitation patterns from a different dataset to see if similar patterns emerge.