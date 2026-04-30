## 1 LOADING DATA
library(data.table)
library(ggplot2)

dta <- readRDS(file = "prec_data.rds")

## Converting to data table
dta_t <- as.data.table(x = dta)

## 2 BASIC INSPECTION
class(x = dta)
class(x = dta_t)
head(x = dta_t)
colnames(x = dta_t)
nrow(x = dta_t)
ncol(x = dta_t)
range(dta_t$DT, na.rm = TRUE)
uniqueN(dta_t$STATION)
uniqueN(dta_t$ELEMENT)

## Natural variable for x: DT column
## Natural variable fo y: VALUE column
## Color: Station,  element, flag, quality (distinct), or value (color gradient)
## Group: Station or elements
## Facet: Elements. station, flag
## Linetype and shape: Element, flag
## Size: Value
## Alpha: Value, quality
## If all stations would be used in one plot it would make it unreadable since there's 432 of them, this would result in
## in too many lines in one plot

## 3 DATA PREPARATION FOR VISUALISATION
## helps us see what the spread was precipitation was for each individual year/month without making the data too granual
dta_t[, YEAR := year(DT)] 
dta_t[, MONTH := month(DT)]

## We can get rid of 0 values to only show the precipitation
dta_t[, NONZERO_VALUE := VALUE > 0]

## Aggregating precipitation per month, can be used as an x axis
monthly_prec <- dta_t[, .(total = sum(VALUE, na.rm = TRUE)), 
                      by = .(STATION, YEAR, MONTH)]

## Creating list of stations with values of non-zero precipitation
nonzero_sum <- aggregate(dta_t$NONZERO_VALUE, by=list(dta_t$STATION), FUN=sum, na.rm=TRUE)

setnames(nonzero_sum, 
         old = c("Group.1", "x"), 
         new = c("STATION", "VALUE"))

nonzero_sum <- as.data.table(nonzero_sum)

## 4 STATION SELECTION STRATEGY
## Creating table with just 6 highest sum values of precipitation per stations. 
top_stations <- nonzero_sum[order(-VALUE), .(STATION, VALUE)][1:6]

## st_sum with just the 6 chosen stations. This will show only the areas where it rained the most
## making it useful for the analysis.This will make the highest extremes clearly visible and readable for the user. Data
## will use the idea about what is "normal". This will get rid of possible droughts, missing data and exclude strange values.
## It is possible that the selected stations are likely located in the mountains or other areas with high precipitation.
## This way we loose the idea on how it looks like across the whole Czech republic.
st_sel <- dta_t[STATION %in% top_stations$STATION]

## 5 TEMPORAL PLOTS
## Facet wrap is used because otherwise all the lines would mix and the plot would become unreadable. Because the data is 
## dense, the light width and lower alpha makes them more distinct and not look just like a single block.Zeros make the data jump down and
## can create continuous lines with no data present, however since the sample here is big, they don't cause major issues.
ggplot(data = st_sel, aes(x = DT, y = VALUE, colour = STATION)) +
  geom_line(linewidth = 0.3, alpha = 0.5) +  
  facet_wrap(~ STATION, ncol = 2) +
  labs(
    x = "time",
    y = "precipitation"
  ) +
  theme(legend.position = "none")
## data: data table only with the six station selection
## mapping: mapping the year as x and the precipitation values as y, each station has a different color
## geom: line - best for continuous data
## stat: default statistics is used, no need for anything extra
## facet: yes, to make the plots more readable and not just stack on top of each other
## scale: no, scales are set automatically to match the highest precipitation
## theme: getting rid of legend because we can already see the names of the stations + they're color coded

## Aggregating the data to get them per month. Monthly aggregation shows for example if they are long therm droughts in 
## the area. On the other hand it doesn't show whether the precipitation happens gradually or if there were only a few
## heavy thunderstorms. It can be as a good comparison in comparing monthly precipitation throughout individual years.
monthly_sum <- st_sel[, .(total = sum(VALUE, na.rm = TRUE)), 
                      by = .(STATION, YEAR, MONTH)]

monthly_sum$YEAR_MONTH <- as.Date(paste0(monthly_sum$YEAR, "-", monthly_sum$MONTH, "-01"))

plot_monthly <- ggplot(data = monthly_sum, aes(x = YEAR_MONTH, y = total, color = STATION)) +
  geom_line() +   
  facet_wrap(~ STATION, ncol = 2) +
  labs(
    x = "month",
    y = "precipitation"
  ) +
  theme(legend.position = "none")
## data: data table with monthly sum of precipitation per station
## mapping: mapping the month and year as x and the precipitation values as y, each station has a different color
## geom: line - best for continuous data
## stat: default statistics is used, no need for anything extra
## facet: yes, to make the plots more readable and not just stack the lines on top of each other
## scale: no, scales are set automatically to match the highest precipitation
## theme: legend is removed as each station has its own color and box

top_events <- st_sel[order(-VALUE)][1:50]

st_sel$top_event <- st_sel$VALUE > 17.0

## The design shows the extreme values highlighted with points making them clearly distinguishable from the lower values.
## The line makes this difference even clearer by separating the values visually.
ggplot(data = st_sel, aes(x = DT, y = VALUE, colour = STATION)) +
  geom_line(linewidth = 0.3) +  
  geom_point(data = st_sel[st_sel$top_event == TRUE, ], size = 2) +
  geom_hline(yintercept = 17.0, linewidth = 0.2) +
  facet_wrap(~ STATION, ncol = 2) +
  labs(
    x = "time",
    y = "precipitation"
  ) +
  theme(legend.position = "none")

## 6 DISTRIBUTION PLOTS
st_sum <- aggregate(x = dta_t$VALUE, by = list(dta_t$STATION), FUN = sum, na.rm = TRUE)

setnames(st_sum,
         old = c("Group.1", "x"), 
         new = c("STATION", "VALUE"))

## The graph shows that most stations show total precipitation slightly below 5000
ggplot(data = st_sum, aes(x = VALUE)) +
  geom_histogram() +
  labs(
    x = "total precipitation",
    y = "n stations"
  )

## Calculate how many percent of values are 0 values. Most stations has a little under 6000 
zero_sum <- dta_t[, .(zeros = sum(VALUE == 0, na.rm = TRUE)),
                  by = STATION]

ggplot(data = zero_sum, aes(x = zeros)) + ## this can be misleading because some stations have lower total amount of measurements
  geom_histogram() +
  labs(
    x = "zero precipitation",
    y = "n stations"
  )

## Redoing the graphs with different scale to see the percentage of 0 value per station. This will create a better idea
## on how the stations perform.

zero_percent <- dta_t[, .(
  zeros = sum(VALUE == 0, na.rm = TRUE),
  total_obs = sum(!is.na(VALUE))
), by = STATION]

zero_percent <- zero_percent[, pct_zeros := (zeros / total_obs) * 100] 

## Most of the stations have around 90% zero precipitation. Stations that fall outside of this can either be in extremely wet 
## or dry areas or there could be an issue with their measurements
ggplot(data = zero_percent, aes(x = pct_zeros)) +
  geom_histogram() +
  labs(
    x = "zero precipitation percentage",
    y = "n stations"
  )

## 7 QUALITY AND SUSPICIOUS DATA
unique(dta_t$FLAG)
unique(dta_t$QUALITY) ## no missing quality
flagged_z <- dta_t[FLAG == "Z"]
flagged_empty <- dta_t[FLAG == ""]
flagged_na <- dta_t[is.na(FLAG)]

## The graphs are almost empty because all the values flagged Z are 0 values
library(cowplot)
p_z <- ggplot(data = flagged_z, aes(x = VALUE)) +
  geom_histogram() +
  scale_y_continuous(trans='log10')
p_empty <- ggplot(data = flagged_empty, aes(x = VALUE)) +
  geom_histogram() +
  scale_y_continuous(trans='log10')
p_na <- ggplot(data = flagged_na, aes(x = VALUE)) +
  geom_histogram() +
  scale_y_continuous(trans='log10')
plot_grid(p_z, p_empty, p_na, ncol=3)
## data: data table only with the flagged values selected
## mapping: mapping the hours as x and the precipitation values as y, each station has a different color
## geom: line - best for continuous data
## stat: default statistics is used (total)
## facet: yes, to make the plots more readable and not just stack on top of each other
## scale: no, scales are set automatically to match the highest precipitation
## theme: legend is unnecessary as the stations are visible in the plot

## Looking for data where values suspiciously jump from 0 to very high precipitation
dta_t[, hourly_jump := VALUE - shift(VALUE), by = STATION]
suspicious_data <- dta_t[hourly_jump > 50 | hourly_jump < -50]

weird_stations <- unique(suspicious_data$STATION)

plot_sus <- dta_t[STATION %in% weird_stations]

ggplot() +
  geom_line(data = plot_sus, aes(x = DT, y = VALUE, color = QUALITY)) +
  geom_point(data = suspicious_data, aes(x = DT, y = VALUE), color = "red4", size = 2) +
  facet_wrap(~ STATION, ncol(6)) +
  labs(
    title = "Suspicious precipitation jumps",
    x = "time",
    y = "precipitation"
  )
## data: data table only with the stations where suspicious jumps occurred
## mapping: mapping the year as x and the precipitation values as y, each station has a different color
## geom: line - best for continuous data, point - to highlight the unexpected jumps
## stat: default statistics is used
## facet: yes, to make the plots more readable and not just stack on top of each other
## scale: no, scales are set automatically to match the highest precipitation
## theme: hiding legend as the station names area already visible

st_sel[, cum_sum := cumsum(VALUE), by = STATION]
dry_periods <- st_sel[, .(duration = .N), by = .(STATION, cum_sum)]
dry_periods[, consecutive_zeros := duration - 1]
dry_periods <- dry_periods[consecutive_zeros > 0]

## This histogram shows the distribution of the dry period lengths across the 6 most wet stations
ggplot(data = dry_periods, aes(x = consecutive_zeros, fill = STATION)) +
  geom_histogram(bins = 50, color = "black", alpha = 0.8) +
  facet_wrap(~ STATION, scales = "free_y") +
  labs(
    title = "How many hours stay dry",
    x = "length of dry period",
    y = "occurences"
  ) +
  scale_y_log10() +
  theme(legend.position = "none")

## 8 ELEMENT COMPARISON## 8 ELETRUEMENT COMPARISON
## There's only one unique element for all the measurements so no comparisons can't be done here

## 9 Grammar and graphics = under individual plots

## 10 FACETING AND SCALE DECISION
## Facet grid crams all plots into single squished horizontal or vertical line, facet wrap was chosen because we need 
## a lot of width for it to be readable

## A standard legend needs constant visual comparing to match which color is which, since this makes the plots a little hard to
## navigate, I used the direct labels above each graph instead.

## I used alpha to make the lines semi-transparent When working with large data sets, the lines can squish together making the final
## the data lost in some spaces where they overlap. When rainfall occurs in multiple hours the area becomes a bit darker.

## 11 RESHAPE TASK
st_wide <- st_sel[, .(
  dry = sum(VALUE == 0, na.rm = TRUE),
  rain = sum(VALUE > 0, na.rm = TRUE)
), by = STATION]

dta_m <- melt(
  data = st_wide, 
  id.vars = "STATION", 
  measure.vars = c("dry", "rain"),
  variable.name = "event", 
  value.name = "count"
)

dry_rain <- ggplot(data = dta_m, aes(x = STATION, y = count, fill = event)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("dry" = "brown", "rain" = "steelblue")) +
  labs(
    title = "Dry and rain hours per station",
    x = "station",
    y = "hours",
    fill = "Condition"
  ) +
  theme_minimal() 
dry_rain
## The reshaping was necessary because for plotting the y axis we only need a single column, this combined in in one col
## along with the count to create the graph. 

## The melted data in the long format gave us the opportunity to use position dodge to create a grouped bar chart
## where the dry and wet hours are placed next to each other

## If we created the chart with the original data it would place the wet and rain hours on top of each other and not side by side
## which could look misleading. It would also not create the legend leaving users confused on the colors mean. 

library(patchwork)

p1 <- ggplot(data = monthly_sum, aes(x = YEAR_MONTH, y = total, color = STATION)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  labs(title = "monthly precipitation", 
       x = "date", 
       y = "precipitation") +
  theme(legend.position = "right")

p2 <- ggplot(data = dta_m, aes(x = STATION, y = count, fill = event)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("dry" = "brown", "rain" = "steelblue")) +
  labs(title = "dry and rain hours", x = "station", y = "hours", fill = "Condition") +
  theme_minimal() 

p3 <- ggplot(data = dry_periods, aes(x = consecutive_zeros)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.8) +
  scale_y_log10() +
  labs(title = "lenght of dry periodss", x = "dry hours", y = "count")

p4 <- ggplot() +
  geom_line(data = plot_sus, aes(x = DT, y = VALUE, group = STATION)) +
  geom_point(data = suspicious_data, aes(x = DT, y = VALUE), color = "red4", size = 2) +
  labs(title = "suspicious Jumps >50mm", x = "time", y = "precipitation") 

report <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Precipitation extremes and data quality")

report

## The panel shows evaluating precipitation needs multiple perspectives. First two panels show the seasonal volume
## and the fact that dry hours are way more common than the wet hours. The two other panels show that extreme outliers
## exist in the data, which could signal extreme weather events or faulty measurements by the stations

## The reader is drawn mostly to the bright red dots in panel 4 signaling there are suspicious data that need to be
## taken in consideration before trusting the monthly totals.

## We don't know if the jumps in the last panel are severe storms or sensor failures. Similarly, we can't be sure whether
## the dry periods are seasonal droughts or if the sensors went offline and incorrectly reported zeros.

ggsave(
  filename = "fig_dry_and_rain_comparison.png",
  plot = dry_rain,
  width = 12,
  height = 6,
  units = "cm"
)

ggsave(
  filename = "fig_monthly_precipitation.png",
  plot = plot_monthly,
  width = 10,
  height = 7,
  units = "cm"
)

## Which visualisation was the most informative?
## The one with the precipitation per month, it clearly showed the rainfall per the whole data set without the data being
## too skewed

## Which visualisation was the hardest to design well?
## The one with the suspicious jumps in the data. It required selecting the appropriate jumps and drawing the line to make
## everything clear

## Which plotting choices were most important for readability?
## Deciding whether the legend should be present or not. In some cases it would make the whole plot a little unreadable.
## Also the overplotting created issues and made the graphs unreadable without further adjustments.

## What suspicious data problems became visible only after plotting?
## The spikes in the precipitation. This highlighted that there could be some issues with the stations since the precipitation
## fell from nearly 0 to 50 and right back. The histogram also revealed that some stations measure unusual amounts of
## high precipitation.

## Which station looked most unusual?
## 0-203-0-10501021001 - it had the most jumps between the heavy and zero precipitation which is not how usually precipitation
## occurs in an hourly manner

## Which ELEMENT looked most trustworthy, and which least trustworthy?
## Neither. There's only one unique element. 

## Which default ggplot2 choices would have produced a poor or misleading result here?
## By default the alpha is set to 0 making very dense data unreadable. The stacked bar (default of geom_col) would stack the 
## data on top of each other making it visually unreadable. The scale for histogram would force all the values into one block, 
## by using the logarithm scale it allows even the tiny values to be visible. 