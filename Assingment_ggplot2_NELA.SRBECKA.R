
library(ggplot2)
library(data.table)
library(scales)
library(lubridate)
library(patchwork)
#library check

#LOADING THE DATA
dta <- readRDS(file = "/Users/nelasrbecka/Desktop/prec_data.rds")
dta_t <- as.data.table(x = dta)

#INSPECTION OF THE DATA FOR PLOTTING
class(x = dta)
class(x = dta_t)
head(x = dta_t)
#dataframe/datatable/shows the first few enteries

names(x = dta_t)#shows column names
dim(x = dta_t)#shows number of all rows and columns
range(dta_t$DT, na.rm = TRUE)#shows whole time range in enteries
dta_t[, unique(STATION)]#shows all stations
dta_t[, unique(ELEMENT)]#the legendary return of SRA1H and conformation of our solitary element

#1 the varible DT is the best candidatefor xaxsis cuz time is almost always the independant variable used to show trends and progression
#2 the variable VALUE is best for yaxis as it is the actual messurement we wish to visulise
#3 STATION: color, group, facet. as we saw the 1715 stations color and facet will help with distinguishing
#VALUE: alpha, size. could be used for emphasis
#4 mapping STATION to shape or linetype would be a bad idea since theres like 1700 stations


#DATA PREP FOR VISUALS
dta_t[, yr := year(x = DT)] #good if we later have to compare prec across the years
dta_t[, mo := month(x = DT)] #good for showing seasonal patterns
dta_t[, has_precip := VALUE > 0]#good if we need to diferentiate rainy days from dry days
dta_t[, monthly_sum := sum(VALUE, na.rm = TRUE), by = .(STATION, yr, mo)]#makes station-to-station comparisons cleared
dta_t[, is_missing_q := is.na(QUALITY) | QUALITY == ""]#usefull to show if some variables lack metadata


#STATION SELECTION STRAT
global_sd <- sd(dta_t$VALUE, na.rm = TRUE)
global_mean <- mean(dta_t$VALUE, na.rm = TRUE)

dt_stats <- dta_t[, .(
  total_precip = sum(VALUE, na.rm = TRUE),

  suspicious_count = sum(VALUE < 0 | VALUE > (global_mean + 3 * global_sd), na.rm = TRUE)
), by = STATION]
# the suspicious stations will be defiened as those who are statistical outliers from the colective mean

#the 3 wettest and the 3 most suspicous stations are seleceted for st_sel
st_top_precip <- dt_stats[order(-total_precip)][1:3, STATION]
st_suspicious <- dt_stats[order(-suspicious_count)][!(STATION %in% st_top_precip)][1:3, STATION]

st_sel <- as.character(c(st_top_precip, st_suspicious))
dt <- dta_t[STATION %in% st_sel]

#the rule indentifies 3 stations with the highest prec and 3 stations flaged as most suspicious by the
#criteria finding outliers by the mean rather then just value < 0 as this aproaach is not suficient enough

#selecting the high performing and hig error station we could compare a normal heavy rain pattern with a sensor error

#by focusing on extremes we will lose the middle ground in the plot. we wont see the stations with moderate consistant rainfall
#or those with perfectly clean but low volume record


#TEMPORAL PLOTS
#plot1: a raw time series for selected stations
p1 <- ggplot(data = dt, mapping = aes(x = DT, y = VALUE)) +
  geom_line(color = "steelblue") +
  facet_wrap(facets = ~STATION, scales = "free_y") +
  labs(title = "Daily precipitation trends",
       subtitle = "Raw values for 6 selected stations",
       x = "Date", y = "Precipitation (mm)") +
  theme_minimal()

plot(x = p1)
#facet warp is used to avoid overplotting. if all 6 stations were placed on one axis the lines would overlap and be unreadable
#zeros have to stay visible to cuz they represent dry days and cannot be cut from the visulisaton of the data


#plot2: Aggregated time series 
dt_mon <- dt[, .(mean.prec = mean(VALUE, na.rm = TRUE)),
             by = .(STATION, yr, mo)]
#computes aggregaton monthly per sattion

# Create a date proxy for the x-axis (1st of each month)
dt_mon[, date_label := as.Date(paste(yr, mo, "01", sep = "-"))]

# Compute monthly average precipitation for each selected station
dt_mon <- dt[, .(mean_prec = mean(x = VALUE, na.rm = TRUE)), 
                 by = .(STATION, yr, mo)]

dt_mon[, date_label := as.Date(paste(yr, mo, "01", sep = "-"))]

p2 <- ggplot(data = dt_mon,
             mapping = aes(x = date_label, y = mean_prec)) +
  geom_col(fill = "darkblue", width = 25) +
  facet_wrap(facets = ~STATION) +
  labs(title = "Monthly Average Precipitation",
       x = "Time (Monthly)",
       y = "Mean Precipitation (mm)") 
plot(x = p2)

#monthly agregation is appropraite as it smooths out daily noise and makes seasonal differances clearer
#after agregation the wet and dry periods become visible
#we do lose information regarding single day rains as they bleed into the monthy average


#plot3 — threshold exceedance view using 99th percentile

threshold <- quantile(dt$VALUE, probs = 0.995, na.rm = TRUE)

dt[, extreme := VALUE > threshold]

p3 <- ggplot(dt, aes(x = DT, y = VALUE)) +
  
  geom_point(
    data = dt[extreme == FALSE],
    color = "grey80",
    alpha = 0.1,
    size = 0.35) +
  
  geom_point(
    data = dt[extreme == TRUE],
    color = "firebrick",
    alpha = 0.9,
    size = 1.2) +
  
  geom_hline(
    yintercept = threshold,
    color = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  facet_wrap(~ STATION, scales = "free_y") +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Plot 3: Extreme Precipitation Events by Station",
    subtitle = paste("Red points exceed the 99th percentile threshold of",
                     round(threshold, 2), "mm"),
    x = "Year",
    y = "Precipitation (mm)"
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

plot(x = p3)

#plot three was killing me...
#the plot uses a 99.5th precentile threshold to define extreme prec events wich ia aproprate for skewed data 
#where a few large values dominate
#most observations are shown in very light grey to provide context, while only the highest values are red, the dashed line indicates the cutoff
#this makes the plot FINALY easy to read, reduces visual clutter


#DISTRIBUTIONS PLOTS

#plot1: distributions across stations
p6A <- ggplot(dt, aes(x = STATION, y = VALUE)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Precipitation by Station")
plot(x = p6A)

#plot2: handeling zeros and skews
p6B <- ggplot(dt[VALUE > 0], aes(x = VALUE)) +
  geom_histogram() +
  scale_x_log10()
plot(x = p6B)

#logscale is usefull because prec is highly skewed with many small values and few big ones
#many prec values are zero, so the dominate the distribution, removing them allows us to see the distribution of rainfalů more clearly
#a histogram could be misleading cur the large number of zero or near zero values would dominate the plot


#QUALITY AND SUSPICIOUS-DATA PLOTS
#the explicit suspicion rule
#missing, QUALITY, impossible negative value, or prec above the 99.5th precentile

sus_threshold <- quantile(dt$VALUE, probs = 0.995, na.rm = TRUE)
dt[, suspicious := is.na(QUALITY) | QUALITY == "" | VALUE < 0 | VALUE > sus_threshold]

#plot7A: suspicous values over time by station
p7a <- ggplot(dt, aes(x = DT, y = VALUE)) +
  geom_point(color = "grey80", alpha = 0.15, size = 0.35) +
  geom_point(
    data = dt[suspicious == TRUE],
    aes(x = DT, y = VALUE),
    color = "firebrick1",
    size = 1.3,
    alpha = 0.9
  ) +
  facet_wrap(~ STATION, scales = "free_y") +
  labs(
    title = "Plot7A: Suspicious Precipitation Values Over Time",
    subtitle = paste("Suspicious = missing QUALITY, negative values, or above",
                     round(sus_threshold, 2), "mm"),
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

plot(x = p7a)


#plot7B: percent suspicious by station
dt_quality <- dt[, .(
  n = .N,
  missing_quality = sum(is.na(QUALITY) | QUALITY == ""),
  suspicious_count = sum(suspicious, na.rm = TRUE),
  suspicious_percent = 100 * mean(suspicious, na.rm = TRUE)
), by = STATION]

p7b <- ggplot(dt_quality, aes(x = reorder(STATION, suspicious_percent),
                              y = suspicious_percent)) +
  geom_col(fill = "firebrick2") +
  coord_flip() +
  labs(
    title = "Plot7B: Percent of Suspicious Records by Station",
    subtitle = "Suspicious records include missing QUALITY, negative values, or extreme precipitation",
    x = "Station",
    y = "Suspicious records (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot(x = p7b)

#the suspicous values are difined as observations with missing QUALITY, impossible negative prec, or prec above the 99.5th prec.

#Plot7A combines time, station and suspicous status. It shows whether suspicous values occur randomly
#or cluster during certain periods or stack at particular stations.

#anomalies easier to compare and helps identify stations eith unusually high quality concerns


#PART 8 WAS SKIPPED ON ACOUNTS OF ONLY ONE ELEMENT


#GRAMMAR OF GRAPHICS EXPLANATION

#Plot 1 - Daily prec trends 

#data:
#The plot uses the filtered dataset dt, which contains daily precipitation
#values for 6 selected stations.

#mapping:
#DT is mapped to the x-axis and VALUE to the y-axis.

#geom:
#geom_line() is used to show temporal trends.

#stat:
#The default identity statistic is used (no transformation).

#facet:
#facet_wrap(~ STATION, scales = "free_y") is used to separate stations
#and avoid overplotting, while allowing each station its own scale.

#scale:
#Free y-scales are used because stations have different precipitation ranges.

#theme:
#theme_minimal() is used to reduce visual clutter and emphasize the data.


#Plot 2 - Monthly aggregation

#data:
#The plot uses dt_mon, which contains monthly average precipitation per station.

#mapping:
#date_label is mapped to the x-axis and mean_prec to the y-axis.

#geom:
#geom_col() is used to display aggregated values as bars.

#stat:
#Aggregation is computed manually beforehand, so the identity statistic is used.

#facet:
#facet_wrap(~ STATION) is used to compare stations separately.

#scale:
#A date scale is used on the x-axis to represent monthly progression.

#theme:
#The default theme is used to keep the focus on differences in monthly averages.


#Plot 3 — Extreme eventd

#data:
#The plot uses dt, with an additional logical variable 'extreme'
#indicating threshold exceedances.

#mapping:
#DT is mapped to the x-axis and VALUE to the y-axis.

#geom:
#geom_point() is used to display observations, with color distinguishing extremes.
#geom_hline() is used to indicate the threshold.

#stat:
#The default identity statistic is used.

#facet:
#facet_wrap(~ STATION, scales = "free_y") allows comparison across stations.

#scale:
#Y-axis expansion is adjusted to improve visibility of extreme values.

#theme:
#theme_bw() is used for clarity, and minor grid lines are removed to reduce clutter.


#Plot 6 — Distribution - boxplot

#data:
#The plot uses dt, containing precipitation values for selected stations.

#mapping:
#STATION is mapped to the x-axis and VALUE to the y-axis.

#geom:
#geom_boxplot() is used to summarize the distribution of precipitation values.

#stat:
#The boxplot uses built-in statistical summaries (median, quartiles, outliers).

#facet:
#Faceting is not used because station comparison is already handled on the x-axis.

#scale:
#No transformation is applied, though skewness is visible in the distribution.
#A log10 scale is used to adress strong right-skewness in precipitation data

#theme:
#A simple theme is used to emphasize differences between station distributions.


# FACETING AND SCALE DECISIONS

#facet_warp() vs facet_grid()
#I used facet_wrap(~ STATION) instead of facet_grid() because I am comparing
#multiple stations without a second grouping variable. facet_wrap() allows a
#flexible layout that uses space efficiently. facet_grid() would force a rigid
#row/column structure that is unnecessary here and would waste space.

#Free vs fixed scales
#I used scales = "free_y" in faceting because different stations have very
#different precipitation ranges. Using fixed scales would compress stations
#with lower values and make their variation hard to see. Free scales allow
#each station’s variability and extreme events to be visible.

#I initially tried using text labels to mark all extreme points in plot3, but this
#resulted in severe overplotting and made the plot unreadable. I removed the
#labels and instead used color and size to highlight extreme values, which
#communicates the information more clearly.


# RESHAPE TASK
dt_sum <- dt[, .(
  mean_prec = mean(VALUE, na.rm = TRUE),
  max_prec = max(VALUE, na.rm = TRUE),
  zero_pct = mean(VALUE == 0, na.rm = TRUE) * 100
), by = STATION]

dta_m <- melt(
  dt_sum,
  id.vars = "STATION",
  variable.name = "metric",
  value.name = "value"
)

p11 <- ggplot(dta_m, aes(x = STATION, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparison of Station-Level Metrics",
    x = "Station",
    y = "Value"
  ) +
  theme_bw()

plot(x = p11)

#reshaping was needed because rhe summary data were originally in a wide format,
#with seperate columns for each metric
#this structure is not well suited for ggplot, wich expects data in a long format

#by reshaping into long format using melt() I created a single metric variable and a corresponding value column
#this creates a simple way to compare multiple summaries in one plot

#this improves the design by enabling grouped comparisons across stations without
#the need to manually create separate plots for each metric


# MULTI-PANEL FINAL FIGS

final_fig <- (p1 | p3) /
  (p6A | p7b) +
  plot_annotation(
    title = "Precipitation Patterns, Extremes, and Data Quality Across Selected Stations",
    caption = paste(
      "This figure compares daily precipitation patterns, extreme-event exceedances,",
      "station-level distributions, and suspicious-data rates.",
      "The reader should first notice that a few stations have much larger extremes",
      "and different quality/anomaly patterns.",
      "Remaining uncertainty comes from missing QUALITY values and the fact that only",
      "six selected stations are shown."
    ),
    theme(
      strip.text = element_text(size = 7),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8)
    )
  )

plot(x = final_fig)

#the figure shows how prec varies across selected stations, 
#highlighting both typical patterns and extreme events. It combines temporal trends,
#distribution summaries and data-quality indicators to show that while most prec values
#are low some stations experience

#the reader should notice the contrast between common low prec and the rare large extremes
#differances in station are also apparent with some stations showing larger variability and maximums than others

#uncertainty remains due to incomplate QUALITY information and the presance of 
#suspicois values that may reflect measurements error rather than real prec records


# SAVING OUTPUT
ggsave(
  filename = "fig_extreme_precipitation_events.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "fig_final_multipanel_report.png",
  plot = final_fig,
  width = 14,
  height = 9,
  dpi = 300
)


# FINAL DISCUSSION
#1 p3 is the most informative and shows clearly highlighted rare high-prec events
#that are not visible in raw time series

#2 p3 was absolute hell with overplotting i had a lot of difficulty making extreme values
#visible without clutter, it will haunt my nightmares...

#3 using faceting, alpha transparency, and color highlighting were most important for readability

#4 long runs of zero values and unusually repeated values that became visible only after plotting

#5 the station with the highest maximum values and widest spread was most unusual 
# by my counts it was station 0-20000-0-11787

#6 only one ELEMENT recorded cant make comparisons

#7 no faceting and full opacity that lead to overplotting and hidding important patterns at start



#fun fact when i was overlooking the scrip i almost acidently deleted it and had something akin to a heart attack


