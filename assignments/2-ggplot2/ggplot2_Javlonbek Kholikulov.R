## ggplot2 Assignment - Precipitation Data Analysis
## Javlonbek Kholikulov


## Part 1 - Load data

dta <- readRDS(file = "C:/Users/xoliq/Downloads/prec_data.rds")

library(data.table)
library(ggplot2)
library(lubridate)
library(scales)
library(patchwork)

dta_t <- as.data.table(x = dta)


## Part 2 - Basic inspection for plotting

class(x = dta)
class(x = dta_t)

head(x = dta_t)
colnames(x = dta_t)

nrow(x = dta_t)
ncol(x = dta_t)

min(dta_t$DT)
max(dta_t$DT)

uniqueN(x = dta_t$STATION)
unique(x = dta_t$ELEMENT)

summary(dta_t$VALUE)

sum(is.na(dta_t$QUALITY))
sum(!is.na(dta_t$FLAG) & dta_t$FLAG != "")

## x axis candidates: DT, year, month
## y axis candidate: VALUE (precipitation mm)
## colour / group: STATION (for comparison across stations)
## facet: STATION (too many to show on one panel)
## STATION mapped to colour with all 431 levels would be unreadable
## FLAG is a character code and should not go on the y axis directly


## Part 3 - Data preparation for visualisation

## year and month allow time-based grouping and faceting
dta_t[, year  := year(x = DT)]
dta_t[, month := month(x = DT)]

## day-of-year allows overlaying seasonal cycles across years
dta_t[, doy := yday(x = DT)]

## has_rain separates dry hours from truly rainy ones
## most values are zero; keeping them obscures the rainfall distribution
dta_t[, has_rain := VALUE > 0]

## suspicious flags values above 100mm/h or below zero 
dta_t[, suspicious := VALUE > 100 | VALUE < 0]

## has_flag marks rows where FLAG is not empty
dta_t[, has_flag := !is.na(FLAG) & FLAG != ""]

## monthly totals used in several temporal plots
dta_monthly <- dta_t[, .(monthly_total = sum(VALUE, na.rm = TRUE)),
                     by = .(STATION, year, month)]
dta_monthly[, date := as.Date(paste(year, month, "01", sep = "-"))]


## Part 4 - Station selection strategy

## compute per-station summary to define selection rule
st_stats <- dta_t[, .(total_rainfall = sum(VALUE,    na.rm = TRUE),
                      num_records    = .N,
                      num_rainy      = sum(has_rain,  na.rm = TRUE),
                      max_value      = max(VALUE,     na.rm = TRUE),
                      num_suspicious = sum(suspicious, na.rm = TRUE),
                      num_flagged    = sum(has_flag,  na.rm = TRUE)),
                  by = STATION]

st_stats[, rain_freq := num_rainy / num_records]
st_stats <- st_stats[order(-total_rainfall)]

## rule: top 6 stations by total accumulated rainfall
## these have the most variation and are more likely to show seasonal patterns
## what is lost: dry stations and stations with interesting flag patterns are excluded
st_sel <- st_stats[1:6, STATION]

st_sel
st_stats[STATION %in% st_sel]

dta_sel <- dta_t[STATION %in% st_sel]


## Part 5 - Temporal plots

## Plot A - raw hourly time series
## geom_line connects hourly readings and reveals spikes and gaps
## free_y scales prevent low-rainfall stations from appearing flat
## zeros are kept visible because the dry baseline matters

p1 <- ggplot(data = dta_sel,
             mapping = aes(x = DT, y = VALUE)) +
  geom_line(linewidth = 0.3, colour = "steelblue") +
  facet_wrap(facets = ~ STATION, ncol = 2, scales = "free_y") +
  labs(title = "Plot A: Raw hourly precipitation time series",
       subtitle = "Free y-scales because station magnitudes differ",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))

p1

## Plot B - monthly aggregated time series
## monthly totals smooth noise and reveal seasonal cycles
## fixed y-scales allow direct comparison of absolute amounts across stations
## information lost: individual extreme hourly events are absorbed into the monthly sum

dta_month_sel <- dta_monthly[STATION %in% st_sel]

p2 <- ggplot(data = dta_month_sel,
             mapping = aes(x = date, y = monthly_total)) +
  geom_line(colour = "steelblue") +
  geom_point(size = 1, colour = "steelblue") +
  facet_wrap(facets = ~ STATION, ncol = 2) +
  scale_x_date(date_labels = "%Y") +
  labs(title = "Plot B: Monthly precipitation totals",
       subtitle = "Fixed y-scales allow direct comparison across stations",
       x = "Date",
       y = "Monthly total (mm)") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))

p2

## Plot C - extreme event view
## shows only the top 10 hourly values per station
## red dashed line marks 50mm as a visual threshold for extreme events
## point size encodes magnitude so the largest events stand out

dta_extremes <- dta_sel[, .SD[order(-VALUE)][1:10], by = STATION]

p3 <- ggplot(data = dta_extremes,
             mapping = aes(x = DT, y = VALUE, colour = STATION)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "red", linewidth = 0.7) +
  geom_point(mapping = aes(size = VALUE), alpha = 0.8) +
  annotate(geom = "text", x = min(dta_extremes$DT), y = 52,
           label = "50mm threshold", colour = "red", hjust = 0, size = 3) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Plot C: Top 10 extreme hourly events per station",
       subtitle = "Point size and colour both encode magnitude",
       x = "Date",
       y = "Precipitation (mm)",
       colour = "Station",
       size = "Value (mm)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6))

p3


## Part 6 - Distribution plots

## Distribution plot 1 - boxplot of non-zero values
## zeros are excluded because they inflate the distribution and make median = 0 for all stations
## log10 y-scale is used because precipitation is extremely right-skewed
## stations ordered by median so ranking is immediately visible

p4 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = reorder(STATION, VALUE, FUN = median), y = VALUE)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.8, outlier.alpha = 0.4) +
  coord_flip() +
  scale_y_log10(labels = label_number()) +
  labs(title = "Distribution plot 1: Non-zero precipitation by station",
       subtitle = "Log10 y-scale; stations ordered by median",
       x = "Station",
       y = "Precipitation mm (log10 scale)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))

p4

## Distribution plot 2 - ECDF comparing stations
## ECDF avoids bin-width decisions that can distort histograms for skewed data
## log x-scale spreads out the dense low-value region

p5 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = VALUE, colour = STATION)) +
  stat_ecdf() +
  scale_x_log10(labels = label_number()) +
  labs(title = "Distribution plot 2: ECDF of non-zero precipitation by station",
       subtitle = "ECDF avoids bin-width decisions; log x-scale handles skew",
       x = "Precipitation mm (log10 scale)",
       y = "Cumulative proportion",
       colour = "Station") +
  theme_bw() +
  theme(legend.text = element_text(size = 6),
        legend.position = "bottom")

p5


## Part 7 - Quality and suspicious-data plots

## suspicious defined in Part 3: VALUE > 100 or VALUE < 0
## 100mm/h is above any recorded hourly intensity in Europe

## Quality plot 1 - flag rate tile by station and month
## geom_tile shows presence of flags as fill colour per station-month cell
## combines temporal (x), quality indicator (fill), and station comparison (y)

dta_flag_monthly <- dta_t[STATION %in% st_sel,
                          .(flag_count = sum(has_flag, na.rm = TRUE),
                            total      = .N,
                            flag_rate  = mean(has_flag, na.rm = TRUE)),
                          by = .(STATION, year, month)]
dta_flag_monthly[, date := as.Date(paste(year, month, "01", sep = "-"))]

p6 <- ggplot(data = dta_flag_monthly,
             mapping = aes(x = date, y = STATION, fill = flag_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred",
                      labels = label_percent()) +
  scale_x_date(date_labels = "%Y") +
  labs(title = "Quality plot 1: Proportion of flagged observations per month",
       subtitle = "Dark red = high flag rate; white = no flags",
       x = "Date",
       y = "Station",
       fill = "Flag rate") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))

p6

## Quality plot 2 - suspicious and flagged counts per station per year
## long format required so quality_type can be mapped to fill colour

dta_quality <- dta_t[STATION %in% st_sel,
                     .(num_suspicious      = sum(suspicious,   na.rm = TRUE),
                       num_flagged         = sum(has_flag,     na.rm = TRUE),
                       num_missing_quality = sum(is.na(QUALITY))),
                     by = .(STATION, year)]

dta_m <- melt(data       = dta_quality,
              id.vars    = c("STATION", "year"),
              measure.vars = c("num_suspicious", "num_flagged", "num_missing_quality"),
              variable.name = "quality_type",
              value.name    = "count")

p7 <- ggplot(data = dta_m,
             mapping = aes(x = year, y = count, fill = quality_type)) +
  geom_col(position = "dodge") +
  facet_wrap(facets = ~ STATION, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("num_suspicious"      = "firebrick",
                               "num_flagged"         = "orange",
                               "num_missing_quality" = "grey50"),
                    labels = c("Suspicious (>100 or <0)",
                               "Has flag",
                               "Missing quality")) +
  labs(title = "Quality plot 2: Quality problems by station and year",
       subtitle = "Three quality indicators compared across stations",
       x = "Year",
       y = "Count of observations",
       fill = "Problem type") +
  theme_bw() +
  theme(strip.text = element_text(size = 7),
        legend.position = "bottom")

p7


## Part 8 - Element comparison plots

el_sel <- unique(x = dta_t$ELEMENT)
el_sel

## only one element (SRA1H) exists so comparison is across stations
## violin + boxplot overlay shows full shape and summary statistics together
## log y-scale needed because linear scale compresses all variation near zero

p8 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = reorder(STATION, VALUE, FUN = mean), y = VALUE)) +
  geom_violin(fill = "lightblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.size = 0.5) +
  coord_flip() +
  facet_wrap(facets = ~ ELEMENT) +
  scale_y_log10(labels = label_number()) +
  labs(title = "Element comparison plot 1: Value distribution by station",
       subtitle = "Violin + boxplot overlay; faceted by element (only SRA1H exists)",
       x = "Station",
       y = "Precipitation mm (log10 scale)",
       caption = "Only non-zero values shown. Log scale handles extreme skew.") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))

p8

## seasonal pattern by station and element
## if stations disagree on seasonality the element's reliability is questioned

dta_monthly_mean <- dta_t[STATION %in% st_sel,
                          .(mean_rain = mean(VALUE, na.rm = TRUE)),
                          by = .(STATION, month, ELEMENT)]

p9 <- ggplot(data = dta_monthly_mean,
             mapping = aes(x = month, y = mean_rain, colour = STATION)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(facets = ~ ELEMENT) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Element comparison plot 2: Mean hourly rainfall by month and station",
       subtitle = "Disagreement in seasonality would question element reliability",
       x = "Month",
       y = "Mean hourly precipitation (mm)",
       colour = "Station") +
  theme_bw() +
  theme(legend.text = element_text(size = 6),
        legend.position = "bottom")

p9

## seasonal patterns look consistent across stations which suggests SRA1H is reliable
## suspicious values were extremely rare given the size of the dataset


## Part 9 - Grammar of Graphics explanation

## p1 (raw hourly time series):
## data:    dta_sel - dta_t filtered to 6 selected stations
## mapping: x = DT, y = VALUE
## geom:    geom_line - connects hourly observations to show continuity
## stat:    identity - raw values plotted without summary
## facet:   facet_wrap by STATION - separates 6 stations into readable panels
## scale:   scales = "free_y" - each station gets its own y range
## theme:   theme_bw; strip text reduced for long station names

## p4 (boxplot of non-zero values):
## data:    dta_sel filtered to VALUE > 0
## mapping: x = STATION reordered by median, y = VALUE
## geom:    geom_boxplot - shows median, IQR, and outliers
## stat:    boxplot stat computes quartiles internally
## facet:   none
## scale:   scale_y_log10 - handles extreme right skew
## theme:   coord_flip for readable station labels; theme_bw

## p6 (quality tile):
## data:    dta_flag_monthly - flag rates aggregated by station and month
## mapping: x = date, y = STATION, fill = flag_rate
## geom:    geom_tile - one tile per station-month cell
## stat:    identity - flag_rate already computed in preparation
## facet:   none - station is on the y axis instead
## scale:   scale_fill_gradient white to darkred
## theme:   theme_bw; axis text reduced

## p7 (quality bar chart):
## data:    dta_m - long-format quality summary from melt()
## mapping: x = year, y = count, fill = quality_type
## geom:    geom_col with position = "dodge" - side-by-side bars
## stat:    identity - counts already computed before plotting
## facet:   facet_wrap by STATION, free_y scales
## scale:   scale_fill_manual with custom colours per quality type
## theme:   legend moved to bottom; strip text reduced


## Part 10 - Faceting and scale decisions

## Decision 1: facet_wrap over facet_grid
## facet_grid forces a strict grid; with one grouping variable it wastes space
## facet_wrap with ncol = 2 gives a compact two-column layout
## tried facet_grid(STATION ~ .) for p2 - all stations in one tall column, unreadable

## Decision 2: free vs fixed y-scales
## Plot A uses free_y: stations have different magnitudes; fixed scale flattens low stations
## Plot B uses fixed: the goal is comparing absolute monthly totals across stations
## opposite choices because the question being asked is different in each plot

## Decision 3: log vs linear scale
## p4, p5, p8 all use log scale on the value axis
## precipitation is highly right-skewed; linear scale compresses all variation near zero
## tried linear for p4 first - all boxes were flat near zero, uninformative - rejected

## Decision 4: ordering factor levels
## stations in p4 and p8 are ordered by median/mean using reorder()
## default alphabetical ordering has no statistical meaning


## Part 11 - Reshape step

head(x = dta_m)
str(dta_m)

## dta_quality had three separate count columns: num_suspicious, num_flagged, num_missing_quality
## ggplot works best with tidy long data where one column holds values and another holds the type
## melt() converts the three columns into quality_type (variable) and count (value)
## this lets us map quality_type to fill colour in a single geom_col call
## without reshaping we would need three separate geom_col layers, making the code fragile


## Part 12 - Multi-panel final figure

pA <- ggplot(data = dta_month_sel[STATION %in% st_sel[1:3]],
             mapping = aes(x = date, y = monthly_total, colour = STATION)) +
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "A: Monthly totals", x = NULL, y = "mm", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6),
        plot.title = element_text(size = 10))

pB <- ggplot(data = dta_sel[VALUE > 0 & VALUE <= 50],
             mapping = aes(x = VALUE, colour = STATION)) +
  stat_ecdf() +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "B: ECDF non-zero values", x = "mm", y = "Cumulative prop.", colour = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10))

pC <- ggplot(data = dta_flag_monthly,
             mapping = aes(x = date, y = STATION, fill = flag_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred", labels = label_percent()) +
  scale_x_date(date_labels = "%Y") +
  labs(title = "C: Flag rate by station/month", x = NULL, y = NULL, fill = "Rate") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 10))

pD <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = reorder(STATION, VALUE, FUN = median), y = VALUE)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.5) +
  coord_flip() +
  scale_y_log10(labels = label_number()) +
  labs(title = "D: Station distributions", x = NULL, y = "mm (log10)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 10))

p_final <- (pA + pB) / (pC + pD)
p_final

## story: the six wettest stations show clear seasonal cycles with wetter winters and autumns
## flags are concentrated in specific months at two stations, suggesting instrument issues
## precipitation distributions are right-skewed and stations differ slightly in intensity
## reader should notice first: which years produced the highest monthly totals
## uncertainty: FLAG codes are undefined in the dataset; totals may be affected by data gaps


## Part 13 - Saving output

ggsave(filename = "fig_monthly_timeseries.png",
       plot     = p2,
       width    = 10,
       height   = 6,
       dpi      = 300)

ggsave(filename = "fig_final_combined.png",
       plot   = p_final,
       width  = 12,
       height = 8,
       dpi    = 300)


## Part 14 - Final discussion

## Most informative visualisation:
## The monthly time series (p2) was most informative. Fixed y-scales across panels
## made it easy to compare absolute rainfall amounts across all six stations simultaneously,
## and the seasonal cycle became clearly visible after aggregation smoothed out hourly noise.

## Hardest to design:
## The quality tile plot (p6) required the most iteration. The first attempt used
## geom_point() on a logical TRUE/FALSE y-axis, which just stacked dots at two positions
## and was unreadable. Switching to monthly flag rates with geom_tile() made the
## temporal structure of flagging problems immediately clear.

## Most important choices for readability:
## 1. Faceting by STATION - without it six overlapping lines are unreadable
## 2. Removing zeros before distribution plots - keeping zeros makes median = 0 everywhere
## 3. Log scale on the value axis - linear scale hides all variation near zero
## 4. Ordering stations by median in boxplots - alphabetical order has no statistical meaning

## Suspicious problems visible only after plotting:
## The flag tile (p6) showed that flagging is concentrated in specific months,
## suggesting instrument failures rather than random errors.
## The extreme events plot (p3) showed that values near the 50mm threshold are
## concentrated in very few events, not spread evenly across the record.

## Most unusual station:
## 0-203-0-10801028001 stood out with a max value of 50.3mm in a single hour,
## which I noticed in Plot C as the only event close to the 50mm threshold.

## Most trustworthy element:
## SRA1H is the only element so no comparison is possible.
## From the seasonal plot it looks consistent across stations and I found
## almost no suspicious values in the data.

## Default ggplot2 choices that would have been misleading:
## 1. Plotting all 431 stations - colour mapping with 431 levels is unreadable
## 2. Fixed y-scales on the raw hourly plot (p1) - low-rainfall stations appear flat
## 3. Including zeros in boxplots - all medians become zero, hiding differences
## 4. Linear scale for skewed data - all variation invisible near zero
## 5. Alphabetical station ordering in boxplots - meaningless ordering

