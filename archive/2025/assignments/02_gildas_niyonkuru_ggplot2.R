## Assignment 2: ggplot2
## Gildas Pacifique Niyonkuru
## Czech University of Life Sciences Prague

library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

dta <- readRDS(file = "prec_data.rds")

dta_t <- as.data.table(x = dta)


## basic inspection

class(x = dta)
class(x = dta_t)

head(x = dta_t)

names(x = dta_t)

nrow(x = dta_t)
ncol(x = dta_t)

range(x = dta_t$DT, na.rm = TRUE)

dta_t[, uniqueN(x = STATION)]
dta_t[, uniqueN(x = ELEMENT)]

dta_t[, .(min = min(x = VALUE, na.rm = TRUE),
          max = max(x = VALUE, na.rm = TRUE),
          n_na = sum(is.na(x = VALUE)),
          n_zero = sum(VALUE == 0, na.rm = TRUE))]

## I decided to use DT as x and VALUE as y since that is the most natural
## STATION is what I want to use for colour and faceting
## I noticed plotting all 431 stations at once is impossible to read
## so I knew I had to select a subset before plotting anything


## data preparation

## I added year and month because I wanted to aggregate later
dta_t[, yr := year(x = DT)]
dta_t[, mo := month(x = DT)]
dta_t[, hr := hour(x = DT)]

## I created has_rain to separate actual rain events from dry hours
dta_t[, has_rain := VALUE > 0]

## I used the 99th percentile to flag extreme values
## I did not want to hardcode a threshold so this felt more data-driven
q99 <- dta_t[VALUE > 0, quantile(x = VALUE,
                                  probs = 0.99,
                                  na.rm = TRUE)]

dta_t[, suspicious := VALUE > q99]
dta_t[, missing_quality := is.na(x = QUALITY) | QUALITY == 0]

## I computed daily and monthly totals because hourly data is too noisy to plot
dta_daily <- dta_t[, .(daily_total = sum(x = VALUE, na.rm = TRUE)),
                   by = .(STATION, date = as.Date(x = DT))]

dta_monthly <- dta_t[, .(monthly_total = sum(x = VALUE, na.rm = TRUE)),
                     by = .(STATION, yr, mo)]


## station selection

## I wanted a mix of high-rainfall and problematic stations
## so I picked the 3 with highest total precipitation
## and 3 with the most suspicious values
## I think this makes the visual comparison more interesting
## than just picking the top 6 by one criterion

st_stats <- dta_t[, .(total_prec = sum(x = VALUE, na.rm = TRUE),
                      n_suspicious = sum(suspicious, na.rm = TRUE),
                      n_nonzero = sum(has_rain, na.rm = TRUE),
                      n_flagged = sum(!is.na(x = FLAG))),
                  by = STATION]

top3_prec <- st_stats[order(-total_prec)][1:3, STATION]
top3_susp <- st_stats[order(-n_suspicious)][1:3, STATION]

st_sel <- unique(x = c(top3_prec, top3_susp))

if (length(x = st_sel) < 6) {
    extra <- st_stats[!STATION %in% st_sel][order(-total_prec)][1:(6 - length(st_sel)), STATION]
    st_sel <- c(st_sel, extra)
}

st_sel <- st_sel[1:6]

dta_sel <- dta_t[STATION %in% st_sel]
dta_daily_sel <- dta_daily[STATION %in% st_sel]
dta_monthly_sel <- dta_monthly[STATION %in% st_sel]

## I am aware I lose information about average stations
## but for visual analysis I think extremes are more useful to show


## temporal plots

## I used facet_wrap here because I tried colour first
## but 6 overlapping lines were impossible to read
## I also kept zeros visible to show the dry periods
p1 <- ggplot(data = dta_sel[yr >= 2022],
             mapping = aes(x = DT,
                           y = VALUE,
                           colour = STATION)) +
  geom_line(alpha = 0.4,
            linewidth = 0.3) +
  facet_wrap(facets = ~ STATION,
             ncol = 2,
             scales = "free_y") +
  scale_colour_viridis_d(guide = "none") +
  labs(title = "Raw Hourly Precipitation",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1

## I switched to monthly aggregation here because the hourly plot
## was too noisy to see any seasonal pattern
## I wanted to see if there is a clear wet and dry season
p2 <- ggplot(data = dta_monthly_sel,
             mapping = aes(x = as.Date(paste0(yr, "-", mo, "-01")),
                           y = monthly_total,
                           colour = STATION)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(facets = ~ STATION,
             ncol = 2,
             scales = "free_y") +
  scale_colour_viridis_d(guide = "none") +
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year") +
  labs(title = "Monthly Total Precipitation",
       x = "Month",
       y = "Total Precipitation (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2

## I wanted to highlight the extreme events specifically
## I used red points on top of the daily line so you can see
## where the spikes happen in time without losing context
p3 <- ggplot(data = dta_daily_sel,
             mapping = aes(x = date,
                           y = daily_total,
                           colour = STATION)) +
  geom_line(alpha = 0.5,
            linewidth = 0.4) +
  geom_point(data = dta_sel[suspicious == TRUE],
             mapping = aes(x = as.Date(x = DT),
                           y = VALUE),
             colour = "red",
             size = 2,
             alpha = 0.7) +
  facet_wrap(facets = ~ STATION,
             ncol = 2,
             scales = "free_y") +
  scale_colour_viridis_d(guide = "none") +
  labs(title = "Daily Precipitation with Extreme Events",
       subtitle = "Red = values above 99th percentile",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_bw()

p3


## distribution plots

## I removed zeros here on purpose
## when I included them the whole histogram was just a spike at zero
## and I could not see anything about the actual rain distribution
p4 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = VALUE,
                           fill = STATION)) +
  geom_histogram(bins = 50,
                 alpha = 0.7) +
  facet_wrap(facets = ~ STATION,
             ncol = 2,
             scales = "free_y") +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Distribution of Non-Zero Precipitation",
       x = "Precipitation (mm)",
       y = "Count") +
  theme_bw()

p4

## I tried linear scale first but it was useless
## the distribution was so right-skewed that everything
## collapsed near zero so I switched to log scale
p5 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = VALUE,
                           colour = STATION)) +
  geom_density(linewidth = 0.8) +
  scale_x_log10(labels = label_number()) +
  scale_colour_viridis_d() +
  labs(title = "Density of Non-Zero Precipitation (log scale)",
       x = "Precipitation (mm)",
       y = "Density") +
  theme_bw()

p5

## I used stat_ecdf because I wanted to compare the full distributions
## not just the shape - I can see which stations have heavier tails
p6 <- ggplot(data = dta_sel[VALUE > 0],
             mapping = aes(x = VALUE,
                           colour = STATION)) +
  stat_ecdf(linewidth = 0.8) +
  scale_x_log10() +
  scale_colour_viridis_d() +
  labs(title = "ECDF of Non-Zero Precipitation",
       x = "Precipitation (mm)",
       y = "Cumulative Probability") +
  theme_bw()

p6


## quality plots

## I defined suspicious as either flagged or above 99th percentile
## I wanted to make this explicit in code rather than just visual
dta_sel[, quality_flag := fcase(
  !is.na(x = FLAG), "flagged",
  suspicious == TRUE, "extreme value",
  default = "normal"
)]

## I wanted to see quality problems in time not just as counts
## so I mapped colour to the quality flag over the time series
## this way I can see if problems cluster in certain periods
p7 <- ggplot(data = dta_sel[yr >= 2020],
             mapping = aes(x = DT,
                           y = VALUE,
                           colour = quality_flag)) +
  geom_point(size = 0.8,
             alpha = 0.6) +
  facet_wrap(facets = ~ STATION,
             ncol = 2,
             scales = "free_y") +
  scale_colour_manual(values = c("normal" = "grey70",
                                 "extreme value" = "orange",
                                 "flagged" = "red"),
                      name = "Quality") +
  labs(title = "Data Quality Over Time",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_bw() +
  theme(legend.position = "bottom")

p7

## I used a grouped bar chart to compare quality issues across stations
## I wanted to see which station has the most problems overall
quality_sum <- dta_sel[, .(n_flagged = sum(!is.na(x = FLAG)),
                            n_extreme = sum(suspicious, na.rm = TRUE),
                            n_missing_q = sum(missing_quality)),
                       by = STATION]

quality_long <- melt(data = quality_sum,
                     id.vars = "STATION",
                     variable.name = "issue_type",
                     value.name = "count")

p8 <- ggplot(data = quality_long,
             mapping = aes(x = STATION,
                           y = count,
                           fill = issue_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("n_flagged" = "red",
                               "n_extreme" = "orange",
                               "n_missing_q" = "steelblue"),
                    name = NULL,
                    labels = c("Flagged", "Extreme", "Missing quality")) +
  labs(title = "Quality Issues by Station",
       x = "Station",
       y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8


## element comparison

el_sel <- unique(x = dta_t$ELEMENT)

## there is only one element SRA1H in this dataset
## so I compared distributions across stations within that element
## I used log scale because the linear scale made all boxes look flat
p9 <- ggplot(data = dta_t[VALUE > 0 & STATION %in% st_sel],
             mapping = aes(x = STATION,
                           y = VALUE,
                           fill = ELEMENT)) +
  geom_boxplot(outlier.size = 0.5,
               outlier.alpha = 0.3) +
  scale_y_log10() +
  scale_fill_viridis_d() +
  facet_wrap(facets = ~ ELEMENT) +
  labs(title = "Precipitation Distribution by Station",
       x = "Station",
       y = "Precipitation (mm) log scale") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9


## reshape for plotting

dta_events <- dta_sel[VALUE > 0,
                      .(n_events = .N,
                        max_event = max(x = VALUE, na.rm = TRUE)),
                      by = .(STATION, yr, mo)]

dta_wide2 <- merge(x = dta_monthly_sel,
                   y = dta_events,
                   by = c("STATION", "yr", "mo"),
                   all.x = TRUE)

## I used melt here because I wanted to put three metrics
## into facets in one plot - without reshaping I would need
## three separate plots which is harder to compare
dta_m <- melt(data = dta_wide2,
              id.vars = c("STATION", "yr", "mo"),
              measure.vars = c("monthly_total", "n_events", "max_event"),
              variable.name = "metric",
              value.name = "value")

p10 <- ggplot(data = dta_m[STATION == st_sel[1]],
              mapping = aes(x = as.Date(paste0(yr, "-", mo, "-01")),
                            y = value,
                            colour = metric)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(facets = ~ metric,
             ncol = 1,
             scales = "free_y") +
  scale_colour_viridis_d(guide = "none") +
  labs(title = paste("Monthly Metrics —", st_sel[1]),
       x = "Month",
       y = "Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p10


## final multi-panel figure
## I combined four panels to tell a complete story
## temporal trends + distribution + quality over time + quality counts

final_fig <- (p2 | p5) / (p7 + theme(legend.position = "none") | p8) +
  plot_annotation(
    title = "Precipitation Data Visual Report",
    subtitle = "Monthly trends | Distribution | Quality over time | Station comparison",
    caption = "I am not certain whether extreme values are real events or sensor errors."
  )

final_fig


## save figures

ggsave(filename = "fig_station_timeseries.png",
       plot = p2,
       width = 10,
       height = 6,
       dpi = 150)

ggsave(filename = "fig_quality_flags.png",
       plot = p7,
       width = 10,
       height = 6,
       dpi = 150)

ggsave(filename = "fig_final_report.png",
       plot = final_fig,
       width = 14,
       height = 10,
       dpi = 150)


## final discussion

## the most informative plot for me was p7 because I could see
## that quality problems cluster in specific time periods per station
## that was something I could not see from tables at all

## the hardest to design was p3 because I had to balance
## the background daily line and the red extreme points
## without making it too cluttered

## the most important choices I made were removing zeros from histograms
## switching to log scale and using free_y in facets
## without these the plots were either flat or unreadable

## after plotting I noticed some stations have very sudden jumps
## that look more like sensor errors than real rain events
## I also saw some stations with suspiciously long flat zero runs

## the station with the most suspicious values looked most unusual to me
## SRA1H looks consistent overall but stations with many flagged rows
## I would not trust for further analysis without more investigation

## the default ggplot2 choices that would have misled me:
## including zeros in the histogram made everything look like zero
## linear scale on VALUE hid the entire distribution shape
## and trying to colour all 431 stations at once was completely unreadable
