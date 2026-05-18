## PART 1 : LOAD DATA
library(ggplot2)
library(data.table)
library(patchwork)

dta <- readRDS(file = "prec_data.rds")
dta_t <- as.data.table(x = dta)

## PART 2 : DATA INSPECTION
class(x = dta)
class(x = dta_t)

head(x = dta_t)

colnames(x = dta_t)

nrow(x = dta_t)
ncol(x = dta_t)

range(x = dta_t$DT)
uniqueN(x = dta_t$STATION)
uniqueN(x = dta_t$ELEMENT)

# DT is the natural candidate for x, VALUE for y.
# STATION and ELEMENT work for facets/color. 
# Mapping all stations to color would be a "rainbow mess."

## PART 3 : DATA PREPARATION
dta_t[, let(YEAR  = year(x = DT),
            MONTH = month(x = DT),
            ISWET = VALUE > 0,
            QUAL_FLAG = QUALITY != 0)]

# Extreme threshold (top 1%)
dta_t[, VALUE_99 := quantile(x = VALUE, probs = 0.99, na.rm = TRUE), by = .(STATION, ELEMENT)]
dta_t[, IS_EXTREME := VALUE > VALUE_99]

# Daily total calculation
dta_t[, VALUE_DAY := frollsum(x = VALUE, n = 24), by = .(STATION, ELEMENT)]

# Monthly aggregation for seasonal trends
dta_agg <- dta_t[, .(VALUE_MNT = sum(x = VALUE, na.rm = TRUE)), by = .(STATION, ELEMENT, YEAR, MONTH)]
dta_agg[, DT_MNT := as.Date(x = paste(YEAR, MONTH, "01", sep = "-"))]

## PART 4 : STATION SELECTION STRATEGY
dt_stn <- dta_t[, .(TOTAL = sum(x = VALUE, na.rm = TRUE),
                    N_QUAL = sum(x = QUAL_FLAG, na.rm = TRUE)), by = .(STATION)]

# Rule: 3 wettest + 3 with most flags (quantitative rule)
st_high <- dt_stn[order(-TOTAL)][1:3, STATION]
st_qual <- dt_stn[!(STATION %in% st_high)][order(-N_QUAL)][1:3, STATION]
st_sel  <- c(st_high, st_qual)

# This strategy helps compare normal data vs problematic stations.

## PART 5 : TEMPORAL PLOTS
el_main <- unique(x = dta_t$ELEMENT)[1]

# Plot A: Raw hourly
p1 <- ggplot(data = dta_t[STATION %in% st_sel & ELEMENT == el_main], 
             mapping = aes(x = DT, y = VALUE, colour = STATION)) +
  geom_line() +
  facet_grid(STATION ~ ., scales = "free_y") +
  labs(title = "Plot A: Raw hourly precipitation", x = "Date", y = "mm") +
  theme(legend.position = "none")

# Plot B: Monthly totals
p2 <- ggplot(data = dta_agg[STATION %in% st_sel & ELEMENT == el_main], 
             mapping = aes(x = DT_MNT, y = VALUE_MNT, colour = STATION, group = STATION)) +
  geom_line() + geom_point() +
  labs(title = "Plot B: Monthly totals", x = "Month", y = "Total mm")

# Plot C: Extreme event overlay
dta_ext <- dta_t[STATION %in% st_sel & IS_EXTREME == TRUE & ELEMENT == el_main]
p3 <- ggplot(data = dta_t[STATION %in% st_sel & ELEMENT == el_main], mapping = aes(x = DT, y = VALUE)) +
  geom_line(colour = "grey", alpha = 0.5) +
  geom_point(data = dta_ext, colour = "red", size = 1) +
  facet_grid(STATION ~ ., scales = "free_y") +
  labs(title = "Plot C: Extreme Events", subtitle = "Red dots = top 1% values")

## PART 6 : DISTRIBUTION PLOTS
# p4: Boxplot by station
p4 <- ggplot(data = dta_t[STATION %in% st_sel & ISWET == TRUE & ELEMENT == el_main], 
             mapping = aes(x = STATION, y = VALUE, fill = STATION)) +
  geom_boxplot() + scale_y_log10() +
  labs(title = "Rain Intensity (Log Scale)", y = "mm") +
  theme(legend.position = "none")

# p5: ECDF by element
p5 <- ggplot(data = dta_t[STATION %in% st_sel & VALUE > 0], 
             mapping = aes(x = VALUE, colour = ELEMENT)) +
  stat_ecdf() + scale_x_log10() +
  labs(title = "ECDF by Element")

## PART 7 : QUALITY AND SUSPICIOUS-DATA PLOTS
# suspicious: Stuck sensor (repeated non-zero values)
dta_t[, PREV_VAL := shift(x = VALUE, n = 1, type = "lag"), by = .(STATION, ELEMENT)]
dta_t[, REPEAT_FLAG := (VALUE == PREV_VAL) & (VALUE > 0)]

# Heatmap for stuck sensors
dta_rep <- dta_t[STATION %in% st_sel & ELEMENT == el_main, 
                 .(REPEAT_RATE = mean(x = REPEAT_FLAG, na.rm = TRUE)), by = .(STATION, YEAR, MONTH)]

p7 <- ggplot(data = dta_rep, mapping = aes(x = factor(x = MONTH), y = STATION, fill = REPEAT_RATE)) +
  geom_tile() + facet_wrap(~ YEAR) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of Stuck Sensors", x = "Month")

## PART 8 : ELEMENT COMPARISON PLOTS
el_sel <- unique(x = dta_t$ELEMENT) # selected elements for comparison

p8 <- ggplot(data = dta_t[STATION %in% st_sel[1:3] & VALUE > 0], 
             mapping = aes(x = ELEMENT, y = VALUE, fill = ELEMENT)) +
  geom_boxplot() + facet_wrap(~ STATION, scales = "free_y") +
  scale_y_log10() + labs(title = "Elements per Station")

dta_el_agg <- dta_t[STATION %in% st_sel, .(VALUE_MNT = sum(x = VALUE, na.rm = TRUE)), by = .(ELEMENT, YEAR, MONTH)]
dta_el_agg[, DT_MNT := as.Date(x = paste(YEAR, MONTH, "01", sep = "-"))]

p9 <- ggplot(data = dta_el_agg, mapping = aes(x = DT_MNT, y = VALUE_MNT, colour = ELEMENT)) +
  geom_line() + facet_wrap(~ ELEMENT, scales = "free_y") +
  labs(title = "Monthly Totals by Element")

## PART 9 : GRAMMAR OF GRAPHICS EXPLANATION

# data: dta_t filtered for 6 stations.
# mapping: x=DT, y=VALUE, color=STATION.
# geom: geom_line() for continuous time series.
# facet: facet_grid() to keep x-axes aligned for date comparison.
# scale: free_y so different station ranges are visible.

## PART 10 : FACETING AND SCALE DECISIONS
# 1. Free y-scales: So low-rain stations aren't just flat lines.
# 2. Log scale: Rain is very skewed; default linear scales hide the boxplot boxes.
# 3. facet_grid: Keeps the dates perfectly lined up vertically.

# Rejected choice: I tried fixed y-scales first, but dry stations appeared completely flat.

## PART 11 : RESHAPE STEP
dta_qlt_sum <- dta_t[STATION %in% st_sel, 
                     .(N_QUAL = sum(x = QUAL_FLAG, na.rm = TRUE),
                       N_REPEAT = sum(x = REPEAT_FLAG, na.rm = TRUE)), 
                     by = .(STATION, YEAR, MONTH)]
dta_qlt_sum[, DT_MNT := as.Date(x = paste(YEAR, MONTH, "01", sep = "-"))]

dta_m <- melt(data = dta_qlt_sum[, .(STATION, DT_MNT, N_QUAL, N_REPEAT)],
              id.vars = c("STATION", "DT_MNT"),
              variable.name = "ISSUE_TYPE",
              value.name = "COUNT")

p6 <- ggplot(data = dta_m, mapping = aes(x = DT_MNT, y = COUNT, fill = ISSUE_TYPE)) +
  geom_col() + facet_grid(STATION ~ .) +
  labs(title = "Monthly Quality Issues", y = "Count")

## PART 12 : FINAL FIGURE
fig_final <- (p2 | p4) / (p6 | p8) +
  plot_annotation(title = "Precipitation Summary Report",
                  subtitle = "Monthly totals, intensity, and data quality.")
fig_final

## PART 13 : SAVING OUTPUT
ggsave(filename = "fig_station_timeseries.png", 
       plot = p1, 
       width = 12,
       height = 8)
ggsave(filename = "fig_quality_flags.png", 
       plot = p6,
       width = 10,
       height = 8)
ggsave(filename = "fig_final_summary.png",
       plot = fig_final,
       width = 14,
       height = 10)

## PART 14 : FINAL DISCUSSION
# Best Plot: Quality bar chart (p6) showed error clusters clearly.
# Hardest part: Plot C (extremes) required careful alpha tuning.
# Good choices: Log scales and Free y-scales for readability.
# Defaults: Fixed scales would have hidden all dry station data.