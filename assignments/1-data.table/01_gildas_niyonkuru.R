## data.table assignment
## Gildas Pacifique Niyonkuru
## Czech University of Life Sciences Prague

library(data.table)

dta <- readRDS(file = "prec_data.rds")

dta_t <- as.data.table(x = dta)


## basic inspection

class(x = dta)
class(x = dta_t)

head(x = dta_t)

object.size(x = dta)
object.size(x = dta_t)

names(x = dta_t)

nrow(x = dta_t)
ncol(x = dta_t)

str(object = dta_t)

summary(object = dta_t)

## STATION and ELEMENT are identifiers
## DT is datetime, VALUE is the rain measurement
## FLAG and QUALITY need checking
## X looks empty

dta_t[, uniqueN(x = STATION)]

dta_t[, .(min = min(x = VALUE, na.rm = TRUE),
          max = max(x = VALUE, na.rm = TRUE),
          n_na = sum(is.na(x = VALUE)),
          n_zero = sum(VALUE == 0, na.rm = TRUE),
          n_neg = sum(VALUE < 0, na.rm = TRUE))]

dta_t[, .N, by = FLAG]

dta_t[, .N, by = QUALITY]

dta_t[, .N, by = X]


## filter VALUE > 0

nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE, FLAG, QUALITY)]

nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = STATION]

nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = .(STATION, year(x = DT))]


## duplicate analysis

sum(duplicated(x = dta_t))

dta_t[duplicated(x = dta_t)][1:6]

## logical duplicates - same station and time
dt_dups <- dta_t[, .(n = .N),
                 by = .(STATION, DT)]

dt_dups[n > 1, .N]

dt_dups[n > 1][1:10]

## same station at same hour should not have two rows
## this is a data quality problem


## cleaning

dta_t[, missing_value := is.na(x = VALUE)]

dta_t[, neg_value := VALUE < 0]

dta_t[, has_flag := !is.na(x = FLAG)]

## X column has no information so remove it
dta_t[, X := NULL]

## remove exact duplicates
dta_t <- unique(x = dta_t)

nrow(x = dta_t)

## flag values above 99th percentile
q99 <- dta_t[VALUE > 0, quantile(x = VALUE,
                                  probs = 0.99,
                                  na.rm = TRUE)]

dta_t[, extreme_value := VALUE > q99]


## rename and add columns

setnames(x = dta_t,
         old = c("QUALITY"),
         new = c("quality_code"),
         skip_absent = TRUE)

setnames(x = dta_t,
         old = c("FLAG"),
         new = c("flag_code"),
         skip_absent = TRUE)

dta_t[, yr := year(x = DT)]

dta_t[, mo := month(x = DT)]

## helper column then remove it
dta_t[, row_id := 1:.N,
      by = STATION]

dta_t[, row_id := NULL]


## station summary

st_sum <- dta_t[, .(n_rows = .N,
                    mean_val = mean(x = VALUE, na.rm = TRUE),
                    sd_val = sd(x = VALUE, na.rm = TRUE),
                    min_val = min(x = VALUE, na.rm = TRUE),
                    max_val = max(x = VALUE, na.rm = TRUE),
                    n_nonzero = sum(VALUE > 0, na.rm = TRUE),
                    n_missing = sum(missing_value),
                    n_flagged = sum(has_flag, na.rm = TRUE),
                    n_extreme = sum(extreme_value, na.rm = TRUE)),
                by = STATION]

st_sum[order(-n_rows)][1:10]

st_sum[order(-mean_val)][1:10]

st_sum[order(-n_flagged)][1:10]

st_sum[order(-n_extreme)][1:10]


## element summary

el_sum <- dta_t[, .(n_rows = .N,
                    n_stations = uniqueN(x = STATION),
                    mean_val = mean(x = VALUE, na.rm = TRUE),
                    sd_val = sd(x = VALUE, na.rm = TRUE),
                    prop_missing = mean(x = is.na(x = VALUE)),
                    n_flagged = sum(has_flag, na.rm = TRUE),
                    n_questionable = sum(quality_code > 0,
                                        na.rm = TRUE)),
                by = ELEMENT]

print(x = el_sum)

## only one element in this dataset


## merge task

dt <- data.table(VALUE = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                          to = as.POSIXct(x = "2026-01-01 00:00:00"),
                          by = "hour"))

## take one station for the merge
one_station <- nz[STATION == nz[1, STATION], .(DT, VALUE)]

## merge to see gaps in the time series
dt_merged <- merge(x = dt,
                   y = one_station,
                   by = "DT",
                   all.x = TRUE)

setnames(x = dt_merged,
         old = c("VALUE.x", "VALUE.y"),
         new = c("zero_fill", "observed"),
         skip_absent = TRUE)

dt_merged[, no_data := is.na(x = observed)]

dt_merged[, .(total = .N,
              with_data = sum(!no_data),
              without_data = sum(no_data))]

## the helper table fills in all expected hours
## merging shows which hours have no recorded rain
## useful for finding gaps in the station data


## reshape task

top5 <- st_sum[order(-n_rows)][1:5, STATION]

dt_yr <- dta_t[STATION %in% top5 & VALUE > 0,
               .(mean_val = round(x = mean(x = VALUE,
                                           na.rm = TRUE),
                                  digits = 2)),
               by = .(STATION, yr)]

## wide format - one column per station
dt_wide <- dcast(data = dt_yr,
                 formula = yr ~ STATION,
                 value.var = "mean_val")

print(x = dt_wide)

## back to long
dt_long <- melt(data = dt_wide,
                id.vars = "yr",
                variable.name = "STATION",
                value.name = "mean_val")

print(x = dt_long)

## wide format was easier to compare stations across years


## final conclusions

## main problems:
## - X column was empty and removed
## - some stations had duplicate rows for the same hour
## - a few VALUE entries were negative which is not possible for rain
## - FLAG and QUALITY were mostly empty

## uncertain decisions:
## - zeros might be real zeros or missing data, hard to tell
## - quality_code of 0 could mean good or just not filled in
## - 99th percentile cutoff for extreme values is a rough guess

## most suspicious stations had the most extreme and flagged values
## the most useful operations were := for flagging,
## grouping with by, and dcast/melt for reshaping

cat("rows after cleaning:", nrow(x = dta_t), "\n")
cat("stations:", nrow(x = st_sum), "\n")
cat("elements:", nrow(x = el_sum), "\n")
