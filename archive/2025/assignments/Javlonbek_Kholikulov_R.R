## Part 1. setup
## read raw data
dta <- readRDS(file = "C:/Users/xoliq/Downloads/prec_data.rds")

## load package
library(data.table)

## convert to data.table
dta_t <- as.data.table(x = dta)


## Part 2. basic inspection
## check object types
class(x = dta)
class(x = dta_t)

## first 6 rows
head(x = dta_t)

## size of both objects
object.size(x = dta)
object.size(x = dta_t)

## column names
colnames(x = dta_t)

## dimensions
nrow(x = dta_t)
ncol(x = dta_t)

## STATION: identifier for each weather station
## ELEMENT: type of measurement, all SRA1H (hourly rainfall)
## DT: date and time of each recorded hour
## VALUE: rainfall amount in mm
## FLAG: mostly empty, unclear meaning, needs checking
## QUALITY: mostly zero, needs checking
## X: completely empty column, should be removed


## Part 3. filtering and summary statistics
## filter only hours with rainfall
nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

## how many rainy hours recorded
nrow(x = nz)

## rainfall stats per station
nz[, .(mean = mean(x = VALUE),
       sd   = sd(x = VALUE),
       iqr  = IQR(x = VALUE),
       min  = min(x = VALUE),
       max  = max(x = VALUE)),
   by = STATION]

## station 0-203-0-10802004001 recorded 105.6mm max, needs investigation


## rainfall stats per station per year
nz[, .(mean = mean(x = VALUE),
       sd   = sd(x = VALUE),
       iqr  = IQR(x = VALUE),
       min  = min(x = VALUE),
       max  = max(x = VALUE)),
   by = .(STATION, year(x = DT))]

## Part 4. duplicate analysis
## count exact duplicate rows
sum(duplicated(x = dta_t))

## show examples of exact duplicates
dta_t[duplicated(x = dta_t)]

## check for logical duplicates - same station, same timestamp
dt <- dta_t[, .N, by = .(STATION, DT)]

## show logical duplicates
dt[N > 1]

## count logical duplicates
nrow(x = dt[N > 1])

## 2922 logical duplicates found
## likely caused by daylight saving time clock change in march
## same hour recorded twice for every station on the same dates
## exact duplicates are safe to remove


## Part 5. cleaning
## flag missing values in VALUE
dta_t[, missing_val := is.na(x = VALUE)]

## flag suspicious rainfall values
dta_t[, suspicious := VALUE < 0 | VALUE > 100]

## remove exact duplicates
dta_t <- unique(x = dta_t)

## create helper column to check missing QUALITY
dta_t[, helper_quality := is.na(x = QUALITY)]

## flag rows where FLAG column has an entry
dta_t[, has_flag := !is.na(x = FLAG) & FLAG != ""]

## remove helper column
dta_t[, helper_quality := NULL]


## Part 6. renaming and column work
## rename VALUE to clearer name
setnames(x = dta_t,
         old = c("VALUE"),
         new = c("precipitation_mm"),
         skip_absent = TRUE)

## extract year from DT for easier grouping
dta_t[, year := year(x = DT)]

## combined flag for any problematic row
dta_t[, is_flagged := has_flag | suspicious]

## row id within each station
dta_t[, row_id := 1:.N, by = STATION]


## Part 7. station summary
## summarise each station across all records
st_sum <- dta_t[, .(total_records        = .N,
                    avg_rainfall         = mean(x = precipitation_mm, na.rm = TRUE),
                    rainfall_variability = sd(x = precipitation_mm, na.rm = TRUE),
                    lowest_rainfall      = min(x = precipitation_mm, na.rm = TRUE),
                    highest_rainfall     = max(x = precipitation_mm, na.rm = TRUE),
                    rainy_hours          = sum(precipitation_mm > 0, na.rm = TRUE),
                    suspicious_records   = sum(suspicious, na.rm = TRUE)),
                by = STATION]

## stations with most records
st_sum[order(-total_records)]

## stations with highest average rainfall
st_sum[order(-avg_rainfall)]

## stations with suspicious values
st_sum[suspicious_records > 0][order(-suspicious_records)]

## most problematic station
st_sum[STATION == "0-203-0-10802004001"]

## station 0-203-0-11640 has highest average rainfall (0.195mm/hr)
## station 0-203-0-10802004001 is the only one with a suspicious record
## stations with ~61000 records have the most complete data


## Part 8. element summary
## summarise by element type
el_sum <- dta_t[, .(total_records   = .N,
                    num_stations    = uniqueN(x = STATION),
                    avg_rainfall    = mean(x = precipitation_mm, na.rm = TRUE),
                    sd_rainfall     = sd(x = precipitation_mm, na.rm = TRUE),
                    prop_missing    = mean(x = is.na(x = precipitation_mm)),
                    flagged_rows    = sum(has_flag, na.rm = TRUE),
                    suspicious_rows = sum(suspicious, na.rm = TRUE)),
                by = ELEMENT]

## view element summary
el_sum

## only one element present: SRA1H (hourly rainfall accumulation)
## cannot compare reliability across elements as there is only one
## SRA1H covers 431 stations with very low proportion of missing values (0.14%)
## only 1 suspicious row out of 25 million records
## SRA1H is the only option for further analysis and appears broadly reliable


## Part 9. required data.table features
## as.data.table(x = dta)              - Part 1
## filtering in i                      - Part 3, nz <- dta_t[VALUE > 0, ...]
## column selection with .()           - Part 3, .(STATION, DT, VALUE)
## grouped summary with by =           - Part 3, by = STATION
## .N                                  - Part 4, dt <- dta_t[, .N, by = .(STATION, DT)]
## :=                                  - Part 5, missing_val, suspicious, has_flag
## removing a column with := NULL      - Part 5, helper_quality := NULL
## setnames()                          - Part 6
## merge()                             - Part 10
## reshape with dcast() and melt()     - Part 11
## helper table dt                     - Part 4


## Part 10. merge task
## create helper table with zero values over full hourly date sequence
dt <- data.table(precipitation_mm = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                          to   = as.POSIXct(x = "2024-12-31 23:00:00"),
                          by   = "hour"))

## how many hours in full sequence
nrow(x = dt)

## merge with non-zero rainfall data
## all.x = TRUE keeps every hour, NAs where no rainfall was recorded
nz_merged <- merge(x     = dt,
                   y     = nz,
                   by    = "DT",
                   all.x = TRUE)

## view result
head(x = nz_merged)
nrow(x = nz_merged)

## how many hours had actual rainfall recorded
sum(!is.na(x = nz_merged$STATION))

## dt is a complete hourly time series from 2018 to 2024 with no gaps
## merging with nz maps real rainfall onto that timeline
## hours with no recorded rainfall show NA in the STATION column
## this makes it easy to spot gaps or periods where stations went silent


## Part 11. reshape task
## take top 5 stations by record count to keep it manageable
top5 <- st_sum[order(-total_records)][1:5, STATION]

## filter to top 5 stations for year 2022 only
dt_sub <- dta_t[STATION %in% top5 & year == 2022,
                .(STATION, DT, precipitation_mm)]

## reshape to wide format - each station gets its own column
dt_wide <- dcast(data      = dt_sub,
                 formula   = DT ~ STATION,
                 value.var = "precipitation_mm")

## view wide result
head(x = dt_wide)
ncol(x = dt_wide)
nrow(x = dt_wide)

## melt back to long format
dt_long <- melt(data          = dt_wide,
                id.vars       = "DT",
                variable.name = "STATION",
                value.name    = "precipitation_mm")

## view long result
head(x = dt_long)

## dcast put each station side by side making it easy to compare hours
## you can clearly see when multiple stations recorded rain at the same time
## melting back to long confirms no data was lost in the reshape


## Part 12. final conclusions

## main data problems:
## - 2846 duplicate rows from daylight saving time clock change in march
## - station 0-203-0-10802004001 recorded 105.6mm in one hour - implausible
## - column X was completely empty
## - FLAG and QUALITY columns were mostly empty or zero

## what was cleaned:
## - exact duplicates removed with unique()
## - suspicious and missing values flagged with :=
## - helper columns removed with := NULL

## uncertain decisions:
## - FLAG column meaning was unclear and could not be fully interpreted
## - correct reading for the duplicated clock-change hour is unknown

## most suspicious station:
## 0-203-0-10802004001 - only station above 100mm, fewer records than others

## most useful operations:
## := , .N , uniqueN() , dcast() , melt() , merge()