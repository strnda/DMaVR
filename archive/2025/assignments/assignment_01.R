## load data
dta <- readRDS(file = "prec_data.rds")
## load package
library(data.table)
library(lubridate)
## convert to data.table
dta_t <- as.data.table(x = dta)

## basic inspection
class(dta_t)
class(dta)
head(dta_t)
head(dta)
object.size(dta_t)
object.size(dta)

## filter VALUE > 0
nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]
## summary by station
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)), by = STATION]
## summary by station and year
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
       by = .(STATION, year(x = DT))]

## duplicate analysis
dups <- dta_t[duplicated(dta_t)]
n_dupa <- nrow(dups)

head(dups)

     ##logical duplicates
## logical duplicates aren't exact copies but have same measurement context,
## here Logical duplicates are rows that have the same STATION and DT, 
## because each station is expected to have only one precipitation value per day

n_logical_dups <- nrow(dta_t) - nrow(unique(dta_t[, .(STATION, DT)]))
## they can be removed, because they don't contain any new information

## cleaning using :=
dta_clean <- dta_t
dta_clean[, missing_values := is.na(VALUE)]
dta_clean <- unique(dta_clean)
dta_clean[, sus_values := VALUE < 0 | VALUE > 500]

## rename columns
dta_final <- dta_clean

setnames(x = dta_final,
         old = c("VALUE", "DT"),
         new = c("PRECIP", "DATE"),
         skip_absent = TRUE)
dta_final[, YEAR := year(DATE)] 

## station summary
st_sum <- dta_final[, .(n_rows = .N,
  mean_value = mean(PRECIP, na.rm = TRUE),
  sd_value = sd(PRECIP, na.rm = TRUE),
  min_value = min(PRECIP, na.rm = TRUE),
  max_value = max(PRECIP, na.rm = TRUE),
  n_nonzero = sum(PRECIP > 0, na.rm = TRUE),
  n_sus = sum(sus_values | FLAG %in% c("S","E"), na.rm = TRUE)
), by = STATION]

summary(st_sum)
#most problematic station: 0-203-0-10802004001
st_sum[which.max(n_nonzero)]
#Highest non-zero precipitation: 0-203-0-11640
#stations with: 61363 rows has most records 
#For my data non of them are suspicious

## element summary
el_sum <- dta_final[, .(
  n_rows = .N,
  n_stations = uniqueN(STATION),
  mean_value = mean(PRECIP, na.rm = TRUE),
  sd_value = sd(PRECIP, na.rm = TRUE),
  prop_missing = mean(is.na(PRECIP)),
  n_flagged = sum(!is.na(FLAG)),
  n_questionable = sum(sus_values | FLAG %in% c("S","E"), na.rm = TRUE)
), by = ELEMENT]

## There is only one type of Element which is: SRA1H, and has huge number of observations
## so this is the most and least reliable.

## Do different ELEMENTs seem to have different scales?
## there is only one element

## Which ELEMENT would you trust most for further analysis?
## probably SRA1H, it seems reliable I guess, since it's the ONLY one :)

## merge task
dta_t[, DT := as.POSIXct(DT)]

nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

dta_helper <- data.table(
  DATE = seq(
    from = as.POSIXct("2018-01-01 00:00:00"),
    to   = as.POSIXct("2026-01-01 00:00:00"),
    by   = "hour"
  ),
  PRECIP = 0
)

stations <- unique(dta_t$STATION)

dta_helper <- dta_helper[, .(STATION = stations), by = DATE]

## helper table ensures every hour for every station exists,
## even if nothing was recorded originally.
## Then we merge. If original table has presipitation values, helper table will
## have them es well, but if it doesn't, helper table will have zeros instead.
## This way data is more consistent

nz[, DT := as.POSIXct(DT)]

dta_merge <- merge(
  dta_helper,
  nz[, .(STATION, DATE = DT, VALUE)],
  by = c("STATION", "DATE"),
  all.x = TRUE
)

dta_merge[, PRECIP := ifelse(is.na(VALUE), 0, VALUE)]
dta_merge[, VALUE := NULL]

head(dta_merge)

## reshape task
dta_wide <- dcast(
  data = dta_merge,
  formula = DATE ~ STATION,
  value.var = "PRECIP",
  fun.aggregate = sum
)

dta_long <- melt(
  data = dta_wide,
  id.vars = "DATE",
  variable.name = "STATION",
  value.name = "VALUE"
)


head(dta_long)

## final conclusions

## Data problems were that some of them had duplicates or logical duplicates with 
## the same station and datetime, and there were missing values in the dataset.

## In final dataset no precipitation values were negative or extremely high, 
## and no rows were flagged as suspicious

## For me the most suspicious was SRA1H, because that was the only one element that existed. I 
## checked it multiple times, thinking I deleted other elements somehow. :]

## For me all of them were useful but my favorite is unique().

