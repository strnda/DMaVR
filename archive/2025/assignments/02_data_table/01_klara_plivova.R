library(data.table)
library(ggplot2)

## Import the data
dta <- readRDS(file = "prec_data.rds")

## Converting to data table
dta_t <- as.data.table(x = dta)

## Showing classes and first rows
class(x = dta)
class(x = dta_t)
head(x = dta)
head(x = dta_t)

## Showing object sizes
object.size(x = dta)
object.size(x = dta_t)

## Names of columns, number of rows, number of columns
colnames(x = dta_t)
nrow(x = dta_t)
ncol(x = dta_t)

summary(object = dta_t)
## Identifiers: Station together with year creates that. There shouldn't be two duplicate values.
## Categorical: Station alone is categorical. The data can be grouped by measurements from one station. Element is also
## categorical.
## Needs cleaning: The element is constant. X only includes NA values. Value column needs 0 and NAs to be removed.

## Only include no zero data with relevant columns
nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

## Calculating statistics by station
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       IQR = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = STATION]

## Calculating statistics by station and year
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       IQR = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = .(STATION, year(x = DT))]

## Counting duplicate rows and showing examples
sum(duplicated(x = dta_t))
dta_t[duplicated(x = dta_t)][1:5]

## Logical duplicate is when station and dt are the same in two rows. These values should be removed - 
## one station cannot take 2 different measurements at the same time
sum(duplicated(x = dta_t, 
               by = c("STATION", "DT")))


## Flagging missing values
dta_t[, miss := is.na(x = VALUE)]

## Flagging duplicate rows
dta_t[, dupe := duplicated(x = dta_t, 
                           by = c("STATION", "DT"))]

## Checking if there are negative values (these would make no sense for precipitation)
sum(dta_t[, VALUE < 0], na.rm = TRUE)

## The flag column only includes NAs, empty values and the character Z. This tells me little about what the column 
## should mean, the data in these rows seem reliable.
dta_t[, unique(x = FLAG)]
dta_t[FLAG == "Z", 1:9]
dta_t[FLAG == "", 1:9]

## The quality column includes values 0, 3 and 4. The value 4 seemed to be linked to NAs in the value column.
## This will be cleaned while excluding missing values. The quality value 3 has noticeably higher mean value than 
## stations with quality 0. This could indicate some issues with the 3 quality measurements, but since I have no concrete
## idea of what the quality indicates I will keep the rows.
dta_t[, unique(x = QUALITY)]
sum(dta_t$QUALITY == 0)
sum(dta_t$QUALITY == 4)
sum(dta_t$QUALITY == 3)
dta_t[QUALITY == 4, 1:9]
dta_t[QUALITY == 3, 1:9]
ggplot(data = dta_t[QUALITY == 3], aes(x = VALUE)) + 
  geom_histogram(bins = 30)

ggplot(data = dta_t[QUALITY == 0], aes(x = VALUE)) + 
  geom_histogram(bins = 30)

q_zero <- dta_t[QUALITY == 0]
mean(q_zero$VALUE)
q_three <- dta_t[QUALITY == 3]
mean(q_three$VALUE)

## Deleting the X column since it only includes NAs
dta_t[, X := NULL]

## Deleting missing values and duplicates 
dta_clean <- dta_t[miss == FALSE & 
                     dupe == FALSE]

## Getting rid of helper columns
dta_clean[, miss := NULL]
dta_clean[, dupe := NULL]

## Setting names
setnames(x = dta_clean,
         old = c("DT"),
         new = c("DATE"),
         skip_absent = TRUE)

## Indicator for missing quality
dta_clean[, missing_q := !is.na(x = QUALITY)]


## Column for suspicious values above 99th percentile
dta_clean[, suspicious := VALUE > as.numeric(quantile(x = VALUE, 
                                                      probs = 0.99,
                                                      na.rm = TRUE))]

## Station summary
st_sum <- dta_clean[, .(rows = .N, 
                        mean = mean(x = VALUE > 0),
                        sd = sd(x = VALUE),
                        min = min(x = VALUE), 
                        max = max(x = VALUE),
                        non_zero = sum(x = VALUE > 0),
                        suspicious_values = sum(suspicious == TRUE)),
                    by = STATION]

## There's no apparent deviation in the number of measurements taken by stations and also no issues with distribution of 
## mean values
ggplot(data = st_sum, aes(x = rows)) + 
  geom_histogram(bins = 30)

ggplot(data = st_sum, aes(x = mean)) +
  geom_histogram(bins = 50)

## All mins are correctly set to 0. If this value would be higher there could be an issue with some of the stations.
sum(x = st_sum$min)

## Some stations have higher proportion of non-zero values. This could indicate some issue with the measurements or 
## those stations are simply somewhere where it rains more (like mountains).
st_sum[order(-non_zero / rows)][1:5, c("STATION", "rows", "non_zero")]

# Highest non-zero precipitation
st_sum[order(-max)][1:5, c("STATION", "max")]

# Most records
st_sum[order(-rows)][1:5, c("STATION", "rows")]

# Most suspicious values
st_sum[order(-suspicious_values)][1:5, c("STATION", "suspicious_values")]


## Element summary
el_sum <- dta_clean[, .(rows = .N,
                        stations = uniqueN(x = STATION),
                        mean = mean(x = VALUE),
                        sd = sd(x = VALUE), 
                        missing_values = sum(is.na(x = VALUE) / .N),
                        flagged = sum(x = !is.na(FLAG)),
                        quality = sum(QUALITY != 0)),
                    by = ELEMENT]

## There's only one element. The most reliable one would have enough rows (number of observations), solid number of
## stations, low percentage of missing values, and a low number of 3 valued quality measurements.

## Creating helper table with zero precipitation values for all missing times
dt <- data.table(VALUE = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                          to = as.POSIXct(x = "2026-01-01 00:00:00"),
                          by = "hour"))

## Merging the two data sets by the columns DATE and VALUE. The full outer join keeps data from both tables, creating 
## new table with measurements for every single hour. This is useful because it fills holes in the data, allowing for 
## more accurate analysis. The only disadvantage is that we lost the station information for 0 precipitation.
mrg <- merge(x = nz, 
             y = dt,
             by = c("DT", "VALUE"),
             all = TRUE)       

## The reshape makes the data more readable for humans. This way I can see the precipitation by station in every hour.
dta_wide <- dcast(data = dta_clean,
                  formula = DATE ~ STATION,
                  value.var = "VALUE")

## The main problems with the data were the missing precipitation values and duplicate rows. The X column was 
## empty and had to be removed. Quality 4 only included NA values. These had to be cleaned for the analysis.

## Uncertain was the decision around quality 3. There is no concrete issue with the data, but it could signal lower 
## quality measurements, which should ideally be cleaned. Flag was mostly empty and the flagged data didn't seem suspicious,
## however, if I knew the meaning, this could signal some issues with the station measurements. Also, 99th percentile 
## for suspicious values was just guessing.

## Most problematic stations had low number of records, suspiciously high precipitation, and highest count of suspicious 
## values. 

## The most useful operation were filtering in j, and the := operator for flagging certain rows. 