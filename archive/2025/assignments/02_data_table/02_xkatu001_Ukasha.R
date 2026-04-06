# load the data as dta
# dta <- readRDS(file = "D:/CZU/sem 2/DMaVR/data/prec_data.rds")
dta <- readRDS(file = "C:/Users/strnadf/Downloads/prec_data.rds")

## in base r calculate mean, sd, iqr, min, max for prec,
## for each station and each year


## use the data.table now!

# install.packages("data.table")
library(data.table)

# check class of dta object(dataframe)
class(x = dta)

# convert dta to data.table
dta_t <- as.data.table(x = dta)

# check class of dta_t object(data.table)
class(dta_t)

# first few rows of dta and dta_t
head(x = dta)
dta_t

# check size of dta and dta_t in memory
object.size(x = dta_t)
object.size(x = dta)

# column names
colnames(x = dta_t)
# number of rows and columns
nrow(x = dta_t)
ncol(x = dta_t)
# identifiers DT
# categorical variables - STATION, Flag, Element,
# cleaning - X, Flag, Element, STATION
# dta_t[i, j, by]

# Indexing dta_t is different from dta. use [] like pandas
# i is for rows, j is for columns, by is for grouping
# first 20 rows of dta_t
dta_t[1:20,]
# column DT
dta_t[, DT]
# columns DT and VALUE
dta_t[, .(DT, VALUE)]

# filter rows where VALUE > 0 and select columns STATION, DT, VALUE
dta_t[VALUE > 0, .(STATION, DT, VALUE)]
## .() is the same as list()

# nz with only rows where VALUE > 0 and columns STATION, DT, VALUE
nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

# calculate mean, sd, iqr, min, max for prec, for each station
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = STATION]

# the same but each year
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = .(STATION, year(x = DT))]

## auto indent -> ctrl + i

# rm("year")
# gc()

dta_t[VALUE > 0, .(mean = mean(x = VALUE),
                   sd = sd(x = VALUE),
                   iqr = IQR(x = VALUE),
                   min = min(x = VALUE),
                   max = max(x = VALUE)),
      by = .(STATION, year(x = DT))]

nz[, new_col := TRUE]
nz

setnames(x = nz,
         old = c("new_col", "not_exists"),
         new = c("RENAME", "something"),
         skip_absent = TRUE)

nz[, RENAME := NULL]
nz[, id := 1:.N,
   by = STATION]

# dta_wide <- dcast(data = nz,
#                   formula = id + DT ~ STATION,
#                   value.var = "VALUE")
#
# # dta_wide <- dcast(data = dta_t[, .(STATION, DT, VALUE)],
# #                   formula = DT ~ STATION,
# #                   value.var = "VALUE")
#
# dta_long <- melt(data = dta_wide,
#                  id.vars = c("id", "DT"))
#
# dta

## HW

## merge nz back with zeroes without use of the original dataset.

?data.table::merge

dt <- data.table(VALUE = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                          to = as.POSIXct(x = "2026-01-01 00:00:00"),
                          by = "hour"))

## read up on these functions

?fread()
?fwrite()

?frollapply()

?data.table::merge

# Duplicate Analysis
# count duplicates in dta_t
duplicates <- dta_t[duplicated(x = dta_t,), ]
# print the number of duplicates(excluding first unique occurrence)
cat("Number of duplicate rows in dta_t:", nrow(duplicates), "\n")
# counting both unique and duplicate rows
all_dups <- dta_t[duplicated(x = dta_t, fromLast = TRUE) | duplicated(x = dta_t), ]
cat("Total number of rows that are duplicates (including unique occurrences):", nrow(all_dups), "\n")
# examples of duplicates
all_dups[1:5,]
# Logical duplicates( STATION and DT are the same but variation in VALUE or other columns)
logical_dups <- dta_t[duplicated(x = dta_t[, .(STATION, DT)]) | duplicated(x = dta_t[, .(STATION, DT)], fromLast = TRUE), ]
cat("Number of logical duplicate rows in dta_t (same STATION and DT):", nrow(logical_dups), "\n")
logical_dups[1:5,]

# The should be removed before any analysis, as they can bias results.

# Cleaning
# missing values
missing_values <- dta_t[is.na(x = VALUE), ]
missing_x <- dta_t[is.na(x = X), ]
missing_flag <- dta_t[is.na(x = FLAG), ]
cat("Number of rows with missing values in VALUE column:", nrow(missing_values), "\n")
cat("Number of rows with missing values in X column:", nrow(missing_x), "\n")
cat("Number of rows with missing values in FLAG column:", nrow(missing_flag), "\n")

# percent of rows with missing data in select columns
total_rows <- nrow(x = dta_t)
percent_missing_value <- (nrow(missing_values) / total_rows) * 100
percent_missing_x <- (nrow(missing_x) / total_rows) * 100
percent_missing_flag <- (nrow(missing_flag) / total_rows) * 100
cat("Percentage of rows with missing values in VALUE column:", round(percent_missing_value, 2), "%\n")
cat("Percentage of rows with missing values in X column:", round(percent_missing_x, 2), "%\n")
cat("Percentage of rows with missing values in FLAG column:", round(percent_missing_flag, 2), "%\n")

# X column has 100% missing values, so it can be removed without losing any information.

cleaned_dta_t = dta_t[, X := NULL]
# removing duplicates
cleaned_dta_t <- cleaned_dta_t[!duplicated(x = cleaned_dta_t), ]
#checking suspicious values in VALUE column
# filtering for value > 0
cleaned_dta_t <- cleaned_dta_t[VALUE > 0, ]
summary(cleaned_dta_t$VALUE)
# more than 75% of data is below 1, some values are significantly higher
# these are suspisious
# how many values are above 1?
high_values <- cleaned_dta_t[VALUE > 1, ]
cat("Number of rows with VALUE > 1:", nrow(high_values), "\n")
# many values in Flag are missing, what is the purpose of this column?
# flaged values
flaged_values <- cleaned_dta_t[!is.na(x = FLAG), ]
cat("Number of rows with non-missing FLAG values:", nrow(flaged_values), "\n")
# examples of flaged values
flaged_values
# appears flag is an empty string simillar to NA. investigate unique values in Flag and the class of Flag
unique_flags <- unique(x = cleaned_dta_t$FLAG)
cat("Unique values in FLAG column:", unique_flags, "\n")
cat("Class of FLAG column:", class(cleaned_dta_t$FLAG), "\n")
# if FLAG is character, then empty string can be treated as NA

cleaned_dta_t[FLAG == "", FLAG := NA]
# now we can check how many flaged values we have
flaged_values <- cleaned_dta_t[!is.na(x = FLAG), ]
cat("Number of rows with non-missing FLAG values after cleaning:", nrow(flaged_values), "\n")
# view flagged values after cleaning
flaged_values

# Quality
# summary of quality column
summary(cleaned_dta_t$QUALITY)
# missing values in quality
missing_quality <- cleaned_dta_t[is.na(x = QUALITY), ]
# percent of missing values in quality
percent_missing_quality <- (nrow(missing_quality) / total_rows) * 100
cat("Percentage of rows with missing values in QUALITY column:", round(percent_missing_quality, 2), "%\n")

# check unique values in quality
unique_quality <- unique(x = cleaned_dta_t$QUALITY)
cat("Unique values in QUALITY column:", unique_quality, "\n")

# Quality and Flag are categorical.. convert them to factors
cleaned_dta_t[, FLAG := as.factor(FLAG)]
cleaned_dta_t[, QUALITY := as.factor(QUALITY)]
# check class of FLAG and QUALITY after conversion
cat("Class of FLAG column after conversion:", class(cleaned_dta_t$FLAG), "\n")
cat("Class of QUALITY column after conversion:", class(cleaned_dta_t$QUALITY), "\n")

# new column for suspisious values(greater than 99th percentile)
suspicious_threshold <- quantile(cleaned_dta_t$VALUE, probs = 0.99, na.rm = TRUE)
cleaned_dta_t[, SUSPICIOUS := VALUE > suspicious_threshold]
# check how many suspicious values we have
suspicious_values <- cleaned_dta_t[SUSPICIOUS == TRUE, ]
cat("Number of rows with suspicious values (VALUE > 99th percentile):", nrow(suspicious_values), "\n")

# rename dt to Datetime
setnames(x = cleaned_dta_t,
         old = "DT",
         new = "Datetime")
# check column names after renaming
colnames(x = cleaned_dta_t)

# Station Summary
st_sum <- cleaned_dta_t[, .(mean = mean(x = VALUE),
                         sd = sd(x = VALUE),
                         iqr = IQR(x = VALUE),
                         min = min(x = VALUE),
                         suspicious_count = sum(SUSPICIOUS),
                         nrows = .N,
                         nrows_no_suspicious = sum(!SUSPICIOUS),
                         max = max(x = VALUE)),
                     by = STATION]
# check station summary
# most problematic stations are those with the highest number of suspicious values and the highest mean VALUE
st_sum[order(-mean,-suspicious_count)]
# highest max
st_sum[order(-max)]
# most records
st_sum[order(-nrows)]
# most suspicious values
st_sum[order(-suspicious_count)]


# Element Summary
# Station Summary
el_sum <- cleaned_dta_t[, .(mean = mean(x = VALUE),
                            sd = sd(x = VALUE),
                            iqr = IQR(x = VALUE),
                            min = min(x = VALUE),
                            suspicious_count = sum(SUSPICIOUS),
                            nrows = .N,
                            missing_percent = (sum(is.na(x = VALUE)) / .N) * 100,
                            nrows_no_suspicious = sum(!SUSPICIOUS),
                            qquality = sum(QUALITY == 3, na.rm = TRUE),
                            max = max(x = VALUE)),
                        by = ELEMENT]
# check element summary
# reliable elements(lowest suspicious count and lowest mean VALUE)
el_sum[order(mean, suspicious_count)]
# unreliable elements(highest suspicious count and highest mean VALUE)
el_sum[order(-mean, -suspicious_count)]
# investigate element column(element scales)
cleaned_dta_t[, unique(ELEMENT)]
# one element

# Merge
# Helper table with 0  (20 row for each station)
ht <- data.table(STATION = rep(unique(cleaned_dta_t$STATION), each = 20),
                 VALUE = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                                to = as.POSIXct(x = "2026-01-01 00:00:00"),
                                by = "hour")[1:20])
# columns in nz
colnames(x = nz)
colnames(x = ht)
# merge helper table with nz
merged_data <- merge(x = nz, y = ht, by = c("STATION", "DT"), all = TRUE)
# check merged data
merged_data

# merge using station and date columns,
# but only where an entry exists in either nz or ht

# reshape
# values to wide form using dcast
dta_wide <- dcast(data = nz,
                  formula = DT ~ STATION,
                  value.var = "VALUE")
# creates a table with dt as rows and stations as columns,
# values are the VALUE column from nz
# we can investigate total value observations over a given time from all
# stations



# Final Discussion
# main Data Problems
# 1. Duplicates - can bias results, should be removed
# 2. Missing Values - can lead to loss of information, should be handled appropriately (imputation or removal)
# 3. Lack of documentation about columns (e.g. FLAG) - makes it difficult to interpret data, should be clarified with data provider

# What I cleaned
# 1. Removed duplicates
# 2. Removed rows with VALUE <= 0
# 3. Treated empty strings in FLAG as NA
# 4. Converted FLAG and QUALITY to factors

# Suspisious station
# 0-20000-0-11787 high mean VALUE and highest number of suspicious values

# Usefull data.table operations
# 1. Grouping and summarizing data using by
# 2. Filtering rows based on conditions
# 3. Creating new columns using := operator
# 4. Indexing using [] and i,j,by syntax

