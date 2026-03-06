dta <- readRDS(file = "C:/Users/strnadf/Downloads/prec_data.rds")

## in base r calculate mean, sd, iqr, min, max for prec,
## for each station and each year


## use the data.table now!

# install.packages("data.table")
library(data.table)

class(x = dta)

dta_t <- as.data.table(x = dta)

class(dta_t)

head(x = dta)
dta_t

object.size(x = dta_t)
object.size(x = dta)

# dta_t[i, j, by]

dta_t[1:20,]
dta_t[, DT]
dta_t[, .(DT, VALUE)]

dta_t[VALUE > 0, .(STATION, DT, VALUE)]
## .() is the same as list()

nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

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

