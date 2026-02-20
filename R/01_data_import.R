## https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/2025/

getwd()

## main path
pth <- "C:/Users/strnadf/Downloads/"
# pth <- "/home/user/Downloads/"

fls <- list.files(path = pth,
                  recursive = TRUE,
                  pattern = ".csv",
                  full.names = TRUE)

fls

dta <- read.table(file = fls[1], 
                  header = TRUE, 
                  sep = ",")

head(x = dta)

dim(x = dta)
str(object = dta)

object.size(x = dta)

dta$STATION <- as.factor(x = dta$STATION)

str(object = dta)

object.size(x = dta)

dta$ELEMENT <- as.factor(x = dta$ELEMENT)

object.size(x = dta)

as.Date(x = dta$DT[1])
as.POSIXlt(x = dta$DT[1],
           format = "%Y-%m-%dT%H:%M")

gsub(pattern = "T|Z",
     replacement = " ",
     x = dta$DT[1])

dta$DT <- as.POSIXct(x = gsub(pattern = "T|Z",
                              replacement = " ",
                              x = dta$DT),
                     format = "%Y-%m-%d %H:%M ")

str(object = dta)

object.size(x = dta)

saveRDS(object = dta,
        file = paste(pth, "data_prec_1h.rds"))

x <- readRDS(file = paste(pth, "data_prec_1h.rds"))

str(object = x)

summary(object = dta)

plot(x = dta$DT,
     y = dta$VALUE, 
     type = "h")

## iqr, mean, sd, boxplot

?distribution

mean(x = dta$VALUE)
mean(x = dta$VALUE[dta$VALUE > 0])

sd(x = dta$VALUE)


dta <- read.table(file = fls[1], 
                  header = TRUE, 
                  sep = ",")

