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
## homework -> do some reading about ACF & distribution function|quantile function|density

?distribution

mean(x = dta$VALUE)
mean(x = dta$VALUE[dta$VALUE > 0])

sd(x = dta$VALUE)

## 

fls_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/2025/1h-0-20000-0-11414-SRA1H-202503.csv"

dta <- read.table(file = fls_url[1], 
                  header = TRUE, 
                  sep = ",")

dta$STATION <- as.factor(x = dta$STATION)
dta$ELEMENT <- as.factor(x = dta$ELEMENT)
dta$DT <- as.POSIXct(x = gsub(pattern = "T|Z",
                              replacement = " ",
                              x = dta$DT),
                     format = "%Y-%m-%d %H:%M ")

base_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/"

yr <- 2025

scan(file = paste0(base_url, yr, "/"))
readLines(con = paste0(base_url, yr, "/"))

res <- readLines(con = paste0(base_url, yr, "/"))

head(x = res)

res <- res[grep(pattern = "\\.csv",
                x = res)]

test <- strsplit(x = res[1], 
                 split = '"')
test[[1]][2]

res_l <- strsplit(x = res, 
                  split = '"')

## really useful... 
fls <- sapply(X = res_l, 
              FUN = "[[",
              index = 2)

base_url
yr
fls[42]

read.table(file = paste0(base_url, yr, "/", fls[42]),
           sep = ",",
           header = TRUE)

## import all prec data for al available years...
## save it as an *.rds
