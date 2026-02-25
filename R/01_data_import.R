## only 2025
## https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/2025/

## link to download all years
## https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/


getwd()

## main path
pth <- "C:/Users/Eugenia/Downloads/"
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

## import data directly from the web

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


####
# My solution:
# started from very beginning
base_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/"


# note: I tried with all available years, but it is too much for my computer, so I decided to select only 2024 and 2025
years_list <- c("2024", "2025")

# create an empty vector to collect all csv file urls for selected years
all_files_urls <- c()

for (yr in years_list) {
  # use it as logs to see the progress of the loop, because it can take a while
  cat("Processing year:", yr, "\n")
  
  year_page <- readLines(con = paste0(base_url, yr, "/"))
  
  year_page <- year_page[grep(pattern = "\\.csv",
                              x = year_page)]
  
  year_split <- strsplit(x = year_page,
                         split = '"')
  
  files_year <- sapply(X = year_split,
                       FUN = "[[",
                       index = 2)
  
  files_full <- paste0(base_url, yr, "/", files_year)
  
  all_files_urls <- c(all_files_urls, files_full)
}

# check what we collected
length(all_files_urls)


## import and merge data 
prec_all_years <- NULL

for (url in all_files_urls) {
  # to track the progress
  cat("Importing:", url, "\n")
  
  dta <- read.table(file = url,
                    header = TRUE,
                    sep = ",")
  
  # apply same cleaning 
  dta$STATION <- as.factor(dta$STATION)
  dta$ELEMENT <- as.factor(dta$ELEMENT)
  dta$DT <- as.POSIXct(gsub(pattern = "T|Z",
                            replacement = " ",
                            x = dta$DT),
                       format = "%Y-%m-%d %H:%M ")
  
  # append safely
  if (is.null(prec_all_years)) {
    prec_all_years <- dta
  } else {
    prec_all_years <- rbind(prec_all_years, dta)
  }
}

# check what we have befpre creating rds file 
str(prec_all_years)
object.size(prec_all_years)

# rds file creation
saveRDS(object = prec_all_years,
        file = paste0(pth, "precipitation_1h_2024_2025.rds"))
