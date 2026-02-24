# https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/2025/
# Search for data in the downloads folder
pth <- "~/Downloads/"

# import the data (different than working directory)
fls <- list.files(path = pth, 
                  pattern = ".csv",
                  recursive = TRUE, # repeats until it finds all files
                  full.names = TRUE) # gets the full path for the specific file
fls

dta <- read.table(file = fls[1], 
                  header = TRUE, # treats the first line as header
                  sep = ",") # separates the data with a comma
head(dta)

# gets the size of the data set
dim(x = dta)
str(object = dta)

# gets the byte size of the object
object.size(x = dta)

# converts the STATION column to integers
dta$STATION <- as.factor(x = dta$STATION)

str(object = dta)
object.size(x = dta) # here the size gets smaller bcs integers take less space than characters

dta$ELEMENT <- as.factor(x = dta$ELEMENT)

# as.Date(x = dta$DT[1])
# as.POSIXct(x = dta$DT[1]) # this only gets the date with CEST - we need to add the format
# as.POSIXct(x = dta$DT[1], 
#            format = "%Y-%m-%dT%H:%M") # still only returns year, month and day

# gsub(pattern = "T|Z",
#      replacement = " ",
#      x = dta$DT[1])

dta$DT <- as.POSIXct(x = gsub(pattern = "T|Z", # finds the T and Z characters
                              replacement = " ", # replaces the characters with space
                              x = dta$DT),
                     format = "%Y-%m-%d %H:%M ")

head(dta)

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

# acf, iqr (inter quantile range), mean, sd (standard deviation), boxplot, density
# This has to be calculated only on non-zero data - since precipitation is or is not happening the numbers will be heavily skewed to 0
mean(x = dta$VALUE)
mean(x = dta$VALUE[dta$VALUE > 0])

sd(x = dta$VALUE)

# Getting the data directly from web instead of downloading them manually 

fls_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/2025/"
dta2 <- read.table(file = fls_url[1], 
                   header = TRUE,
                   sep = ",")
dta2$STATION <- as.factor(x = dta2$STATION)
dta2$ELEMENT <- as.factor(x = dta2$ELEMENT)
dta2$DT <- as.POSIXct(x = gsub(pattern = "T|Z", 
                               replacement = " ",
                               x = dta2$DT),
                      format = "%Y-%m-%d %H:%M ")

base_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/1hour/precipitation/"
yr <- 2025

scan(file = paste0(base_url, yr, "/"))
readLines(con = paste0(base_url, yr, "/"))

res <- readLines(con = paste0(base_url, yr, "/"))

head(x = res)

res <- res[grep(pattern = "\\.csv", # the \\ means the dot is a part of the pattern
                x = res)]

test <- strsplit(x = res[1],
                 split = '"')
test[[1]][2] # This is a list with 1 column, we are searching for the second row

res_l <- strsplit(x = res,
                  split = '"')

# Works faster than looping through lists - very useful 
fls <- sapply(X = res_l, 
              FUN = "[[",
              index = 2)

base_url
yr
fls[42]

read.table(file = paste0(base_url, yr, "/", fls[42]), 
           sep = ",",
           header = TRUE)
