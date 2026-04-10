# Path :"C:/Users/xalvc001/Documents/DMaVR"

## libraries
# install.packages("sf")
library(data.table)
library(ggplot2)
library(sf)
# import file 

prec <- readRDS(file = "C:/Users/xalvc001/Documents/DMaVR/prec_data.rds")

View(prec)

## import all prec data for al available years...
## save it as an *.rds

base_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/metadata/"


## Reading the url and filtering the name of the files we are looking for

res <- readLines(con = paste0(base_url))

res <- res[grep(pattern = "\\.csv",
                x = res)]  
res_l <- strsplit(x = res,
                  split = '"')

fls <- sapply(X = res_l,
              FUN = "[[",
              index = 2)

# Downloading the files 

meta_1 <- fread(input = paste0(base_url, fls[1]),
                sep = ",",
                header = TRUE,
                stringsAsFactors = TRUE)


class(meta_1)


meta_2 <- fread(input = paste0(base_url, fls[2]),
                sep = ",",
                header = TRUE,
                stringsAsFactors = TRUE)

meta_3 <- fread(input = paste0(base_url, fls[3]),
                sep = ",",
                header = TRUE,
                stringsAsFactors = TRUE)

meta_4 <- fread(input = paste0(base_url, fls[4]),
                sep = ",",
                header = TRUE,
                stringsAsFactors = TRUE)

## Filtering only the stations we have in the precipitation file
meta <- meta_1[WSI %in% unique(x = prec$STATION)]

View(meta)

?sf

plot(x = meta$GEOGR1,
     y = meta$GEOGR2)


meta_shp <- st_as_sf(x = meta[!is.na(x = meta$GEOGR1) & !is.na(x = meta$GEOGR2),],
                     coords = c("GEOGR1","GEOGR2"),
                     crs = 4326)

plot(x = meta_shp["ELEVATION"])


st_write(obj = meta_shp,
         dsn = "C:/Users/xalvc001/Documents/DMaVR/r_shape.shp")

test <- st_read(dsn = "C:/Users/xalvc001/Documents/DMaVR/r_shape.shp")


ggplot(meta_shp)+
  geom_sf(aes(color = ELEVATION))


ggplot()+
  geom_sf(data = meta_shp)+
  geom





### Calculate from original data, max precipitation per station, visualize max values on a map 

prec_table <- as.data.table(prec)
Max_prec <- prec_table[,max(VALUE), by = STATION]

View(Max_prec)

setnames(Max_prec,
         old = "V1",
         new = "Max_P")


meta_max <- merge(x = as.data.table(x = meta_shp),
                  y = Max_prec,
                  by.x = "WSI",
                  by.y = "STATION",
                  all.x = T)

max_shp <- st_sf(meta_max)

class(max_shp)
ggplot(max_shp)+
  geom_sf(mapping = aes(color = Max_P))


### repeat on other metadata
## select all stations above 750m above sea level, calculate annual maxima
## draw maps for all years ideally in one plot



