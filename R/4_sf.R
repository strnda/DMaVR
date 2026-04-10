# install.packages(c("ggplot2", "data.table", "sf"))
library(data.table); library(ggplot2); library(sf)

prec <- readRDS(file = "../Downloads/prec_data.rds")
prec <- as.data.table(x = prec)

prec <- prec[, .(STATION, DT, VALUE)]

object.size(x = prec)

gc()

base_url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/metadata/"

res <- readLines(con = paste0(base_url))
res <- res[grep(pattern = "\\.csv",
                x = res)]
res_l <- strsplit(x = res, 
                  split = '"')
fls <- sapply(X = res_l, 
              FUN = "[[",
              index = 2)

meta_1 <-  fread(input = paste0(base_url, fls[1]),
                 sep = ",",
                 header = TRUE, 
                 stringsAsFactors = TRUE)

any(unique(x = prec$STATION) == meta_1$WSI)
any(unique(x = prec$STATION) %in% meta_1$WSI)

meta <- meta_1[WSI %in% unique(x = prec$STATION), ]

?sf

plot(x = meta$GEOGR1,
     y = meta$GEOGR2)

meta_shp <- st_as_sf(x = meta[!is.na(x = GEOGR1) & !is.na(x = GEOGR2), ], 
                     coords = c("GEOGR1", "GEOGR2"), 
                     crs = 4326)

plot(x = meta_shp["ELEVATION"])

st_write(obj = meta_shp,
         dsn = "../Desktop/r_shape.shp")

test <- st_read(dsn = "../Desktop/r_shape.shp")

ggplot(data = meta_shp) +
  geom_sf(mapping = aes(colour = ELEVATION)) +
  theme_bw()


sub_stat <- meta_shp[meta_shp$ELEVATION >= 1000, ]

class(x = sub_stat)

ggplot() +
  geom_sf(data = meta_shp,
          mapping = aes(colour = ELEVATION)) +
  geom_sf(data = sub_stat,
          colour = "red") +
  theme_bw()

buff <- st_buffer(x = sub_stat,
                  dist = 30000)

# dst <- st_distance(x = meta_shp[5, ],
#                    y = meta_shp)
# as.vector(x = dst)

ggplot() +
  geom_sf(data = meta_shp,
          mapping = aes(colour = ELEVATION)) +
  geom_sf(data = sub_stat,
          colour = "red") +
  geom_sf(data = buff,
          colour = "yellow4", 
          fill = NA) +
  theme_bw()

# st_intersects(x = )

## HW determin the encoding
iconv(x = meta_shp$FULL_NAME, 
      from = "cp1252", 
      to = "ASCII")

## now -> claculate max prec per station and draw a map of the maxima 

mx <- prec[, .(mx = max(VALUE, 
                        na.rm = TRUE)),
           by = STATION]

## pay atention to the arguments!
meta_mx <- merge(x = as.data.table(x = meta_shp),
                 y = mx,
                 by.x = "WSI",
                 by.y =  "STATION", 
                 all.x = TRUE)

mx_shp <- st_sf(meta_mx)

ggplot(data = mx_shp) +
  geom_sf(mapping = aes(colour = mx))

## another HW -> select all staions above 750 m above the seas level,
## calculate annual maxima, draw maps for all years (idealy in one plot)

## ...
