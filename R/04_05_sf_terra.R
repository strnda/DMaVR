# install.packages(c("ggplot2", "data.table", "sf"))
library(data.table); library(ggplot2); library(sf)

prec <- readRDS(file = "C:/Users/strnadf/Downloads/prec_data.rds")
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
         dsn = "C:/Users/strnadf/Desktop/r_shape.shp")

test <- st_read(dsn = "C:/Users/strnadf/Desktop/r_shape.shp")

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
## draw 15km buffer around the stations 
## calculate annual maxima, draw maps for all available years and stations
## within the buffer (idealy in one plot)

buff_15 <- st_buffer(x = meta_shp[meta_shp$ELEVATION > 750, ], 
                     dist = 15000)

# terra::crs(buff_15)

hw_stat <- st_intersects(x = buff_15,
                         y = meta_shp)

hw_stat <- meta_shp[unique(x = unlist(x = hw_stat,
                                      recursive = TRUE)), ]

# plot(x = hw_stat)

hw_prec <- prec[STATION %in% hw_stat$WSI, ]
ann_mx <- hw_prec[, .(mx = max(VALUE, 
                               na.rm = TRUE)),
                  by = .(STATION, year(x = DT))]

hw_stat <- merge(x = as.data.table(x = hw_stat),
                 y = ann_mx,
                 by.x = "WSI",
                 by.y = "STATION", 
                 all.y = TRUE, 
                 allow.cartesian = TRUE)

hw_stat <- st_sf(hw_stat)

p <- ggplot(data = hw_stat) +
  geom_sf(mapping = aes(colour = mx)) +
  scale_color_continuous(palette = "Spectral") +
  facet_wrap(facets = ~year, 
             ncol = 1)

## and now the rasters

# install.packages("terra", "geodata")
library(terra); library(geodata)

rst_data <- elevation_30s(country = "CZE",
                          path = tempdir())

plot(x = rst_data)

# rm(list = ls())
# gc()

plot(x = log(rst_data))
plot(x = rst_data + 5000)

ex <- extract(x = rst_data,
              y = buff_15,
              xy = TRUE)

# ex
# unique(x = ex$ID)
# 
# val_mat <- values(x = rst_data)

xyFromCell(object = rst_data,
           cell = 5220)

rst_data[5220]

# install.packages("tidyterra")
library(tidyterra)

ggplot() +
  geom_spatraster(data = rst_data, 
                  na.rm = TRUE)

ggplot() +
  geom_spatraster_contour(data = rst_data) +
  geom_sf(data = mx_shp, 
          colour = "black")




m <- matrix(data = rnorm(n = 25 * 25),
            nrow = 25,
            ncol = 25)

r <- rast(x = m)
r
plot(x = r)

writeRaster(x = r,
            filename = "../raster.tif")

test <- rast(x = "../raster.tif")

test

##

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = meta_shp,
                   label = ~WSI,
                   clusterOptions = markerClusterOptions())

rst_data

library(plotly)

# plot_ly(data = rst_data,
#         z = ~CZE_elv_msk, 
#         colors = colorRampPalette(c('green','yellow','darkred'))(300)) %>% 
#   add_surface() %>% 
#   layout(title = '3d', 
#          scene = list(xaxis = list(title = 'long'),
#                       yaxis = list(title = 'lat'),
#                       zaxis = list(title = 'alt')))

p <- ggplot(data = hw_stat[hw_stat$year == 2024,]) +
  geom_sf(mapping = aes(colour = mx)) +
  scale_color_continuous(palette = "Spectral") +
  facet_wrap(facets = ~year, 
             ncol = 1)

ggplotly(p = p)

