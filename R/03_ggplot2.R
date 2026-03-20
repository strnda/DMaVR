library(ggplot2); library(data.table)

dta <- readRDS(file = "../Downloads/prec_data.rds")
dta <- as.data.table(x = dta)

## clear the data - leav nly staton id, date and totals in the dataset,
## aggregate the prec to various time steps (day, week and moth)
## plot (in base R) at least 5 stations in all time steps in one plot 
## (each variable has its own panel)

dta[, let(VALUE_DAY = frollsum(x = VALUE,
                               n = 24),
          VALUE_WEK = frollsum(x = VALUE,
                               n = 24 * 7),
          VALUE_MNT = frollsum(x = VALUE,
                               n = 24 * 30)),
    by = .(STATION)]


dta_m <- melt(data = dta[, .(STATION, DT, 
                             VALUE, VALUE_DAY, VALUE_WEK, VALUE_MNT)],
              id.vars = c("STATION", "DT"))

ggplot(data = dta_m[STATION %in% unique(x = STATION)[1:5],],
       mapping = aes(x = DT,
                     y = value,
                     colour = variable)) +
  geom_line() +
  facet_grid(variable ~ STATION) + 
  theme(legend.position = "bottom")

##########

plot(x = rnorm(n = 1000),
     main = "plot")

hist(x = rnorm(n = 10000))


p <- plot(x = 0)
p <- ggplot(data = data.frame(0))

dta <- data.frame(x = rnorm(n = 1000),
                  y = rbeta(n = 1000,
                            shape1 = .2,
                            shape2 = .8, 
                            ncp = .25))

ggplot(data = dta) ## data

ggplot(data = dta,
       mapping = aes(x = x, ## aestetics (mapping)
                     y = y))

ggplot(data = dta) +
  geom_point(mapping = aes(x = x,
                           y = y,
                           size = y), ## size mapping
             shape = "*", ## geoetries
             # size = 5, ## size setting 
             colour = "grey50") ## colour setting


ggplot(data = dta[1:10,]) +
  geom_point(mapping = aes(x = x,
                           y = y,
                           colour = factor(x = y)), 
             shape = "*", 
             size = 5) +
  scale_color_manual(values = c("steelblue4", "red4", ## scales
                                "pink", "darkorange4",
                                "cadetblue1", "brown2",
                                "chocolate2", "darkred",
                                "yellow4", "deepskyblue3"))

ggplot(data = dta) +
  geom_point(mapping = aes(x = x,
                           y = y,
                           colour = y), 
             shape = "*", 
             size = 5) +
  scale_color_gradient2(low = "royalblue4", 
                        mid = "white",
                        high = "red4",
                        midpoint = .25) +
  scale_y_log10()

ggplot(data = dta,
       mapping = aes(x = x,
                     y = y,
                     colour = y)) +
  geom_point(shape = "*", 
             size = 5) +
  geom_smooth(method = "loess", 
              se = FALSE) + ## statistics
  scale_color_gradient2(low = "royalblue4", 
                        mid = "white",
                        high = "red4",
                        midpoint = .25)


ggplot(data = dta,
       mapping = aes(x = x,
                     y = y,
                     colour = y)) +
  geom_point(shape = "*", 
             size = 5) +
  scale_color_gradient2(low = "royalblue4", 
                        mid = "white",
                        high = "red4",
                        midpoint = .25) + 
  scale_y_log10() +
  coord_polar() ## coordinates

ggplot(data = dta,
       mapping = aes(x = x,
                     y = y,
                     colour = y)) +
  geom_point(shape = "*", 
             size = 5) +
  scale_color_gradient2(low = "royalblue4", 
                        mid = "white",
                        high = "red4",
                        midpoint = .25) + 
  scale_y_log10() +
  coord_polar() +
  theme_dark() +
  theme(legend.position = "none")

ggplot(data = dta_m[STATION %in% unique(x = STATION)[1:5],],
       mapping = aes(x = DT,
                     y = value,
                     colour = variable)) +
  geom_line() +
  facet_grid(variable ~ STATION) + 
  theme(legend.position = "bottom")

ggplot(data = dta_m[STATION %in% unique(x = STATION)[1:5],]) +
  stat_ecdf(mapping = aes(x = value,
                          colour = variable)) +
  facet_grid(variable ~ STATION) + 
  theme(legend.position = "bottom")


ggplot(data = dta_m[STATION %in% unique(x = STATION)[1:5],]) +
  geom_boxplot(mapping = aes(x = as.factor(month(x = DT)),
                             y = value,
                             group = month(x = DT),
                             fill = variable)) +
  facet_grid(variable ~ STATION) + 
  theme(legend.position = "bottom")
