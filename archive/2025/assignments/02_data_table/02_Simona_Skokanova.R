library(data.table)
library(fst)

#setwd("C:/Users/simca/Documents/czu/R/assignments/2")

#load data from assignment 1
data <- fst(path = "data_snipet.fst")
data <- as.data.table(x = data)

#task 1
#assign hydrological years
data[, HYR := ifelse(MNTH %in% c(10,11,12), YR + 1, YR)]

#task 2
#compute total precipitation and observed runoff by ID
result <- aggregate(cbind(PRCP, OBS_RUN) ~ ID, data, FUN = sum)
#compute runoff coefficient
result$RC <- result$OBS_RUN / result$PRCP
#new dataset
result <- result[, c("ID", "RC")]

#task 3

result <- as.data.table(result)
#classify catchments to coefficient classes
result[, RC_class := cut(RC, breaks = quantile(result$RC, probs = seq(0, 1, 0.2)), 
                      labels = c("Very Low", "Low", "Moderate", "High", "Very High"), 
                      include.lowest = TRUE)]
#randomly select one from each group
selected_catchments <- result[, .SD[sample(.N, 1)], by = RC_class]

#task 4
#get the rest of the columns for the selected catchments for future computation
selected <- merge(data, selected_catchments, by = "ID")

#compute monthly mean of PRCP, PET and OBS_RUN
month_mean <- selected[, .(
  mean_PRCP = mean(PRCP, na.rm = TRUE),
  mean_PET = mean(PET, na.rm = TRUE),
  mean_OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

#compute total annual values 
annual <- selected[, .(
  PRCP = sum(PRCP, na.rm = TRUE),
  PET = sum(PET, na.rm = TRUE),
  OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID)]

#compute water balance (monthly and annual)
month_mean[, WB := mean_PRCP - mean_PET]
annual[, WB := PRCP - PET]

pot_deficit <- month_mean[WB < 0]

#task 5

#mean mohthly SWE
monthly_SWE <- data[, .(mean_SWE = mean(SWE, na.rm = TRUE))
                    , by = .(ID, HYR, MNTH)]

#maximum SWE within each hydrological year
max_SWE <- data[, .(max_SWE = max(SWE, na.rm = TRUE))
                    , by = .(ID, HYR)]

#merge max SWE back to original data set
data <- merge(data, max_SWE, by = c("ID", "HYR"), all.x = TRUE)

#compute snowmelt rate
data[, snowmelt := max_SWE - SWE]

#merge monthly mean_OBS_RUN from task 4
data <- merge(data, month_mean[, .(ID, HYR, MNTH, mean_OBS_RUN)], by = c("ID", "HYR", "MNTH"), all.x = TRUE)

#compute correlation between snowmelt and observed runoff for wanted months
cor_snowmelt_runoff <- cor(data[MNTH %in% c(3, 4, 5), .(snowmelt, mean_OBS_RUN)], use = "complete.obs")

#interpretetion of the results:
#correlation coefficient 0.31: positive but weak correlation between snowmelt and runooff
#the weak correlation suggests that other factors influence runoff besides snowmelt, for example:
#Infiltration and groundwater recharge
#Vegetation effects
#Catchment storage capacity