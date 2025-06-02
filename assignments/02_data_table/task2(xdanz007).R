#Load package
library(data.table)
#Load csv file
dta <- fread("camels_us.csv")

#PART 1

#Task 1 - add hydrological year column
dta[, HYR := ifelse(MNTH %in% c(10,11,12), YR+1, YR)]  # october, nov, dec → next year

#Task 2 - runoff coefficient
rc_table <- dta[, .(
  total_prcp = sum(PRCP, na.rm=T),
  total_runoff = sum(OBS_RUN, na.rm=T)
), by = ID]

rc_table[, RC := total_runoff / total_prcp]  # RC formula

#Task 3 - classify
dta <- merge(dta, rc_table[, .(ID, RC)], by="ID", all.x = TRUE)

dta[, RC_class := cut(RC,
                      breaks = quantile(RC, probs = seq(0,1,0.2), na.rm=T),
                      labels = c("Very Low","Low","Moderate","High","Very High"),
                      include.lowest = T)]

selected_catchments <- dta[, .SD[sample(.N,1)], by = RC_class]
selected_ids <- unique(selected_catchments$ID)


#PART 2

#Task 4 - monthly + annual balance
dta_selected <- dta[ID %in% selected_ids]

monthly_balance <- dta_selected[, .(
  PRCP = mean(PRCP, na.rm=T),
  PET = mean(PET, na.rm=T),
  OBS_RUN = mean(OBS_RUN, na.rm=T)
), by = .(HYR, ID, MNTH)]

monthly_balance[, WB := PRCP - PET]
monthly_balance[, Deficit := WB < 0]

annual_balance <- monthly_balance[, .(
  PRCP = sum(PRCP, na.rm=T),
  PET = sum(PET, na.rm=T),
  OBS_RUN = sum(OBS_RUN, na.rm=T),
  WB = sum(WB, na.rm=T)
), by = .(HYR, ID)]


#Task 5 - snowmelt contribution
snow_data <- dta_selected[, .(
  mean_SWE = mean(SWE, na.rm=T)
), by = .(HYR, ID, MNTH)]

max_swe <- snow_data[, .(
  max_SWE = max(mean_SWE, na.rm=T)
), by = .(HYR, ID)]

snow_data <- merge(snow_data, max_swe, by = c("HYR", "ID"))
snow_data[, snowmelt := max_SWE - mean_SWE]

spring_data <- merge(
  snow_data[MNTH %in% 3:5],
  monthly_balance[MNTH %in% 3:5, .(HYR, ID, MNTH, OBS_RUN)],
  by = c("HYR", "ID", "MNTH")
)

cor_results <- spring_data[, .(
  cor_snowmelt_runoff = cor(snowmelt, OBS_RUN, use = "complete.obs")
), by = ID]

#PRINT result
print(cor_results)
