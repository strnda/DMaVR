## Import the data

RDSdata <- readRDS(file = "C:/Users/CRISTIAN/Escritorio/DMaVR/prec_data.rds")
View(RDSdata)


## invoke library data table

library(data.table)

## transform data into data table
rds_table <- as.data.table(RDSdata)
View(rds_table)



## Inspection

class(RDSdata) ## "data.frame"

class(rds_table)  ## "data.table" "data.frame"

class(rds_table$STATION)  ## factor

levels(rds_table$STATION) ## 431

class(rds_table$ELEMENT) ## factr

class(rds_table$DT) ## "POSIXct" "POSIXt"

class(rds_table$VALUE) ## "numeric"

class(rds_table$FLAG) ## "character"

object.size(RDSdata) ## 1132077608 bytes

object.size(rds_table) ## 1029165488 bytes 

colnames(RDSdata) ## "STATION" "ELEMENT" "DT"  "VALUE"   "FLAG" "QUALITY" "X"

nrow(RDSdata) ## 25728181

ncol(RDSdata)  ## 7

colnames(rds_table) ## "STATION" "ELEMENT" "DT"  "VALUE"   "FLAG" "QUALITY" "X"

nrow(rds_table) ## Also 25728181

ncol(rds_table) ## 7




rds_table[is.na(FLAG) == TRUE]

rds_table[is.na(QUALITY) == FALSE]

rds_table[is.na(X) == FALSE]



# Which variables look like identifiers = ELEMENT 
# 
# Which variables are categorical == STATION, FLAG, QUALITY
# 
# Which variables need cleaning ==  X, it seems to carry no info

mean(rds_table[VALUE])

IQR(rds_table$VALUE)

tail(rds_table)

tail(rds_table[VALUE > 2])

head(rds_table[VALUE > 10])




## CLEANING USING ":="   (removing columns with no info)

rds_table <- rds_table[, c("X") := NULL,]

View(rds_table)

# object.size(rds_table) ## 1029165488  

## remove zero values, 



rds_nz <- rds_table[VALUE > 0, .(STATION,DT,VALUE)]

object.size(rds_nz) ## 53895576 
View(rds_nz)

## calculating iqr, mean, standard dev, min and max values by station



## STATION SUMMARY NO ZEROS

Station_sum <- rds_nz[,.(Mean = mean(x = VALUE),
                Stdev = sd(x = VALUE, na.rm = T),
                InterQ = IQR(x = VALUE > 0, na.rm = T),
                Minimum = min(VALUE, na.rm = T),
                Maximum = max(VALUE, na.rm = T)), 
                by = STATION
             ]


View(Station_sum)



## STATION SUMMARY WITH ZEROS 

st_sum <- rds_table[, .(Mean = mean(x = VALUE),
                        StDev = sd(x = VALUE),
                        InterQ = IQR(x = VALUE, na.rm = T),
                        Minimum = min(VALUE),
                        Maximum = max(VALUE),
                        RCount = .N,
                        ZRows = sum(VALUE ==0)),
                    by = STATION]


View(st_sum)


### STATION 0-203-0-11696 HAS THE MOST ZERO ROWS WITH A TOTAL OF 57518

### STATION 0-203-0-11640 HAS THE HIGHEST NON-ZERO PRECIPITATION



object.size(Station_sum) ### 56136  
View(Station_sum)




## SUMMARY BY YEAR AND STATION NO ZEROS


St_year_sum <- rds_nz[,.(Mean = mean(x = VALUE),
                         Stdev = sd(x = VALUE),
                         InterQ = IQR(x = VALUE),
                         Minimum = min(VALUE),
                         Maximum = max(VALUE)), 
                      by = .(STATION, year(DT))
]
object.size(St_year_sum) # 179032 
View(St_year_sum)




## SUMMARY BY ELEMENT NO ZEROS

unique(rds_table$ELEMENT)

Ele_nz <- rds_table[VALUE > 0, .(ELEMENT,DT,VALUE)]
View(Ele_nz)

Element_sum <- Ele_nz[,.(Mean = mean(x = VALUE),
                         Stdev = sd(x = VALUE),
                         InterQ = IQR(x = VALUE),
                         Minimum = min(VALUE),
                         Maximum = max(VALUE)),
                      by = ELEMENT]

View(Element_sum)




## SUMMARY BY ELEMENT AND YEAR, NO ZEROS

El_year_sum <- Ele_nz[,.(Mean = mean(x = VALUE),
                         Stdev = sd(x = VALUE),
                         InterQ = IQR(x = VALUE),
                         Minimum = min(VALUE),
                         Maximum = max(VALUE)),
                      by = .(ELEMENT, year(DT))]


View(El_year_sum)


## THERE IS ONLY ONE ELEMENT IN THE WHOLE DATASET




### CHECKING FOR DUPLICATES

Dupl_table <- rds_nz[duplicated(rds_nz)]

View(Dupl_table)

rm(Dupl_table)

## in my no zeroes data table (rds_nz) I found 4 duplicates 
## a logical duplicate is the result from the function duplicated(),
## where it will return a logical vector FALSE if the row is not duplicated
## or if it's the first time it shows up
## also it will return a logical vector TRUE, if the row is a duplicate
## of another row.

## They are essentially a repetition of an already recorded data
## therefore it has to be removed from the data table.

Unique_table <- rds_nz[!duplicated(rds_nz)]

View(Unique_table)


### Checking for, duplicates, missing values, negative values on rds_table

duplicates <- rds_table[duplicated(rds_table)] ## 2846 duplicates

rds_table <- rds_table[!duplicated(rds_table)] ## duplicates removed


View(duplicates)
rm(duplicates)

View(rds_table)

rds_negative <- rds_table[, any(VALUE < 0, na.rm = T)]

head(rds_negative)  ## No negative values 
rm(rds_negative)

extreme <- rds_table[VALUE > 50] ## 42 extreme precipitation cases, risk of flood
View(extreme)


highest <- rds_table[DT %between% c("2024-08-01 00:00:00","2024-08-01 23:59:59")]


highest <- highest[VALUE > 50] 

View(highest)  ## 1 result of extreme precipitation 105/hr, risk of flooding
## One suspicious value, very high precipitation, not even close to the 
## observations caught in the following or prior hours, it is an 
## out-of-the-ordinary observation but nothing too weird.




## DISCUSSING QUALITY

# Quality is a extremely vague variable, it only contains 3 different values
# "0,3,4" and it does not provide further information, this could be related
# to several things, like data quality, water quality, or even 
# gauge quality as in the position of the measuring device, we do not possess
# enough insight on the data to determine what the value means 
# but also most values are 0, so it does not seem to be deterministic.




## DISCUSSING FLAG

# Flag contains just one value, which is "Z", for what I read, it seems very 
# unlikely to be a time zone as is present in very few observations
# again as in the Quality situation, we possess even less details about what 
# Flag could mean, as there is only one value, and it's missing in most of 
# the observations. Again it does not seem to be deterministic.

flag <- rds_table[, unique(FLAG)]

View(flag)

print(rds_table[,unique(QUALITY)])

print(flag)

print(rds_table[FLAG == "Z",])
head(rds_table[FLAG == "Z",])

print(rds_table[QUALITY == 3])



### creating data table to merge with zeros again

dt <- data.table(VALUE = 0,
                 DT = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                          to = as.POSIXct(x = "2026-01-01 00:00:00"),
                          by = "hour"))

object.size(dt) # 1123912 
View(dt)




## MERGING TASK

View(Unique_table)

Mtable <- merge(Unique_table, dt, by = c("DT","VALUE"), all = TRUE)
 
## Renaming a Column
setnames(x = Mtable,
         old = c("DT"),
         new =  c("DATE"))



## Creating helper columns to classify the precipitation rate and ID

Mtable <- Mtable[, RATE := fcase(VALUE == 0, "No Precipitation",
                                 VALUE <= 12.5,"Moderate",
                                 VALUE <= 50, "Very Heavy",
                                 VALUE <= 100, "Extreme",
                                 VALUE > 100, "Catastrophic")]



Mtable <- Mtable[,ID := 1:.N,
                 by = STATION]


setcolorder(Mtable, c("ID", "STATION", "DATE", "VALUE", "RATE"))

object.size(Mtable)

View(Mtable)


## RESHAPE TASK

table_wide <- dcast(data = Mtable,
                    formula = ID + DATE + RATE  ~ STATION,
                    value.var = "VALUE")

View(table_wide)

## WHAT THE RESHAPE HELPED ME SEE
## unfortunately not much, my pc crashed when I tried to see the table
## I did get a glimpse of it and it seems extremely inconvenient to have 
## this dataset reshaped to wide data, as the amount of Unique station numbers
## is very big "431", generating a lot of NA's and empty values in some rows 
## as each station id only shows up very few times per hour and date but most
## dates will not have data for most stations at a time.






##############  FINAL CONCLUSIONS  #######################################

## MAIN PROBLEMS OF THE DATA SET
### The original data set has several columns with little to no information
### making it impossible to decipher what they even meant 


## WHAT I CLEANED
### Columns QUALITY, X, AND FLAG were removed
### Also all duplicates were removed from the data set


## WHAT DECISIONS WERE UNCERTAIN
### While trying to check for anomalies and other details about the data set
### deciding to create more variables was a point to debate
### as I was not sure if it was better to create a new object or replace
### the existing one, ultimately creating several extra objects
### just to avoid making "big" mistakes.

## WHICH STATION OR ELEMENT LOOKED MORE SUSPICIOUS
### There was only one Element in the whole data set so not much to 
### debate there 
### As per the Stations, several stations seemed to be filled with zero values 
### therefore having their statistics heavily weighted 



## WHAT DATA TABLE OPERATIONS WERE THE MOST USEFULL
### The syntax of the data table "dt[j,i,by]" proved to be very easy to use
### as well as the ".()" operator to input several arguments into just one field
### making it really easy to modify the data set

 
# saveRDS(whole_table, "Prec.rds")











