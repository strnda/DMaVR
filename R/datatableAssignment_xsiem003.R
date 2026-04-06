## data table assignment
## Martyna Sierpniowska - Bartelik
## xsiem003

## installing package and loading it
install.packages("data.table")
library(data.table)

## loading the file
dta <- readRDS(file = "prec_data.rds")
dta_t <- as.data.table(x = dta)           ## converts to data table

## basic inspection stuff
class(x = dta)                            ## data.frame
head(x = dta)                             ## first 6 rows
object.size(x = dta)                      ## 1132077608 bytes, thats a big number

class(x = dta_t)                          ## data.table, data.frame also shows up in the output
head(x = dta_t)                           ## difference between this and dta is that the type of data shows up in <brackets>
object.size(x = dta_t)                    ## 1132078424 bytes, bigger than dta

## I actually don't think I had to do head(dta) but I did it anyway, thought it would make me smart

names(x = dta)                           ## just shows names of columns, the same as dta_t
names(x = dta_t)                         ## yes

nrow(x = dta)                            ## amount of rows I think
nrow(x = dta_t)                          ## didn't change

ncol(x = dta)                            ## amount of columns (7)
ncol(x = dta_t)                          ## also remains unchanged (still 7)

## STATION and ELEMENT are identifiers
## VALUE is precipitation
## DT is date and time
## FLAG and QUALITY look weird to me, don't know what's up with that
## I don't know what X is supposed to be

## filter time! VALUE > 0

nz <- dta_t[VALUE > 0,
            .(STATION, 
              ELEMENT, 
              DT, 
              VALUE, 
              FLAG, 
              QUALITY, 
              X)]                        ## not sure if all of these are necessary but better have everything than miss something

## summary by STATION
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = STATION]                         ## was it supposed to give me 400 different values?


## anyway

## summary by STATION and year
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = .(STATION, year(x = DT))]        ## even more values, about 3000

## analyzing duplicates

## exact duplicates
dta_t[duplicated(dta_t)]                 ## checks for exact dupes in the data set

## counting duplicates
dta_t[, .N, 
      by = names(dta_t)][N>1]            ## N in the output tells me how many times the line appears, in this case its 2

## logical duplicates (station, element and time repeats)
dta_t[, .N,
      by = .(STATION, 
             DT, 
             ELEMENT)][N>1]              ## it's strange that there's two entries for the same date and same hour, 
                                         ## so i guess i'll be removing that

## cleaning time

## removing the x column because it doesnt appear to do anything
dta_t[, X := NULL]

## removing all dupes
dta_t <- unique(x = dta_t)

## negative precipitation ?
dta_t[, negv := VALUE < 0]               ## negv ... negative value

## missing precipitation ?
dta_t[, missv := is.na(x = VALUE)]       ## missv ... missing value

## flag ?
dta_t[, flagp := !is.na(x = FLAG)]       ## flagp ... flag present

## extreme prec values with 99% 
## (i had to google this)
p99 <- dta_t[VALUE > 0,
             quantile(x = VALUE,
                      probs = 0.99,
                      na.rm = TRUE)]

## very high value ?
dta_t[, hval := VALUE > p99]             ## hval ... high value

## checking how many of my indicators return true
dta_t[, sum(negv, na.rm = TRUE)]         ## returns 0 true entries
dta_t[, sum(missv, na.rm = TRUE)]        ## returns 36998 true entries, will be removed
dta_t[, sum(flagp, na.rm = TRUE)]        ## 192819 true entries
dta_t[, sum(hval, na.rm = TRUE)]         ## 26806 true entries

## removing missv rows bc empty measurements are unneccesary, why are you keeping empty jars and hogging space
dta_t <- dta_t[!is.na(VALUE)]

## renaming time

## renaming value since its name isnt intuitive, value of what
setnames(x = dta_t,
         old = c("VALUE"),
         new = c("PRCPTN"),
         skip_absent = TRUE)

## renaming DT for the same reason
setnames(x = dta_t,
         old = c("DT"),
         new = c("DATE_TIME"),
         skip_absent = TRUE)

## station summary time (I could use a beer by this point ngl)
st_sum <- dta_t[, .(n_rows = .N,
                   meanp = mean(x = PRCPTN, na.rm = TRUE),
                   sdp = sd(x = PRCPTN, na.rm = TRUE),
                   minp = min(x = PRCPTN, na.rm = TRUE),
                   maxp = max(x = PRCPTN, na.rm = TRUE),
                   nnz = sum(PRCPTN > 0, na.rm = TRUE),
                   nflag = sum(flagp, na.rm = TRUE),
                   nsus = sum(hval, na.rm = TRUE)),
               by = STATION]

## checking
st_sum[order(-n_rows)][1:5]
st_sum[order(nnz)][1:5]
st_sum[order(nsus)][1:5]
st_sum[order(minp)][1:5]

## element summary time,, 
el_sum <- dta_t[, .(n_rows = .N,
                    nstations = uniqueN(STATION),
                    mean = mean(x = PRCPTN, na.rm = TRUE),
                    sd = sd(x = PRCPTN, na.rm = TRUE),
                    flagged = sum(!is.na(FLAG)),
                    badq = sum(!is.na(QUALITY))),
                by = ELEMENT]

## merging

## helper table
dt <- data.table(
  DATE_TIME = seq(from = as.POSIXct(x = "2018-01-01 00:00:00"),
                  to = as.POSIXct(x = "2026-01-01 00:00:00"),
                  by = "hour"),
  PRCPTN = 0
)

## some other adjustments for merge
setnames(x = nz,
         old = c("VALUE"),
         new = c("PRCPTN"),
         skip_absent = TRUE)

setnames(x = nz,
         old = c("DT"),
         new = c("DATE_TIME"),
         skip_absent = TRUE)

## merge
merged <- merge(x = dt,
                y = nz,
                by = "DATE_TIME",
                all.x = TRUE)

## reshaping time

## bc i renamed DT (had to google that one too)
dta_t[, year := as.integer(format(x = DATE_TIME, "%Y"))]

## making it wide
wide <- dcast(data = dta_t,
              STATION ~ year,
              value.var = "PRCPTN",
              fun.aggregate = mean)

## it lets me see all years filtered by station but it also looks weird and broken so i don't know actually. 
## i think renaming the things i chose was a bad idea. 
## but everything i do is a bad idea anyway. 
## the code works on my machine.

## final thoughts and conclusions

## x column was weird and also the fact the whole table had duplicates
## made me think that maybe the equipment was faulty so maybe whoever owns it should get that checked
## packages good, base r only bad
## i didn't know what all the column names meant, like what does flag/quality mean in this context
## value was also weird but i could guess it was rainfall based on the file name
## note to the person who took measurements and noted them: name your columns in a way that a lay man like me, a noob, 
## can understand what your thing is telling me
## idk, I didn't really notice anything else, if it isnt here then its probably been already commented in the code or I'm blind/stupid

## this also wasnt just a "simple copypaste" i actually got impostor syndrome from doing this, and ragequit a couple times




