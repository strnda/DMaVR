###ASSIGNMENT 01 
#xthak_002


## PART 1: DATA LOAD
#read the data
dta <- readRDS(file = "prec_data.rds")

#install data.table 
library(data.table)

dta_t <- as.data.table(x = dta)

## PART 2 : BASIC INSPECTION

#identify class
class(x=dta)
class(x=dta_t)

#first six rows
head(x=dta_t)

#size 
object.size(x=dta)

object.size(x=dta_t)

#column names
colnames(dta_t)

#number rows and columns
nrow(dta_t)
ncol(dta_t)


## PART 3 :FILTERING AND SUMMARY STATISTICS

nz <- dta_t[VALUE > 0, .(STATION, DT, VALUE)]

##by STATION
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = STATION]

## BY STATION WITH DT
nz[, .(mean = mean(x = VALUE),
       sd = sd(x = VALUE),
       iqr = IQR(x = VALUE),
       min = min(x = VALUE),
       max = max(x = VALUE)),
   by = .(STATION, year(x = DT))]


## PART 4: DUPLICATE ANALYSIS

#counting  exact duplicates
#same rows recorded multiple times
#.n counts the rows

dup<-dta_t[,.N, by =.(STATION,DT,VALUE,ELEMENT)][N>1]

#exampples
head(x=dup)

#logical duplicates
#same station has two recordss at same time 

dup_log<-dta_t[,.N, by=.(STATION,DT)][N>1]

nrow(x=dup_log)

#examples
head(x=dup_log)

# same station recorded multiple times
# more than one value in same time 
# most likely be removed


## PART 5 : CLEANING

#remove  duplicates

dta_t<- unique(x=dta_t)

## flagged values that are missing 
dta_t[, missing_val := is.na(x=VALUE)]

##flag negative precipitation 
dta_t[,susp:= VALUE<0]

## missing quality 
dta_t[, missing_qual := is.na(x=QUALITY)]

##removing the helper   suspicious column 
dta_t[,susp :=NULL]

##checking falg and qualtiy column
dta_t[,.N, by = FLAG]

dta_t[, .N, by = QUALITY]


## PART 6 : RENAMING AND COLUMN WORK 
###renaming 
setnames(x=dta_t,
         old=c("QUALITY"),
         new=c("QUAL"),
         skip_absent = TRUE)

## year column DT
dta_t[,yr:= year(x= DT)]

##row id within each station
dta_t[, id:=1:.N,
      by= STATION]


###PART 7 : STATION SUMMARY

st_sum<- dta_t[, .(
  n_rows=.N,
  meanV= mean(x=VALUE , na.rm=TRUE),
  sdV=sd(x=VALUE , na.rm=TRUE),
  minV=min(x=VALUE , na.rm = TRUE),
  maxV=max(x=VALUE , na.rm = TRUE),
  non0=sum(VALUE>0 ,na.rm=TRUE),
  nmiss=sum(missing_val,na.rm = TRUE),
  nflag= sum(!is.na(x=FLAG) & FLAG !="" , na.rm=TRUE)
), by = STATION]

st_sum

##highest non zero precipitation
st_sum[order(-maxV)]

##most records
st_sum[order(-n_rows)]

## most suspicious
st_sum[order(-nflag)]


##PART 8 : ELEMENT SUMMARY
                   
el_sum <- dta_t[, .(
  n_rows=.N,
  n_stat=uniqueN(x=STATION),
  meanV=mean(x=VALUE, na.rm=TRUE),
  sdV=sd(x=VALUE, na.rm=TRUE),
  propM=mean(x=is.na(x=VALUE)),
  nflag=sum(!is.na(x=FLAG) & FLAG !=""),
  nques=sum(!is.na(x=QUAL) & QUAL!="")
  ),   by = ELEMENT]                   

el_sum

#most reliable : low missing/flagged

el_sum[order(propM,nflag)]

#least reliable
el_sum[order(-propM)]


## PART 9/10:MERGE TASK

##Helper table with zero values
##represents the hour where no rain
dt<-data.table( VALUE=0 ,
                DT= seq(from = as.POSIXct(x="2018-01-01 00:00:00"),
                        to = as.POSIXct(x="2026-01-01 00:00:00"),
                        by = "hour"))


##merge non zero with zero rain hour
st_nz<-nz[STATION == nz$STATION[1]]

##main merge:all hours fron dt

mrg<- merge( x=dt,
             y=st_nz[, .(DT,VALUE)],
             by="DT",
             all.x=TRUE)

##value.y=real rain if value.y-NA no rain recorded at that hour

head(x=mrg)
tail(x=mrg)


###PART 11: RESHAPE TASK

##taking station year summaries and reshaping to wide form 
## so each stat is col and yr is row

st_yr<- nz[, .(mean_val=mean(x=VALUE)),
           by = .(STATION,yr=year(x=DT))]

##reshape year as row and stat as col

dta_wide<-dcast( data= st_yr,
                 formula= yr ~ STATION,
                 value.var="mean_val")

head(x=dta_wide)

##back to loong form 

dta_long<- melt(data= dta_wide,
                id.vars="yr",
                variable.name="STATION",
                value.name="mean_val")

head(x=dta_long)

##long form is better fro grouped analysis
##wide makes it easy to compare side by side

###PART 12 : FINAL CONCLUSION 

# main data problems:
# some VALUEs missing 
# suspicious where precipitation is negative 
# duplicates existed : logical and exact 
# duplicates had to be removed


##cleaned:
## duplicates removed using unique()
# flagged missing values using :=


## uncertain about logical duplicates,flagged values

##most useful data.table operations
# := 
# dcaast() , melt()
# summary using BY
# .N for counting rows per group 
