#This script prepares Cetacean Tagging Data from the BMMRO-NOAA tagging effort (2009-2014) for a variety of analyses and plots
source("~/Grad School/Research/3_2012_Whale Tagging/Cetacean_Analysis_Functions_0.4.R")

############    TAGS    ############
#1. Import .csv file 
TAGS=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/Tags with genders.csv",stringsAsFactors = F)

#1. Exclude non-target species from all dataframes
EXCLUDED_SPP=c("Gray whale","Killer whale_b2","Humpback whale","Killer whale_B1","Killer whale_A",
               "Killer whale_T","Fin whale","Killer whale_C","Antarctic minke whale","Antarctic minke whal")
TAGS=TAGS[!TAGS$Species %in% EXCLUDED_SPP, ] #run this first because there are some duplicate PTT numbers on gray whales that will mess up merges

#3. Assign each Species a SPP code
TAGS=SPP(TAGS)

#4. Assign each SPP code a FAM code
TAGS[TAGS$SPP%in%c("Pe","Gm","Sb"),"FAM"]="De"
TAGS[TAGS$SPP%in%c("Md","Me","Zc"),"FAM"]="Zi"
TAGS[TAGS$SPP%in%c("Pm"),"FAM"]="Ph"

#2. Standardize date-time stamp: create DATE_TIME field as a chron object and extract MONTHS, YEARS, J_DATE, and DAY_NIGHT
TAGS=DATE_TIME(TAGS,VAR="Deploy.date",FORMAT= "%m/%d/%y %H:%M")

#4. Create a unique EVENT code to denote each tagging event or group tagged
TAGS$EVENT=paste(TAGS$J_DATE,TAGS$YEAR,TAGS$SPP,1,sep="_")

#manually modify EVENT code for individuals of the same SPP tagged on the same JDATE and YEAR, but at non-contiguous times  
TAGS[TAGS$PTT==93239,"EVENT"]="140_2009_Pm_2"
TAGS[TAGS$PTT==111670,"EVENT"]="127_2012_Md_2"
TAGS[TAGS$PTT==108425,"EVENT"]="166_2011_Pm_2"

#create a count of EVENT codes (i.e. how many individuals tagged in each event)
for(i in unique(TAGS$EVENT))
{TAGS[TAGS$EVENT==i,"EVENT_COUNT"]=nrow(TAGS[TAGS$EVENT==i,])}

#5. Label Tags as SPOT or SPLASH type
TAGS[substr(TAGS$Tag.type,1,4)=="MK10","TYPE"]="SPLASH"
TAGS[substr(TAGS$Tag.type,1,7)=="AM-S240","TYPE"]="SPOT"

############    MICERTA   ############
MICERTA=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/Micerta Table S1.csv",stringsAsFactors = F)

############    BATHY    ############
#Import a bathymetry DEM for the NW Bahamas provided by D. Claridge (2013)
library(rgdal);library(raster)
BATHY=raster(readGDAL("~/GIS/Baselayers/Bahamas DEM/asciito_aute2"))
projection(BATHY)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

BATHY_SLOPE=terrain(BATHY, opt='slope')
BATHY_ASPECT=terrain(BATHY, opt='aspect')
BATHY_HILL=hillShade(terrain(BATHY, opt='slope'), terrain(BATHY, opt='aspect'), 70, 90)
OCEAN_COL=colorRampPalette(c("cadetblue1","deepskyblue2","deepskyblue3","deepskyblue4"),bias=1.65)(100)

############    ARGOS    ############
#Import output queries from ARGOSPRD.mdb database in which Date format has been standardized 
#to MM/DD/YY HH:MM using Excel formating
ARGOS=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/q_unfiltered locations numeric classes 2.csv",
               colClasses=c(rep("character",4),rep("numeric",4),"character",rep("numeric",4)))


#1. Link PTT with species and assign each Species a SPP code
ARGOS=SPP(ARGOS)
ARGOS=ARGOS[ARGOS$SPP%in%c("Md","Gm","Pm","Zc","Pe","Sb"),]


ARGOS_temp = read.csv("/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Md_AUTEC_Argos.csv",stringsAsFactors = F)
colnames(ARGOS_temp) = c("PTT", "Date", "LastOfLatitude", "LastOfLongitude","Error","LastOfSemi.major.axis","LastOfSemi.minor.axis","LastOfEllipse.orientation")
ARGOS_temp[,c("Deploy.date","End.date","Class","Number")] = NA
ARGOS_temp$Species = "Blainville's beaked whale"
ARGOS_temp$SPP = "Md"
ARGOS_temp = ARGOS_temp[,colnames(ARGOS)]
ARGOS_temp = ARGOS_temp[!ARGOS_temp$PTT%in%unique(ARGOS$PTT),]
ARGOS = rbind(ARGOS,ARGOS_temp)

#2. Standardize labeling of Latitude and Longitude accross data.frames
ARGOS$LAT=ARGOS$LastOfLatitude
ARGOS$LONG=ARGOS$LastOfLongitude
ARGOS$MAJOR=ARGOS$LastOfSemi.major.axis
ARGOS$MINOR=ARGOS$LastOfSemi.minor.axis
ARGOS$ORIENTATION=ARGOS$LastOfEllipse.orientation

#Remove records from gray whales that received overlapping tag numbers (112031,112032,112033)
ARGOS=ARGOS[ARGOS$LONG>-100,]

#2. Standardize date-time stamp: create DATE_TIME field as a chron object and extract MONTHS, YEARS, J_DATE, and DAY_NIGHT
ARGOS=DATE_TIME(ARGOS,"Date","%m/%d/%y %H:%M")

#3. Add a column for a REGION classificication based on arbitrary LAT and LONG bounding boxes
ARGOS=REGION(ARGOS)

#4. Add a sequential indentifier SEQ_ID accross all records
ARGOS=ARGOS[order(ARGOS$PTT,ARGOS$DATE_TIME),]
ARGOS$SEQ_ID=seq(1,nrow(ARGOS))

#6. Calculate transmission START_TIME, END_TIME, and DURATION for each PTT in ARGOS
for(i in unique(ARGOS$PTT))
{ARGOS[ARGOS$PTT==i,"START_DATE"]=min(ARGOS[ARGOS$PTT==i,"DATE_TIME"])
ARGOS[ARGOS$PTT==i,"END_DATE"]=max(ARGOS[ARGOS$PTT==i,"DATE_TIME"])}
ARGOS$DURATION=ARGOS$END_DATE-ARGOS$START_DATE

#8. Extract Bathymetric (BATHY) depth at ARGOS coordinates (for filtering out impossible points over land)
library(raster)
ARGOS$BATHY_ARGOS=extract(BATHY,ARGOS[,c("LONG","LAT")])

#9. Calculate distance and time interval between successive ARGOS fixes for speed filtering
library(Imap)
ARGOS_DIST_temp=data.frame()
for(i in unique(ARGOS$PTT))
{#For each PTT isolate the variables necessary to calculate velocity and SEQ_ID to merge back into dataset
  ARGOS_temp=ARGOS[ARGOS$PTT==i,c("SEQ_ID","LAT","LONG","DATE_TIME")]
  ARGOS_temp[,c("DIST_AHEAD","DIST_BACK","INT_AHEAD","INT_BACK")]=0
  #Calculate geodesic distance between points 1 and 2 through n-1 and n
  ARGOS_temp[1:(nrow(ARGOS_temp)-1),"DIST_AHEAD"]=gdist(ARGOS_temp[1:(nrow(ARGOS_temp)-1),"LONG"],
                                                        ARGOS_temp[1:(nrow(ARGOS_temp)-1),"LAT"],
                                                        ARGOS_temp[2:nrow(ARGOS_temp),"LONG"],
                                                        ARGOS_temp[2:nrow(ARGOS_temp),"LAT"],units="km")
  ARGOS_temp[2:nrow(ARGOS_temp),"DIST_BACK"]=ARGOS_temp[1:(nrow(ARGOS_temp)-1),"DIST_AHEAD"]
  #Calculate time interval between fixes 1 and 2 through n-1 and n
  ARGOS_temp[1:(nrow(ARGOS_temp)-1),"INT_AHEAD"]=abs(as.numeric(ARGOS_temp[1:(nrow(ARGOS_temp)-1),"DATE_TIME"])-
                                                       as.numeric(ARGOS_temp[2:nrow(ARGOS_temp),"DATE_TIME"]))*24
  ARGOS_temp[2:nrow(ARGOS_temp),"INT_BACK"]=ARGOS_temp[1:(nrow(ARGOS_temp)-1),"INT_AHEAD"]
  ARGOS_DIST_temp=rbind(ARGOS_DIST_temp,ARGOS_temp)}
#Calculate velocity (km/h) between fixes 1 and 2 through n-1 and n
ARGOS_DIST_temp$VEL_AHEAD=ARGOS_DIST_temp$DIST_AHEAD/ARGOS_DIST_temp$INT_AHEAD
ARGOS_DIST_temp$VEL_BACK=ARGOS_DIST_temp$DIST_BACK/ARGOS_DIST_temp$INT_BACK
ARGOS_DIST_temp$VEL_AHEAD=ifelse(is.na(ARGOS_DIST_temp$VEL_AHEAD)|
                                   is.infinite(ARGOS_DIST_temp$VEL_AHEAD),0,ARGOS_DIST_temp$VEL_AHEAD)
ARGOS_DIST_temp$VEL_BACK=ifelse(is.na(ARGOS_DIST_temp$VEL_BACK)|
                                  is.infinite(ARGOS_DIST_temp$VEL_BACK),0,ARGOS_DIST_temp$VEL_BACK)

ARGOS_DIST_temp=ARGOS_DIST_temp[!duplicated(ARGOS_DIST_temp$SEQ_ID) & !is.na(ARGOS_DIST_temp$SEQ_ID),]
#Merge records back to ARGOS based on SEQ_ID
ARGOS=merge(ARGOS,subset(ARGOS_DIST_temp,select=c(SEQ_ID,DIST_AHEAD,DIST_BACK,INT_AHEAD,INT_BACK,VEL_AHEAD,VEL_BACK)),all.x=T,by="SEQ_ID")

#Clean-up temp objects
remove(ARGOS_temp,ARGOS_DIST_temp)

############    ARGOS CTCRW    ############
#1. Create a data.frame to house CTCRW model hourly predictions using all the available ARGOS data
ARGOS_CTCRW=data.frame()

for(i in unique(ARGOS$PTT))
{#Run CTCRW model and generate default hourly predictions on ARGOS records for each PTT after
  # filtering out records where the following conditions are met:
  # MAJOR axis == 0
  # LAT,LONG,MAJOR,MINOR, or ORIENTATION are NA 
  # BATHY is NA or is >0 (i.e. land) 
  # VEL_AHEAD and VEL_BACK are greater than 100km/h over a measurement duration of more than 1 minute
  CTCRW_temp=try(CTCRW(DATA=ARGOS[ARGOS$PTT==i & ARGOS$MAJOR>0 & !is.na(ARGOS$LAT) & !is.na(ARGOS$LONG) 
                                  & !is.na(ARGOS$MAJOR) & !is.na(ARGOS$MINOR) & !is.na(ARGOS$ORIENTATION)
                                  & !is.na(ARGOS$BATHY_ARGOS) & ARGOS$BATHY_ARGOS>0 
                                  & !ARGOS$SEQ_ID%in%which(ARGOS$VEL_AHEAD>100 & ARGOS$VEL_BACK>100 
                                                           & ARGOS$INT_AHEAD>1/60 & ARGOS$INT_BACK>1/60),]))
  if(class(CTCRW_temp)=="try-error"){next}
  CTCRW_temp=subset(CTCRW_temp,select=c(PTT,SEQ_ID,SPP,DATE_TIME,YEAR,MONTH,DAY,HOUR,MIN,SEC,J_DATE,
                                        MAJOR,MINOR,ORIENTATION,LONG_0,LAT_0,locType,speed,naive.p.val,
                                        PRED_TIME,LONG_CRW,LAT_CRW))
  ARGOS_CTCRW=as.data.frame(rbind(ARGOS_CTCRW,CTCRW_temp))
  remove(CTCRW_temp)}


#3. Save the original LAT and LONG in new variables (e.g. LAT_ARGOS), and then reassign LAT and LONG as the CTCRW predictions 
ARGOS_CTCRW$LAT_ARGOS=ARGOS_CTCRW$LAT_0
ARGOS_CTCRW$LONG_ARGOS=ARGOS_CTCRW$LONG_0
ARGOS_CTCRW$LONG=ARGOS_CTCRW$LONG_CRW
ARGOS_CTCRW$LAT=ARGOS_CTCRW$LAT_CRW

#4. Restandardize DATE_TIME
ARGOS_CTCRW$DATE_TIME=as.chron(ARGOS_CTCRW$PRED_TIME)
ARGOS_CTCRW$YEAR=as.numeric(as.character(years(ARGOS_CTCRW$DATE_TIME)))
ARGOS_CTCRW$MONTH=as.numeric(months(ARGOS_CTCRW$DATE_TIME))
ARGOS_CTCRW$DAY=as.numeric(days(ARGOS_CTCRW$DATE_TIME))
ARGOS_CTCRW$HOUR=as.numeric(hours(ARGOS_CTCRW$DATE_TIME)) #extract HOUR as a number
ARGOS_CTCRW$MIN=as.numeric(minutes(ARGOS_CTCRW$DATE_TIME)) #extract MIN as a number
ARGOS_CTCRW$SEC=as.numeric(seconds(ARGOS_CTCRW$DATE_TIME)) #extract SEC as a number
ARGOS_CTCRW$DATE=paste("X",ARGOS_CTCRW$YEAR,"_",
                       sprintf("%02d",ARGOS_CTCRW$MONTH),"_",
                       sprintf("%02d",ARGOS_CTCRW$DAY),sep="") 

#Extract BATHY depth at ARGOS_CTCRW Locations
ARGOS_CTCRW$BATHY=extract(BATHY,ARGOS_CTCRW[,c("LONG","LAT")])

#9. Add LIGHT_DARK information to ARGOS_CTCRW
ARGOS_CTCRW=LIGHT_DARK(ARGOS_CTCRW,START_END=F,TIME_VAR="DATE_TIME")
detach("package:oce", unload=TRUE);library(raster)

############    COAST    ############
#Import a simplified Coastal outline file derived from the h (high-res) level of the
#Global Self-consistent, Hierarchical, High-resolution Geography Database (GSHHG)
library(rgdal);library(rgeos);COAST=readOGR("/Users/trevorjoyce/GIS/Baselayers/Coastlines","COAST")
projection(COAST)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
COAST=gIntersection(COAST,GRID(LAT_1=23, LONG_1=-79.5, LAT_2=23, LONG_2=-75.5, 
                               LAT_3=27, LONG_3=-75.5, LAT_4=27, LONG_4=-79.5))

############    AUTEC    ############
#Import a polygon delineating the boundaries of the AUTEC range provided by J. Durban (11/2014)
library(rgdal);AUTEC=readOGR("/Users/trevorjoyce/GIS/Baselayers/AUTEC","autecshape")
projection(AUTEC)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

############    TAT    ############
#Import output queries from ARGOSPRD.mdb database in which Date format has been standardized 
#to MM/DD/YY HH:MM using Excel formating
TAT=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/q_TAT2.csv",
             colClasses=c("numeric","character",rep("numeric",13)))

#1. reverse the order of temperature bin labels (backwards in ARGOS.mdb)
names(TAT)[names(TAT) %in% c("T04","T46","T68","T810","T1012","T1214","T1416","T1618","T1820",           
                             "T2022","T2224","T2460")] <- c("T2460","T2224","T2022","T1820","T1618",
                                                            "T1416","T1214","T1012","T810","T68","T46","T04")

#1. Link PTT with species and assign each Species a SPP code
TAT=SPP(TAT)

#Add Age, Sex, and WhaleID code from TAGS table to TAT based onn PTT
TAT$SEQ_ID=seq(1,nrow(TAT))
TAT=merge(TAT,subset(TAGS,select=c(PTT,Sex,Age,Whale.ID)),by.y="PTT",by.x="Ptt",all.x=T)
TAT=TAT[order(TAT$SEQ_ID),]

#2. Standardize date-time stamp: create DATE_TIME field as a chron object and extract MONTHS, YEARS, J_DATE, and DAY_NIGHT
TAT=DATE_TIME(TAT,"RDate","%m/%d/%y %H:%M")

#3. Where spatial information is missing find ARGOS LAT and LONG with the closest DATE_TIME
TAT=LAT_LONG(TAT,LAT=NA,LONG=NA)

#4. Add a column for a REGION classificication based on arbitrary LAT and LONG bounding boxes
TAT=REGION(TAT)

#5. Calculate start and end time for TAT histograms

    #START TIME in each TAT is the RDate or DATE_TIME field
    TAT$START_TIME=times(paste(sprintf("%02d",hours(TAT$DATE_TIME)),
                               sprintf("%02d",minutes(TAT$DATE_TIME)),
                               sprintf("%02d",seconds(TAT$DATE_TIME)),sep=":"))
    #Calculate END TIME in each TAT as the START_TIME + 6 hrs or the next time boundary in sequence
    for(i in 1:(nrow(TAT)))
    {TAT$END_TIME[i]=ifelse(TAT$START_TIME[i]%in%times(c("05:00:00","11:00:00","17:00:00","23:00:00")),
                            ifelse(TAT$START_TIME[i]+times("06:00:00")>1,TAT$START_TIME[i]+times("06:00:00")-1,TAT$START_TIME[i]+times("06:00:00")),
                            times(c("05:00:00","11:00:00","17:00:00","23:00:00"))[which(times(c("05:00:00","11:00:00","17:00:00","23:00:00"))>TAT$START_TIME[i])[1]])}
    TAT$END_TIME=times(paste(sprintf("%02d",hours(TAT$END_TIME)),
                             sprintf("%02d",minutes(TAT$END_TIME)),
                             sprintf("%02d",seconds(TAT$END_TIME)),sep=":"))
    #Convert END_TIME from a time to a date-time chron format, accounting for the fact that the END_TIME could be in the next day
    TAT$END_TIME=ifelse(TAT$END_TIME>TAT$START_TIME,floor(TAT$DATE_TIME)+as.numeric(TAT$END_TIME),ceiling(TAT$DATE_TIME)+as.numeric(TAT$END_TIME))
    TAT$START_TIME=TAT$DATE_TIME
    #Calculate the duration in chron units (1.00 = 1day)
    TAT$DURATION=as.numeric(TAT$END_TIME-TAT$START_TIME)*24

#6. Extract TAT locations based on start and end times
TAT=LAT_LONG_CTCRW(DATA=TAT,PERIOD=TAT$DURATION,DIST=T,DEPTH=T)
TAT[,c("LAT_BEST24","LONG_BEST24")]=TAT[,c("LAT","LONG")]
TAT[!is.nan(TAT$LAT_AVG),c("LAT","LONG")]=TAT[!is.nan(TAT$LAT_AVG),c("LAT_AVG","LONG_AVG")]
TAT[!is.nan(TAT$LAT_AVG),c("LAT_LONG_FLAG")]=2
TAT$BATHY_DEPTH_AVG=NA
for(i in 1:nrow(TAT)){TAT$BATHY_DEPTH_AVG[i]=mean(as.numeric(unlist(strsplit(TAT$BATHY_DEPTH[i],","))),na.rm=T)}
for(i in 1:nrow(TAT)){TAT$BATHY_DEPTH_SE[i]=sd(as.numeric(unlist(strsplit(TAT$BATHY_DEPTH[i],","))),na.rm=T)/sqrt(length(as.numeric(unlist(strsplit(TAT$BATHY_DEPTH[i],",")))))}

#7. Day Night classification based on start and end and local sunrise and sunset times
TAT=LIGHT_DARK(TAT)
TAT$DAY_NIGHT=ifelse(TAT$LIGHT_RATIO>0.85,"Day","Night")
TAT$MOON_PRESENT=ifelse(TAT$MOON_LIGHT/TAT$DURATION>0.85 & TAT$MOON_MEAN_ILLUM>=0.10,"Present","Absent")
detach("package:oce", unload=TRUE);library(raster)

#8. Add geographic covariates to TAT for the prediction of isotherm depths at TAT locations
TAT=NEAR(DATA=TAT,ID="SEQ_ID",NEAR_OBJ=BATHY_C50)
names(TAT)[names(TAT) %in% c("NEAR_DIST","NEAR_LONG","NEAR_LAT")] <- c("DIST_BATHY_C50","LONG_BATHY_C50","LAT_BATHY_C50")
TAT$DIST_CHAN=ifelse(TAT$DIST_BATHY_C50/DIST_CHAN_MAX<1,TAT$DIST_BATHY_C50/DIST_CHAN_MAX,1)
TAT=NEAR(DATA=TAT,ID="SEQ_ID",NEAR_OBJ=FLST)
names(TAT)[names(TAT) %in% c("NEAR_DIST","NEAR_LONG","NEAR_LAT")] <- c("DIST_FLST","LONG_FLST","LAT_FLST")
TAT$LAT_LONG_0.1=paste(substr(sprintf("%f",TAT$LAT),1,4),substr(sprintf("%f",TAT$LONG),1,5),sep="_")
TAT$LAT_LONG_0.5=paste(substr(sprintf("%f",ifelse((TAT$LAT-floor(TAT$LAT))<0.5,floor(TAT$LAT),floor(TAT$LAT)+0.5)),1,4),
                       substr(sprintf("%f",ifelse(abs(TAT$LONG-ceiling(TAT$LONG))<0.5,ceiling(TAT$LONG),ceiling(TAT$LONG)-0.5)),1,5),sep="_")
TAT$LAT_LONG_1.0=paste(floor(TAT$LAT),floor(TAT$LONG),sep="_")
TAT=NEAR(DATA=TAT,ID="SEQ_ID",NEAR_OBJ=BATHY_C50_1)
names(TAT)[names(TAT) %in% c("NEAR_DIST","NEAR_LONG","NEAR_LAT")] <- c("DIST_BATHY_C50_1","LONG_BATHY_C50_1","LAT_BATHY_C50_1")
TAT$BATHY_SLOPE=extract(BATHY_SLOPE,cbind(TAT$LONG,TAT$LAT),method='bilinear')
TAT$BATHY_ASPECT=extract(BATHY_ASPECT,cbind(TAT$LONG,TAT$LAT),method='bilinear')

#9. Predict isotherm depths using a range of models onto the locations and times of TAT histograms
for(i in seq(4,24,2))
{TAT=ISOTHERM_PREDICT(DATA=TAT,ISO=i,GLM=T,GAM=T,GLM_OA=T)}

TAT$ISO4=TAT$GLM_ISO4; TAT$ISO6=TAT$GLM_OA_ISO6; TAT$ISO8=TAT$GLM_OA_ISO8; TAT$ISO10=TAT$GLM_OA_ISO10;
TAT$ISO12=TAT$GLM_OA_ISO12; TAT$ISO14=TAT$GLM_OA_ISO14; TAT$ISO16=TAT$GLM_OA_ISO16; TAT$ISO18=TAT$GLM_OA_ISO18
TAT$ISO20=TAT$GAM_ISO20; TAT$ISO22=TAT$GAM_ISO22; TAT$ISO24=TAT$GAM_ISO24

TAT$SE_ISO4=TAT$GLM_SE_ISO4; TAT$SE_ISO6=TAT$GLM_OA_SE_ISO6; TAT$SE_ISO8=TAT$GLM_OA_SE_ISO8; TAT$SE_ISO10=TAT$GLM_OA_SE_ISO10;
TAT$SE_ISO12=TAT$GLM_OA_SE_ISO12; TAT$SE_ISO14=TAT$GLM_OA_SE_ISO14; TAT$SE_ISO16=TAT$GLM_OA_SE_ISO16; TAT$SE_ISO18=TAT$GLM_OA_SE_ISO18
TAT$SE_ISO20=TAT$GAM_SE_ISO20; TAT$SE_ISO22=TAT$GAM_SE_ISO22; TAT$SE_ISO24=TAT$GAM_SE_ISO24

TAT=subset(TAT,select=-c(GLM_ISO4,GLM_SE_ISO4,GAM_ISO4,GAM_SE_ISO4,GLM_OA_ISO4,GLM_OA_SE_ISO4,
                         GLM_ISO6,GLM_SE_ISO6,GAM_ISO6,GAM_SE_ISO6,GLM_OA_ISO6,GLM_OA_SE_ISO6,
                         GLM_ISO8,GLM_SE_ISO8,GAM_ISO8,GAM_SE_ISO8,GLM_OA_ISO8,GLM_OA_SE_ISO8,
                         GLM_ISO10,GLM_SE_ISO10,GAM_ISO10,GAM_SE_ISO10,GLM_OA_ISO10,GLM_OA_SE_ISO10,
                         GLM_ISO12,GLM_SE_ISO12,GAM_ISO12,GAM_SE_ISO12,GLM_OA_ISO12,GLM_OA_SE_ISO12,
                         GLM_ISO14,GLM_SE_ISO14,GAM_ISO14,GAM_SE_ISO14,GLM_OA_ISO14,GLM_OA_SE_ISO14,
                         GLM_ISO16,GLM_SE_ISO16,GAM_ISO16,GAM_SE_ISO16,GLM_OA_ISO16,GLM_OA_SE_ISO16,
                         GLM_ISO18,GLM_SE_ISO18,GAM_ISO18,GAM_SE_ISO18,GLM_OA_ISO18,GLM_OA_SE_ISO18,
                         GLM_ISO20,GLM_SE_ISO20,GAM_ISO20,GAM_SE_ISO20,GLM_OA_ISO20,GLM_OA_SE_ISO20,
                         GLM_ISO22,GLM_SE_ISO22,GAM_ISO22,GAM_SE_ISO22,GLM_OA_ISO22,GLM_OA_SE_ISO22,
                         GLM_ISO24,GLM_SE_ISO24,GAM_ISO24,GAM_SE_ISO24,GLM_OA_ISO24,GLM_OA_SE_ISO24))

#Calculate the mean value predicted by the best available model at each isotherm, and calculate the min and max 
#observed depth of each isotherm within the study area for axis labels in a variety of comparison plots
for(i in seq(4,24,2))
{TAT[,paste("MED_ISO",i,sep="")]=median(eval(parse(text=paste("CONTOUR_GRID$ISO",i,sep=""))),na.rm=T)
 TAT[,paste("MIN_ISO",i,sep="")]=min(CTD_STND[CTD_STND$TEMP==i,"DEPTH"],na.rm=T)
 TAT[,paste("MAX_ISO",i,sep="")]=max(CTD_STND[CTD_STND$TEMP==i,"DEPTH"],na.rm=T)}

#13. Add TYPE idendifier for subsequent merge with SERIES_TAD to make TAT_TAD
TAT$TYPE="TAT"
TAT=TAT[order(TAT$Ptt,TAT$DATE_TIME),]

############    DIVE_THRESHOLDS    ############

#Create a data.frame of foraging DEPTH and TEMP dive thresholds to define foraging ranges
DIVE_THRESHOLDS=data.frame(SPP=c("Pe","Gm","Pm","Md","Zc"), DEPTH_DAY=c(50,100,600,650,800), 
                           DEPTH_NIGHT=c(0,0,600,650,800), TEMP=c(24,24,14,10,8)) 

############    SERIES    ############

#Import output queries from ARGOSPRD.mdb database in which Date format has been standardized 
#to MM/DD/YY HH:MM using Excel formating
SERIES=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/t_Depth_series.csv",
                colClasses=c("numeric","numeric",rep("character",3),rep("numeric",4))) 

#1. Link PTT with species and assign each Species a SPP code
SERIES=SPP(SERIES)
SERIES=SERIES[!is.na(SERIES$SPP),]

#5. Add a sequential ID column for reordering after merges
SERIES$SEQ_ID=seq(1,nrow(SERIES))

#Add Age, Sex, and WhaleID code from TAGS table to SERIES based onn PTT
SERIES=merge(SERIES,subset(TAGS,select=c(PTT,Sex,Age,Whale.ID)),by.y="PTT",by.x="Ptt",all.x=T)
SERIES=SERIES[order(SERIES$SEQ_ID),]

#2. Standardize date-time stamp: create DATE_TIME field as a chron object and extract MONTHS, YEARS, J_DATE, and DAY_NIGHT
SERIES$Day=str_replace(SERIES$Day, pattern=" 0:00", replacement="")
SERIES$Date=paste(SERIES$Day,SERIES$Time)
SERIES=DATE_TIME(SERIES,"Date","%m/%d/%y %H:%M:%S")

#3. Where spatial information is missing find ARGOS LAT and LONG with the closest DATE_TIME
SERIES=LAT_LONG(SERIES,SERIES$Latitude,SERIES$Longitude)

#4. Add a column for a REGION classificication based on arbitrary LAT and LONG bounding boxes
SERIES=REGION(SERIES)

#5. Add a column that assigns a code specific to each message and copies down the message duration information
#Add a message identifier number that rises sequentially within each J_DATE for each PTT 
SERIES=SERIES[order(SERIES$Ptt,SERIES$DATE_TIME),] #establish reading order
SERIES$MSG_ID=NA
for(i in unique(SERIES$Ptt))
{for(j in which(SERIES$Ptt==i)[1:(nrow(SERIES[SERIES$Ptt==i,])-1)])
  {if(SERIES[j+1,"DATE_TIME"]-SERIES[j,"DATE_TIME"]>15/(60*24)) #Add a 1 (placeholder) to each row where DATE_TIME is more than 15min different from previous row
    {SERIES[j+1,"MSG_ID"]=1}}
 SERIES[which(SERIES$Ptt==i)[1],"MSG_ID"]=1} #Start the MSG_ID sequence with the first row in each Ptt

#Change 1's to sequential numbers
for(i in unique(SERIES$Ptt))
{SERIES[SERIES$Ptt==i & !is.na(SERIES$MSG_ID),"MSG_ID"]=seq(1:length(SERIES[SERIES$Ptt==i & !is.na(SERIES$MSG_ID),"MSG_ID"]))}
  
#Copy down this MSG_ID number
for(i in 2:nrow(SERIES))
{if(is.na(SERIES$MSG_ID[i]))
  SERIES[i,"MSG_ID"]=SERIES[i-1,"MSG_ID"]}

#Create a MSG_ID code
SERIES$MSG_ID=paste(SERIES$Ptt,SERIES$MSG_ID,sep="_")

#Assign a START_TIME, END_TIME, and DURATION to each observation within a message
for(i in unique(SERIES$MSG_ID))
{SERIES[SERIES$MSG_ID==i,"MSG_START_TIME"]=SERIES[SERIES$MSG_ID==i,"DATE_TIME"][1]
 SERIES[SERIES$MSG_ID==i,"MSG_END_TIME"]=SERIES[SERIES$MSG_ID==i,"DATE_TIME"][length(SERIES[SERIES$MSG_ID==i,"DATE_TIME"])]
 SERIES[SERIES$MSG_ID==i,"MSG_DURATION"]=as.numeric(SERIES[SERIES$MSG_ID==i,"MSG_END_TIME"]-SERIES[SERIES$MSG_ID==i,"MSG_START_TIME"])*24}


#6. Assign a sequential DIVE_ID within each MSG_ID by identifying d_DEPTH sign transitions near the surface 
#Copy down this MSG_ID number
for(i in 2:nrow(SERIES))
{SERIES[i,"d_DEPTH"]=SERIES[i,"Depth"]-SERIES[i-1,"Depth"]}
SERIES[1,"d_DEPTH"]=0

#Label the rows where d_DEPTH changes sign from negative or 0 to positive
SERIES$DIVE_ID=NA
SERIES[which(c(0,diff(ifelse(sign(SERIES$d_DEPTH)<=0,0,1)))>0),"DIVE_ID"]=1

#Eliminate pauses and bumps in time series below 50m as new dives
for(i in which(SERIES$DIVE_ID==1))
{if(SERIES[i-1,"Depth"]>50){SERIES$DIVE_ID[i]=NA}}

#Step the DIVE_ID back by 1 row so that dives start on the shallowest depth
SERIES$DIVE_ID=c(SERIES$DIVE_ID[2:nrow(SERIES)],NA)

#Change 1's to sequential numbers
for(i in unique(SERIES$MSG_ID))
{SERIES[which(SERIES$MSG_ID==i)[1],"DIVE_ID"]=1 #Make the first observations in each MSG_ID have DIVE_ID=1
  SERIES[SERIES$MSG_ID==i & !is.na(SERIES$DIVE_ID),"DIVE_ID"]=seq(1:length(SERIES[SERIES$MSG_ID==i & !is.na(SERIES$DIVE_ID),"DIVE_ID"]))}

#Copy down this DIVE_ID number
for(i in 2:nrow(SERIES))
{if(is.na(SERIES$DIVE_ID[i]))
  SERIES[i,"DIVE_ID"]=SERIES[i-1,"DIVE_ID"]}

#Create a DIVE_ID code
SERIES$DIVE_ID=paste(SERIES$MSG_ID,SERIES$DIVE_ID,sep="_")

#
for(i in unique(SERIES$DIVE_ID))
{SERIES[SERIES$DIVE_ID==i,"START_TIME"]=SERIES[SERIES$DIVE_ID==i,"DATE_TIME"][1]
 SERIES[SERIES$DIVE_ID==i,"END_TIME"]=SERIES[SERIES$DIVE_ID==i,"DATE_TIME"][length(SERIES[SERIES$DIVE_ID==i,"DATE_TIME"])]
 SERIES[SERIES$DIVE_ID==i,"DURATION"]=as.numeric(SERIES[SERIES$DIVE_ID==i,"END_TIME"]-SERIES[SERIES$DIVE_ID==i,"START_TIME"])*24
 SERIES[SERIES$DIVE_ID==i,"DEPTH_DIVE_MAX"]=max(SERIES[SERIES$DIVE_ID==i,"Depth"])}



#6. Assign each dive exceeding a SPP specific depth threshold BOUT_ID sequential number that is unique within each message
SERIES$BOUT_ID=NA

for(i in unique(SERIES[SERIES$SPP=="Pe" & SERIES$DEPTH_DIVE_MAX>=50,"MSG_ID"]))
  for(j in 1:length(unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=50,"DIVE_ID"])))
{SERIES[SERIES$DIVE_ID==unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=50,"DIVE_ID"])[j],"BOUT_ID"] = j}

for(i in unique(SERIES[SERIES$SPP=="Gm" & SERIES$DEPTH_DIVE_MAX>=50,"MSG_ID"]))
  for(j in 1:length(unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=50,"DIVE_ID"])))
  {SERIES[SERIES$DIVE_ID==unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=50,"DIVE_ID"])[j],"BOUT_ID"] = j}

for(i in unique(SERIES[SERIES$SPP=="Pm" & SERIES$DEPTH_DIVE_MAX>=600,"MSG_ID"]))
  for(j in 1:length(unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=600,"DIVE_ID"])))
  {SERIES[SERIES$DIVE_ID==unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=600,"DIVE_ID"])[j],"BOUT_ID"] = j}

for(i in unique(SERIES[SERIES$SPP=="Md" & SERIES$DEPTH_DIVE_MAX>=650,"MSG_ID"]))
  for(j in 1:length(unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=650,"DIVE_ID"])))
  {SERIES[SERIES$DIVE_ID==unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=650,"DIVE_ID"])[j],"BOUT_ID"] = j}

for(i in unique(SERIES[SERIES$SPP=="Zc" & SERIES$DEPTH_DIVE_MAX>=800,"MSG_ID"]))
  for(j in 1:length(unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=800,"DIVE_ID"])))
  {SERIES[SERIES$DIVE_ID==unique(SERIES[SERIES$MSG_ID==i & SERIES$DEPTH_DIVE_MAX>=800,"DIVE_ID"])[j],"BOUT_ID"] = j}

#Copy down this BOUT_ID to subsequent rows within each MSG_ID
for(i in unique(SERIES$MSG_ID))
{if(is.na(SERIES[SERIES$MSG_ID==i,"BOUT_ID"][1])){SERIES[SERIES$MSG_ID==i,"BOUT_ID"][1]=0} #if the MSG doesn't start on a deep dive add a 0 for BOUT_ID (will be subsequently removed)
 for(j in which(SERIES$MSG_ID==i))
 {if(is.na(SERIES[j,"BOUT_ID"])){SERIES[j,"BOUT_ID"]=SERIES[j-1,"BOUT_ID"]}
 }
}

#Identify partial bouts at the end of each MSG_ID
for(i in unique(SERIES$MSG_ID))
{SERIES[SERIES$MSG_ID==i & SERIES$BOUT_ID==max(SERIES[SERIES$MSG_ID==i,"BOUT_ID"]),"BOUT_ID"]=0}

#Identify partial bouts at the start of each MSG_ID
for(i in unique(SERIES$MSG_ID))
{if(SERIES[SERIES$MSG_ID==i,"BOUT_ID"][1]==1 & sign(SERIES[SERIES$MSG_ID==i,"d_DEPTH"][1])!=1)
{SERIES[SERIES$MSG_ID==i & SERIES$BOUT_ID==1,"BOUT_ID"]=0}}

#Create a BOUT_ID code
SERIES[SERIES$BOUT_ID==0,"BOUT_ID"]=NA
SERIES[!is.na(SERIES$BOUT_ID),"BOUT_ID"]=paste(SERIES[!is.na(SERIES$BOUT_ID),"MSG_ID"],
                                             SERIES[!is.na(SERIES$BOUT_ID),"BOUT_ID"],sep="_")

#Calculate the DURATION of each BOUT_ID and the amount of FORAGING time spent below species specific DIVE_THRESHOLD 
SERIES[is.na(SERIES$BOUT_ID),"BOUT_ID"]=999
for(i in unique(SERIES[SERIES$BOUT_ID!=999,"BOUT_ID"]))
{SERIES[SERIES$BOUT_ID==i,"BOUT_DURATION"]=as.numeric(SERIES[SERIES$BOUT_ID==i,"DATE_TIME"][length(SERIES[SERIES$BOUT_ID==i,"DATE_TIME"])]-
                                                        SERIES[SERIES$BOUT_ID==i,"DATE_TIME"][1])*24
 SERIES[SERIES$BOUT_ID==i,"BOUT_FORAGING"]=
   as.numeric(max(SERIES[SERIES$BOUT_ID==i & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==SERIES[SERIES$BOUT_ID==i,"SPP"][1],"DEPTH_DAY"],"DATE_TIME"])-
              min(SERIES[SERIES$BOUT_ID==i & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==SERIES[SERIES$BOUT_ID==i,"SPP"][1],"DEPTH_DAY"],"DATE_TIME"]))*24}
SERIES[SERIES$BOUT_ID==999,"BOUT_ID"]=NA


#7. Extract CTCRW MLE estimates for all the DATE_TIME stamps in SERIES that fall between the first and last ARGOS position fix for a given PTT
SERIES_CRW_temp=data.frame()
for(i in unique(SERIES$Ptt))
{#Run CTCRW model and generate default hourly predictions on ARGOS records for each PTT after
  # filtering out records where the following conditions are met:
  # MAJOR axis == 0
  # LAT,LONG,MAJOR,MINOR, or ORIENTATION are NA 
  # BATHY is NA or is >0 (i.e. land) 
  # VEL_AHEAD and VEL_BACK are greater than 100km/h over a measurement duration of more than 1 minute
  CTCRW_temp=try(CTCRW(DATA=ARGOS[ARGOS$PTT==i & ARGOS$MAJOR>0 & !is.na(ARGOS$LAT) & !is.na(ARGOS$LONG) 
                                  & !is.na(ARGOS$MAJOR) & !is.na(ARGOS$MINOR) & !is.na(ARGOS$ORIENTATION)
                                  & !is.na(ARGOS$BATHY_ARGOS) & ARGOS$BATHY_ARGOS>0 
                                  & !ARGOS$SEQ_ID%in%which(ARGOS$VEL_AHEAD>100 & ARGOS$VEL_BACK>100 
                                                           & ARGOS$INT_AHEAD>1/60 & ARGOS$INT_BACK>1/60),],
                       PRED_TIMES = SERIES[SERIES$Ptt==i,"DATE_TIME"]))
  if(class(CTCRW_temp)=="try-error"){next}
  CTCRW_temp=subset(CTCRW_temp,select=c(PTT,SEQ_ID,SPP,DATE_TIME,YEAR,MONTH,DAY,HOUR,MIN,SEC,J_DATE,
                                        MAJOR,MINOR,ORIENTATION,LONG_0,LAT_0,locType,speed,naive.p.val,
                                        PRED_TIME,LONG_CRW,LAT_CRW))
  SERIES_CRW_temp=as.data.frame(rbind(SERIES_CRW_temp,CTCRW_temp))
  remove(CTCRW_temp)}


#Merge SERIES_CRW and SERIES_SIM records for START_TIME and END_TIME of each SERIES segment using DATE_ID and START/END_ID codes
SERIES=merge(SERIES,subset(SERIES_CRW_temp,select=c(SEQ_ID,locType,speed,naive.p.val,PRED_TIME,LONG_CRW,LAT_CRW)),by="SEQ_ID",all.x=T)
remove(SERIES_CRW_temp)

#Replace default LAT and LONG (best within 24 hours) with LAT_CRW and LONG_CRW and reflect this change in corresponding LAT_LONG_FLAG
SERIES[,c("LAT_BEST24","LONG_BEST24")]=SERIES[,c("LAT","LONG")]
SERIES[!is.na(SERIES$LAT_CRW),c("LAT","LONG")]=SERIES[!is.na(SERIES$LAT_CRW),c("LAT_CRW","LONG_CRW")]
SERIES[!is.na(SERIES$LAT_CRW),c("LAT_LONG_FLAG")]=2

#8. Extract DEPTHS at CRW MLE and SIM locations from the BATHY raster at the SERIES
SERIES$DEPTH_CRW=extract(x=BATHY,y=SERIES[,c("LONG_CRW","LAT_CRW")])

#9. Add LIGHT_DARK information to SERIES 
SERIES=LIGHT_DARK(SERIES,START_END=F,TIME_VAR="DATE_TIME")
detach("package:oce", unload=TRUE);library(raster)

#Modify DAY_NIGHT variable based on the local SUNRISE and SUNSET times 
SERIES$DAY_NIGHT=ifelse(SERIES$DATE_TIME>SERIES$SUNRISE & SERIES$DATE_TIME<=SERIES$SUNSET,"Day","Night")
SERIES$MOON_PRESENT=ifelse(SERIES$MOON_ALT>0 & SERIES$MOON_ILLUM>=0.10,"Present","Absent")

############    BEHAV    ############
#Import output queries from ARGOSPRD.mdb database in which Date format has been standardized 
#to MM/DD/YY HH:MM using Excel formating
BEHAV=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/t_Depth_behavior.csv",
               colClasses=c("numeric",rep("character",4),"numeric","character",rep("numeric",3),"character"))

#1. Link PTT with species and assign each Species a SPP code
BEHAV=SPP(BEHAV)

#Add Age, Sex, WhaleID, MASS_MED, MB, and FAM code from TAGS table to BEHAV based on PTT
if("Sex"%in%colnames(BEHAV)){BEHAV=subset(BEHAV,select=-c(Sex,Age,Whale.ID,MASS_MED,MB,FAM))}
BEHAV=merge(BEHAV,subset(TAGS,select=c(PTT,Sex,Age,Whale.ID,MASS_MED,MB,FAM)),by.y="PTT",by.x="Ptt",all.x=T)
BEHAV=BEHAV[order(BEHAV$SEQ_ID),]

#2. Standardize date-time stamp: create DATE_TIME field as a chron object and extract MONTHS, YEARS, J_DATE, and DAY_NIGHT
BEHAV=DATE_TIME(BEHAV,"Start","%m/%d/%Y %H:%M")
BEHAV$START_TIME=chron(dates=unlist(strsplit(BEHAV$Start," "))[seq(1,2*nrow(BEHAV),2)],
                     times=paste(unlist(strsplit(BEHAV$Start," "))[seq(2,2*nrow(BEHAV),2)],":00",sep=""))
BEHAV$END_TIME=chron(dates=unlist(strsplit(BEHAV$End," "))[seq(1,2*nrow(BEHAV),2)],
                       times=paste(unlist(strsplit(BEHAV$End," "))[seq(2,2*nrow(BEHAV),2)],":00",sep=""))
BEHAV$DURATION=(BEHAV$END_TIME-BEHAV$START_TIME)*24

#3. Where spatial information is missing find ARGOS LAT and LONG with the closest DATE_TIME
BEHAV=LAT_LONG(BEHAV,LAT=NA,LONG=NA)

#4. Add a column for a REGION classificication based on arbitrary LAT and LONG bounding boxes
BEHAV=REGION(BEHAV)
BEHAV[!is.na(BEHAV$Number) & BEHAV$Number!=10,"Number"]=NA
BEHAV=BEHAV[order(BEHAV$Ptt,BEHAV$DATE_TIME,BEHAV$Number),]
BEHAV$SEQ_ID=seq(1:nrow(BEHAV))

####
PCA_DEPTH_DURATION_Zc = prcomp(log(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc" & BEHAV$DepthMin>0,c("DepthMin","DurationMin")]),center=T,scale=T)
plot(PCA_DEPTH_DURATION_Zc$x[,1],PCA_DEPTH_DURATION_Zc$x[,2])
BEHAV[,c("PC1","PC2")]=NA
BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc",c("PC1","PC2")]=PCA_DEPTH_DURATION_Zc$x
plot(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc" & BEHAV$PC1<=1,"DurationMin"],
     BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc" & BEHAV$PC1<=1,"DepthMin"],xlim=c(0,5000),ylim=c(0,2000))
points(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc" & BEHAV$PC1>1,"DurationMin"],
       BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Zc" & BEHAV$PC1>1,"DepthMin"],col="red")

PCA_DEPTH_DURATION_Md = prcomp(log(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$DepthMin>0 ,c("DepthMin","DurationMin")]),center=T,scale=T)
plot(PCA_DEPTH_DURATION_Md$x[,1],PCA_DEPTH_DURATION_Md$x[,2])
BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$DepthMin>0,c("PC1","PC2")]= -1 * PCA_DEPTH_DURATION_Md$x
plot(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$PC1<=2,"DurationMin"],
     BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$PC1<=2,"DepthMin"],xlim=c(0,5000),ylim=c(0,2000))
points(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$PC1> 2,"DurationMin"],
       BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Md" & BEHAV$PC1> 2,"DepthMin"],col="red")

PCA_DEPTH_DURATION_Pm = prcomp(log(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$DepthMin>0 ,c("DepthMin","DurationMin")]),center=T,scale=T)
plot(PCA_DEPTH_DURATION_Pm$x[,1],PCA_DEPTH_DURATION_Pm$x[,2])
BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$DepthMin>0,c("PC1","PC2")]=PCA_DEPTH_DURATION_Pm$x
plot(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$PC1<=0,"DurationMin"],
     BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$PC1<=0,"DepthMin"],xlim=c(0,5000),ylim=c(0,2000))
points(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$PC1>0,"DurationMin"],
       BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pm" & BEHAV$PC1>0,"DepthMin"],col="red")

PCA_DEPTH_DURATION_Gm = prcomp(log(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$DepthMin>0 ,c("DepthMin","DurationMin")]),center=T,scale=T)
plot(PCA_DEPTH_DURATION_Gm$x[,1],PCA_DEPTH_DURATION_Gm$x[,2])
BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$DepthMin>0,c("PC1","PC2")]= -1 * PCA_DEPTH_DURATION_Gm$x
plot(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$PC1<=0,"DurationMin"],
     BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$PC1<=0,"DepthMin"],xlim=c(0,5000),ylim=c(0,2000))
points(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$PC1>0,"DurationMin"],
       BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Gm" & BEHAV$PC1>0,"DepthMin"],col="red")


PCA_DEPTH_DURATION_Pe = prcomp(log(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$DepthMin>0 ,c("DepthMin","DurationMin")]),center=T,scale=T)
plot(PCA_DEPTH_DURATION_Pe$x[,1],PCA_DEPTH_DURATION_Pe$x[,2])
BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$DepthMin>0,c("PC1","PC2")]= PCA_DEPTH_DURATION_Pe$x
plot(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$PC1<=-2,"DurationMin"],
     BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$PC1<=-2,"DepthMin"],xlim=c(0,5000),ylim=c(0,2000))
points(BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$PC1> -2,"DurationMin"],
       BEHAV[BEHAV$What=="Dive" & BEHAV$SPP=="Pe" & BEHAV$PC1> -2,"DepthMin"],col="red")



#5. Add a column that assigns a code specific to each message and copies down the message duration information
#Add a message identifier number that rises sequentially within each J_DATE for each PTT 
for(i in unique(paste(BEHAV[BEHAV$What=="Message","Ptt"],BEHAV[BEHAV$What=="Message","J_DATE"],sep="_")))
{BEHAV[paste(BEHAV$Ptt,BEHAV$J_DATE,sep="_")==i 
       & BEHAV$What=="Message","MSG_ID"]=seq(1,nrow(BEHAV[paste(BEHAV$Ptt,BEHAV$J_DATE,sep="_")==i & 
                                                            BEHAV$What=="Message",]))}

#Creage the MSG_ID unique identifier for each message
BEHAV[BEHAV$What=="Message","MSG_ID"]=paste(BEHAV[BEHAV$What=="Message","Ptt"],BEHAV[BEHAV$What=="Message","J_DATE"],
                                            BEHAV[BEHAV$What=="Message","MSG_ID"],sep="_")

#Copy down this MSG_ID
for(i in 2:nrow(BEHAV))
{if(is.na(BEHAV$MSG_ID[i]))
  BEHAV[i,"MSG_ID"]=BEHAV[i-1,"MSG_ID"]}

#Assign all dives and surface intervals within a message the START_TIME, END_TIME, and DURATION of the message
for(i in unique(BEHAV$MSG_ID))
{BEHAV[BEHAV$MSG_ID==i,"MSG_DURATION"]=BEHAV[BEHAV$What=="Message" & BEHAV$MSG_ID==i,"DurationMin"]
 BEHAV[BEHAV$MSG_ID==i,"MSG_START_TIME"]=BEHAV[BEHAV$What=="Message" & BEHAV$MSG_ID==i,"START_TIME"]
 BEHAV[BEHAV$MSG_ID==i,"MSG_END_TIME"]=BEHAV[BEHAV$What=="Message" & BEHAV$MSG_ID==i,"END_TIME"]}

#Identify longer STRETCH's of continuous BEHAV log data that can be analyzed for inter-dive interval 
BEHAV$STRETCH_ID=NA
BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[1],"STRETCH_ID"]=1
for(i in 2:length(unique(BEHAV$MSG_ID)))
{if(abs(as.numeric(BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i-1],"MSG_END_TIME"][1])-
        as.numeric(BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i],"MSG_START_TIME"][1]))<=(2/(60*24)))
{BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i],"STRETCH_ID"]=
  BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i-1],"STRETCH_ID"][1]}
  else
  {BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i],"STRETCH_ID"]=
    BEHAV[BEHAV$MSG_ID==unique(BEHAV$MSG_ID)[i-1],"STRETCH_ID"][1]+1}}

  
#Create inter deep dive intervals (IDDI) metric for successive deep dives within a contiguous STRETCH of data
BEHAV$IDDI=NA

#Calculate IDDI based on specific deep dive threshold (PC1) for Zc
for(j in unique(BEHAV[BEHAV$SPP=="Zc","STRETCH_ID"]))
{#Create a dataframe for each stretch with Message rows removed
BEHAV_temp=BEHAV[BEHAV$STRETCH_ID==j & BEHAV$What%in%c("Dive","Surface"),]
#Create an index of the dives that exceeded the species specific threshold
INDEX_DIVE_temp=which(BEHAV_temp$STRETCH_ID==j & BEHAV_temp$What=="Dive" & BEHAV_temp$PC1>1)
#For each index that had a deep dive following it sum the dives and surface intervals between dives 
if(length(INDEX_DIVE_temp)>1)
{for(i in 1:(length(INDEX_DIVE_temp)-1))
{BEHAV[which(BEHAV$STRETCH_ID==j & BEHAV$What=="Dive" & BEHAV$PC1>1)[i],"IDDI"]=
    sum(BEHAV_temp[(INDEX_DIVE_temp[i]+1):(INDEX_DIVE_temp[i+1]-1),"DurationMin"],na.rm=T)}
remove(BEHAV_temp,INDEX_DIVE_temp)}}

#Calculate IDDI based on specific deep dive threshold (PC1) for Md
for(j in unique(BEHAV[BEHAV$SPP=="Md","STRETCH_ID"]))
{#Create a dataframe for each stretch with Message rows removed
  BEHAV_temp=BEHAV[BEHAV$STRETCH_ID==j & BEHAV$What%in%c("Dive","Surface"),]
  #Create an index of the dives that exceeded the species specific threshold
  INDEX_DIVE_temp=which(BEHAV_temp$STRETCH_ID==j & BEHAV_temp$What=="Dive" & BEHAV_temp$PC1>2)
  #For each index that had a deep dive following it sum the dives and surface intervals between dives 
  if(length(INDEX_DIVE_temp)>1)
  {for(i in 1:(length(INDEX_DIVE_temp)-1))
  {BEHAV[which(BEHAV$STRETCH_ID==j & BEHAV$What=="Dive" & BEHAV$PC1>2)[i],"IDDI"]=
    sum(BEHAV_temp[(INDEX_DIVE_temp[i]+1):(INDEX_DIVE_temp[i+1]-1),"DurationMin"],na.rm=T)}
    remove(BEHAV_temp,INDEX_DIVE_temp)}}

#Calculate IDDI based on specific deep dive threshold (PC1) for Pm
for(j in unique(BEHAV[BEHAV$SPP=="Pm","STRETCH_ID"]))
{#Create a dataframe for each stretch with Message rows removed
  BEHAV_temp=BEHAV[BEHAV$STRETCH_ID==j & BEHAV$What%in%c("Dive","Surface"),]
  #Create an index of the dives that exceeded the species specific threshold
  INDEX_DIVE_temp=which(BEHAV_temp$STRETCH_ID==j & BEHAV_temp$What=="Dive" & BEHAV_temp$PC1>0)
  #For each index that had a deep dive following it sum the dives and surface intervals between dives 
  if(length(INDEX_DIVE_temp)>1)
  {for(i in 1:(length(INDEX_DIVE_temp)-1))
  {BEHAV[which(BEHAV$STRETCH_ID==j & BEHAV$What=="Dive" & BEHAV$PC1>0)[i],"IDDI"]=
    sum(BEHAV_temp[(INDEX_DIVE_temp[i]+1):(INDEX_DIVE_temp[i+1]-1),"DurationMin"],na.rm=T)}
    remove(BEHAV_temp,INDEX_DIVE_temp)}}


#Calculate IDDI based on specific deep dive threshold (PC1) for Gm
for(j in unique(BEHAV[BEHAV$SPP=="Gm","STRETCH_ID"]))
{#Create a dataframe for each stretch with Message rows removed
  BEHAV_temp=BEHAV[BEHAV$STRETCH_ID==j & BEHAV$What%in%c("Dive","Surface"),]
  #Create an index of the dives that exceeded the species specific threshold
  INDEX_DIVE_temp=which(BEHAV_temp$STRETCH_ID==j & BEHAV_temp$What=="Dive" & BEHAV_temp$PC1>1.5)
  #For each index that had a deep dive following it sum the dives and surface intervals between dives 
  if(length(INDEX_DIVE_temp)>1)
  {for(i in 1:(length(INDEX_DIVE_temp)-1))
  {BEHAV[which(BEHAV$STRETCH_ID==j & BEHAV$What=="Dive" & BEHAV$PC1>1.5)[i],"IDDI"]=
    sum(BEHAV_temp[(INDEX_DIVE_temp[i]+1):(INDEX_DIVE_temp[i+1]-1),"DurationMin"],na.rm=T)}
    remove(BEHAV_temp,INDEX_DIVE_temp)}}

#Calculate IDDI based on specific deep dive threshold (PC1) for Pe
for(j in unique(BEHAV[BEHAV$SPP=="Pe","STRETCH_ID"]))
{#Create a dataframe for each stretch with Message rows removed
  BEHAV_temp=BEHAV[BEHAV$STRETCH_ID==j & BEHAV$What%in%c("Dive","Surface"),]
  #Create an index of the dives that exceeded the species specific threshold
  INDEX_DIVE_temp=which(BEHAV_temp$STRETCH_ID==j & BEHAV_temp$What=="Dive" & BEHAV_temp$PC1>0)
  #For each index that had a deep dive following it sum the dives and surface intervals between dives 
  if(length(INDEX_DIVE_temp)>1)
  {for(i in 1:(length(INDEX_DIVE_temp)-1))
  {BEHAV[which(BEHAV$STRETCH_ID==j & BEHAV$What=="Dive" & BEHAV$PC1>0)[i],"IDDI"]=
    sum(BEHAV_temp[(INDEX_DIVE_temp[i]+1):(INDEX_DIVE_temp[i+1]-1),"DurationMin"],na.rm=T)}
    remove(BEHAV_temp,INDEX_DIVE_temp)}}


#6. Assign each dive exceeding a SPP specific depth threshold BOUT_ID sequential number that is unique within each message
BEHAV[BEHAV$What=="Message","BOUT_ID"]=0

for(i in unique(BEHAV[BEHAV$SPP=="Pe" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,"MSG_ID"]))
{BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Pe" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,"BOUT_ID"] =
   seq(1,nrow(BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Pe" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,]))}

for(i in unique(BEHAV[BEHAV$SPP=="Gm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,"MSG_ID"]))
{BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Gm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,"BOUT_ID"] =
   seq(1,nrow(BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Gm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=50,]))}

for(i in unique(BEHAV[BEHAV$SPP=="Pm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=600,"MSG_ID"]))
{BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Pm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=600,"BOUT_ID"] =
   seq(1,nrow(BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Pm" & BEHAV$What=="Dive" & BEHAV$DepthMin>=600,]))}

for(i in unique(BEHAV[BEHAV$SPP=="Md" & BEHAV$What=="Dive" & BEHAV$DepthMin>=650,"MSG_ID"]))
{BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Md" & BEHAV$What=="Dive" & BEHAV$DepthMin>=650,"BOUT_ID"] =
   seq(1,nrow(BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Md" & BEHAV$What=="Dive" & BEHAV$DepthMin>=650,]))}

for(i in unique(BEHAV[BEHAV$SPP=="Zc" & BEHAV$What=="Dive" & BEHAV$DepthMin>=800,"MSG_ID"]))
{BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Zc" & BEHAV$What=="Dive" & BEHAV$DepthMin>=800,"BOUT_ID"] =
   seq(1,nrow(BEHAV[BEHAV$MSG_ID==i & BEHAV$SPP=="Zc" & BEHAV$What=="Dive" & BEHAV$DepthMin>=800,]))}

#Copy down this BOUT_ID to subsequent 
for(i in 2:nrow(BEHAV))
{if(is.na(BEHAV$BOUT_ID[i]))
  BEHAV[i,"BOUT_ID"]=BEHAV[i-1,"BOUT_ID"]}

for(i in unique(paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")))
{BEHAV[paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")==i,"DIVE_DURATION"]=BEHAV[paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")==i,"DurationMin"][1]
 BEHAV[paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")==i,"SURF_DURATION"]=sum(BEHAV[paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")==i,"DurationMin"][2:nrow(BEHAV[paste(BEHAV$MSG_ID,BEHAV$BOUT_ID,sep="_")==i,])])}

for(i in unique(BEHAV$MSG_ID))
{if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==1){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==1,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}
 if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==2){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==2,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}
 if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==3){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==3,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}
 if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==4){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==4,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}
 if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==5){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==5,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}
 if(max(BEHAV[BEHAV$MSG_ID==i,"BOUT_ID"])==6){BEHAV[BEHAV$MSG_ID==i & BEHAV$BOUT_ID==6,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=0}}

BEHAV[BEHAV$BOUT_ID==0,c("BOUT_ID","DIVE_DURATION","SURF_DURATION")]=NA
BEHAV[!is.na(BEHAV$BOUT_ID),"BOUT_ID"]=paste(BEHAV[!is.na(BEHAV$BOUT_ID),"MSG_ID"],
                                             BEHAV[!is.na(BEHAV$BOUT_ID),"BOUT_ID"],sep="_")

#BEHAV_PTT=data.frame(Ptt=unique(BEHAV$Ptt))
#BEHAV_PTT=SPP(BEHAV_PTT)
#for(i in 1:nrow(BEHAV_PTT))
#{BEHAV_PTT[BEHAV_PTT$Ptt==i,"SUM_SURFACE"]=sum(BEHAV[BEHAV$Ptt==i & BEHAV$Ptt==i,BEHAV$Duration])}

#7. Extract CTCRW MLE estimates for all the DATE_TIME stamps in BEHAV that fall between the first and last ARGOS position fix for a given PTT
BEHAV_CRW=as.data.frame(matrix(nrow=0,ncol=7)); 
colnames(BEHAV_CRW)=c("PTT","DATE_TIME","CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED")
BEHAV_SIM=as.data.frame(matrix(nrow=0,ncol=4));
colnames(BEHAV_SIM)=c("PTT","DATE_TIME","LONG_SIM","LAT_SIM")

#Loop to predict CTCRW MLE locations and simulated locations each BEHAV START_TIME and END_TIME 
for(i in unique(BEHAV$Ptt))
{#Create CTCRW predictions of ARGOS positions and remove outliers 
  BEHAV_temp=CTCRW(PTT=i,OUTPUT="PRED")
BEHAV_temp<-BEHAV_temp[BEHAV_temp$locType=="o",c("SEQ_ID", "NUM_TIME", "PTT", "Deploy.date", "End.date", "Date", 
                                                 "Class", "LastOfLatitude", "LastOfLongitude", "Number", "Error", 
                                                 "LastOfSemi.major.axis", "LastOfSemi.minor.axis", 
                                                 "LastOfEllipse.orientation", "Species", "SPP", "DATE_TIME", 
                                                 "YEAR", "MONTH", "J_DATE", "DAY_NIGHT", "LAT", "LONG", 
                                                 "LONG_360", "naive.p.val")]  
BEHAV_temp$OUTLIERS<-paste(BEHAV_temp[BEHAV_temp$naive.p.val<1-0.95^(1/length(BEHAV_temp$LAT)),"SEQ_ID"],collapse=",")
BEHAV_temp$N_REMOVED<-length(BEHAV_temp[BEHAV_temp$naive.p.val<1-0.95^(1/length(BEHAV_temp$LAT)),"SEQ_ID"])
BEHAV_temp<-BEHAV_temp[BEHAV_temp$naive.p.val>1-0.95^(1/length(BEHAV_temp$LAT)),]
BEHAV_temp=subset(BEHAV_temp,select=-c(naive.p.val))

#Predict CTCRW MLE locations at the START_TIME and END_TIME of each BEHAV segment
BEHAV_CRW_temp=CTCRW(DATA=BEHAV_temp,PTT=i,
                 PRED_TIMES=c(BEHAV[BEHAV$Ptt==i 
                                  & BEHAV$START_TIME>=min(BEHAV_temp$DATE_TIME) 
                                  & BEHAV$END_TIME<=max(BEHAV_temp$DATE_TIME) ,c("START_TIME")], 
                              BEHAV[BEHAV$Ptt==i 
                                    & BEHAV$START_TIME>=min(BEHAV_temp$DATE_TIME) 
                                    & BEHAV$END_TIME<=max(BEHAV_temp$DATE_TIME) ,c("END_TIME")]), 
                 OUTPUT="FINAL")
BEHAV_CRW=rbind(BEHAV_CRW,BEHAV_CRW_temp[,c("PTT","DATE_TIME","CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED")])

#Simulate locations from the posterior distribution of CTCRW model at the START_TIME and END_TIME of each BEHAV segment
BEHAV_SIM_temp=try(CTCRW(DATA=BEHAV_temp,PTT=i,
                 PRED_TIMES=c(BEHAV[BEHAV$Ptt==i 
                                    & BEHAV$START_TIME>=min(BEHAV_temp$DATE_TIME) 
                                    & BEHAV$END_TIME<=max(BEHAV_temp$DATE_TIME) ,c("START_TIME")], 
                              BEHAV[BEHAV$Ptt==i 
                                    & BEHAV$START_TIME>=min(BEHAV_temp$DATE_TIME) 
                                    & BEHAV$END_TIME<=max(BEHAV_temp$DATE_TIME) ,c("END_TIME")]), 
                 OUTPUT="SIM",N_SIM=40))
if(class(BEHAV_SIM_temp)=="try-error"){next}
BEHAV_SIM_temp$PTT=i #Add PTT code for later merge to BEHAV

BEHAV_SIM=rbind(BEHAV_SIM,BEHAV_SIM_temp[,c("PTT","DATE_TIME","LONG_SIM","LAT_SIM")])
remove(BEHAV_temp,BEHAV_CRW_temp,BEHAV_SIM_temp)}

#Create DATE_ID and START/END_ID codes to use in linking to BEHAV records
BEHAV_CRW$DATE_ID=paste(BEHAV_CRW$PTT,BEHAV_CRW$DATE_TIME)
BEHAV_SIM$DATE_ID=paste(BEHAV_SIM$PTT,as.chron(BEHAV_SIM$DATE_TIME))
BEHAV$START_ID=paste(BEHAV$Ptt,BEHAV$START_TIME)
BEHAV$END_ID=paste(BEHAV$Ptt,BEHAV$END_TIME)

#Merge BEHAV_CRW and BEHAV_SIM records for START_TIME and END_TIME of each BEHAV segment using DATE_ID and START/END_ID codes
BEHAV=merge(BEHAV,BEHAV_CRW[!duplicated(BEHAV_CRW$DATE_ID),c("DATE_ID","CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED")],by.x="START_ID",by.y="DATE_ID",all.x=T)
BEHAV=merge(BEHAV,BEHAV_SIM[!duplicated(BEHAV_SIM$DATE_ID),c("DATE_ID","LONG_SIM","LAT_SIM")],by.x="START_ID",by.y="DATE_ID",all.x=T)
names(BEHAV)[names(BEHAV) %in% c("CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED","LONG_SIM","LAT_SIM")] = 
  c("LAT_CRW_1","LAT_SE_CRW_1","LONG_CRW_1","LONG_SE_CRW_1","CRW_SPEED_1","LONG_SIM_1","LAT_SIM_1")
BEHAV=merge(BEHAV,BEHAV_CRW[!duplicated(BEHAV_CRW$DATE_ID),c("DATE_ID","CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED")],by.x="END_ID",by.y="DATE_ID",all.x=T)
BEHAV=merge(BEHAV,BEHAV_SIM[!duplicated(BEHAV_SIM$DATE_ID),c("DATE_ID","LONG_SIM","LAT_SIM")],by.x="END_ID",by.y="DATE_ID",all.x=T)
names(BEHAV)[names(BEHAV) %in% c("CRW_LAT","CRW_LAT_SE","CRW_LONG","CRW_LONG_SE","CRW_SPEED","LONG_SIM","LAT_SIM")] = 
  c("LAT_CRW_2","LAT_SE_CRW_2","LONG_CRW_2","LONG_SE_CRW_2","CRW_SPEED_2","LONG_SIM_2","LAT_SIM_2")
BEHAV=BEHAV[order(BEHAV$Ptt,BEHAV$DATE_TIME,BEHAV$Number),]

remove(BEHAV_CRW,BEHAV_SIM)

#Calculate spatial uncertainty of from SIM
BEHAV$LONG_SD=NA;BEHAV$LAT_SD=NA
for(i in which(!is.na(BEHAV$LONG_SIM_1)))
{BEHAV$LONG_SD[i] = sd((as.numeric(unlist(strsplit(BEHAV$LONG_SIM_1[i],","))) + 
                          as.numeric(unlist(strsplit(BEHAV$LONG_SIM_2[i],","))))/2)                       
 BEHAV$LAT_SD[i] = sd((as.numeric(unlist(strsplit(BEHAV$LAT_SIM_1[i],","))) + 
                         as.numeric(unlist(strsplit(BEHAV$LAT_SIM_2[i],","))))/2)}

#Replace default LAT and LONG (best within 24 hours) with LAT_CRW and LONG_CRW and reflect this change in corresponding LAT_LONG_FLAG
BEHAV[,c("LAT_BEST24","LONG_BEST24")]=BEHAV[,c("LAT","LONG")]
BEHAV[!is.na(BEHAV$LAT_CRW_1),c("LAT")]=rowSums(BEHAV[!is.na(BEHAV$LAT_CRW_1),c("LAT_CRW_1","LAT_CRW_2")])/2
BEHAV[!is.na(BEHAV$LONG_CRW_1),c("LONG")]=rowSums(BEHAV[!is.na(BEHAV$LONG_CRW_1),c("LONG_CRW_1","LONG_CRW_2")])/2
BEHAV[!is.na(BEHAV$LAT_CRW_1),c("LAT_LONG_FLAG")]=2

#8. Extract DEPTHS at CRW MLE and SIM locations from the BATHY raster at the BEHAV
detach("package:oce", unload=TRUE);library(raster)
BEHAV$DEPTH_CRW=extract(x=BATHY,y=(BEHAV[,c("LONG_CRW_1","LAT_CRW_1")]+BEHAV[,c("LONG_CRW_2","LAT_CRW_2")])/2)
BEHAV[c("DEPTH_SIM","DEPTH_AVG","DEPTH_SD","DEPTH_MIN","DEPTH_MAX")]=NA
for(i in which(!is.na(BEHAV$LONG_SIM_1)))
{BEHAV$DEPTH_SIM[i]=paste(extract(BATHY,cbind((as.numeric(unlist(strsplit(BEHAV$LONG_SIM_1[i],",")))+
                                                 as.numeric(unlist(strsplit(BEHAV$LONG_SIM_2[i],","))))/2,
                                              (as.numeric(unlist(strsplit(BEHAV$LAT_SIM_1[i],",")))+
                                                 as.numeric(unlist(strsplit(BEHAV$LAT_SIM_2[i],","))))/2)),collapse=",")
 BEHAV$DEPTH_AVG[i]=mean(as.numeric(unlist(strsplit(BEHAV$DEPTH_SIM[i],","))),na.rm=T)
 BEHAV$DEPTH_SD[i]=sd(as.numeric(unlist(strsplit(BEHAV$DEPTH_SIM[i],","))),na.rm=T)
 BEHAV$DEPTH_MAX[i]=max(as.numeric(unlist(strsplit(BEHAV$DEPTH_SIM[i],","))),na.rm=T)
 BEHAV$DEPTH_MIN[i]=min(as.numeric(unlist(strsplit(BEHAV$DEPTH_SIM[i],","))),na.rm=T)}

#Calculate the proportion of local bottom depth for each BEHAV$What==Dive
BEHAV$DEPTH_PROP_CRW=NA; BEHAV$DEPTH_PROP_AVG=NA
for(i in which(BEHAV$What=="Dive"))
 {BEHAV$DEPTH_PROP_CRW[i]=BEHAV$DepthMin[i]/BEHAV$DEPTH_CRW[i] 
  BEHAV$DEPTH_PROP_AVG[i]=BEHAV$DepthMin[i]/BEHAV$DEPTH_AVG[i]}


#9. Add LIGHT_DARK information to SERIES TAD 
BEHAV$DATE_TIME_MID=as.chron((as.numeric(BEHAV$START_TIME)+as.numeric(BEHAV$END_TIME))/2)
BEHAV=LIGHT_DARK(BEHAV,START_END=F,TIME_VAR="DATE_TIME_MID")
#Modify DAY_NIGHT variable based on the local SUNRISE and SUNSET times 
BEHAV$DAY_NIGHT=ifelse(BEHAV$DATE_TIME_MID>BEHAV$SUNRISE & BEHAV$DATE_TIME_MID<=BEHAV$SUNSET,"Day","Night")
BEHAV$MOON_PRESENT=ifelse(BEHAV$MOON_ALT>0 & BEHAV$MOON_ILLUM>=0.10,"Present","Absent")

#10. Classify dives as FORAGING or non-FORAGING based on a depth threshold from BEHAV histograms 
# and DTAG data from the literature 
for(i in c("Pe","Gm","Md","Zc","Pm"))
{BEHAV[BEHAV$SPP==i & BEHAV$What=="Dive" & BEHAV$DAY_NIGHT=="Day" &
        BEHAV$DepthMin<DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==i,"DEPTH_DAY"],"FORAGING"]="N"
BEHAV[BEHAV$SPP==i & BEHAV$What=="Dive" & BEHAV$DAY_NIGHT=="Night" &
        BEHAV$DepthMin<DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==i,"DEPTH_NIGHT"],"FORAGING"]="N"
BEHAV[BEHAV$SPP==i & BEHAV$What=="Dive" & BEHAV$DAY_NIGHT=="Day" &
        BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==i,"DEPTH_DAY"],"FORAGING"]="Y"
BEHAV[BEHAV$SPP==i & BEHAV$What=="Dive" & BEHAV$DAY_NIGHT=="Night" &
        BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==i,"DEPTH_NIGHT"],"FORAGING"]="Y"}


############    MORPH    ############

library(stringr)
#Create a MORPH table by concatenating NMNH Whale Collection Search tables for each species
MORPH=data.frame()
for(i in 1:6)
{MORPH_temp=read.csv(paste("~/Grad School/Research/3_2012_Whale Tagging/Data/",
                           c("nmnhsearch-20150713005458-Peponocephala electra.csv",
                             "nmnhsearch-20150713005657-Globicephala macrorhynchus.csv",
                             "nmnhsearch-20150713005821-Mesoplodon densirostris.csv",
                             "nmnhsearch-20150713005840-Mesoplodon europaeus.csv",      
                             "nmnhsearch-20150713005947-Ziphius cavirostris.csv",
                             "nmnhsearch-20150713010802-Physeter macrocephalus.csv")[i],sep=""),stringsAsFactors=F)
MORPH_temp$SPP=c("Pe","Gm","Md","Me","Zc","Pm")[i]
MORPH=rbind(MORPH,MORPH_temp)}

#Add two data columns splitting multipart morphometric measurements into two variables (DATA1, DATA2) at "; "
MORPH=data.frame(MORPH,DATA1=str_split_fixed(MORPH$Measurements,pattern="; ",n=2)[,1],
                 DATA2=str_split_fixed(MORPH$Measurements,pattern="; ",n=2)[,2])

#Remove measurement labels up to and including ": " and substitute with ""
#Regex syntax: beginning of the string (^), any character (.) repeated zero or more times (*) up to the first (?) ": "
MORPH$DATA1=gsub("^.*: ","",MORPH$DATA1)
MORPH$DATA2=gsub("^.*: ","",MORPH$DATA2)

#Extract the first numeric value in the DATA1 string (which should be the values)
MORPH$DATA1A=as.numeric(str_extract(MORPH$DATA1, "[0-9]+"))
#Extract the first alpha value in the DATA1 string (which should be units)
MORPH$DATA1B=str_extract(MORPH$DATA1, "[aA-zZ]+")
#Extract the first numeric value in the DATA2 string (which should be the values)
MORPH$DATA2A=as.numeric(str_extract(MORPH$DATA2, "[0-9]+"))
#Extract the first alpha value in the DATA2 string (which should be units)
MORPH$DATA2B=str_extract(MORPH$DATA2, "[aA-zZ]+")

#Assign DATA1A and DATA2A to LENGTH and MASS variables based on the units in DATA1B and DATA2B
MORPH$LENGTH=ifelse(MORPH$DATA2B%in%c("mm","cm","m","ft","in"),MORPH$DATA2A,NA)
MORPH$MASS=ifelse(MORPH$DATA2B%in%c("kg","g","lb","t"),MORPH$DATA2A,NA)
MORPH$LENGTH=ifelse(MORPH$DATA1B%in%c("mm","cm","m","ft","in"),MORPH$DATA1A,MORPH$LENGTH)
MORPH$MASS=ifelse(MORPH$DATA1B%in%c("kg","g","lb","t"),MORPH$DATA1A,MORPH$MASS)

#Convert various units of LENGTH to m and units of MASS to kg
MORPH$LENGTH=ifelse(MORPH$DATA1B%in%"mm"|MORPH$DATA2B%in%"mm",MORPH$LENGTH/1000,MORPH$LENGTH)
MORPH$LENGTH=ifelse(MORPH$DATA1B%in%"cm"|MORPH$DATA2B%in%"cm",MORPH$LENGTH/100,MORPH$LENGTH)
MORPH$LENGTH=ifelse(MORPH$DATA1B%in%"ft"|MORPH$DATA2B%in%"ft",MORPH$LENGTH/3.28084,MORPH$LENGTH)
MORPH$LENGTH=ifelse(MORPH$DATA1B%in%"in"|MORPH$DATA2B%in%"in",MORPH$LENGTH/39.3701,MORPH$LENGTH)
MORPH$MASS=ifelse(MORPH$DATA1B%in%"g"|MORPH$DATA2B%in%"g",MORPH$MASS/1000,MORPH$MASS)
MORPH$MASS=ifelse(MORPH$DATA1B%in%"lb"|MORPH$DATA2B%in%"lb",MORPH$MASS/2.20462,MORPH$MASS)
MORPH$MASS=ifelse(MORPH$DATA1B%in%"t"|MORPH$DATA2B%in%"t",MORPH$MASS/0.001,MORPH$MASS)

#Extract SEX from Sex.Stage and standardize SEX classification 
MORPH$SEX=str_trim(str_split_fixed(MORPH$Sex.Stage, pattern=":", n=2)[,1])
MORPH$SEX=ifelse(MORPH$SEX%in%c("Multiple animals of mixed sex"),NA,MORPH$SEX)
MORPH$SEX=substr(MORPH$SEX,1,1)

#Extract AGE from Sex.Stage and standardize AGE classification 
MORPH$AGE=str_split_fixed(MORPH$Sex.Stage, pattern=":", n=2)[,2]
MORPH$AGE=str_trim(gsub(":","",MORPH$AGE))
MORPH$AGE=ifelse(MORPH$AGE%in%c("Adult","adult","Pregnant  118 cm fetus","Adult/young  accompanied by a calf",
                                "Adult/old  relatively heavy scarring, teeth worn down to gums",
                                "Mature  518 cm; Male  Calf  370 cm",
                                "Lactating  photos show milk oozing from the nipples"),"A",MORPH$AGE)
MORPH$AGE=ifelse(MORPH$AGE%in%c("Subadult","Maturing","Juvenile","Young"),"SA",MORPH$AGE)
MORPH$AGE=ifelse(MORPH$AGE%in%c("Neonate","Calf","Calf  400 cm"),"C",MORPH$AGE)
MORPH$AGE=ifelse(MORPH$AGE%in%c("fetus","Fetus","Fetus ?"),"F",MORPH$AGE)

#Extract GENUS, SPECIES, and SOURCES and standardize formatting
MORPH$GENUS=word(MORPH$Name.Hierarchy,1)
MORPH$SPECIES=word(MORPH$Name.Hierarchy,2)
MORPH$PRIMARY_SOURCE=ifelse(MORPH$Museum.Abbreviation=="USNM",paste(MORPH$Museum.Abbreviation,MORPH$Catalog.Number),
                            MORPH$Catalog.Number)
MORPH$SECONDARY_SOURCE="NMNH Whale Collection Search"

#Save a copy of complete NMNH Whale Collection Search table
MORPH_ALL=MORPH

#Import manually entered whale morphometric data from Whale Mass.csv and append this data to standardized MORPH columns
MORPH_temp=read.csv(paste("~/Grad School/Research/3_2012_Whale Tagging/Data/",
                          "Whale Mass.csv",sep=""),stringsAsFactors=F)
MORPH=rbind(MORPH[,c("GENUS","SPECIES","SPP","MASS","LENGTH","SEX","AGE","PRIMARY_SOURCE","SECONDARY_SOURCE")],
            MORPH_temp[MORPH_temp$DUPLICATE=="N",c("GENUS","SPECIES","SPP","MASS","LENGTH","SEX","AGE",
                                                   "PRIMARY_SOURCE","SECONDARY_SOURCE")])

#Clean up temp variables
remove(MORPH_temp)

#Fit log-log linear models of MASS as a function of length for each SPP that will be used to
#predict mean and median MASS for each SEX and AGE class based on LENGTH frequency of strandings
LM_Pe_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Pe" & !is.na(MORPH$MASS),])
LM_Gm_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Gm" & !is.na(MORPH$MASS),])
LM_Md_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Md" & !is.na(MORPH$MASS),])
LM_Me_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Me" & !is.na(MORPH$MASS),])
LM_Zc_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Zc" & !is.na(MORPH$MASS),])
LM_Pm_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Pm" & !is.na(MORPH$MASS),])

###########   MASS    ############
#Create a data.frame for each unique combination of SPP, SEX, and AGE
MASS=expand.grid(SPP=c("Pe","Gm","Md","Me","Zc","Pm"),SEX=c("M","F"),AGE=c("SA","A"),stringsAsFactors=F)
MASS=MASS[order(MASS$SPP,MASS$SEX,MASS$AGE),]

#Assign upper and lower LENGTH thresholds to the SEX-AGE classes: F-A,F-SA,M-A,M-SA in each SPP
#Adult/sub-adult threshold value is somewhat subjective but based primarily on minimum age of 
#reproductive maturity indicated in necropsy notes from NMNH data search
MASS[MASS$SPP=="Pe","THRESH_LOW"]=c(2.30,1.50,2.45,1.50)
MASS[MASS$SPP=="Pe","THRESH_HIGH"]=c(3.00,2.30,3.00,2.45)

MASS[MASS$SPP=="Gm","THRESH_LOW"]=c(2.93,2.00,3.95,3.00)
MASS[MASS$SPP=="Gm","THRESH_HIGH"]=c(7.00,2.93,7.00,3.95)

MASS[MASS$SPP=="Md","THRESH_LOW"]=c(3.50,1.50,3.50,1.50)
MASS[MASS$SPP=="Md","THRESH_HIGH"]=c(6.00,3.50,6.00,3.50)

MASS[MASS$SPP=="Me","THRESH_LOW"]=c(3.50,1.50,3.50,1.50)
MASS[MASS$SPP=="Me","THRESH_HIGH"]=c(6.50,3.50,6.50,3.50)

MASS[MASS$SPP=="Zc","THRESH_LOW"]=c(4.60,2.50,4.90,2.50)
MASS[MASS$SPP=="Zc","THRESH_HIGH"]=c(8.00,4.60,8.00,4.90)

MASS[MASS$SPP=="Pm","THRESH_LOW"]=c(8.50,3.00,12.00,8.00)
MASS[MASS$SPP=="Pm","THRESH_HIGH"]=c(14.00,8.50,25.00,12.00)

#Using these threshholds calculate the mean and median LENGTH and MASS of each SEX-AGE class in each SPP
#MASS based on predictictions from log-log model of MASS as a function of LENGTH fitted above
for(i in 1:nrow(MASS))
{MASS[i,"LENGTH_MEAN"]=mean(MORPH[MORPH$SPP==MASS$SPP[i] & MORPH$SEX==MASS$SEX[i] & 
                                    MORPH$LENGTH>=MASS$THRESH_LOW[i] & MORPH$LENGTH<MASS$THRESH_HIGH[i],"LENGTH"],na.rm=T)
MASS[i,"LENGTH_MED"]=median(MORPH[MORPH$SPP==MASS$SPP[i] & MORPH$SEX==MASS$SEX[i] & 
                                    MORPH$LENGTH>=MASS$THRESH_LOW[i] & MORPH$LENGTH<MASS$THRESH_HIGH[i],"LENGTH"],na.rm=T)
MASS[i,"MASS_MEAN"]=10^mean(predict(eval(parse(text=paste("LM_",MASS$SPP[i],"_MORPH",sep=""))),
                                    newdata=MORPH[MORPH$SPP==MASS$SPP[i] & MORPH$SEX==MASS$SEX[i] & 
                                                    MORPH$LENGTH>=MASS$THRESH_LOW[i] & 
                                                    MORPH$LENGTH<MASS$THRESH_HIGH[i],]),na.rm=T)
MASS[i,"MASS_MED"]=10^median(predict(eval(parse(text=paste("LM_",MASS$SPP[i],"_MORPH",sep=""))),
                                     newdata=MORPH[MORPH$SPP==MASS$SPP[i] & MORPH$SEX==MASS$SEX[i] & 
                                                     MORPH$LENGTH>=MASS$THRESH_LOW[i] & 
                                                     MORPH$LENGTH<MASS$THRESH_HIGH[i],]),na.rm=T)}

MASS[MASS$SPP=="Pe","MB"]= 10^0.511*10^(0.220 * MICERTA[MICERTA$SPP_NAME=="Peponocephala electra  ","Z_MAX"])  
MASS[MASS$SPP=="Gm","MB"]=68.2
MASS[MASS$SPP=="Pm","MB"]=70.0
MASS[MASS$SPP=="Me","MB"]=74.1
MASS[MASS$SPP=="Md","MB"]=69.2
MASS[MASS$SPP=="Zc","MB"]=43.2

############    TAGS UPDATE   ############

#Assign each SEX-AGE class in each SPP a median LENGTH and MASS of based on predictions from 
#log-log models of MASS as a function of LENGTH
TAGS[,c("MASS_MED","LENGTH_MED","MB")]=NA
for(i in 1:nrow(MASS))
{TAGS[TAGS$SPP==MASS$SPP[i] & TAGS$Sex==MASS$SEX[i] & TAGS$Age==MASS$AGE[i] ,c("MASS_MED")]=MASS[i ,c("MASS_MED")]
TAGS[TAGS$SPP==MASS$SPP[i] & TAGS$Sex==MASS$SEX[i] & TAGS$Age==MASS$AGE[i] ,c("LENGTH_MED")]=MASS[i ,c("LENGTH_MED")]
TAGS[TAGS$SPP==MASS$SPP[i] & TAGS$Sex%in%c("U","F or U","U or AF") ,c("MASS_MED")]=
  mean(c(MASS[MASS$SPP==MASS$SPP[i] & MASS$SEX=="F" & MASS$AGE=="A",c("MASS_MED")],
         MASS[MASS$SPP==MASS$SPP[i] & MASS$SEX=="M" & MASS$AGE=="SA",c("MASS_MED")]),na.rm=T)
TAGS[TAGS$SPP==MASS$SPP[i] & TAGS$Sex%in%c("U","F or U","U or AF") ,c("LENGTH_MED")]=
  mean(c(MASS[MASS$SPP==MASS$SPP[i] & MASS$SEX=="F" & MASS$AGE=="A",c("LENGTH_MED")],
         MASS[MASS$SPP==MASS$SPP[i] & MASS$SEX=="M" & MASS$AGE=="SA",c("LENGTH_MED")]),na.rm=T)
TAGS[TAGS$SPP==MASS$SPP[i],c("MB")]= MASS[MASS$SPP==MASS$SPP[i],"MB"][1]}

TAGS[TAGS$SPP=="Gm" & TAGS$Sex%in%c("F or M") ,c("MASS_MED")]=
  MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A",c("MASS_MED")]
TAGS[TAGS$SPP=="Gm" & TAGS$Sex%in%c("F or M") ,c("LENGTH_MED")]=
  MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A",c("LENGTH_MED")]

TAGS[TAGS$SPP=="Pe" & TAGS$Sex%in%c("U") ,c("MASS_MED")]=
  mean(c(MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A",c("MASS_MED")],
         MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A",c("MASS_MED")]),na.rm=T)
TAGS[TAGS$SPP=="Pe" & TAGS$Sex%in%c("U") ,c("LENGTH_MED")]=
  mean(c(MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A",c("LENGTH_MED")],
         MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A",c("LENGTH_MED")]),na.rm=T)

#5. Add a code to TAGS that indicates whether it appears in ARGOS data
for(i in unique(ARGOS$PTT))
{TAGS[TAGS$PTT==i,"ARGOS"]=nrow(ARGOS[ARGOS$PTT==i,])}
TAGS[is.na(TAGS$ARGOS),"ARGOS"]=0

#6. Calculate transmission START_TIME, END_TIME, and DURATION for each PTT in ARGOS and TAGS tables
for(i in unique(ARGOS$PTT))
{TAGS[TAGS$PTT==i,"START_LAT"]=ARGOS[ARGOS$PTT==i & ARGOS$DATE_TIME==min(ARGOS[ARGOS$PTT==i,"DATE_TIME"]),"LAT"][1]
TAGS[TAGS$PTT==i,"START_LONG"]=ARGOS[ARGOS$PTT==i & ARGOS$DATE_TIME==min(ARGOS[ARGOS$PTT==i,"DATE_TIME"]),"LONG"][1]
TAGS[TAGS$PTT==i,"END_LAT"]=ARGOS[ARGOS$PTT==i & ARGOS$DATE_TIME==max(ARGOS[ARGOS$PTT==i,"DATE_TIME"]),"LAT"][1]
TAGS[TAGS$PTT==i,"END_LONG"]=ARGOS[ARGOS$PTT==i & ARGOS$DATE_TIME==max(ARGOS[ARGOS$PTT==i,"DATE_TIME"]),"LONG"][1]
TAGS[TAGS$PTT==i,"START_DATE"]=min(ARGOS[ARGOS$PTT==i,"DATE_TIME"])
TAGS[TAGS$PTT==i,"END_DATE"]=max(ARGOS[ARGOS$PTT==i,"DATE_TIME"])}

#Calculate the duration of each tag transmission period
TAGS$DURATION=TAGS$END_DATE-TAGS$START_DATE

#6. Add a code to TAGS that indicates whether it appears in ARGOS_CTCRW data
for(i in unique(ARGOS_CTCRW$PTT))
{TAGS[TAGS$PTT==i,"ARGOS_CTCRW"]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==i,])}
TAGS[is.na(TAGS$ARGOS_CTCRW),"ARGOS_CTCRW"]=0

#6. Classify ARGOS_CTCRW records by tag TYPE
for(i in TAGS[TAGS$TYPE=="SPLASH" & TAGS$ARGOS_CTCRW>0,"PTT"])
{ARGOS_CTCRW[ARGOS_CTCRW$PTT==i,"TYPE"]="SPLASH"}

for(i in TAGS[TAGS$TYPE=="SPOT" & TAGS$ARGOS_CTCRW>0,"PTT"])
{ARGOS_CTCRW[ARGOS_CTCRW$PTT==i,"TYPE"]="SPOT"}

#5. Add a code to TAGS that indicates any individual PTT tag returned PDT data
for(i in unique(PDT$Ptt))
{TAGS[TAGS$PTT==i,"PDT"]=nrow(PDT[PDT$Ptt==i & !duplicated(PDT$CAST),])}
TAGS[is.na(TAGS$PDT),"PDT"]=0

#5. Add a code to TAGS that indicates how many TAT were returned by any individual PTT
for(i in unique(TAT$Ptt))
{TAGS[TAGS$PTT==i,"TAT"]=nrow(TAT[TAT$Ptt==i,])}
TAGS[is.na(TAGS$TAT),"TAT"]=0

#5. Add a code to TAGS that indicates how many SERIES were returned by any individual PTT
for(i in unique(SERIES$Ptt))
{TAGS[TAGS$PTT==i,"SERIES"]=nrow(SERIES[SERIES$Ptt==i,])}
TAGS[is.na(TAGS$SERIES),"SERIES"]=0

#13. Add a code to TAGS that indicates how many SERIES_TAD were returned by any individual PTT
for(i in unique(SERIES_TAD$Ptt))
{TAGS[TAGS$PTT==i,"SERIES_TAD"]=nrow(SERIES_TAD[SERIES_TAD$Ptt==i & SERIES_TAD$COUNT>=72,])}

for(i in unique(SERIES_TAD[SERIES_TAD$SPP=="Pe","Ptt"]))
{TAGS[TAGS$PTT==i,"SERIES_TAD"]=nrow(SERIES_TAD[SERIES_TAD$Ptt==i & SERIES_TAD$COUNT>=36,])}

TAGS[is.na(TAGS$SERIES_TAD),"SERIES_TAD"]=0

#5. Add a code to TAGS that indicates how many BEHAV were returned by any individual PTT
for(i in unique(BEHAV$Ptt))
{TAGS[TAGS$PTT==i,"BEHAV"]=nrow(BEHAV[BEHAV$Ptt==i,])
TAGS[TAGS$PTT==i,"DURATION_MAX"]=max(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive","DurationMin"],na.rm=T)
TAGS[TAGS$PTT==i,"DEPTH_MAX"]=max(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive","DepthMin"],na.rm=T)
TAGS[TAGS$PTT==i,"DURATION_MEAN"]=mean(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive" & 
                                             BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)
TAGS[TAGS$PTT==i,"DEPTH_MEAN"]=mean(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive" & 
                                          BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)
TAGS[TAGS$PTT==i,"IDDI_MED"]=median(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive","IDDI"],na.rm=T)
TAGS[TAGS$PTT==i,"IDDI_DURATION_MED"]=median(BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive","IDDI"]/
                                             BEHAV[BEHAV$Ptt==i & BEHAV$What=="Dive","DurationMin"],na.rm=T)
}

TAGS[is.na(TAGS$BEHAV),"BEHAV"]=0

#Handle one Md outlier likely caused by the tag not surfacing for long enough to detect a surface interval
TAGS[TAGS$PTT==111670,"DURATION_MAX"]=max(BEHAV[BEHAV$Ptt==111670 & BEHAV$What=="Dive" & BEHAV$DurationMin<70*60,"DurationMin"],na.rm=T)

#5. Add a code to TAGS that indicates how many HISTO were returned by any individual PTT
for(i in unique(HISTO$Ptt))
{TAGS[TAGS$PTT==i,"HISTO"]=nrow(HISTO[HISTO$Ptt==i,])}
TAGS[is.na(TAGS$HISTO),"HISTO"]=0

TAGS[TAGS$SPP%in%c("Pm","Gm","Pe"),"AEROB"]="AEROB"
TAGS[TAGS$SPP%in%c("Md","Zc"),"AEROB"]="ANAEROB"
TAGS[TAGS$SPP%in%c("Pm"),"FAMILY"]="PHYSETERID"
TAGS[TAGS$SPP%in%c("Md","Zc"),"FAMILY"]="ZIPHIID"
TAGS[TAGS$SPP%in%c("Gm","Pe"),"FAMILY"]="DELPHINID"

############    BIOPSY    ############
BIOPSY=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/Tags_ID_Biopsy.csv",
                stringsAsFactors = F)
BIOPSY=BIOPSY[BIOPSY$Sample.FieldID.1!="",]

#Add a SEQ_ID column for later sorting
BIOPSY$SEQ_ID=seq(1,nrow(BIOPSY))

#Add information from the TAGS data table including: a tagging EVENT identifier, 
#the numbers of records from different data types, and the spatial and temporal extent of ARGOS data 
BIOPSY=merge(BIOPSY,TAGS[,c("PTT","SPP","TAT","BEHAV","HISTO","SERIES","DURATION")], by="PTT",all.x=T)
names(BIOPSY)[names(BIOPSY) %in% c("TAT","BEHAV","SERIES")] = c("N_TAT","N_BEHAV","N_SERIES")
BIOPSY$SPP=as.character(BIOPSY$SPP)
BIOPSY=BIOPSY[order(BIOPSY$SEQ_ID),]

#Calculate summary statistics for the maximum DEPTH of each dive recorded in the BEHAV log and individual DEPTH 
#observations recorded in time SERIES log that exceed species-specific foraging ranges defined as depths > thresholds 
#defined using a qualitative examination of BEHAV histogram plots and TAT boxplots
BIOPSY$DEPTH_MAX_AVG=NA;BIOPSY$DEPTH_MAX_MED=NA;BIOPSY$DEPTH_MAX_SD=NA
BIOPSY$DEPTH_MAX_N=NA;BIOPSY$DEPTH_MAX_MIN=NA;BIOPSY$DEPTH_MAX_MAX=NA
BIOPSY$DEPTH_AVG=NA;BIOPSY$DEPTH_MED=NA;BIOPSY$DEPTH_SD=NA
BIOPSY$DEPTH_N=NA;BIOPSY$DEPTH_MIN=NA;BIOPSY$DEPTH_MAX=NA

for(i in which(BIOPSY$TYPE=="SPLASH" & BIOPSY$N_BEHAV>0))
{BIOPSY$DEPTH_MAX_AVG[i]=mean(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                      & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                                BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                      & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))
BIOPSY$DEPTH_MAX_MED[i]=median(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                       & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                                 BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                       & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))
BIOPSY$DEPTH_MAX_SD[i]=sd(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                  & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                            BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                  & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))
BIOPSY$DEPTH_MAX_N[i]=length(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                     & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                               BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                     & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))
BIOPSY$DEPTH_MAX_MIN[i]=min(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                    & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                              BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                    & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))
BIOPSY$DEPTH_MAX_MAX[i]=max(c(BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Day"
                                    & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DepthMin"],
                              BEHAV[BEHAV$Ptt==BIOPSY$PTT[i] & !is.na(BEHAV$DepthMin) & BEHAV$DAY_NIGHT=="Night"
                                    & BEHAV$DepthMin>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DepthMin"]))}

#Cleanup any records that produced Inf values for DEPTH_MAX_MIN and DEPTH_MAX_MAX
BIOPSY[!is.na(BIOPSY$DEPTH_MAX_N) & BIOPSY$DEPTH_MAX_N==0,which(colnames(BIOPSY)=="DEPTH_MAX_AVG"):ncol(BIOPSY)]=NA


for(i in which(BIOPSY$TYPE=="SPLASH" & BIOPSY$N_SERIES>0))
{BIOPSY$DEPTH_AVG[i]=mean(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                                  & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                           SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                                  & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))
BIOPSY$DEPTH_MED[i]=median(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                                    & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                             SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                                    & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))
BIOPSY$DEPTH_SD[i]=sd(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                               & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                        SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                               & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))
BIOPSY$DEPTH_N[i]=length(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                                  & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                           SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                                  & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))
BIOPSY$DEPTH_MIN[i]=min(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                                 & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                          SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                                 & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))
BIOPSY$DEPTH_MAX[i]=max(c(SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Day"
                                 & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"Depth"],
                          SERIES[SERIES$Ptt==BIOPSY$PTT[i] & SERIES$DAY_NIGHT=="Night"
                                 & SERIES$Depth>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"Depth"]))}

#Cleanup any records that produced Inf values for DEPTH_MIN and DEPTH_MAX
BIOPSY[!is.na(BIOPSY$DEPTH_N) & BIOPSY$DEPTH_N==0,which(colnames(BIOPSY)=="DEPTH_AVG"):ncol(BIOPSY)]=NA


#Calculate summary statistics for DEPTH and TEMP from TAT_REP data derived from TAT histograms based on 
#observed time spent within species-specific foraging ranges defined as time spent at depths > thresholds 
#defined using a qualitative examination of BEHAV histogram plots and TAT boxplots
BIOPSY$TEMP_AVG=NA; BIOPSY$TEMP_MED=NA; BIOPSY$TEMP_SD=NA
BIOPSY$TEMP_N=NA; BIOPSY$TEMP_MIN=NA; BIOPSY$TEMP_MAX=NA

for(i in which(BIOPSY$TYPE=="SPOT" & BIOPSY$N_TAT>0))
{BIOPSY$TEMP_AVG[i]=mean(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                                  & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  BIOPSY$TEMP_MED[i]=median(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                                    & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  BIOPSY$TEMP_SD[i]=sd(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                               & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  BIOPSY$TEMP_N[i]=length(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                                & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  BIOPSY$TEMP_MIN[i]=min(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                                 & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  BIOPSY$TEMP_MAX[i]=max(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i]
                                 & TAT_REP$TEMP<=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"TEMP"] ,"TEMP"])
  
  BIOPSY$DEPTH_AVG[i]=mean(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                 & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                             TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                     & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 BIOPSY$DEPTH_MED[i]=median(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                            TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 BIOPSY$DEPTH_SD[i]=sd(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                   & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                           TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                   & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 BIOPSY$DEPTH_N[i]=length(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                  & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                          TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                  & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 BIOPSY$DEPTH_MIN[i]=min(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                            TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 BIOPSY$DEPTH_MAX[i]=max(c(TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO>=0.75
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_DAY"] ,"DEPTH"],
                            TAT_REP[TAT_REP$PTT==BIOPSY$PTT[i] & TAT_REP$LIGHT_RATIO<=0.25
                                    & TAT_REP$DEPTH>=DIVE_THRESHOLDS[DIVE_THRESHOLDS$SPP==BIOPSY$SPP[i],"DEPTH_NIGHT"] ,"DEPTH"]))
 }

#Calculate a TEMP_AVG for each DEPTH_AVG from SPLASH tag records by taking an average of CTD readings on that 
#DEPTH_AVG levels accross the study area
for(i in which(BIOPSY$TYPE=="SPLASH" & !is.na(BIOPSY$DEPTH_AVG)))
{BIOPSY_CTD_INTERP_temp=CTD_INTERP(DEPTH=BIOPSY$DEPTH_AVG[i])$TEMP
BIOPSY$TEMP_AVG[i]=mean(BIOPSY_CTD_INTERP_temp)
BIOPSY$TEMP_MED[i]=median(BIOPSY_CTD_INTERP_temp)
BIOPSY$TEMP_SD[i]=sd(BIOPSY_CTD_INTERP_temp)
BIOPSY$TEMP_N[i]=length(BIOPSY_CTD_INTERP_temp)
BIOPSY$TEMP_MIN[i]=min(BIOPSY_CTD_INTERP_temp)
BIOPSY$TEMP_MAX[i]=max(BIOPSY_CTD_INTERP_temp)}
remove(BIOPSY_CTD_INTERP_temp)


save.image("~/Grad School/Research/3_2012_Whale Tagging/Cetacean_Analysis_Workspace_1.0.RData")

############   PHYLO_DATA    ############

# Assemble a dataframe of species with complete maximum dive duration (T_MAX), body mass (MASS), and 
# myoglobin concentration (Mb_MAX) information from Micerta et al. 2013 supplement Table 1
PHYLO_DATA=MICERTA[MICERTA$SPP_NAME%in%c("Kogia breviceps  ","Delphinapterus leucas  ","Monodon monoceros ",
                                         "Phocoena phocoena  ", "Orcinus orca  ",
                                         "Stenella attenuata ","Hyperoodon ampullatus "), 
                   c("COMMON_NAME","SPP_NAME","T_MAX","MASS","Mb_MAX")]
library(stringr)
PHYLO_DATA$SPP_NAME=str_trim(PHYLO_DATA$SPP_NAME)
PHYLO_DATA$SPP=c("Kb","Dl","Mm","Pp","Oo","Sa","Ha")

# Re-scale maximum dive duration (T_MAX) from seconds to minutes
PHYLO_DATA$T_MAX=PHYLO_DATA$T_MAX/60

# Add species specific information on maximum dive duration (T_MAX), body mass (MASS), 
# and/or myoglobin concentration (Mb_MAX) from literature sources not reported in Micerta et al. 2013
PHYLO_DATA[PHYLO_DATA$SPP=="Kb",c("T_MAX","MASS","Mb_MAX")]=c(18,151,59.2) #T_MAX and MASS from juvenile individual in Scott et al. 2001, Mb_MAX from Kielhorn et al. 2013

# Import a table of inter-deep dive interval (IDDI) information from literature dive profile records
PHYLO_IDDI=read.csv("~/Grad School/Research/3_2012_Whale Tagging/Data/IDDI.csv",header = T)

# Calculate the median inter-deep dive interval (IDDI) for each SPP in PHYLO_DATA
for(i in PHYLO_DATA$SPP)
{PHYLO_DATA[PHYLO_DATA$SPP==i,"IDDI"]=median(PHYLO_IDDI[PHYLO_IDDI$SPP==i,"IDDI"])}

# Add rows for the study species investigated in the Bahamas
PHYLO_DATA=rbind(PHYLO_DATA,data.frame(COMMON_NAME=c("Blainville's beaked whale","Cuvier's beaked whale",
                                                     "Melon-headed whale","Short-finned pilot whale",
                                                     "Sperm whale"),
                                       SPP_NAME=c("Mesoplodon densirostris","Ziphius cavirostris",
                                                  "Peponocephala electra","Globicephala macrorhynchus",
                                                  "Physeter macrocephalus"),
                                       T_MAX=NA,MASS=NA,Mb_MAX=NA,
                                       SPP=c("Md","Zc","Pe","Gm","Pm"),IDDI=NA))

# Calculate species level values of maximum dive duration (T_MAX), body mass (MASS), 
# inter-deep dive interval (IDDI), and myoglobin concentration (Mb_MAX) using the data 
# used to build linear and linear mixed effects models at the individual level
for(i in which(is.na(PHYLO_DATA$T_MAX)))
{PHYLO_DATA[i,"T_MAX"]=max(TAGS[TAGS$SPP==PHYLO_DATA$SPP[i],"DURATION_MAX"],na.rm=T)/60
PHYLO_DATA[i,"Mb_MAX"]=TAGS[TAGS$SPP==PHYLO_DATA$SPP[i],"MB"][1]
PHYLO_DATA[i,"IDDI"]=median(TAGS[TAGS$SPP==PHYLO_DATA$SPP[i],"IDDI_MED"],na.rm=T)/60
PHYLO_DATA[i,"MASS"]=median(MASS[MASS$SPP==PHYLO_DATA$SPP[i] & MASS$AGE=="A","MASS_MED"],na.rm=T)}

# Fix the calculation of P. macrocephalus mass to reflect the demographics represented in this study
PHYLO_DATA[PHYLO_DATA$SPP=="Pm","MASS"]=median(c(MASS[MASS$SPP=="Pm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],
                                                MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"]),na.rm=T)
# Fix the calculation of M. densirostris T_MAX to exclude an apparently erroneous (117min) dive record 
PHYLO_DATA[PHYLO_DATA$SPP=="Md","T_MAX"]=max(TAGS[TAGS$SPP=="Md" & TAGS$DURATION_MAX<7000,"DURATION_MAX"],na.rm=T)/60

# Split out GENUS and SPECIES into separate columns 
PHYLO_DATA[,c("GENUS","SPECIES")]=str_split_fixed(PHYLO_DATA$SPP_NAME, " ", 2)

# Assign each row unique name based on Ge_species format for linking with PHYLO_TREE tip names
rownames(PHYLO_DATA)=paste(substr(PHYLO_DATA$GENUS,1,2),PHYLO_DATA$SPECIES,sep="_")

############   PHYLO_TREE    ############

# Read a NEXUS format comprehensive phylogenetic tree of the crown cetacean lineage with 
# maximum likelihood branch length estimates from McGowen et al. 2009 
PHYLO_TREE=read.nexus("/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/McGowenetal2009_Cetacea.tre")

# Remove extraneous tips in phylogenetic tree so that length(PHYLO_TREE$tip.labels) equals nrow(PHYLO_DATA),
# necessary for calculation of correlation matrix subsequent phylogenetic generalized least squares (PGLS) analysis
PHYLO_TREE=drop.tip(PHYLO_TREE,tip=c("Su_scrofa", "Bo_taurus", "Hi_amphibius", "He_liberiensis", 
                                     "De_tropicalis", "De_delphis", "De_capensis", "Tu_truncatus", 
                                     "Tu_aduncus", "St_coeruleoalba", "St_clymene", "St_frontalis", 
                                     "St_longirostris", "La_hosei", "So_chinensis", "So_fluviatilis", 
                                     "So_guianensis", "St_bredanensis", "Fe_attenuata", "Gl_melas", 
                                     "Ps_crassidens", "Gr_griseus", "Or_brevirostris", "Li_peronii", 
                                     "Li_borealis", "La_obscurus", "La_obliquidens", "La_cruciger", 
                                     "La_australis", "Ce_heavisidii", "Ce_hectori", "Ce_eutropia", 
                                     "Ce_commersonii","La_albirostris", "La_acutus", "Ph_sinus", 
                                     "Ph_spinipinnis","Au_dioptrica", "Ph_dalli", "Ne_phocaenoides", 
                                     "In_geoffrensis","Po_blainvillei", "Li_vexillifer", "Me_layardii",
                                     "Me_carlhubbsi","Me_bowdoini", "Me_traversii", "Me_ginkgodens", 
                                     "Me_europaeus","Me_mirus", "Me_perrini", "Me_grayi", 
                                     "Me_peruvianus", "Me_stejnegeri","Me_hectori", "Me_bidens", 
                                     "In_pacificus", "Hy_planifrons", "Ta_shepherdi", "Be_bairdii", 
                                     "Be_arnuxii", "Pl_minor", "Pl_gangetica", "Ko_simus", 
                                     "Me_novaeangliae", "Ba_physalus", "Es_robustus", "Ba_musculus", 
                                     "Ba_borealis", "Ba_brydei", "Ba_edeni", "Ba_omurai", 
                                     "Ba_bonaerensis","Ba_acutorostrata", "Ca_marginata", 
                                     "Eu_australis", "Eu_glacialis", "Eu_japonica", "Ba_mysticetus"))
