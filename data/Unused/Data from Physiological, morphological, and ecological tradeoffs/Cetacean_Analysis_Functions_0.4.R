##### DATA IMPORT #####
# Function to import World Ocean Database (WOD) .csv outputs, and assign the relevant header field data to the tabular data
# and output as a dataframe without the header rows
WOD_IMPORT=function(DATA,TYPE)
{WOD=DATA
 WOD_index=which(WOD[,1]=="CAST                        ")
 for(i in 1:(length(WOD_index)-1))
 { #Extract important variables from the header of each cast
   WOD[WOD_index[i]:WOD_index[i+1]-1,"CAST"]=WOD[WOD_index[i],3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"CRUISE"]=WOD[WOD_index[i]+1,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"LAT"]=WOD[WOD_index[i]+4,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"LONG"]=WOD[WOD_index[i]+5,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"YEAR"]=WOD[WOD_index[i]+6,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"MONTH"]=WOD[WOD_index[i]+7,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"DAY"]=WOD[WOD_index[i]+8,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"TIME"]=WOD[WOD_index[i]+9,3]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"PLATFORM"]=WOD[WOD_index[i]+13,5]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"INSTITUTE"]=WOD[WOD_index[i]+14,5]
   WOD[WOD_index[i]:WOD_index[i+1]-1,"Z_BOTTOM"]=WOD[WOD_index[i]+16,3]
 }
 #Extract important variables from the header of the last cast (separated to avoid error)
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"CAST"]=WOD[WOD_index[length(WOD_index)],3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"CRUISE"]=WOD[WOD_index[length(WOD_index)]+1,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"LAT"]=WOD[WOD_index[length(WOD_index)]+4,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"LONG"]=WOD[WOD_index[length(WOD_index)]+5,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"YEAR"]=WOD[WOD_index[length(WOD_index)]+6,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"MONTH"]=WOD[WOD_index[length(WOD_index)]+7,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"DAY"]=WOD[WOD_index[length(WOD_index)]+8,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"TIME"]=WOD[WOD_index[length(WOD_index)]+9,3]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"PLATFORM"]=WOD[WOD_index[length(WOD_index)]+13,5]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"INSTITUTE"]=WOD[WOD_index[length(WOD_index)]+14,5]
 WOD[WOD_index[length(WOD_index)]:nrow(WOD),"Z_BOTTOM"]=WOD[WOD_index[length(WOD_index)]+16,3]
 
 WOD=WOD[!is.na(as.numeric(as.character(WOD[,1]))),]
 colnames(WOD)=c("SEQ_ID","DEPTH","DEPTH_FLAG","V4","TEMP","TEMP_FLAG","V7","SAL","SAL_FLAG",
                 "V10","OXY","OXY_FLAG","V13","CHL","CHL_FLAG","V16","V17","V18",
                 "CAST","CRUISE","LAT","LONG","YEAR","MONTH","DAY","TIME","PLATFORM","INSTITUTE","Z_BOTTOM")
 WOD=subset(WOD,select=c(SEQ_ID,DEPTH,DEPTH_FLAG,TEMP,TEMP_FLAG,SAL,SAL_FLAG,OXY,OXY_FLAG,CHL,
                         CHL_FLAG,CAST,CRUISE,LAT,LONG,YEAR,MONTH,DAY,TIME,PLATFORM,INSTITUTE,Z_BOTTOM))
 WOD$TYPE=TYPE
 return(WOD)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### DATA PREPARATION #####

# Function to assign SPP codes (e.g. Md, Gm) to tables with unique Ptt or PTT numbers drawing on the TAGS table 
SPP=function(DATA)
{SPP_CODES=data.frame(Species=c("Blainville's beaked whale","Short-finned pilot whale","Sperm whale",
                                "Cuvier's beaked whale","Melon-headed whale","Rough-toothed dolphin",
                                "Killer whale_b2","Humpback whale","Killer whale_B1","Killer whale_A",
                                "Killer whale_C","Antarctic minke whale","Antarctic minke whal"),
                      SPP=c("Md","Gm","Pm","Zc","Pe","Sb",
                            "KWG","Mn","KWB","KWA","KWC","Bb","Bb"),stringsAsFactors=F)
#Where Species is present in DATA match each Species with corresponding SPP code
if("Species"%in%colnames(DATA))
{for(i in SPP_CODES$Species)
{DATA[!is.na(DATA$Species) & DATA$Species==i,"SPP"]=SPP_CODES[SPP_CODES$Species==i,"SPP"]}}

#Where Species is absent in DATA assign SPP code based on Ptt or PTT code
else
{PTT=ifelse("Ptt"%in%colnames(DATA),"Ptt","PTT")
DATA=merge(DATA,TAGS[,c("PTT","SPP")],by.x=PTT,by.y="PTT",all.x=T)}

#output DATA
return(DATA)}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#standardize GMT  DATE_TIME stamp as a chron object for simple extraction of MONTHS, YEARS, J_DATE, and DAY_NIGHT
DATE_TIME=function(DATA,VAR,FORMAT)
{require(chron)
  DATA$DATE_TIME=as.chron(strptime(DATA[,VAR], format=FORMAT, tz = "UTC"))
  DATA$YEAR=as.numeric(as.character(years(DATA$DATE_TIME))) #extract YEAR as a number (rather than factor)
  DATA$MONTH=as.numeric(months(DATA$DATE_TIME)) #extract MONTH as a number
  DATA$DAY=as.numeric(days(DATA$DATE_TIME)) #extract DAY as a number
  DATA$HOUR=as.numeric(hours(DATA$DATE_TIME)) #extract HOUR as a number
  DATA$MIN=as.numeric(minutes(DATA$DATE_TIME)) #extract MIN as a number
  DATA$SEC=as.numeric(seconds(DATA$DATE_TIME)) #extract SEC as a number
  for(i in unique(DATA$YEAR)){DATA[DATA$YEAR==i,"J_DATE"]=julian(DATA[DATA$YEAR==i,"MONTH"],
                                                                 as.numeric(days(DATA[DATA$YEAR==i,"DATE_TIME"])),
                                                                 DATA[DATA$YEAR==i,"YEAR"],
                                                                 origin=c(12,31,(i-1)))} #Calculate Day of Year - doesn't work unless looped because origin only first value in data frame
  return(DATA)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Where spatial information is missing find ARGOS LAT and LONG with the closest DATE_TIME
LAT_LONG=function(DATA,LAT,LONG)
{DATA$LAT=LAT;DATA$LONG=LONG
DATA$LAT_LONG_FLAG=0
for(i in which(is.na(DATA$LAT)))
{ARGOS_temp=ARGOS[ARGOS$PTT==DATA[i,"Ptt"],]
DATA[i,c("LAT","LONG")]=ARGOS_temp[which.min(abs(ARGOS_temp$DATE_TIME-DATA[i,"DATE_TIME"])),c("LAT","LONG")]
DATA[i,c("LAT_LONG_FLAG")]=1}
return(DATA)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Predict the locations at specified time steps using a Continuous-Time Correlated Random Walk (CTCRW) model 
#based on Johnson et al. 2008 and crawl package
CTCRW=function(DATA,PRED_TIMES=NULL,TIME_VAR="DATE_TIME")
{require(sp); require(Rcpp); require(rgdal); require(mvtnorm);require(raster);
  require(splines); require(RcppArmadillo); require(crawl)
  
  # Reformat TIME_VAR to as.POSIXct with a timezone of "GMT"
  Sys.setenv(TZ="GMT")
  DATA[,TIME_VAR] = as.POSIXct(DATA[,TIME_VAR], tz = "GMT")
  
  # preserve original lat lon for comparison to fitted and back transformed mu.x mu.y once the model has run 
  DATA$LONG_0=DATA$LONG; DATA$LAT_0=DATA$LAT
  
  # Create SpatialPointsDataFrame LAT and LONG which can be projected to NEW_PROJ
  coordinates(DATA) = ~LONG+LAT # transform into SpatialPoints DATAframe for reprojection to PCS
  proj4string(DATA) = CRS("+proj=longlat +datum=WGS84")
  
  ## Create a custom Projected Coordinate system projection for movement modeling specific to the bounding dimensions of each track
  MED_LAT <- median(coordinates(DATA)[,2])
  MED_LONG <- median(coordinates(DATA)[,1])
  LAT_1<-bbox(DATA)[2,1]+diff(range(bbox(DATA)[2,]))/3
  LAT_2<-bbox(DATA)[2,2]-diff(range(bbox(DATA)[2,]))/3
  NEW_PROJ= paste("+proj=aea +lat_1=",LAT_1," +lat_2=",LAT_2," +lat_0=",MED_LAT,
                  " +lon_0=", MED_LONG, "+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m",sep="")
  DATA=spTransform(DATA, CRS(NEW_PROJ))
  
  # convert back to DATA.frame. Unnecessary for this anaysis, but useful for other models
  DATA=as(DATA, "data.frame")
  
  #Extract MAJOR axis, MINOR axis, and axis ORIENTATION and apply argosDiag2Cov to transform this 
  #location error into a covariance matrix that approximates the location error with a bivariate 
  #Gaussian distribution
  DIAG_DATA = model.matrix(~MAJOR+MINOR+ORIENTATION, DATA)[,-1]
  DATA = cbind(DATA, argosDiag2Cov(DIAG_DATA[,1], DIAG_DATA[,2], DIAG_DATA[,3]))
  
  #Define initial values for model based on starting location
  INIT = list(a=c(DATA$LONG[1],0,DATA$LAT[1],0),
              P=diag(c(5000^2,10^2, 5000^2, 10^2)))
  
  #Define fixed and free parameters (free param set to NA)
  #ln.sd.x and ln.sd.y: location error model parameters (n=2) these errors are known from Argos error ellipses so fixed at 1 
  #random error: sigma parameters 
  #autocorrelation: Beta para
  FIX_PAR = c(1,1,NA,NA)
  
  #Defind 
  THETA = c(log(5400),4)
  
  #Define priors for model    
  REG_PRIOR = function(PAR){-abs(PAR[2]-4)/1}
  
  #Fit CTCRW model    
  MODEL_FIT= crwMLE(mov.model=~1, 
                    err.model=list(x=~ln.sd.x-1, y=~ln.sd.y-1, rho=~error.corr), 
                    data=DATA, 
                    Time.name="DATE_TIME", 
                    initial.state=INIT, 
                    coord=c("LONG","LAT"),
                    fixPar = FIX_PAR, 
                    theta=THETA,
                    prior=REG_PRIOR,
                    control=list(REPORT=10, trace=1), 
                    initialSANN=list(maxit=1000), attempts=5)
  
  
  
  #Create a sequence of prediction times at regular hourly intervals over the duration of the ARGOS track (default)
  if(is.null(PRED_TIMES))
  {PRED_TIMES=as.POSIXlt(MODEL_FIT$data$DATE_TIME,tz="GMT")
  PRED_TIMES$min=0
  PRED_TIMES$sec=0
  PRED_TIMES=seq(min(PRED_TIMES)+3600, max(PRED_TIMES), 3600)}
  
  #Convert custom prediction times (from for example BEHAV) to POSIXlt in order to run CTCRW predict 
  if(!is.null(PRED_TIMES))
  {PRED_TIMES=as.POSIXlt(PRED_TIMES,tz="GMT")} 
  
  # Generate CTCRW predicted locations from MODEL_FIT  and PRED_TIMES defined above
  PRED_DATA = crwPredict(MODEL_FIT, predTime=PRED_TIMES)
  
  #print Root Mean Squared Error score 
  RMSE = sqrt(mean((PRED_DATA$LONG-PRED_DATA$predObs.x)^2 + (PRED_DATA$LAT-PRED_DATA$predObs.y)^2, na.rm=TRUE))
  print(paste("RMSE:",RMSE))
  
  #Convert custom prediction times back to POSIXct
  PRED_DATA$PRED_TIME = as.POSIXct(PRED_DATA$TimeNum, origin="1970-01-01", tz="GMT")
  
  # Back transform projection of CTCRW predicted locations (mu.x and mu.y) back to LAT_CRW and LONG_CRW
  PRED_DATA$mu.x_0=PRED_DATA$mu.x;PRED_DATA$mu.y_0=PRED_DATA$mu.y
  coordinates(PRED_DATA)=~mu.x+mu.y
  proj4string(PRED_DATA)=CRS(NEW_PROJ)
  PRED_DATA = spTransform(PRED_DATA,CRS("+proj=longlat +datum=WGS84"))
  PRED_DATA$LONG_CRW=coordinates(PRED_DATA)[,1]
  PRED_DATA$LAT_CRW=coordinates(PRED_DATA)[,2]
  PRED_DATA = as(PRED_DATA, "data.frame")
  
  #Output CTCRW predicted location  
  return(PRED_DATA)}

#Example: CTCRW(DATA=ARGOS[ARGOS$PTT==113561 & ARGOS$Major>0 & ARGOS$LAT<=-20,])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Take the regular predicted output from CTCRW and estimate the average location of PDT and TAT
LAT_LONG_CTCRW=function(DATA,PERIOD,DIST=F,DEPTH=F)
{if(length(PERIOD)==1){PERIOD=rep(PERIOD,nrow(DATA))}
 for(i in 1:nrow(DATA))
 {DATA[i,c("LAT_ARGOS")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
                                           & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
                                           & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
                                           & ARGOS_CTCRW$locType=="o","LAT_ARGOS"],collapse=",")
  DATA[i,c("LONG_ARGOS")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
                                            & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
                                            & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
                                            & ARGOS_CTCRW$locType=="o","LONG_ARGOS"],collapse=",")
  #DATA[i,c("LAT_CRW_OBS")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
  #                                           & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
  #                                           & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
  #                                           & ARGOS_CTCRW$LOCTYPE=="o","CRW_LAT"],collapse=",")
  #DATA[i,c("LONG_CRW_OBS")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
  #                                            & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
  #                                            & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
  #                                            & ARGOS_CTCRW$LOCTYPE=="o","CRW_LONG"],collapse=",")
  DATA[i,c("LAT_CRW_PRED")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
                                              & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
                                              & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
                                              & ARGOS_CTCRW$locType=="p","LAT_CRW"],collapse=",")
  DATA[i,c("LONG_CRW_PRED")]=paste(ARGOS_CTCRW[ARGOS_CTCRW$PTT==DATA[i,"Ptt"] 
                                               & ARGOS_CTCRW$DATE_TIME>=DATA[i,"DATE_TIME"] 
                                               & ARGOS_CTCRW$DATE_TIME<=DATA[i,"DATE_TIME"]+(PERIOD[i]/24)
                                               & ARGOS_CTCRW$locType=="p","LONG_CRW"],collapse=",") 
  DATA$LAT_AVG[i]=mean(as.numeric(unlist(strsplit(DATA$LAT_CRW_PRED[i],","))),na.rm=T)
  DATA$LONG_AVG[i]=mean(as.numeric(unlist(strsplit(DATA$LONG_CRW_PRED[i],","))),na.rm=T)
  if(DIST==T)
  {require(Imap)
   DATA$DIST_PATH[i]=gdist.total(longlat=data.frame(LONG=as.numeric(unlist(strsplit(DATA$LONG_CRW_PRED[i],","))),
                                                    LAT=as.numeric(unlist(strsplit(DATA$LAT_CRW_PRED[i],",")))),
                                 units="km",segments=F)}
  if(DEPTH==T)
  {require(raster)
   DATA$BATHY_DEPTH[i]=paste(extract(BATHY,data.frame(LONG=as.numeric(unlist(strsplit(DATA$LONG_CRW_PRED[i],","))),
                                                      LAT=as.numeric(unlist(strsplit(DATA$LAT_CRW_PRED[i],","))))),
                             collapse=",")}
 }
 return(DATA)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REGION=function(DATA)
{DATA[DATA$LAT>26.30 & DATA$LAT<=26.75 & DATA$LONG>-79.00 & DATA$LONG<=-77.55, "REGION"]="NWPN" #NW Providence Channel-North of Ridge
 DATA[DATA$LAT>25.55 & DATA$LAT<=26.30 & DATA$LONG>-79.00 & DATA$LONG<=-77.55, "REGION"]="NWPS" #NW Providence Channel-South of Ridge
 DATA[DATA$LAT>25.10 & DATA$LAT<=26.65 & DATA$LONG>-77.55 & DATA$LONG<=-76.35, "REGION"]="SEPC" #SE Providence Channel
 DATA[DATA$LAT>26.65 & DATA$LAT<=28.00 & DATA$LONG>-79.00 & DATA$LONG<=-76.60, "REGION"]="NABA" #N Abaco Shelf
 DATA[DATA$LAT>25.10 & DATA$LAT<=25.55 & DATA$LONG>-78.25 & DATA$LONG<=-77.55, "REGION"]="TOTO" #Tongue of the Ocean
 DATA[DATA$LAT>24.00 & DATA$LAT<=25.10 & DATA$LONG>-78.25 & DATA$LONG<=-77.00, "REGION"]="TOTO" #Tongue of the Ocean
 DATA[DATA$LAT>23.25 & DATA$LAT<=24.10 & DATA$LONG>-78.00 & DATA$LONG<=-76.40, "REGION"]="TOTO" #Tongue of the Ocean
 DATA[DATA$LAT>24.00 & DATA$LAT<=28.00 & DATA$LONG>-80.50 & DATA$LONG<=-79.00, "REGION"]="FLST" #Florida Straight
 DATA[is.na(DATA$REGION),"REGION"]="OTBB" #Outside bounding boxes
 return(DATA)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Day Night classification based on start and end time and local sunrise and sunset times (which are estimated from date and location)
LIGHT_DARK=function(DATA, START_END=T,TIME_VAR="DATE_TIME")
{require(maptools);require(chron)
  Sys.setenv(TZ="GMT")
  DATA$SUNRISE=as.chron(sunriset(as.matrix(cbind(DATA$LONG,DATA$LAT)), as.POSIXct(DATA[,TIME_VAR],tz="GMT"), direction="sunrise", POSIXct.out=TRUE)$time)
  #restore sunset to chron times object
  DATA$SUNSET=as.chron(sunriset(as.matrix(cbind(DATA$LONG,DATA$LAT)), as.POSIXct(DATA[,TIME_VAR],tz="GMT"), direction="sunset", POSIXct.out=TRUE)$time)
  DATA$SUN_ELEV=solarpos(as.matrix(cbind(DATA$LONG,DATA$LAT)), as.POSIXct(DATA[,TIME_VAR],tz="GMT"), direction="sunset", POSIXct.out=TRUE)[,2]
  DATA$SUN_AZI=solarpos(as.matrix(cbind(DATA$LONG,DATA$LAT)), as.POSIXct(DATA[,TIME_VAR],tz="GMT"), direction="sunset", POSIXct.out=TRUE)[,1]
  
  #source the oce package with contains moonAngle astronomical calculator and set the system time zone to UTC
  require(oce)
  Sys.setenv(TZ="UTC")
  DATA[,TIME_VAR]=as.POSIXct(as.chron(DATA[,TIME_VAR]))
  DATA$MOON_ALT=moonAngle(t=as.POSIXct(as.character(DATA[,TIME_VAR]),tz="UTC"),
                          longitude=DATA$LONG,latitude=DATA$LAT)$altitude
  DATA$MOON_ILLUM=moonAngle(t=as.POSIXct(as.character(DATA[,TIME_VAR]),tz="UTC"),
                            longitude=DATA$LONG,latitude=DATA$LAT)$illuminatedFraction
  DATA[,TIME_VAR]=as.chron(DATA[,TIME_VAR])
  
  #For types of DATA that contain START_TIMEs, END_TIMEs, and DURATIONS calculate the percent of time 
  #that each observation w/LAT and LONG is illuminated by the SUN and the MOON
  if(START_END==T)
  { #SUN CALCULATIONS
    DATA$START_TIME=as.chron(DATA$START_TIME)
    DATA$END_TIME=as.chron(DATA$END_TIME)
    #Extract light period before sunset DURATION-(END_TIME-SUNSET)
    DATA$LIGHT=ifelse(DATA$SUNSET-DATA$START_TIME>0 & DATA$SUNSET-DATA$START_TIME< DATA$END_TIME-DATA$START_TIME,
                      DATA$DURATION-(DATA$END_TIME-DATA$SUNSET)*24,DATA$DURATION)
    #Light period before sunset (SUNSET-START_TIME)
    DATA$LIGHT=ifelse(DATA$SUNRISE-DATA$START_TIME>0 & DATA$SUNRISE-DATA$START_TIME< DATA$END_TIME-DATA$START_TIME,
                      DATA$LIGHT-(DATA$SUNRISE-DATA$START_TIME)*24,DATA$LIGHT)
    DATA$LIGHT=ifelse(DATA$START_TIME>=DATA$SUNSET & DATA$END_TIME<(DATA$SUNRISE+1),0,DATA$LIGHT)
    DATA$LIGHT=ifelse(DATA$START_TIME<DATA$SUNRISE & DATA$END_TIME<=DATA$SUNRISE,0,DATA$LIGHT)
    #Calculate the absolute amount of time during each DATA that falls between sunset and sunrise (DARK)
    DATA$DARK=DATA$DURATION-DATA$LIGHT
    #Express these as a proportion of the total duration of each DATA
    DATA$DARK_RATIO=DATA$DARK/DATA$DURATION
    DATA$LIGHT_RATIO=DATA$LIGHT/DATA$DURATION
    
    #MOON CALCULATIONS
    
    #convert START and END_TIMES to POSIXct format for moonAngle
    DATA$START_TIME=as.POSIXct(as.chron(DATA$START_TIME))
    DATA$END_TIME=as.POSIXct(as.chron(DATA$END_TIME))
    #calculate the Altitude above the horizon in degrees for each START and END TIME and LAT LONG coordinate pair
    #in DATA (negative values mean that the moon is below the horizon)
    DATA$START_MOON_ALT=moonAngle(t=as.POSIXct(as.character(DATA$START_TIME),tz="UTC"),
                                  longitude=DATA$LONG,latitude=DATA$LAT)$altitude
    DATA$END_MOON_ALT=moonAngle(t=as.POSIXct(as.character(DATA$END_TIME),tz="UTC"),
                                longitude=DATA$LONG,latitude=DATA$LAT)$altitude
    
    #for the rows in data where START_MOON_ALT and END_MOON_ALT are of different (a moonrise or set between 
    #START and END TIMEs) select the MOON_RISESET time when the transition occurs to the nearest second
    for(i in which(sign(DATA$START_MOON_ALT)!=sign(DATA$END_MOON_ALT)))
    {DATA_temp=moonAngle(t=as.POSIXct(DATA$START_TIME[i]:DATA$END_TIME[i],tz="UTC",origin="1970-01-01 00:00:00"),
                         longitude=DATA$LONG[i],latitude=DATA$LAT[i])$altitude
    DATA$MOON_RISESET[i]=as.POSIXct((DATA$START_TIME[i]:DATA$END_TIME[i])[which(sign(DATA_temp)!=sign(DATA_temp[c(2:length(DATA_temp),length(DATA_temp))]))],
                                    tz="UTC",origin="1970-01-01 00:00:00")
    remove(DATA_temp)
    }
    
    #for each rows in data calculate a mean Altitude above the horizon (MOON_MEAN_ALT) and 
    #the mean fraction of the lunar disk that is illuminated (MOON_MEAN_ILLUM)
    for(i in 1:nrow(DATA))
    {DATA$MOON_MEAN_ALT[i]=mean(moonAngle(t=as.POSIXct(DATA$START_TIME[i]:DATA$END_TIME[i],tz="UTC",origin="1970-01-01 00:00:00"),
                                          longitude=DATA$LONG[i],latitude=DATA$LAT[i])$altitude)
    DATA$MOON_MEAN_ILLUM[i]=mean(moonAngle(t=as.POSIXct(DATA$START_TIME[i]:DATA$END_TIME[i],tz="UTC",origin="1970-01-01 00:00:00"),
                                           longitude=DATA$LONG[i],latitude=DATA$LAT[i])$illuminatedFraction)
    }
    
    #convert START, END, and MOON_RISESET times back to chron format for calculations
    DATA$MOON_RISESET=as.chron(as.POSIXct(DATA$MOON_RISESET,tz="UTC",origin="1970-01-01 00:00:00"))
    DATA$START_TIME=as.chron(DATA$START_TIME)
    DATA$END_TIME=as.chron(DATA$END_TIME)
    
    #set of rules to calculate exact amount of time that the moon is above the horizon between each START and END TIME
    DATA$MOON_LIGHT=ifelse(sign(DATA$START_MOON_ALT)==1 & sign(DATA$END_MOON_ALT)==1,DATA$DURATION,NA) # 100% up
    DATA$MOON_LIGHT=ifelse(sign(DATA$START_MOON_ALT)==-1 & sign(DATA$END_MOON_ALT)==-1,0,DATA$MOON_LIGHT) # 0% up
    DATA$MOON_LIGHT=ifelse(sign(DATA$START_MOON_ALT)==-1 & sign(DATA$END_MOON_ALT)==1,
                           DATA$DURATION-abs(DATA$START_TIME-DATA$MOON_RISESET)*24,DATA$MOON_LIGHT) #moon rose between START and END TIME
    DATA$MOON_LIGHT=ifelse(sign(DATA$START_MOON_ALT)==1 & sign(DATA$END_MOON_ALT)==-1,
                           DATA$DURATION-abs(DATA$END_TIME-DATA$MOON_RISESET)*24,DATA$MOON_LIGHT) #moon set between START and END TIME
    DATA$MOON_DARK=DATA$DURATION-DATA$MOON_LIGHT 
  }
  return(DATA)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculate Geodesic proximity (WGS1984 ellipsoid) by taking the minimum distance from the vertices of a line or polygon  
NEAR=function(DATA,ID,NEAR_OBJ)
{require(rgdal);require(raster);require(rgeos);require(Imap)
 if(class(NEAR_OBJ)[[1]]=="SpatialLinesDataFrame"|class(NEAR_OBJ)[[1]]=="SpatialLines")
 {NEAR_OBJ=as.data.frame(coordinates(NEAR_OBJ)[[1]][[1]])}
 if(class(NEAR_OBJ)[[1]]=="SpatialPolygonsDataFrame"|class(NEAR_OBJ)[[1]]=="SpatialPolygons")
 {NEAR_OBJ=as.data.frame(coordinates(as(NEAR_OBJ,"SpatialLines"))[[1]][[1]])}
 NEAR_LONG=matrix(rep(NEAR_OBJ$x,nrow(DATA)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=T)
 NEAR_LAT=matrix(rep(NEAR_OBJ$y,nrow(DATA)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=T)
 DATA_LONG=matrix(rep(DATA[,"LONG"],nrow(NEAR_OBJ)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=F)
 DATA_LAT=matrix(rep(DATA[,"LAT"],nrow(NEAR_OBJ)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=F)
 NEAR_DIST=gdist(NEAR_LONG,NEAR_LAT,DATA_LONG,DATA_LAT,units="km")
 return(data.frame(DATA,
                   NEAR_DIST=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_DIST[i,which.min(NEAR_DIST[i,])]}),
                   NEAR_LONG=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_LONG[i,which.min(NEAR_DIST[i,])]}),
                   NEAR_LAT=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_LAT[i,which.min(NEAR_DIST[i,])]})))}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CTD_INTERP=function(TEMPERATURE=NA,DEPTH=NA,OXY=F,N=length(unique(CTD[CTD$TYPE%in%c("PFL","CTD","PDT") & CTD$YEAR>=1990 & 
                                                                        CTD$TEMP_FLAG==0 & !CTD$REGION%in%c("OTBB","NABA","FLST"),"CAST"])))
{#1. Create CTD_STND to standardize calculation the depths of isotherms 4-24C
  CTD_STND=as.data.frame(matrix(nrow=0,ncol=16))
  colnames(CTD_STND)=c("CAST","TYPE","SPP","DEPTH","TEMP","LAT","LONG","REGION","DATE_TIME","YEAR","MONTH",
                       "J_DATE","DAY_NIGHT","LAT_LONG_0.1","LAT_LONG_0.5","LAT_LONG_1.0")
  
  #2. Loop to linearly interpolate isotherm DEPTH onto standard TEMPERATURE levels  
  if(!is.na(TEMPERATURE[1])){
    for(i in unique(CTD[CTD$TYPE%in%c("PFL","CTD","PDT") & CTD$YEAR>=1990 & CTD$TEMP_FLAG==0 & 
                          !CTD$REGION%in%c("OTBB","NABA","FLST"),"CAST"])[1:N])
    {CTD_temp=CTD[CTD$CAST==i & CTD$TEMP_FLAG==0,]
     CTD_temp=CTD_temp[rev(order(CTD_temp$TEMP)),]
     if(max(CTD_temp$TEMP)<min(TEMPERATURE)) next
     if(min(CTD_temp$TEMP)>max(TEMPERATURE)) next
     for(j in TEMPERATURE)
     {#linear interpolation of depth on temperature levels corresponding to TAT bin boundaries
       CTD_STND_temp=try(data.frame(CTD_temp[1,c("CAST","TYPE","SPP","LAT","LONG","REGION","DATE_TIME","YEAR","MONTH",
                                                 "J_DATE","DAY_NIGHT","LAT_LONG_0.1","LAT_LONG_0.5","LAT_LONG_1.0")],
                                    TEMP=j,
                                    DEPTH=CTD_temp[(which(CTD_temp[,"TEMP"]<=j)[1])-1,"DEPTH"]+
                                      abs(CTD_temp[(which(CTD_temp[,"TEMP"]<=j)[1])-1,"TEMP"]-j)/
                                      abs(CTD_temp[(which(CTD_temp[,"TEMP"]<=j)[1])-1,c("TEMP")]-CTD_temp[which(CTD_temp[,"TEMP"]<=j)[1],c("TEMP")])*
                                      abs(CTD_temp[(which(CTD_temp[,"TEMP"]<=j)[1])-1,c("DEPTH")]-CTD_temp[which(CTD_temp[,"TEMP"]<=j)[1],c("DEPTH")])))
       if(class(CTD_STND_temp)=="try-error") {print(paste(CTD_temp$CAST[1],j));next}
       CTD_STND_temp=subset(CTD_STND_temp,select=c(CAST,TYPE,SPP,DEPTH,TEMP,LAT,LONG,REGION,DATE_TIME,YEAR,MONTH,J_DATE,
                                                   DAY_NIGHT,LAT_LONG_0.1,LAT_LONG_0.5,LAT_LONG_1.0))
       CTD_STND=rbind(CTD_STND,CTD_STND_temp)
       remove(CTD_STND_temp)
     }
     remove(CTD_temp)
    }
    return(CTD_STND)
  }
  
  #3. Loop to linearly interpolate OXY onto specified DEPTH levels
  if(OXY==T){
    CTD_STND=as.data.frame(matrix(nrow=0,ncol=16))
    colnames(CTD_STND)=c("CAST","TYPE","SPP","DEPTH","OXY","LAT","LONG","REGION","DATE_TIME","YEAR","MONTH",
                         "J_DATE","DAY_NIGHT","LAT_LONG_0.1","LAT_LONG_0.5","LAT_LONG_1.0")
    
    for(i in unique(CTD[CTD$TYPE%in%c("PFL","CTD","PDT") & CTD$YEAR>=1990 & CTD$TEMP_FLAG==0 & 
                          !CTD$REGION%in%c("OTBB","NABA","FLST") & !is.na(CTD$OXY),"CAST"])[1:N])
    {CTD_temp=CTD[CTD$CAST==i & !is.na(CTD$OXY),]
     CTD_temp=CTD_temp[rev(order(CTD_temp$TEMP)),]
     if(max(CTD_temp$DEPTH)<min(DEPTH)) next
     if(min(CTD_temp$DEPTH)>max(DEPTH)) next
     for(j in DEPTH)
     {#linear interpolation of temperature onto DEPTH input(s)
       CTD_STND_temp=try(data.frame(CTD_temp[1,c("CAST","TYPE","SPP","LAT","LONG","REGION","DATE_TIME","YEAR","MONTH",
                                                 "J_DATE","DAY_NIGHT","LAT_LONG_0.1","LAT_LONG_0.5","LAT_LONG_1.0")],
                                    DEPTH=j,
                                    OXY=CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,"OXY"]+
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,c("OXY")]-CTD_temp[which(CTD_temp[,"DEPTH"]>=j)[1],c("OXY")])*
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,"DEPTH"]-j)/
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,c("DEPTH")]-CTD_temp[which(CTD_temp[,"DEPTH"]>=j)[1],c("DEPTH")])))
       if(class(CTD_STND_temp)=="try-error") {print(paste(CTD_temp$CAST[1],j));next}
       CTD_STND_temp=subset(CTD_STND_temp,select=c(CAST,TYPE,SPP,DEPTH,OXY,LAT,LONG,REGION,DATE_TIME,YEAR,MONTH,J_DATE,
                                                   DAY_NIGHT,LAT_LONG_0.1,LAT_LONG_0.5,LAT_LONG_1.0))
       CTD_STND=rbind(CTD_STND,CTD_STND_temp)
       remove(CTD_STND_temp)
     }
     remove(CTD_temp)
    }
    return(CTD_STND)
  }
  
  
  #4. Loop to linearly interpolate isotherm TEMP onto standard DEPTH levels
  if(!is.na(DEPTH[1])){
    for(i in unique(CTD[CTD$TYPE%in%c("PFL","CTD","PDT") & CTD$YEAR>=1990 & CTD$TEMP_FLAG==0 & 
                          !CTD$REGION%in%c("OTBB","NABA","FLST"),"CAST"])[1:N])
    {CTD_temp=CTD[CTD$CAST==i & CTD$TEMP_FLAG==0,]
     CTD_temp=CTD_temp[rev(order(CTD_temp$TEMP)),]
     if(max(CTD_temp$DEPTH)<min(DEPTH)) next
     if(min(CTD_temp$DEPTH)>max(DEPTH)) next
     for(j in DEPTH)
     {#linear interpolation of temperature onto DEPTH input(s)
       CTD_STND_temp=try(data.frame(CTD_temp[1,c("CAST","TYPE","SPP","LAT","LONG","REGION","DATE_TIME","YEAR","MONTH",
                                                 "J_DATE","DAY_NIGHT","LAT_LONG_0.1","LAT_LONG_0.5","LAT_LONG_1.0")],
                                    DEPTH=j,
                                    TEMP=CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,"TEMP"]+
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,c("TEMP")]-CTD_temp[which(CTD_temp[,"DEPTH"]>=j)[1],c("TEMP")])*
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,"DEPTH"]-j)/
                                      abs(CTD_temp[(which(CTD_temp[,"DEPTH"]>=j)[1])-1,c("DEPTH")]-CTD_temp[which(CTD_temp[,"DEPTH"]>=j)[1],c("DEPTH")])))
       if(class(CTD_STND_temp)=="try-error") {print(paste(CTD_temp$CAST[1],j));next}
       CTD_STND_temp=subset(CTD_STND_temp,select=c(CAST,TYPE,SPP,DEPTH,TEMP,LAT,LONG,REGION,DATE_TIME,YEAR,MONTH,J_DATE,
                                                   DAY_NIGHT,LAT_LONG_0.1,LAT_LONG_0.5,LAT_LONG_1.0))
       CTD_STND=rbind(CTD_STND,CTD_STND_temp)
       remove(CTD_STND_temp)
     }
     remove(CTD_temp)
    }
    return(CTD_STND)
  }
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##### DATA ANALYSIS #####

#### Function to calculate the marginal R-squared of an lme model object based on the code example of Nakagawa and Schielzeth (2013)
#### Marginal R-squared defines the variance explained by the fixed-effects only component of an lme model

R_SQUARED_M=function(MODEL=NULL,DATA=NULL,N_RANDOM=NULL){
  
  if(class(MODEL)=="gls"){
    LM_MODEL=lm(formula=as.character(LME_DURATION_MAX_MASS_1$call)[2],
                data=eval(parse(text=as.character(LME_DURATION_MAX_MASS_1$call)[3])))
    return(summary(LM_MODEL)$adj.r.squared)}
  
  if(class(MODEL)=="lme"){
    VarF=var(as.vector(fixef(MODEL) %*% t(model.matrix(MODEL,data=DATA))))
    
    VarR=vector()
    
    for(i in 1:N_RANDOM){
      VarR[i]=as.numeric(VarCorr(MODEL)[2*i,1])
    }
    
    VarRes=as.numeric(VarCorr(MODEL)[nrow(VarCorr(MODEL)),1])
    
    return(VarF/sum(c(VarF,VarR,VarRes)))
  }
  
  if(class(MODEL)=="MCMCglmm"){
    
    mVarF <- var(as.vector(apply(MODEL$Sol,2,mean)[1:MODEL$Fixed$nfl] %*% t(MODEL$X)))
    
    return(mVarF/(mVarF+sum(apply(MODEL$VCV,2,mean))))
    
  }
}

#### Function to calculate the conditional R-squared of an lme model object based on the code example of Nakagawa and Schielzeth (2013)
#### Conditional R-squared defines the "variance explained by the entire model" including random effects terms

R_SQUARED_C=function(MODEL=NULL,DATA=NULL,N_RANDOM=NULL){
  
  if(class(MODEL)=="gls"){return(NA)}
  
  if(class(MODEL)=="lme"){
    VarF=var(as.vector(fixef(MODEL) %*% t(model.matrix(MODEL,data=DATA))))
    
    VarR=vector()
    for(i in 1:N_RANDOM){
      VarR[i]=as.numeric(VarCorr(MODEL)[2*i,1])
    }
    
    VarRes=as.numeric(VarCorr(MODEL)[nrow(VarCorr(MODEL)),1])
    
    return(sum(VarF,VarR)/sum(c(VarF,VarR,VarRes)))
  }
  if(class(MODEL)=="MCMCglmm"){
    
    mVarF <- var(as.vector(apply(MODEL$Sol,2,mean)[1:MODEL$Fixed$nfl] %*% t(MODEL$X)))
    

    if(ncol(MODEL$VCV)>1)  
    {return((mVarF+sum(apply(MODEL$VCV,2,mean)[-ncol(MODEL$VCV)]))/(mVarF+sum(apply(MODEL$VCV,2,mean))))}
    
    if(ncol(MODEL$VCV)==1)
    {return(NA)}
  }
}

#### GAM and GLM models of depth as a function spatial and temporal covariates
GAM_ISO_MOD=function(ISO)
{require(mgcv)
 GAM=gam(formula=DEPTH~s(LAT,bs="ts",k=3)+s(LONG,bs="ts",k=3)+s(DIST_CHAN,bs="ts",k=3)+s(DIST_FLST,bs="ts",k=3)+
           s(J_DATE,bs="ts",k=3)+s(LAT,LONG,bs="ts",k=30)+s(LONG,DIST_CHAN,bs="ts",k=30)+
           s(LAT,DIST_CHAN,bs="ts",k=30) + s(DIST_FLST,DIST_CHAN,bs="ts",k=30) + s(LONG,DIST_FLST,bs="ts",k=30)+
           s(LAT,DIST_FLST,bs="ts",k=30),
         family=gaussian(),data=CTD_STND[CTD_STND$TEMP==ISO ,])
 return(GAM)}

GAM_ISO_MOD_SIMP=function(ISO)
{require(mgcv)
 GAM=gam(formula=DEPTH~s(LAT,bs="ts",k=3)+s(LONG,bs="ts",k=3)+s(DIST_CHAN,bs="ts",k=3)+s(LAT,LONG,bs="ts",k=30),
         family=gaussian(),data=CTD_STND[CTD_STND$TEMP==ISO ,])
 return(GAM)}

GLM_ISO_MOD=function(ISO)
{require(MASS)
 GLM=glm(formula="DEPTH~LAT+LONG+DIST_CHAN+DIST_FLST+J_DATE+
         poly(LAT,2)+poly(LONG,2)+poly(DIST_CHAN,2)+poly(DIST_FLST,2)+poly(J_DATE,2)+
         (LAT+LONG+DIST_CHAN+DIST_FLST+J_DATE)^2",
         family=gaussian(),
         data=CTD_STND[CTD_STND$TEMP==ISO ,])
 GLM=stepAIC(GLM)
 return(GLM)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Create an empirical variogram for geographically explicit data and estimate a variogram model 
VARIOG=function(DATA,RESP,FIT=F,MAIN,SILL,RANGE,NUGGET,MOD)
{require(gstat)
 coordinates(DATA)= ~LONG+LAT
 DATA_VG=variogram(eval(parse(text=paste(RESP,"~1"))),DATA)
 if(FIT==T)
 {DATA_VGM=fit.variogram(DATA_VG,mod=vgm(psill=SILL,MOD,range=RANGE,nugget=NUGGET))
  print(DATA_VGM)
  LAT_RANGE=DATA_VGM$range*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                 floor(mean(DATA$LONG)),ceiling(mean(DATA$LAT)),units="km")
  LONG_RANGE=DATA_VGM$range*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                  ceiling(mean(DATA$LONG)),floor(mean(DATA$LAT)),units="km")
  print(LAT_RANGE)
  print(LONG_RANGE)
  DATA_VGM_LINE=variogramLine(DATA_VGM,maxdist=RANGE)}
 
 DATA_VG$DIST_LAT=DATA_VG$dist*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                     floor(mean(DATA$LONG)),ceiling(mean(DATA$LAT)),units="km")
 DATA_VG$DIST_LONG=DATA_VG$dist*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                      ceiling(mean(DATA$LONG)),floor(mean(DATA$LAT)),units="km")
 if(FIT==T)
 {DATA_VGM_LINE$DIST_LAT=DATA_VGM_LINE$dist*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                                  floor(mean(DATA$LONG)),ceiling(mean(DATA$LAT)),units="km")
  DATA_VGM_LINE$DIST_LONG=DATA_VGM_LINE$dist*gdist(floor(mean(DATA$LONG)),floor(mean(DATA$LAT)),
                                                   ceiling(mean(DATA$LONG)),floor(mean(DATA$LAT)),units="km")}
 par(mfrow=c(1,2))
 plot(DATA_VG$DIST_LAT,DATA_VG$gamma,main=paste(MAIN,"LAT",sep=":"),xlab="DIST_LAT",ylab="GAMMA")
 if(FIT==T){lines(DATA_VGM_LINE$DIST_LAT,DATA_VGM_LINE$gamma)
            text(min(DATA_VGM_LINE$DIST_LONG)+20,min(DATA_VGM_LINE$gamma)+100, paste("LAT_RANGE:",round(LAT_RANGE[2],1)))}
 
 plot(DATA_VG$DIST_LONG,DATA_VG$gamma,main=paste(MAIN,"LONG",sep=":"),xlab="DIST_LONG",ylab="GAMMA")
 if(FIT==T){lines(DATA_VGM_LINE$DIST_LONG,DATA_VGM_LINE$gamma)
            text(min(DATA_VGM_LINE$DIST_LONG)+20,min(DATA_VGM_LINE$gamma)+100, paste("LONG_RANGE:",round(LONG_RANGE[2],1)))
            return(data.frame(LAT_RANGE=LAT_RANGE[2],LONG_RANGE=LONG_RANGE[2]))}
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# OPTIM_INTERP - Function to optimally interpolate an oceanographic field from point estimates, 
# based on decorrelation length scales in x and y dimensions (Lx and Ly). This R code is adapt with some modifications 
# from the MatLab script optimalInterp.m (http://www.oceandrivers.com/repository/entry/show/Repository/
# Tutorials+and+Scripts+/Matlab+examples/Interpolation+tools/Objective+Analysis/optimalInterp.m?
# entryid=fe256516-cf35-422b-87c7-418428c932bc) by Bartolome Garau (Email: tgarau@oceandrivers.com, 15-Sep-2012)

#  xObs,yObs,zObs - zObs are oceanographic observations at point locations defined by xObs and yObs
#  xGrid, yGrid - vectors of x and y coordinates at which interpolated values
#  Lx, Ly - decorrelation length scales in x and y dimensions (used in defining gaussian decay funtion)
#  obsNoise - signal-to-noise ratio
#  zGrid, zError - interpolated values and standard error of field estimate
# library(matlab);library(raster)
# Example: Generate an arbitrary 2D scalar field
# x = meshgrid(seq(0,1,length.out=100),seq(0,1,length.out=100))$x
# y = meshgrid(seq(0,1,length.out=100),seq(0,1,length.out=100))$y;
# z = (x^2 + 2* y^2 + 3) * cos(x*y);
# z=raster(z, xmn=0, xmx=1, ymn=0, ymx=1)
# xGrid=as.vector(x)
# yGrid=as.vector(y)
#Get observations at random locations 
# xObs = runif(200)
# yObs = runif(200)
# zObs = extract(z, y=cbind(xObs,yObs), method='bilinear')
#Guesses at length and signal to noise ratio ()
# Lx=0.05
# Ly=0.05
# obsNoise=0.05
#test=OPTIM_INTERP(LAT=yObs, LONG=xObs, VAR=zObs, LM="zObs~xObs+yObs+xObs^2+yObs^2", LAT_MIN=0, LAT_MAX=1,LONG_MIN=0, LONG_MAX=1, CELL_SIZE=0.01, Lx=0.05*gdist(1,1,1,0,"km"), Ly=0.05*gdist(1,1,1,0,"km"), obsNoise=0.05)

OPTIM_INTERP=function(DATA, RESP, MOD, RESID=F, PRED, Lx, Ly, obsNoise)
{require(matlab);require(Imap)
 #Assign variables
 xObs=DATA$LONG 
 yObs=DATA$LAT
 zObs=DATA[,RESP]
 xGrid=PRED$LONG
 yGrid=PRED$LAT
 xDim=nrow(PRED)
 yDim=nrow(PRED)
 
 if(RESID==F)
 {# Detrend zObs by estimating the mean field using the linear or additive model specified in MOD
   lhSide = eval(parse(text=MOD))
   # Use the residuals of this model as response variable in objective analysis
   zObsNew = zObs - predict(lhSide)
   # Calculate the signal to noise ratio
   obsNoise=var(zObsNew)/var(zObs)}
 
 if(RESID==T)
 {zObsNew = zObs}
 
 
 # Length of data to be interpolated
 numObsPts = length(xObs)
 
 # Gaussian decorrelation as a function of distance scales Lx and Ly
 gaussDenomX = 2 * Lx^2
 gaussDenomY = 2 * Ly^2
 
 # Proportion of variability unexplained by linear or quadratic model  
 gamma = obsNoise/var(zObsNew)
 
 # Estimate of the covariance matrix between observation points based on a Gaussian decay of 
 # correlation as a function of distance
 
 # Original computation of distance matrices between observation points
 #dx   = xObs %*% ones(1, numObsPts) - ones(numObsPts, 1) %*% t(xObs)
 #dy   = yObs %*% ones(1, numObsPts) - ones(numObsPts, 1) %*% t(yObs)
 
 #Modified computation of distance matrices using geodesic distances (km) based on WGS84 ellipsoid and a 
 #pythagorean (i.e. planar) approximation of the longitudinal distance
 dx = sqrt(gdist(xObs %*% matrix(1,1,numObsPts), 
                 yObs %*% matrix(1,1,numObsPts),
                 matrix(1,numObsPts, 1) %*% xObs, 
                 matrix(1,numObsPts, 1) %*% yObs,
                 units="km")^2-
             gdist(xObs %*% matrix(1,1,numObsPts), 
                   yObs %*% matrix(1,1,numObsPts),
                   xObs %*% matrix(1,1,numObsPts), 
                   matrix(1,numObsPts, 1) %*% yObs,
                   units="km")^2)
 dy= gdist(xObs %*% matrix(1,1,numObsPts), 
           yObs %*% matrix(1,1,numObsPts),
           xObs %*% matrix(1,1,numObsPts), 
           matrix(1,numObsPts, 1) %*% yObs,
           units="km")
 expo = (dx^2)/ gaussDenomX + (dy^2)/ gaussDenomY
 CovObservations = exp(-expo) + gamma * diag(nrow=numObsPts)
 
 zGrid = zeros(size(xGrid))
 zError = zeros(size(xGrid))
 
 numObs = length(xObs);
 numGrd = length(xGrid);
 
 # Estimate of the covariance matrix between observation and grid points based 
 # on a Gaussian decay of correlation as a function of distance
 
 # Original computation of distance matrices between observation and grid points
 #deltax   = xObs %*% ones(1, numGrd) - ones(numObs, 1) %*% xGrid
 #deltay   = yObs %*% ones(1, numGrd) - ones(numObs, 1) %*% yGrid;
 
 #Modified computation of distance matrices using geodesic distances based on WGS84 ellipsoid and a 
 #pythagorean (i.e. planar) approximation of the longitudinal distance
 deltax = sqrt(gdist(xObs %*% matrix(1,1,numGrd), 
                     yObs %*% matrix(1,1,numGrd),
                     matrix(1,numObs, 1) %*% as.vector(xGrid), 
                     matrix(1,numObs, 1) %*% as.vector(yGrid),
                     units="km")^2-
                 gdist(xObs %*% matrix(1,1,numGrd), 
                       yObs %*% matrix(1,1,numGrd),
                       xObs %*% matrix(1,1,numGrd), 
                       matrix(1,numObs, 1) %*% as.vector(yGrid),
                       units="km")^2)
 deltay = gdist(xObs %*% matrix(1,1,numGrd), 
                yObs %*% matrix(1,1,numGrd),
                xObs %*% matrix(1,1,numGrd), 
                matrix(1,numObs, 1) %*% as.vector(yGrid),
                units="km")
 exponent = (deltax^2)/ gaussDenomX + (deltay^2)/ gaussDenomY
 CovAnalysis = exp(-exponent)
 
 # Matrix inversion to find the optimum weights
 leftHandSide = cbind(CovObservations, rep(1,numObs))
 leftHandSide = rbind(leftHandSide,c(rep(1,numObs),0))
 rightHandSide = rbind(CovAnalysis,rep(1,numGrd))
 Weights = solve(leftHandSide,rightHandSide)
 
 #Interpolate the field
 zGrid  = t(Weights[1:numObs,]) %*% zObsNew;    # interpolated value
 #compute the error
 zError = 1 - diag(t(Weights) %*% rightHandSide); # percent error 
 zError = sd(zObsNew) * (zError^0.5);    # scaled to the observation variance 
 #zError = matrix(zError,nrow=xDim,ncol=yDim,byrow=T) #convert zError into a matrix
 #Retrend the Z variable using predict
 
 if(RESID==F){zGrid = zGrid + predict(lhSide,newdata=data.frame(xObs=xGrid,yObs=yGrid))}
 #zGrid = matrix(zGrid,nrow=xDim,ncol=yDim,byrow=T) #convert zGrid into a matrix
 return(data.frame(PRED,OA_PRED=zGrid,OA_ERROR=zError))}

#Example:
#GLM_ISO12_OA=OPTIM_INTERP(DATA=CTD_STND[CTD_STND$TEMP==12, c("LAT","LONG","DEPTH","GLM_RESID")], 
#                          RESP="GLM_RESID", RESID=T, PRED=CTD_STND[CTD_STND$TEMP==12, c("LAT","LONG","DEPTH","GLM_RESID")], 
#                          Lx=VG[VG$TEMP==12,"GLM_Lx"], Ly=VG[VG$TEMP==12,"GLM_Ly"], 
#                          obsNoise=var(CTD_STND[CTD_STND$TEMP==12 , c("GLM_RESID")])/var(CTD_STND[CTD_STND$TEMP==12 , c("DEPTH")]))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

XV_COMP=function(ISO,LAB=T,YLIM=c(-200,200),ASPE=F)
{source("~/Grad School/Research/3_2012_Whale Tagging/boxplot.quant.R")
 if(LAB==T){par(mar=c(4,4,3,2),cex.axis=0.8)}
 if(LAB==F){par(mar=c(4,2,2,2),cex.axis=1.3)}
 boxplot.quant(CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_ALL_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_1.0_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_0.5_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_0.1_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GLM_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GLM_OA_XV_PRED"],
               CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GAM_XV_PRED"],
               names=c("All","1.0\U00B0","0.5\U00B0","0.1\U00B0","LM","OA","GAM"),col="lightblue",
               pars=list(boxwex=0.5),ylab=NA,ylim=YLIM)
 if(ASPE==T){text(x=1:7,y=rep(YLIM[1]+0.05*(YLIM[2]-YLIM[1]),7),cex=1,
                  labels=c(round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_ALL_XV_PRED"])^2)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_1.0_XV_PRED"])^2,na.rm=T)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_0.5_XV_PRED"])^2,na.rm=T)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"MEAN_LAT_LONG_0.1_XV_PRED"])^2,na.rm=T)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GLM_XV_PRED"])^2)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GLM_OA_XV_PRED"])^2)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0),
                           round(sum((CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]-CTD_STND[CTD_STND$TEMP==ISO,"GAM_XV_PRED"])^2)/
                                   nrow(CTD_STND[CTD_STND$TEMP==ISO,]),0)))}
 if(LAB==T){mtext(c("Mean","Mean","Mean","Mean","","",""),side=1,line=1.8,cex=0.8,at=1:7)
            mtext("Observed-Expected Depth",side=2,line=2.5,cex=1.1)
            mtext( paste("Cross-Validation Comparison (",ISO,"\U00B0","C Isotherm)",sep=""),side=3,line=1,font=2)}
 if(LAB==F){mtext( paste(ISO,"\U00B0","C Isotherm",sep=""),side=3,line=1,font=2,cex=1)}}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
ISOTHERM_PREDICT=function(DATA,ISO,GLM=F,GAM=F,GLM_OA=F,GAM_OA=F,MEAN_ALL=F,MEAN_LAT_LONG_1.0=F,
                          MEAN_LAT_LONG_0.5=F,MEAN_LAT_LONG_0.1=F)
{#Estimate average depth in DATA$LAT_LONG_0.5 bins
  if(MEAN_ALL==T)
  {DATA[,"MEAN_ALL"]=mean(CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"],na.rm=T)
   DATA[,"SE_ALL"]=sd(CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"],na.rm=T)/sqrt(nrow(CTD_STND[CTD_STND$TEMP==ISO,]))
   DATA[,"N_ALL"]=nrow(CTD_STND[CTD_STND$TEMP==ISO,])
   names(DATA)[names(DATA) %in% c("MEAN_ALL","SE_ALL","N_ALL")] <- c(paste("MEAN_ALL_ISO",ISO,sep=""),
                                                                     paste("SE_ALL_ISO",ISO,sep=""),
                                                                     paste("N_ALL_ISO",ISO,sep=""))}
  
  if(MEAN_LAT_LONG_1.0==T)
  {DATA$MEAN_LAT_LONG_1.0=NA
   DATA$SE_LAT_LONG_1.0=NA
   DATA$N_LAT_LONG_1.0=NA
   for(j in unique(CTD_STND[CTD_STND$TEMP==ISO,"LAT_LONG_1.0"]))
   {DATA[DATA$LAT_LONG_1.0%in%j,"MEAN_LAT_LONG_1.0"]=mean(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_1.0%in%j,"DEPTH"],na.rm=T)
    DATA[DATA$LAT_LONG_1.0%in%j,"SE_LAT_LONG_1.0"]=sd(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_1.0%in%j,"DEPTH"],na.rm=T)/sqrt(nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_1.0==j,]))
    DATA[DATA$LAT_LONG_1.0%in%j,"N_LAT_LONG_1.0"]=nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_1.0%in%j,])}
   names(DATA)[names(DATA) %in% c("MEAN_LAT_LONG_1.0","SE_LAT_LONG_1.0","N_LAT_LONG_1.0")] <- c(paste("MEAN_LAT_LONG_1.0_ISO",ISO,sep=""),
                                                                                                paste("SE_LAT_LONG_1.0_ISO",ISO,sep=""),
                                                                                                paste("N_LAT_LONG_1.0_ISO",ISO,sep=""))}
  
  if(MEAN_LAT_LONG_0.5==T)
  {DATA$MEAN_LAT_LONG_0.5=NA
   DATA$SE_LAT_LONG_0.5=NA
   DATA$N_LAT_LONG_0.5=NA
   for(j in unique(CTD_STND[CTD_STND$TEMP==ISO,"LAT_LONG_0.5"]))
   {DATA[DATA$LAT_LONG_0.5%in%j,"MEAN_LAT_LONG_0.5"]=mean(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.5%in%j,"DEPTH"],na.rm=T)
    DATA[DATA$LAT_LONG_0.5%in%j,"SE_LAT_LONG_0.5"]=sd(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.5%in%j,"DEPTH"],na.rm=T)/sqrt(nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.5==j,]))
    DATA[DATA$LAT_LONG_0.5%in%j,"N_LAT_LONG_0.5"]=nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.5%in%j,])}
   names(DATA)[names(DATA) %in% c("MEAN_LAT_LONG_0.5","SE_LAT_LONG_0.5","N_LAT_LONG_0.5")] <- c(paste("MEAN_LAT_LONG_0.5_ISO",ISO,sep=""),
                                                                                                paste("SE_LAT_LONG_0.5_ISO",ISO,sep=""),
                                                                                                paste("N_LAT_LONG_0.5_ISO",ISO,sep=""))}
  
  if(MEAN_LAT_LONG_0.1==T)
  {DATA$MEAN_LAT_LONG_0.1=NA
   DATA$SE_LAT_LONG_0.1=NA
   DATA$N_LAT_LONG_0.1=NA
   for(j in unique(CTD_STND[CTD_STND$TEMP==ISO,"LAT_LONG_0.1"]))
   {DATA[DATA$LAT_LONG_0.1%in%j,"MEAN_LAT_LONG_0.1"]=mean(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.1%in%j,"DEPTH"],na.rm=T)
    DATA[DATA$LAT_LONG_0.1%in%j,"SE_LAT_LONG_0.1"]=sd(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.1%in%j,"DEPTH"],na.rm=T)/sqrt(nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.1==j,]))
    DATA[DATA$LAT_LONG_0.1%in%j,"N_LAT_LONG_0.1"]=nrow(CTD_STND[CTD_STND$TEMP==ISO & CTD_STND$LAT_LONG_0.1%in%j,])}
   names(DATA)[names(DATA) %in% c("MEAN_LAT_LONG_0.1","SE_LAT_LONG_0.1","N_LAT_LONG_0.1")] <- c(paste("MEAN_LAT_LONG_0.1_ISO",ISO,sep=""),
                                                                                                paste("SE_LAT_LONG_0.1_ISO",ISO,sep=""),
                                                                                                paste("N_LAT_LONG_0.1_ISO",ISO,sep=""))}
  
  if(GLM==T)
  {DATA$GLM=predict(eval(parse(text=paste("GLM_ISO",ISO,sep=""))),DATA,se.fit=T)$fit
   DATA$GLM_SE=predict(eval(parse(text=paste("GLM_ISO",ISO,sep=""))),DATA,se.fit=T)$se.fit
   names(DATA)[names(DATA) %in% c("GLM","GLM_SE")] <- c(paste("GLM_ISO",ISO,sep=""),
                                                        paste("GLM_SE_ISO",ISO,sep=""))}
  
  if(GAM==T)
  {DATA$GAM=predict(eval(parse(text=paste("GAM_ISO",ISO,sep=""))),DATA,se.fit=T)$fit
   DATA$GAM_SE=predict(eval(parse(text=paste("GAM_ISO",ISO,sep=""))),DATA,se.fit=T)$se.fit
   names(DATA)[names(DATA) %in% c("GAM","GAM_SE")] <- c(paste("GAM_ISO",ISO,sep=""),
                                                        paste("GAM_SE_ISO",ISO,sep=""))}
  
  if(GLM_OA==T)
  {GLM_OA_temp=try(OPTIM_INTERP(DATA=CTD_STND[CTD_STND$TEMP==ISO,c("LAT","LONG","GLM_RESID")], RESP="GLM_RESID", RESID=T, 
                                               PRED=DATA, Lx=VG[VG$TEMP==ISO, "GLM_Lx"], Ly=VG[VG$TEMP==ISO, "GLM_Ly"], 
                                               obsNoise=var(CTD_STND[CTD_STND$TEMP==ISO,c("GLM_RESID")])/var(CTD_STND[CTD_STND$TEMP==ISO,c("DEPTH")]))[,c("OA_PRED","OA_ERROR")])
   if(class(GLM_OA_temp)=="try-error"){return(DATA);next}
   DATA[,c("GLM_OA","GLM_OA_SE")]=GLM_OA_temp
   DATA$GLM_OA=eval(parse(text=paste("DATA$GLM_ISO",ISO,sep="")))+DATA$GLM_OA
   names(DATA)[names(DATA) %in% c("GLM_OA","GLM_OA_SE")] <- c(paste("GLM_OA_ISO",ISO,sep=""),
                                                              paste("GLM_OA_SE_ISO",ISO,sep=""))}
  
  if(GAM_OA==T)
  {GAM_OA_temp=try(OPTIM_INTERP(DATA=CTD_STND[CTD_STND$TEMP==ISO,c("LAT","LONG","GAM_RESID")], RESP="GAM_RESID", RESID=T, 
                                PRED=DATA, Lx=VG[VG$TEMP==ISO, "GAM_Lx"], Ly=VG[VG$TEMP==ISO, "GAM_Ly"], 
                                obsNoise=var(CTD_STND[CTD_STND$TEMP==ISO,c("GAM_RESID")])/var(CTD_STND[CTD_STND$TEMP==ISO,c("DEPTH")]))[,c("OA_PRED","OA_ERROR")])
   if(class(GAM_OA_temp)=="try-error"){return(DATA);next}
   DATA[,c("GAM_OA","GAM_OA_SE")]=GAM_OA_temp
   DATA$GAM_OA=eval(parse(text=paste("DATA$GAM_ISO",ISO,sep="")))+DATA$GAM_OA
   names(DATA)[names(DATA) %in% c("GAM_OA","GAM_OA_SE")] <- c(paste("GAM_OA_ISO",ISO,sep=""),
                                                              paste("GAM_OA_SE_ISO",ISO,sep=""))}
  
  return(DATA)}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T_PERM=function(DATA1,DATA2,N_PERM=10000)
{T_OBS=t.test(DATA1,DATA2)$statistic
 T_OBS
 
 DATA = c(DATA1,DATA2)
 T_VAL = numeric(N_PERM)
 for (i in 1:N_PERM) 
 {GROUP_1 = DATA[sample(1:length(DATA), size=length(DATA1), replace=F)]
  GROUP_2 = DATA[sample(1:length(DATA), size=length(DATA2), replace=F)]
  T_VAL[i] = t.test(GROUP_1,GROUP_2)$statistic}
 
 T_VAL = abs(T_VAL)             # for a two-tailed test
 length(T_VAL[T_VAL>=abs(T_OBS)])/length(T_VAL) #p-value: proportion of test statistics more extreme than the observed test statistic
}

#EXAMPLE: T_PERM(DATA1=rnorm(50,mean=5),DATA2=rnorm(50,mean=5.5),N_PERM=100000)
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T_PERM=function(DATA1,DATA2,N_PERM=10000)
{T_OBS=t.test(DATA1,DATA2)$statistic
 
 DATA = c(DATA1,DATA2)
 T_VAL = numeric(N_PERM)
 for (i in 1:N_PERM) 
 {GROUP_1 = DATA[sample(1:length(DATA), size=length(DATA1), replace=F)]
  GROUP_2 = DATA[sample(1:length(DATA), size=length(DATA2), replace=F)]
  T_VAL[i] = t.test(GROUP_1,GROUP_2)$statistic}
 
 T_VAL = abs(T_VAL)             # for a two-tailed test
 length(T_VAL[T_VAL>=abs(T_OBS)])/length(T_VAL) #p-value: proportion of test statistics more extreme than the observed test statistic
}

#EXAMPLE: T_PERM(DATA1=rnorm(50,mean=5),DATA2=rnorm(50,mean=5.5),N_PERM=100000)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ANOVA_PERM=function(DATA,RESPONSE,COVAR,N_PERM=10000)
{### calculate the observed F-statistic
AOV_OBS = lm(paste(RESPONSE,COVAR,sep="~"), data=DATA)
F_OBS = summary(AOV_OBS)$fstatistic[1]

F_RAND = numeric(N_PERM)
for(i in 1:N_PERM) 
{DATA$RAND = sample(DATA[,RESPONSE], replace=F)
 ### compute the F-statistic for the shuffled data
 AOV_RAND = lm(paste("RAND",COVAR,sep="~"), data=DATA)
 F_RAND[i] = summary(AOV_RAND)$fstatistic[1]}

### calculate the approximate p-value
length(F_RAND[F_RAND>=F_OBS])/length(F_RAND)}

#EXAMPLE: ANOVA_PERM(DATA=data.frame(DEPTH=c(rnorm(50,mean=5),rnorm(50,mean=5),rnorm(50,mean=5)),
#ID=as.factor(c(rep(1,50),rep(2,50),rep(3,50))),RESPONSE="DEPTH",COVAR="ID",N_PERM=100000)

##### PLOTTING ######
#Plot a vertically oriented histogram using continuous data
HIST_VERT=function(DATA,BREAKS=10,THIN=1,XLIM=c(0,100),XLAB="Frequency (%)",YLAB="Depth (m)",COL="gray",AXES=T,ADD=F)
{HIST_temp=as.numeric(table(cut(DATA,breaks=BREAKS,plot=F)))
 barplot(rev(HIST_temp*100/sum(HIST_temp)), axes = FALSE, xlim = c(0, XLIM[2]), space = 0, horiz = TRUE,
         col=COL, add=ADD)
 if(AXES==T)
 {axis(side=1,at=c(0,XLIM[2]),pos=0,cex=0.01,labels=F,tck=0.001)
 axis(side=2,at=seq(0,length(HIST_temp),THIN),
      labels=rev(BREAKS)[seq(0,length(HIST_temp),THIN)+1],pos=0,las=1)
 axis(side=3,at=seq(0,XLIM[2],10),cex=0.8,pos=length(HIST_temp))
 axis(side=4,at=c(0,length(HIST_temp)),pos=XLIM[2],cex=0.01,labels=F,tck=0.001)
 mtext(YLAB,side=2,line=3)
 mtext(XLAB,side=3,line=1.75)}}

#Plot a horizontally oriented histogram using continuous data
HIST_HORIZ=function(DATA,BREAKS=10,THIN=1,YLIM=c(0,100),XLAB="Dive Duration (min)",YLAB="Frequency (%)",COL="gray",AXES=T,ADD=F)
{HIST_temp=hist(DATA,breaks=BREAKS, plot = F)
 barplot(HIST_temp$counts*100/sum(HIST_temp$counts), axes = FALSE, ylim = c(0, YLIM[2]), space = 0, horiz = F,
         col=COL, add=ADD)
 if(AXES==T)
 {axis(side=1,at=seq(0,length(HIST_temp$counts),THIN),
      labels=HIST_temp$breaks[seq(0,length(HIST_temp$counts),THIN)+1],pos=0,las=1)
 axis(side=2,at=seq(0,YLIM[2],10),cex=0.8,pos=0,las=1)
 axis(side=3,at=c(0,length(HIST_temp$counts)),pos=YLIM[2],cex=0.01,labels=F,tck=0.001)
 axis(side=4,at=c(0,YLIM[2]),pos=length(HIST_temp$counts),cex=0.01,labels=F,tck=0.001)
 mtext(XLAB,side=1,line=2.5)
 mtext(YLAB,side=2,line=2.5)}}

HIST_HORIZ=function(DATA,BREAKS,THIN=1,YLIM=c(0,100),XLAB="Dive Duration (min)",YLAB="Frequency (%)",COL="gray",AXES=T,ADD=F)
{HIST_temp=as.numeric(table(cut(DATA,breaks=BREAKS,plot=F)))
 barplot(HIST_temp*100/sum(HIST_temp), axes = FALSE, ylim = c(0, YLIM[2]), space = 0, horiz = F,
         col=COL, add=ADD)
 if(AXES==T)
 {axis(side=1,at=seq(0,length(HIST_temp),THIN),
       labels=BREAKS[seq(0,length(HIST_temp),THIN)+1],pos=0,las=1)
  axis(side=2,at=seq(0,YLIM[2],10),cex=0.8,pos=0,las=1)
  axis(side=3,at=c(0,length(HIST_temp)),pos=YLIM[2],cex=0.01,labels=F,tck=0.001)
  axis(side=4,at=c(0,YLIM[2]),pos=length(HIST_temp),cex=0.01,labels=F,tck=0.001)
  mtext(XLAB,side=1,line=2.5)
  mtext(YLAB,side=2,line=2.5)}}

#Function to plot individual TAT histograms with depth estimates
#Colors assigned to species Md="lightblue",Zc="lightgreen",Pm="plum",Gm="lightsalmon",Pe="lightgoldenrod1"
TAT_BAR_SINGLE=function(DATA,TEMP_DEPTH="TEMP",MAX_BINS=12,START_STOP=T,LIGHT_DARK=F,COLOR="gray")
{require(chron)
 TEMP_BINS=c(28,seq(24,2,-2)[1:MAX_BINS],NA)
 DEPTH_BINS=c(0,paste(round(DATA[,c("ISO24","ISO22","ISO20","ISO18","ISO16","ISO14",
                                    "ISO12","ISO10","ISO8","ISO6","ISO4")],1)[1:(MAX_BINS-1)],"\U00B1",
                      round(DATA[,c("SE_ISO24","SE_ISO22","SE_ISO20","SE_ISO18","SE_ISO16",
                                    "SE_ISO14","SE_ISO12","SE_ISO10","SE_ISO8","SE_ISO6",
                                    "SE_ISO4")],1)[1:(MAX_BINS-1)],sep=""),
              paste("MBD:",round(DATA[,c("BATHY_DEPTH_AVG")],1),"\U00B1",round(DATA[,c("BATHY_DEPTH_SE")],1),sep=""),"")
 
 if(TEMP_DEPTH=="TEMP"){par(mar=c(2.5,5,3,2.5))}
 if(TEMP_DEPTH=="DEPTH"){par(mar=c(2.5,9,3,2.5))}
 if(TEMP_DEPTH=="BOTH"){par(mar=c(2.5,9,3,5))}
 barplot(height=as.numeric(c(0,rev(DATA[,c("T2460","T2224","T2022","T1820","T1618","T1416",
                                           "T1214","T1012","T810","T68","T46","T04")][1:(MAX_BINS)]))),
         space=0,horiz=T,xlim=c(0,100),axes=F,axisname=F,col=COLOR)
 if(TEMP_DEPTH=="TEMP"){axis(side=2,at=1:(MAX_BINS+1),labels=rev(TEMP_BINS[1:(MAX_BINS+1)]),pos=0,las=1)
                        mtext(paste("Temperature (","\U00B0","C)",sep=""),side=2,line=3,cex=1.25)
                        rect(0,0,100,1,col="darkgray")}
 if(TEMP_DEPTH=="DEPTH"){axis(side=2,at=0:(MAX_BINS+1),labels=rev(DEPTH_BINS),pos=0,las=1)
                         rect(0,0,100,1,col="darkgray") 
                         mtext("Depth (m)",side=2,line=6,cex=1.25)} 
 if(TEMP_DEPTH=="BOTH"){axis(side=2,at=0:(MAX_BINS+1),labels=rev(DEPTH_BINS),pos=0,las=1)
                        axis(side=4,at=0:(MAX_BINS+1),labels=rev(TEMP_BINS),pos=100,las=1)
                        rect(0,0,100,1,col="darkgray") 
                        mtext(paste("Temperature (","\U00B0","C)",sep=""),side=4,line=3,cex=1.25)
                        mtext("Depth (m)",side=2,line=6,cex=1.25)}
 axis(side=3,at=seq(0,100,10),cex=0.8,pos=(MAX_BINS+1))
 axis(side=4,at=c(0,(MAX_BINS+1)),pos=100,cex=0.01,labels=F,tck=0.001)
 axis(side=1,at=c(0,100),pos=0,cex=0.01,labels=F,tck=0.001)
 mtext("Frequency",side=3,line=1.75,cex=1.25)
 
 if(START_STOP==T){START=paste(sprintf("%02d",hours(DATA[,"START_TIME"]-4/24)),
                               sprintf("%02d",minutes(DATA[,"START_TIME"])),sep=":")
                   text(paste("Time Start: ",START,sep=""),x=60,y=4,cex=1.0,pos=4)
                   END=paste(sprintf("%02d",hours(DATA[,"END_TIME"]-4/24)),
                             sprintf("%02d",minutes(DATA[,"END_TIME"])),sep=":")
                   text(paste("Time Stop: ",END,sep=""),x=60,y=3.5,cex=1.0,pos=4)}
 if(LIGHT_DARK==T){text(paste("Time Light: ",round(DATA[,"LIGHT_RATIO"]*100,1),"%",sep=""),x=60,y=3,cex=1.0,pos=4)
                   text(paste("Time Dark: ",round(DATA[,"DARK_RATIO"]*100,1),"%",sep=""),x=60,y=2.5,cex=1.0,pos=4)}}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Function to compare the depth/temp distribution of TAT data between species, day-night, location etc..
TAT_BOX_COMP=function(DATA1,DATA2,TEMP_DEPTH="TEMP",MIN_BINS=1,MAX_BINS=12,RANGE=100,
                      START_STOP=T,LIGHT_DARK=F,LAB=T, COL1="lightblue",COL2="lightblue4",PAT1,CEX_LAB=1.25)
{source("~/Grad School/Research/3_2012_Whale Tagging/boxplot.quant.R")
 BATHY_DEPTH=as.numeric(c(unlist(strsplit(DATA1$BATHY_DEPTH,",")),unlist(strsplit(DATA2$BATHY_DEPTH,","))))
 BATHY_DEPTH_AVG=mean(BATHY_DEPTH,na.rm=T)
 BATHY_DEPTH_SE=sd(BATHY_DEPTH,na.rm=T)/sqrt(length(BATHY_DEPTH))
 BATHY_DEPTH_MIN=min(BATHY_DEPTH,na.rm=T)
 BATHY_DEPTH_MAX=max(BATHY_DEPTH,na.rm=T)
 TEMP_BINS=c(NA,seq(24,4,-2),NA)[MIN_BINS:(MAX_BINS+1)]
 DEPTH_BINS=c(0,round(as.numeric(DATA1[1,c("MED_ISO24","MED_ISO22","MED_ISO20","MED_ISO18","MED_ISO16",
                                           "MED_ISO14","MED_ISO12","MED_ISO10","MED_ISO8","MED_ISO6",
                                           "MED_ISO4")]),1)[MIN_BINS:(MAX_BINS-1)],
              paste("MBD:",round(BATHY_DEPTH_AVG,1),sep=""),NA)
 DEPTH_RANGE=c(c(NA,paste("(",round(DATA1[1,c("MIN_ISO24","MIN_ISO22","MIN_ISO20","MIN_ISO18","MIN_ISO16",
                                         "MIN_ISO14","MIN_ISO12","MIN_ISO10","MIN_ISO8","MIN_ISO6",
                                         "MIN_ISO4")],0),"-",
                         round(DATA1[1,c("MAX_ISO24","MAX_ISO22","MAX_ISO20","MAX_ISO18","MAX_ISO16",
                                         "MAX_ISO14","MAX_ISO12","MAX_ISO10","MAX_ISO8","MAX_ISO6",
                                         "MAX_ISO4")],0),")",sep=""))[MIN_BINS:MAX_BINS],
               paste(round(BATHY_DEPTH_MIN,0),"-",round(BATHY_DEPTH_MAX,0),sep=""),NA)
 
 #DEPTH_RANGE=c(NA,paste("(",rep("111-111",12),")",sep=""),NA)
 
 if(TEMP_DEPTH=="TEMP"){par(mar=c(2.5,5,3,2.5))}
 if(TEMP_DEPTH=="DEPTH"){par(mar=c(2.5,9,3,2.5))}
 if(TEMP_DEPTH=="BOTH"){par(mar=c(2.5,9,3,5))}
 if(TEMP_DEPTH=="SINGLE"){par(mar=c(2.5,9,3,5))}
 
 if(LAB==F){if(TEMP_DEPTH=="TEMP"){par(mar=c(0,3,3,0))}
            if(TEMP_DEPTH=="DEPTH"){par(mar=c(0,6,3,0))}
            if(TEMP_DEPTH=="BOTH"){par(mar=c(0,6,3,3))}
            if(TEMP_DEPTH=="SINGLE"){par(mar=c(2,6,4,3))}}
 
 if(nrow(DATA1)>nrow(DATA2)){FILLER=matrix(NA,ncol=ncol(DATA2),nrow=nrow(DATA1)-nrow(DATA2))
                             colnames(FILLER)=colnames(DATA2)
                             DATA2=rbind(DATA2,FILLER)}
 if(nrow(DATA2)>nrow(DATA1)){FILLER=matrix(NA,ncol=ncol(DATA1),nrow=nrow(DATA2)-nrow(DATA1))
                             colnames(FILLER)=colnames(DATA1)
                             DATA1=rbind(DATA1,FILLER)}
 
 DATA=as.data.frame(cbind(DATA1[,"T2460"],DATA2[,"T2460"],DATA1[,"T2224"],DATA2[,"T2224"],DATA1[,"T2022"],DATA2[,"T2022"],
                          DATA1[,"T1820"],DATA2[,"T1820"],DATA1[,"T1618"],DATA2[,"T1618"],DATA1[,"T1416"],DATA2[,"T1416"],
                          DATA1[,"T1214"],DATA2[,"T1214"],DATA1[,"T1012"],DATA2[,"T1012"],DATA1[,"T810"],DATA2[,"T810"],
                          DATA1[,"T68"],DATA2[,"T68"],DATA1[,"T46"],DATA2[,"T46"],DATA1[,"T04"],DATA2[,"T04"],DATA1[,"T04"],DATA2[,"T04"]))
 colnames(DATA)=c("T2460_1","T2460_2","T2224_1","T2224_2","T2022_1","T2022_2","T1820_1","T1820_2","T1618_1","T1618_2","T1416_1","T1416_2","T1214_1",
                  "T1214_2","T1012_1","T1012_2","T810_1","T810_2","T68_1","T68_2","T46_1","T46_2","T04_1","T04_2","MBD_1","MBD_2")
 
 if(TEMP_DEPTH=="TEMP"){AT=seq(1-0.75,(MAX_BINS-MIN_BINS+1)-0.25,0.5)
                        AT[seq(1,length(AT),2)]=AT[seq(1,length(AT),2)]+0.06
                        AT[seq(2,length(AT),2)]=AT[seq(2,length(AT),2)]-0.06
                        boxplot.quant(DATA[,(2*(MAX_BINS)):(2*(MIN_BINS-1)+1)],
                                      horizontal=T,axes=F,ylim=c(0,RANGE),at=AT,
                                      col=rep(c(COL2,COL1),12),boxwex=0.35)}
 
 if(TEMP_DEPTH=="SINGLE"){AT=seq(1-0.50,(MAX_BINS-MIN_BINS+2)-0.50,1)
                          boxplot.quant(DATA[,rev(c("T2460_1","T2224_1","T2022_1","T1820_1","T1618_1","T1416_1",
                                                    "T1214_1","T1012_1","T810_1","T68_1","T46_1","T04_1","MBD_1"))],
                                        horizontal=T,axes=F,ylim=c(0,RANGE),at=AT,
                                        col=rep(c(COL1),12),boxwex=0.6)}
 
 if(TEMP_DEPTH=="DEPTH"|
      TEMP_DEPTH=="BOTH"){AT=seq(1-0.75,(MAX_BINS-MIN_BINS+2)-0.25,0.5)
                          AT[seq(1,length(AT),2)]=AT[seq(1,length(AT),2)]+0.06
                          AT[seq(2,length(AT),2)]=AT[seq(2,length(AT),2)]-0.06
                          boxplot.quant(DATA[,(2*(MAX_BINS+1)):(2*(MIN_BINS-1)+1)],
                                        horizontal=T,axes=F,ylim=c(0,RANGE),at=AT,
                                        col=rep(c(COL2,COL1),12),boxwex=0.35)}
 
 if(TEMP_DEPTH=="TEMP"){axis(side=2,at=0:(MAX_BINS-MIN_BINS+1),labels=rev(TEMP_BINS),pos=0,las=1)
                        if(LAB==T){mtext(paste("Temperature (","\U00B0","C)",sep=""),side=2,line=3,cex=CEX_LAB)}
                        axis(side=3,at=seq(0,RANGE,10),cex=0.8,pos=(MAX_BINS-MIN_BINS+1))
                        axis(side=4,at=c(0,(MAX_BINS-MIN_BINS+1)),pos=RANGE,cex=0.01,labels=F,tck=0.001)
                        axis(side=1,at=c(0,RANGE),pos=0,cex=0.01,labels=F,tck=0.001)}
 
 if(TEMP_DEPTH=="DEPTH"){axis(side=2,at=0:(MAX_BINS-MIN_BINS+2),labels=rev(DEPTH_BINS),pos=0,las=1)
#                         mtext(side=2,at=c(0,(1:(MAX_BINS-MIN_BINS+1))+0.22,(MAX_BINS-MIN_BINS+2)),
#                               text=rev(DEPTH_BINS),cex=0.75,las=1)
#                         mtext(side=2,at=c(0,(1:(MAX_BINS-MIN_BINS+1))-0.22,(MAX_BINS-MIN_BINS+2)),
#                               text=rev(DEPTH_RANGE),cex=0.6,las=1)
                         rect(0,0,RANGE,1,col="darkgray")
                         if(LAB==T){mtext("Median Depth (m)",side=2,line=6,cex=CEX_LAB)}
                         axis(side=3,at=seq(0,RANGE,10),cex=0.8,pos=(MAX_BINS-MIN_BINS+2))
                         axis(side=4,at=c((MIN_BINS-1),(MAX_BINS+1)),pos=RANGE,cex=0.01,labels=F,tck=0.001)
                         axis(side=1,at=c(0,RANGE),pos=0,cex=0.01,labels=F,tck=0.001)} 
 
 if(TEMP_DEPTH=="BOTH"|
      TEMP_DEPTH=="SINGLE"){axis(side=2,at=0:(MAX_BINS-MIN_BINS+2),labels=rev(DEPTH_BINS),pos=0,las=1)
#                        mtext(side=2,at=c(0,(1:(MAX_BINS-MIN_BINS+1))+0.22,(MAX_BINS-MIN_BINS+2)),
#                              text=rev(DEPTH_BINS),cex=0.75,las=1)
#                        mtext(side=2,at=c(0,(1:(MAX_BINS-MIN_BINS+1))-0.22,(MAX_BINS-MIN_BINS+2)),
#                              text=rev(DEPTH_RANGE),cex=0.6,las=1)
                        axis(side=4,at=0:(MAX_BINS-MIN_BINS+2),labels=c(NA,rev(TEMP_BINS)),pos=RANGE,las=1)
                        rect(0,0,RANGE,1,col="darkgray") 
                        if(LAB==T){mtext(paste("Temperature (","\U00B0","C)",sep=""),side=4,line=3,cex=CEX_LAB)
                                   mtext("Median Depth (m)",side=2,line=6,cex=CEX_LAB)}
                        axis(side=3,at=seq(0,RANGE,10),cex=0.8,pos=(MAX_BINS-MIN_BINS+2))
                        axis(side=1,at=c(0,RANGE),pos=0,cex=0.01,labels=F,tck=0.001)}
 
 if(LAB==T){mtext("Frequency",side=3,line=1.75,cex=CEX_LAB)}}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Function to create SERIES plots to show the variation in proportion of time in 
### different temperature/depth strata over time

TAT_SERIES_PLOT=function(DATA,MOON=T,SUN=T,COL=colorRampPalette(c("white","plum4"))(100),VARIABLE=F,Z_MAX=1800,INTV=150,LAB=T)
{require(chron)
 DATA=DATA[,c("T2460","T2224","T2022","T1820","T1618","T1416",
              "T1214","T1012","T810","T68","T46","T04","LAT","LONG","DATE_TIME",
              "ISO24","ISO22","ISO20","ISO18","ISO16","ISO14",
              "ISO12","ISO10","ISO8","ISO6","ISO4")]
 DATE_TIME_SEQ=c(DATA$DATE_TIME[1],seq(DATA$DATE_TIME[2],DATA$DATE_TIME[nrow(DATA)],6/24))
 if(length(DATE_TIME_SEQ[which(!DATE_TIME_SEQ%in%DATA$DATE_TIME)])>0)
 {DATA_temp=data.frame(T2460=NA,T2224=NA,T2022=NA,T1820=NA,T1618=NA,T1416=NA,T1214=NA,
                       T1012=NA,T810=NA,T68=NA,T46=NA,T04=NA,LAT=NA,LONG=NA,
                       DATE_TIME=DATE_TIME_SEQ[which(!DATE_TIME_SEQ%in%DATA$DATE_TIME)],
                       ISO24=NA, ISO22=NA, ISO20=NA, ISO18=NA, ISO16=NA, ISO14=NA, 
                       ISO12=NA, ISO10=NA, ISO8=NA, ISO6=NA, ISO4=NA)
  DATA=as.data.frame(rbind(DATA,DATA_temp))}
 DATA=DATA[order(DATA$DATE_TIME),]
 DATA$LM_DATE_TIME=paste(substr(as.character(as.POSIXct(DATA$DATE_TIME-4/24,origin="1970-01-01 00:00:00")),6,7),"/",
                         substr(as.character(as.POSIXct(DATA$DATE_TIME-4/24,origin="1970-01-01 00:00:00")),9,10)," ",
                         substr(as.character(as.POSIXct(DATA$DATE_TIME-4/24,origin="1970-01-01 00:00:00")),12,16),sep="")

 BINS=data.frame(MED=as.numeric(TAT[1,c("MED_ISO4","MED_ISO6","MED_ISO8","MED_ISO10","MED_ISO12","MED_ISO14","MED_ISO16",
                                        "MED_ISO18","MED_ISO20","MED_ISO22","MED_ISO24")]))
 
 if(VARIABLE==F)
 {par(mar=c(2.8,5,4.8,2))
  image(x=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,6/24),y=seq(2,26,2),
        z=as.matrix(DATA[,12:1]),col=COL,
        axes=F,xlab=NA,ylab=NA)
  
  axis(side=1,at=c(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24),pos=2,cex=0.8,labels=F,tck=0.001)
  axis(side=2,at=seq(2,26,2),labels=NA,pos=DATA$DATE_TIME[2]-6/24)
  axis(side=2,at=seq(4,24,2),labels=paste(round(BINS$MED,1),sep=""),pos=DATA$DATE_TIME[2]-6/24,las=1)
  if(LAB==T){mtext(side=2,"Depth (m)",line=5)
             mtext(paste("Temperature (","\U00B0","C)",sep=""),side=4,line=2.5)}
  axis(side=3,at=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,6/24),
       labels=NA, cex=0.8,pos=26,las=3)
  axis(side=3,at=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,12/24)[1:length(DATA$LM_DATE_TIME[seq(1,nrow(DATA),2)])],
       labels=DATA$LM_DATE_TIME[seq(1,nrow(DATA),2)], cex=0.8,pos=26,las=3)
  axis(side=4,at=seq(2,26,2),labels=seq(2,26,2),pos=DATA$DATE_TIME[nrow(DATA)]+6/24,las=1)}
 
 # Variable depth blocks
 if(VARIABLE==T)
 {par(mar=c(2,4,4,0))
  plot(x=1:10,y=1:10,xlim=c(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24),ylim=c(Z_MAX,0),
       type="n",axes=F,xlab="",ylab="")
  for(i in 1:nrow(DATA))
  {rect(xleft=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,6/24)[i],
        xright=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,6/24)[i]+6/24,
        ytop=as.numeric(c(0,DATA[i,c("ISO24","ISO22","ISO20","ISO18","ISO16","ISO14",
                                     "ISO12","ISO10","ISO8","ISO6","ISO4")])),
        ybottom=as.numeric(c(DATA[i,c("ISO24","ISO22","ISO20","ISO18","ISO16","ISO14",
                                      "ISO12","ISO10","ISO8","ISO6","ISO4")],Z_MAX)),
        col=COL[cut(as.numeric(DATA[i,c("T2460","T2224","T2022","T1820","T1618","T1416",
                                        "T1214","T1012","T810","T68","T46","T04")]),seq(0,100,length.out=100))],lty=0)}
  axis(side=1,at=c(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24),pos=Z_MAX,cex=0.8,labels=F,tck=0.001)
  axis(side=2,at=seq(0,Z_MAX,INTV),labels=seq(0,Z_MAX,INTV),pos=DATA$DATE_TIME[2]-6/24,las=1)
  if(LAB==T){mtext(side=2,"Depth (m)",line=2.5)}
  axis(side=3,at=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,6/24),
       labels=NA, cex=0.8,pos=0,las=3)
  axis(side=3,at=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+6/24,12/24)[1:length(DATA$LM_DATE_TIME[seq(1,nrow(DATA),2)])],
       labels=DATA$LM_DATE_TIME[seq(1,nrow(DATA),2)], cex=0.8,pos=0,las=3)
  axis(side=4,at=seq(0,Z_MAX,INTV),labels=rep(NA,13),tck=0.0001,pos=DATA$DATE_TIME[nrow(DATA)]+6/24)}
 
 if(SUN==T)
 {SUN=data.frame(DATE_TIME=seq(floor(DATA$DATE_TIME[2]-6/24),
                               ceiling(DATA$DATE_TIME[nrow(DATA)]+6/24)))
  require(maptools)
  Sys.setenv(TZ="GMT")
  SUN$RISE=as.chron(sunriset(as.matrix(cbind(mean(DATA$LONG,na.rm=T),mean(DATA$LAT,na.rm=T))), 
                             as.POSIXct(SUN$DATE_TIME,tz="GMT"), 
                             direction="sunrise", POSIXct.out=TRUE)$time)
  SUN$SET=as.chron(sunriset(as.matrix(cbind(mean(DATA$LONG,na.rm=T),mean(DATA$LAT,na.rm=T))), 
                            as.POSIXct(SUN$DATE_TIME,tz="GMT"), 
                            direction="sunset", POSIXct.out=TRUE)$time)
  rect(xleft=SUN$SET[1:(nrow(SUN)-1)],xright=SUN$RISE[2:nrow(SUN)],
       ybottom=ifelse(VARIABLE,Z_MAX,26),ytop=0,col=rgb(0,0,0,alpha=0.2),lty=0)}
 
 if(MOON==T)
 {require(oce)
  Sys.setenv(TZ="UTC")
  MOON_temp=data.frame(DATE_TIME=seq(DATA$DATE_TIME[2]-6/24,DATA$DATE_TIME[nrow(DATA)]+18/24,by=1/(24*60)))
  MOON_temp$DATE_TIME_POSIX=as.POSIXct(MOON_temp$DATE_TIME)
  MOON_temp$ALT=moonAngle(t=as.POSIXct(as.character(MOON_temp$DATE_TIME_POSIX),tz="UTC"),
                          longitude=mean(DATA$LONG,na.rm=T),latitude=mean(DATA$LAT,na.rm=T))$altitude
  MOON_temp$ILLUM=moonAngle(t=as.POSIXct(as.character(MOON_temp$DATE_TIME_POSIX),tz="UTC"),
                            longitude=mean(DATA$LONG,na.rm=T),latitude=mean(DATA$LAT,na.rm=T))$illuminatedFraction
  
  MOON=data.frame(DATE_TIME=MOON_temp$DATE_TIME[which(sign(MOON_temp$ALT)!=sign(MOON_temp$ALT[c(2:nrow(MOON_temp),nrow(MOON_temp))]))])
  MOON$ALT=MOON_temp[MOON_temp$DATE_TIME%in%MOON$DATE_TIME,"ALT"]
  MOON$ILLUM=MOON_temp[MOON_temp$DATE_TIME%in%MOON$DATE_TIME,"ILLUM"]
  MOON$RISE_SET=ifelse(sign(MOON$ALT)>=0,"SET","RISE")
  MOON_RISE=MOON[seq(which(MOON$RISE_SET=="RISE")[1],nrow(MOON),2),]
  colnames(MOON_RISE)=c("DATE_TIME_RISE","ALT_RISE","ILLUM_RISE","RISE")
  MOON_SET=MOON[seq(which(MOON$RISE_SET=="SET")[1],nrow(MOON),2),]
  colnames(MOON_SET)=c("DATE_TIME_SET","ALT_SET","ILLUM_SET","SET")
  if(nrow(MOON_RISE)!=nrow(MOON_SET)){MOON_SET=MOON_SET[1:nrow(MOON_RISE),]}
  MOON=cbind(MOON_RISE,MOON_SET)
  MOON$ILLUM=(MOON$ILLUM_RISE+MOON$ILLUM_SET)/2
  rect(xleft=MOON$DATE_TIME_RISE,
       xright=ifelse(MOON$DATE_TIME_SET>DATA$DATE_TIME[nrow(DATA)]+6/24,
                     DATA$DATE_TIME[nrow(DATA)]+6/24,MOON$DATE_TIME_SET),
       ybottom=ifelse(VARIABLE,Z_MAX-0.05*Z_MAX,3),
       ytop=ifelse(VARIABLE,Z_MAX-0.14*Z_MAX,5),col=rgb(1,1,1))
  rect(xleft=MOON$DATE_TIME_RISE+(MOON$DATE_TIME_SET-MOON$DATE_TIME_RISE)*(MOON$ILLUM),
       xright=ifelse(MOON$DATE_TIME_SET>DATA$DATE_TIME[nrow(DATA)]+6/24,
                     DATA$DATE_TIME[nrow(DATA)]+6/24,MOON$DATE_TIME_SET),
       ybottom=ifelse(VARIABLE,Z_MAX-0.05*Z_MAX,3),
       ytop=ifelse(VARIABLE,Z_MAX-0.14*Z_MAX,5),col="navy")}}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot TDR time series data on a bi-plot
SERIES_PLOT=function(DATA,START,END,Z_MAX,LWD=2,COLOR="black")
{DATA=DATA[as.POSIXct(as.chron(DATA$DATE_TIME),tz="GMT")>=as.POSIXct(START,tz="GMT") & 
             as.POSIXct(as.chron(DATA$DATE_TIME),tz="GMT")<=as.POSIXct(END,tz="GMT"), c("DATE_TIME","Depth")]
 DATA_NA=DATA[0,]
 for(i in 2:nrow(DATA))
 {if((DATA$DATE_TIME[i]-DATA$DATE_TIME[i-1])>0.003472222){DATA_NA=as.data.frame(rbind(DATA_NA,DATA[i-1,]))}}
 if(nrow(DATA_NA)>0)
 {DATA_NA$DATE_TIME=DATA_NA$DATE_TIME+0.003472222
  DATA_NA$Depth=NA
  DATA=as.data.frame(rbind(DATA,DATA_NA))
  DATA=DATA[order(DATA$DATE_TIME),]}
 par(mar=c(3,3,3,3.5))
 plot(DATA$DATE_TIME,DATA$Depth,ylim=c(Z_MAX,0),type="l",xlab=NA,ylab=NA,axes=F,lwd=LWD,col="black")
 axis(side=2,at=seq(0,Z_MAX,200),pos=DATA$DATE_TIME[1])
 mtext("Depth (m)",side=2,line=1.50)  
 axis(side=3,pos=0,at=seq(DATA$DATE_TIME[1],DATA$DATE_TIME[nrow(DATA)],(DATA$DATE_TIME[nrow(DATA)]-DATA$DATE_TIME[1])/2),labels=DATA$DATE_TIME_LAB[c(1,round((1+nrow(DATA))/2,0),nrow(DATA))],cex=0.8)
 mtext("Time",side=3,line=1.50)}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BEHAV_SERIES_PLOT=function(DATA_BEHAV=NULL, DATA_SERIES=NULL, START, END, SUN=NULL, 
                           INTV_Y=100, N_X_AXIS=10, Z_MAX=2000,XLAB="Time",YLAB="Depth (m)",CEX.AXIS=0.8,
                    COL_BEHAV="black",COL_SERIES="red",TEXT=NULL,TEXT_PC_THRESH=0.85,TIME_DIG_1=1,TIME_DIG_2=16)
{ require(chron);require(maptools)

  plot(1,1,xlim=c(as.POSIXct(START,tz="GMT"),as.POSIXct(END,tz="GMT")), type = "n",
       ylim=c(Z_MAX,0),axes=F,frame.plot=T,xlab=NA,ylab=NA)
  axis(side=2,at=seq(0,Z_MAX,INTV_Y),labels=seq(0,Z_MAX,INTV_Y),las=1, cex.axis=CEX.AXIS)
  axis(side=3,at=seq(as.POSIXct(START,tz="GMT"),as.POSIXct(END,tz="GMT"),length.out=N_X_AXIS),
       labels=substr(as.character(seq(as.POSIXct(START,tz="GMT")-4*60*60,as.POSIXct(END,tz="GMT")-4*60*60,
                                      length.out=N_X_AXIS)),TIME_DIG_1,TIME_DIG_2), cex.axis=CEX.AXIS,las=3)
  mtext(YLAB,side=2,line=3)  
  mtext(XLAB,side=1,line=0.5)
  
  #plot rectangles representing night periods 
  if(!is.null(SUN))
  {for(i in seq(floor(as.numeric(DATA_BEHAV$DATE_TIME[1])),
                ceiling(as.numeric(DATA_BEHAV$DATE_TIME[nrow(DATA_BEHAV)])))) 
  {rect(xleft=sunriset(crds=as.matrix(data.frame(LONG=mean(DATA_BEHAV$LONG,na.rm=T),
                                                 LAT=mean(DATA_BEHAV$LAT,na.rm=T))),
                       dateTime=as.POSIXct(as.chron(i),tz="GMT"), direction=c("sunset"),POSIXct.out=T)$time,
        xright=sunriset(crds=as.matrix(data.frame(LONG=mean(DATA_BEHAV$LONG,na.rm=T),
                                                  LAT=mean(DATA_BEHAV$LAT,na.rm=T))),
                        dateTime=as.POSIXct(as.chron(i+1),tz="GMT"), direction=c("sunrise"),POSIXct.out=T)$time,
        ybottom=Z_MAX,ytop=0,col=gray(0.8),border=NA)}}
  
  #plot dive durations in minutes on plot 
  if(!is.null(TEXT) & nrow(DATA_BEHAV)>0)
  {text(x=as.POSIXct(as.chron(DATA_BEHAV[DATA_BEHAV$What=="Dive" & DATA_BEHAV$PC1>TEXT_PC_THRESH,"DATE_TIME"]),tz="GMT"),
        y=Z_MAX-100,labels=round(DATA_BEHAV[DATA_BEHAV$What=="Dive" & DATA_BEHAV$PC1>TEXT_PC_THRESH,"DurationMin"]/60,0),
        pos=1,offset=0,srt=90,cex=0.6)}
  
  #Subset BEHAV data between START and END times
  if(!is.null(DATA_BEHAV))
  {Sys.setenv(TZ='GMT')
  DATA_BEHAV=DATA_BEHAV[DATA_BEHAV$What!="Message",]
  DATA_BEHAV=DATA_BEHAV[as.POSIXct(as.chron(DATA_BEHAV$DATE_TIME),tz="GMT")>=as.POSIXct(START,tz="GMT") & 
              as.POSIXct(as.chron(DATA_BEHAV$DATE_TIME),tz="GMT")<=as.POSIXct(END,tz="GMT"),]

  for(i in unique(DATA_BEHAV$MSG_ID))
  {#Create numeric vectors representing each dive and surface interval as START_, MID_, and END_TIME 
  #sequence within each MSG_ID
  X=data.frame(START_TIME=as.numeric(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,"START_TIME"][1])
                          +c(0,cumsum(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,"DurationMin"])[-nrow(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,])]/(24*60*60)),
                END_TIME=as.numeric(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,"START_TIME"][1])
                         +cumsum(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,"DurationMin"])/(24*60*60))
  X$MID_TIME=(X$START_TIME+X$END_TIME)/2
  X=as.POSIXct(as.chron(as.numeric(t(cbind(X$START_TIME,X$MID_TIME,X$END_TIME)))),tz="GMT")
  #Create numeric vectors representing each dive and surface interval as START_, MID_, and END_DEPTH 
  #sequence within each MSG_ID
  Y=data.frame(START_DEPTH=rep(NA,nrow(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,])),
               MID_DEPTH=DATA_BEHAV[DATA_BEHAV$MSG_ID==i,"DepthMin"],
               END_DEPTH=rep(NA,nrow(DATA_BEHAV[DATA_BEHAV$MSG_ID==i,])))
  Y[is.na(Y$START_DEPTH),"START_DEPTH"]=0
  Y[is.na(Y$MID_DEPTH),"MID_DEPTH"]=0
  Y[is.na(Y$END_DEPTH),"END_DEPTH"]=0
  Y=as.numeric(t(as.matrix(Y)))
  #draw those vectors as lines on time series plot
  lines(X,Y,col=COL_BEHAV)}
  }
  
  if(!is.null(DATA_SERIES))
  {#Subset SERIES data between START and END times
  DATA_SERIES=DATA_SERIES[as.POSIXct(as.chron(DATA_SERIES$DATE_TIME),tz="GMT")>=as.POSIXct(START,tz="GMT") & 
                          as.POSIXct(as.chron(DATA_SERIES$DATE_TIME),tz="GMT")<=as.POSIXct(END,tz="GMT"),]
  
  #draw lines on time series plot
  for(i in unique(DATA_SERIES$MSG_ID))
  {lines(x=as.POSIXct(as.chron(DATA_SERIES[DATA_SERIES$MSG_ID==i,"DATE_TIME"]),tz="GMT"),
         y=DATA_SERIES[DATA_SERIES$MSG_ID==i,"Depth"],col=COL_SERIES)}}
}

#EXAMPLE:
#BEHAV_SERIES_PLOT(DATA_BEHAV=BEHAV[BEHAV$Ptt==108419,], DATA_SERIES=SERIES[SERIES$Ptt==108419,], 
#                  START="2011-06-13 00:00:00", END="2011-06-15 00:00:00",SUN=T,INTV=100)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Function to plot gridded model predictions and mask out areas shallower than 
#the minimum depth contour for a particular CTD_STND$TEMP
CONTOUR_PLOT=function(DATA,VARIABLE,ISO=24,BREAKS=NA,BATHY_MASK=NA,MASK_LEVEL=NA,BUFFER=0.1,
                      COAST=F,CONTOUR=T,RASTER=T,POINTS=F,TEXT=F,LAB=T,SCALED=F,CONTOUR_INTV=20,ROUND=-1,
                      TEXT_THIN=1,ADD=F,OUT_RAST=F)
{require(rgeos);require(raster);
 if(!is.na(BREAKS)){DATA[,VARIABLE]=ifelse(DATA[,VARIABLE]>max(BREAKS),NA,DATA[,VARIABLE])
                    DATA[,VARIABLE]=ifelse(DATA[,VARIABLE]<min(BREAKS),NA,DATA[,VARIABLE])}
 GRID_Y=seq(from = min(DATA$LAT), to = max(DATA$LAT), by=abs(DATA$LONG[1]-DATA$LONG[2]))
 GRID_X=seq(from = min(DATA$LONG), to = max(DATA$LONG), by=abs(DATA$LONG[1]-DATA$LONG[2]))
 RAST=raster(x=matrix(DATA[,VARIABLE],nrow=length(GRID_Y),ncol=length(GRID_X),byrow=T)[length(GRID_Y):1,],
             xmn=min(DATA$LONG), xmx=max(DATA$LONG), 
             ymn=min(DATA$LAT), ymx=max(DATA$LAT),
             crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
 
 # Creates a custom depth mask for each plot based on the shallowest depth value at which that TEMP_ISO has been observed
 MASK_LEVEL=ifelse(!is.na(MASK_LEVEL),MASK_LEVEL,round(min(CTD_STND[CTD_STND$TEMP==ISO,"DEPTH"]),-1))
 
 if(is.na(BATHY_MASK))
 {BATHY_MASK = rasterToContour(BATHY,levels=MASK_LEVEL)
  BATHY_MASK_SL=BATHY_MASK
  BATHY_MASK=gPolygonize((gUnion(as(STUDY_AREA, "SpatialLines"),BATHY_MASK)))
  BATHY_MASK_INV=BATHY_MASK[gDisjoint(BATHY_MASK,BASIN_POINT,byid=T)]
  BATHY_MASK=BATHY_MASK[gIntersects(BATHY_MASK,BASIN_POINT,byid=T)]
  if(!is.na(BUFFER)){BATHY_MASK=gBuffer(BATHY_MASK,width=BUFFER)}}
 
 #Overlay a polygon figure and assign grid cells whose centers fall outside the polyon to NA
 RAST<- mask(RAST,BATHY_MASK,inverse=F)
 
 #subset the point records on which a model is based
 CTD_STND_temp=CTD_STND[CTD_STND$TEMP==ISO,]
 
 if(is.na(BREAKS)){BREAKS=seq(summary(RAST)[1,1],summary(RAST)[5,1],length.out=100)}
 
 #print DATA heat map and DATAs
 if(LAB==T){ if(RASTER){par(mar=c(4,4,2,0))
                        plot(RAST,col=heat.colors(100),breaks=BREAKS,add=ADD,legend=F)}
             if(POINTS){par(mar=c(4,4,2,4))
                        plot(CTD_STND_temp$LONG,CTD_STND_temp$LAT,
                             col=heat.colors(100)[cut(CTD_STND_temp$DEPTH,breaks=BREAKS)],
                             pch=20,cex=1.3,xlim=c(-79.75,-75.25),ylim=c(23.10,26.90),xlab="",ylab="")}}
 if(LAB==F){ if(RASTER){par(mar=c(2,2,2,0))
                        plot(RAST,col=heat.colors(100),breaks=BREAKS,add=ADD,legend=F)}
             if(POINTS){par(mar=c(2,2,2,0))
                        plot(RAST,col="white",legend=F)
                        points(CTD_STND_temp$LONG,CTD_STND_temp$LAT,
                               col=heat.colors(100)[cut(CTD_STND_temp$DEPTH,breaks=BREAKS)],
                               pch=20,cex=0.8)}}
 
 if(CONTOUR){contour(RAST,levels=seq(round(min(BREAKS),ROUND),round(max(BREAKS),ROUND),CONTOUR_INTV),add=T)} 
 if(exists("BATHY_MASK_INV")){plot(BATHY_MASK_INV,add=T,col="white",lty=0)}
 if(exists("BATHY_MASK_SL")){plot(BATHY_MASK_SL,add=T)}
 if(COAST){plot(COAST,add=T,col="lightgreen")}
 if(LAB){mtext("Longitude",side=1,line=2.5,cex=1.15)
         mtext("Latitude",side=2,line=2.5,cex=1.15)
         text(min(DATA$LONG)+0.25,min(DATA$LAT)+0.45,labels=paste("Variable:",ifelse(POINTS,paste("POINTS_ISO",ISO,sep=""),VARIABLE)),pos=4,offset=0,cex=1.15)
         text(min(DATA$LONG)+0.25,min(DATA$LAT)+0.30,labels=paste( "Depth Mask:",MASK_LEVEL,"m"),pos=4,offset=0,cex=1.15)}
 if(TEXT){text(CTD_STND_temp$LONG[seq(1,nrow(CTD_STND_temp),by=TEXT_THIN)],
               CTD_STND_temp$LAT[seq(1,nrow(CTD_STND_temp),by=TEXT_THIN)],
               round(CTD_STND_temp$DEPTH[seq(1,nrow(CTD_STND_temp),by=TEXT_THIN)],0),
               pos=4,offset=0,cex=0.4)}
 
 if(OUT_RAST==T){return(RAST)}
 print(paste("RAST MIN =",summary(RAST)[1,1],",RAST MED =",summary(RAST)[3,1],",RAST MAX =",summary(RAST)[5,1]))}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a spatial Polygon elipse from the a central coordinate pair, with semi-major (A) and semi-minor (B) axes 
# and angle parameter (PHI); only for visual because have to use a spherical earth assumption which becomes distorted 
# over large length scales
ELLIPSE<- function (LAT, LONG, A, B, PHI=pi/2, N = 100, ...)  
{require(Imap);require(sp) 
 PHI=PHI*pi/180
 A=A/gdist(1,LAT,2,LAT,"m")
 B=B/gdist(LONG,floor(LAT),LONG,ceiling(LAT),"m")
 #Create a sequence of angles around the unit circle 0:2*pi
 THETA <- seq(0, 2*pi, length = N)       
 #Create vectors of XH and YH coordinates representing a horizontal ellipse with A and B axes
 XH <-  A * cos(THETA)        
 YH <-  B * sin(THETA)         
 #Rotate the XH and YH 
 X  <- LAT + cos(PHI) *XH - sin(PHI)*YH 
 X[N]<-X[1]  ## force closure of polygon 
 Y  <- LONG + sin(PHI)*XH + cos(PHI)*YH   
 Y[N]<-Y[1]  ## force closure of polygon 
 return(SpatialPolygons(list(Polygons(list(with(list(x = x, y = y),Polygon(cbind(x,y)))),1))) ) 
}    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Create a North Arrow for plotting on any map
NORTH_ARROW = function(TIP_COORD)
{RANGE_X=par()$usr[1:2]
 RANGE_Y=par()$usr[3:4]
 lines(x=c(TIP_COORD[1],TIP_COORD[1]),
       y=c(TIP_COORD[2],TIP_COORD[2]-0.12*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
 lines(x=c(TIP_COORD[1],TIP_COORD[1]-0.025*(RANGE_X[2]-RANGE_X[1])),
       y=c(TIP_COORD[2],TIP_COORD[2]-0.045*(RANGE_X[2]-RANGE_X[1])),lwd=2)
 text(x=TIP_COORD[1],y=TIP_COORD[2]-0.12*(RANGE_Y[2]-RANGE_Y[1]),
      label="N",cex=1.35,pos=1)}

##### Create a Length bar for plotting on any map
LENGTH_BAR = function(LEFT_COORD,PRECISION=-2,PROPORTION=0.25)
{require(Imap)
 RANGE_X=par()$usr[1:2]
 RANGE_Y=par()$usr[3:4]
 DEG_KM=gdist(RANGE_X[1],RANGE_Y[1],RANGE_X[1]+1,RANGE_Y[1],units="km")
 LENGTH_KM=round(PROPORTION*DEG_KM*(RANGE_X[2]-RANGE_X[1]),PRECISION)
 LENGTH_DEG=(RANGE_X[2]-RANGE_X[1])*LENGTH_KM/(DEG_KM*(RANGE_X[2]-RANGE_X[1]))
 lines(x=c(LEFT_COORD[1],LEFT_COORD[1]+LENGTH_DEG),y=c(LEFT_COORD[2],LEFT_COORD[2]),lwd=2)
 lines(x=c(LEFT_COORD[1]+LENGTH_DEG,LEFT_COORD[1]+LENGTH_DEG),
       y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
 lines(x=c(LEFT_COORD[1]+LENGTH_DEG/2,LEFT_COORD[1]+LENGTH_DEG/2),
       y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
 lines(x=c(LEFT_COORD[1]+LENGTH_DEG/4,LEFT_COORD[1]+LENGTH_DEG/4),
       y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
 lines(x=c(LEFT_COORD[1],LEFT_COORD[1]),
       y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
 text(x=c(LEFT_COORD[1],LEFT_COORD[1]+LENGTH_DEG/4,LEFT_COORD[1]+LENGTH_DEG/2,LEFT_COORD[1]+LENGTH_DEG),
      y=rep(LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1]),4),
      labels=c(round(c(0,LENGTH_KM/4,LENGTH_KM/2),0),paste(round(LENGTH_KM,0),"km")),pos=1)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Function to create a user defined color-bar on a plot
COLOR_BAR=function(XLEFT,YBOTTOM,XRIGHT,YTOP,COLORS,TICKS,ROUND,CEX)
{require(Imap)
 RANGE_Y=abs(YTOP-YBOTTOM)
 RANGE_X=abs(XRIGHT-XLEFT)
 TICKS=round(TICKS,ROUND)
 if(ROUND<0){ROUND=0}
 for(i in 0:(length(COLORS)-1)){rect(XLEFT,YBOTTOM+i*RANGE_Y/length(COLORS),
                                     XRIGHT,YBOTTOM+(i+1)*RANGE_Y/length(COLORS),col=COLORS[i],lty=0)}
 rect(XLEFT,YBOTTOM,XRIGHT,YTOP,col=NA,lty=1,lwd=CEX)
 segments(x0=XRIGHT, y0=seq(YBOTTOM,YTOP,length.out=length(TICKS)), 
          x1 = XRIGHT+0.20*RANGE_X, y1 = seq(YBOTTOM,YTOP,length.out=length(TICKS)),lwd=CEX)
 text(x=rep(XRIGHT+0.20*RANGE_X,length(TICKS)), y=seq(YBOTTOM,YTOP,length.out=length(TICKS)),
      labels=sprintf(paste("%.",ROUND,"f",sep=""),TICKS),pos=4,cex=CEX)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create a grids of different cell sizes to link to CTD summaries
PREDICT_POLYS=function(LAT_MIN, LAT_MAX, LONG_MIN, LONG_MAX, CELL_SIZE)
{#Create grid of prediction cells
  PREDICT_GRID <- GridTopology(cellcentre.offset=c(LONG_MIN+0.5*CELL_SIZE,LAT_MIN+0.5*CELL_SIZE), 
                               cellsize=c(CELL_SIZE,CELL_SIZE), 
                               cells.dim=c(abs(LONG_MAX-LONG_MIN)*1/CELL_SIZE,abs(LAT_MAX-LAT_MIN)*1/CELL_SIZE))
  #Reformat grid as Spatial Polygons
  PREDICT_POLYS <- as.SpatialPolygons.GridTopology(PREDICT_GRID,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #Extract coordinates and create a dataframe
  COORD_POLYS=coordinates(PREDICT_POLYS)
  colnames(COORD_POLYS)=c("LONG_MID","LAT_MID")
  COORD_POLYS=as.data.frame(COORD_POLYS)
  
  #Create a SpatialPolygonsDataFrame
  PREDICT_POLYS=SpatialPolygonsDataFrame(PREDICT_POLYS, COORD_POLYS)
  #projection(CRUISE_DAY_SP)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(PREDICT_POLYS)
}

#Example: CTD_GRID_1.0=PREDICT_POLYS(23.0, 28.0, -80.0, -74.0, 1.0)
#projection(CTD_GRID_1.0)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#function to transform these coordinates into an sp SpatialPolygonDataFrame which is R equivalent of a Shapefile
GRID=function(LAT_1, LONG_1, LAT_2, LONG_2, LAT_3, LONG_3, LAT_4, LONG_4)
{GRID=as.data.frame(cbind(LAT_1, LONG_1, LAT_2, LONG_2, LAT_3, LONG_3, LAT_4, LONG_4))
 GRID_SPoly <- list()
 for (i in 1:nrow(GRID)) 
 {GRID_SPoly[[i]] <- Polygons(list(Polygon(rbind(as.numeric(GRID[i, c("LONG_1","LAT_1")]), 
                                                 as.numeric(GRID[i, c("LONG_2","LAT_2")]),
                                                 as.numeric(GRID[i, c("LONG_3","LAT_3")]),
                                                 as.numeric(GRID[i, c("LONG_4","LAT_4")]),
                                                 as.numeric(GRID[i, c("LONG_1","LAT_1")])))),
                              ID = as.character(i))}
 
 GRID_POLYGONS <- SpatialPolygonsDataFrame(SpatialPolygons(GRID_SPoly), as.data.frame(GRID), 
                                           match.ID = FALSE)
 #set the projection to GCS_WGS84 (default projection for ArcGIS and GPS coordinates)
 projection(GRID_POLYGONS)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 return(GRID_POLYGONS)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Function to overlay points on a Google Maps background (somewhat cumbersome)
MAP=function(DATA,ID,NEW_PLOT=F,POINTS_LINES="POINTS",DIRECTION="DATA$LAT",SPP=c("Md","Gm","Pm","Zc","Pe","Sb"),
             REGION=c("TOTO","SEPC","NWPN","NWPS","FLST","NABA","OTBB"),MONTH=c(1:12),YEAR=c(2009:2014),
             J_DATE=c(1:365),TYPE=c("CTD","PFL","XBT","PDT"),EXTRA_CONDITION=NA,
             LAT_MIN=23.500,LAT_MAX=27.000,LONG_MIN=-79.500,LONG_MAX=-75.000,COLOR=c("yellow","red","J_DATE"),PCH=20)
  
{require(RgoogleMaps)
 if(is.na(EXTRA_CONDITION)){DATA=DATA[DATA$REGION%in%REGION & DATA$MONTH%in%MONTH & DATA$SPP%in%SPP &
                                        DATA$YEAR%in%YEAR & DATA$J_DATE%in%J_DATE & DATA$TYPE%in%TYPE,]}
 if(!is.na(EXTRA_CONDITION)){DATA=DATA[DATA$REGION%in%REGION & DATA$MONTH%in%MONTH & DATA$YEAR%in%YEAR & DATA$SPP%in%SPP &
                                         DATA$J_DATE%in%J_DATE & DATA$TYPE%in%TYPE & eval(parse(text =EXTRA_CONDITION)),]}
 
 DATA=as.data.frame(cbind(DATA[!duplicated(eval(parse(text =ID))),unlist(strsplit(ID,"$",fixed=T))[2]],
                          DATA[!duplicated(eval(parse(text =ID))),]))
 
 if(length(COLOR)>1){COL_PAL=colorRampPalette(COLOR[1:2]);COLOR=COL_PAL(10)[as.numeric(cut(DATA[,COLOR[3]],breaks = 10))]}
 
 DATA=DATA[order(eval(parse(text =DIRECTION))),]
 
 if(NEW_PLOT==T)
 {MAP_temp <<- GetMap.bbox(lonR= range(c(LONG_MIN,LONG_MAX)), latR= range(c(LAT_MIN,LAT_MAX)), 
                           destfile= "MAP_temp.png", maptype="satellite",SCALE=2)
  if("POINTS"%in%POINTS_LINES)
  {PlotOnStaticMap(MAP_temp, lat = DATA[, "LAT"],lon = DATA[, "LONG"],
                   destfile = "MAP_temp.png", cex=0.5,pch=PCH,col=COLOR,add=F)}
  if("LINES"%in%POINTS_LINES)
  {for(i in 1:(nrow(DATA)-1))
  {PlotArrowsOnStaticMap(MAP_temp, lat0 = DATA[i, "LAT"], lon0 = DATA[i, "LONG"],lat1 = DATA[(i+1), "LAT"], lon1 = DATA[(i+1), "LONG"],
                         FUN=segments,cex=0.5,pch=PCH,col=COLOR,add=T)}}}
 if(NEW_PLOT==F)
 {if("POINTS"%in%POINTS_LINES)
 {PlotOnStaticMap(MAP_temp, lat = DATA[, "LAT"],lon = DATA[, "LONG"],
                  destfile = "MAP_temp.png", cex=0.5,pch=PCH,col=COLOR,add=T)}
 if("LINES"%in%POINTS_LINES)
 {for(i in 1:(nrow(DATA)-1))
 {PlotArrowsOnStaticMap(MAP_temp, lat0 = DATA[i, "LAT"], lon0 = DATA[i, "LONG"],lat1 = DATA[(i+1), "LAT"], lon1 = DATA[(i+1), "LONG"],
                        FUN=segments, cex=0.5,pch=PCH,col=COLOR,add=T)}}}}

#Example: 
#MAP(CTD_STND,"DATA$CAST",TYPE=c("PDT","PFL","CTD"),SPP=c("Md","Gm","Pm","Zc","Pe","Sb",NA),COLOR=c("red","yellow","DEPTH"),
#EXTRA_CONDITION=paste("CTD_STND$TEMP==22 & CTD_STND$LAT>=24.20 & CTD_STND$LAT<=26.20 &",
#"CTD_STND$LONG<= -76.000 & CTD_STND$LONG>= -79.200"),PCH=20,
#LAT_MIN=24.70,LAT_MAX=26.70,LONG_MIN=-79.200,LONG_MAX=-76.000,NEW_PLOT=T) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Function to plot all (default) or a subset of CTD on a Depth and Temperature Bi-plot
CTD_PLOT=function(DATA,NEW_PLOT,T_MIN=0,T_MAX=30,Z_MIN=0,Z_MAX=2000,LINES=F,THIN=1,
                  COLOR="black",BREAKS=NA,COL_VAR=NA,REV=F,CEX=1.0)
{DATA=DATA[DATA$DEPTH<=Z_MAX,]
 DATA=DATA[DATA$CAST%in%unique(DATA$CAST)[seq(1,length(unique(DATA$CAST)),by=THIN)],]
 DATA=DATA[order(DATA$CAST,DATA$TEMP),]
 if(!is.na(COL_VAR)){DATA=DATA[order(DATA[,COL_VAR],DATA$CAST,DATA$TEMP),]}
 if(!is.na(BREAKS)){COLOR=COLOR[cut(DATA[,COL_VAR],breaks=BREAKS)]}
 if(NEW_PLOT)
 {par(mar=c(2,5,3,3))
  plot(DATA$TEMP,DATA$DEPTH,type="p",ylim=c(Z_MAX,Z_MIN-50),xlim=c(T_MIN,T_MAX),
       col=COLOR,pch=20,cex=0.5,axes=F,xlab=NA,ylab=NA)
  mtext(paste("Temperature (","\U00B0","C)",sep=""),side=1,line=1.65,cex=CEX)
  mtext("Depth (m)",side=2,line=2.75,cex=CEX)
  axis(side=2,at=seq(Z_MIN,Z_MAX,200),pos=T_MIN,las=1)
  axis(side=1,at=seq(T_MIN,T_MAX,5),pos=Z_MAX,cex=0.7)
  axis(side=4,at=c(Z_MIN,Z_MAX),pos=T_MAX,cex=0.01,labels=F,tck=0.001)
  axis(side=3,at=c(T_MIN,T_MAX),pos=Z_MIN-50,cex=0.01,labels=F,tck=0.001)
  segments(c(T_MIN,T_MAX),Z_MIN, c(T_MIN,T_MAX), Z_MIN-50,lwd=1.5)}
 else
 {points(DATA$TEMP,DATA$DEPTH,type="p",ylim=c(Z_MAX,Z_MIN),xlim=c(T_MIN,T_MAX),col=COLOR,pch=20,cex=0.5)}
 if(REV==F){PLOT_ORDER=unique(DATA$CAST)}
 if(REV==T){PLOT_ORDER=rev(unique(DATA$CAST))}
 if(LINES==T)
 {for(i in PLOT_ORDER)
 {COLOR_i=COLOR
  if(!is.na(BREAKS)){COLOR_i=COLOR[DATA$CAST==i][1]}
  lines(DATA[DATA$CAST==i,"TEMP"],DATA[DATA$CAST==i,"DEPTH"],type="l",ylim=c(Z_MAX,Z_MIN),xlim=c(T_MIN,T_MAX),col=COLOR_i,lwd=0.75)}}}

#Example: CTD_PLOT(DATA=CTD[CTD$TYPE=="CTD" & CTD$REGION%in%c("TOTO","SEPC","NWPN","NWPS") & CTD$DEPTH<2400,],NEW_PLOT=T,Z_MAX=2400,THIN=5,COLOR="yellow")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CTD_SECTION=function(DATA,DIRECTION="LAT",Z_MAX=2000,Z_TYPE="contour")
{require(oce)
 CTD_LIST=DATA[!duplicated(DATA$CAST),c("CAST","LAT","LONG")]
 CTD_LIST=CTD_LIST[order(CTD_LIST[,DIRECTION]),]
 DATA$SAL=0
 for(i in CTD_LIST$CAST) 
 {assign(paste("CTD_",i,sep=""),as.ctd(salinity=DATA[DATA$CAST==i,"SAL"], temperature=DATA[DATA$CAST==i,"TEMP"], 
                                       pressure=DATA[DATA$CAST==i,"DEPTH"]/1.006,station=as.character(i),
                                       latitude=DATA[DATA$CAST==i,"LAT"][1],longitude=DATA[DATA$CAST==i,"LONG"][1]))}
 SECTION=makeSection(get(paste("CTD_",CTD_LIST$CAST[1],sep="")))
 for(i in 2:nrow(CTD_LIST)){SECTION=SECTION+get(paste("CTD_",CTD_LIST$CAST[i],sep=""))}
 plot(sectionGrid(SECTION),which="temperature",contourLevels=seq(2,30,2),ylim=c(0,Z_MAX),
      grid=T,ztype=Z_TYPE,showStations=T,showBottom=F,xtype=ifelse(DIRECTION=="LAT","latitude","longitude"))}

CTD_TRANSECT=function(DIRECTION="LAT",REGION=c("TOTO","SEPC","NWPN","NWPS","FLST","NABA","OTBB"),
                      MONTH=c(1:12),YEAR=c(2009:2014),J_DATE=c(1:365),TYPE=c("CTD","PFL","XBT"),EXTRA_CONDITION=NA,
                      NEW_PLOT,T_MIN=0,T_MAX=30,Z_MIN=0,Z_MAX=2000,COLOR=c("blue","red","TEMP"),TEXT=F)
{DATA=CTD[CTD$REGION%in%REGION & CTD$MONTH%in%MONTH & CTD$YEAR%in%YEAR & CTD$J_DATE%in%J_DATE & CTD$TYPE%in%TYPE,]
 if(!is.na(EXTRA_CONDITION)){DATA=CTD[CTD$REGION%in%REGION & CTD$MONTH%in%MONTH & CTD$YEAR%in%YEAR & CTD$J_DATE%in%J_DATE & CTD$TYPE%in%TYPE & eval(parse(text =EXTRA_CONDITION)),]}
 if(length(COLOR)>1){COL_PAL=colorRampPalette(COLOR[1:2]);COLOR=COL_PAL(100)[as.numeric(cut(DATA[,COLOR[3]],breaks = 100))]}
 if(NEW_PLOT)
 {par(mar=c(2.5,3,3,2.5))
  plot(DATA[,DIRECTION],DATA$DEPTH,type="p",ylim=c(Z_MAX,Z_MIN-50),
       xlim=c(range(DATA[,DIRECTION])[1],range(DATA[,DIRECTION])[2]),
       col=COLOR,pch=20,cex=0.5,axes=F,xlab=NA,ylab=NA)
  mtext(DIRECTION,side=3,line=1.75,cex=1.0)
  mtext("Depth",side=2,line=1.75,cex=1.1)
  axis(side=2,at=seq(Z_MIN,Z_MAX,200),pos=range(DATA[,DIRECTION])[1])
  axis(side=3,at=seq(range(DATA[,DIRECTION])[1],range(DATA[,DIRECTION])[2],5),pos=Z_MIN-50,cex=0.7)
  axis(side=4,at=c(Z_MIN,Z_MAX),pos=range(DATA[,DIRECTION])[2],cex=0.01,labels=F,tck=0.001)
  axis(side=1,at=c(range(DATA[,DIRECTION])[1],range(DATA[,DIRECTION])[2]),pos=Z_MAX,cex=0.01,labels=F,tck=0.001)
  segments(c(T_MIN,T_MAX),Z_MIN, c(T_MIN,T_MAX), Z_MIN-50)}
 else
 {points(DATA[,DIRECTION],DATA$DEPTH,type="p",ylim=c(Z_MAX,Z_MIN-50),
         xlim=c(range(DATA[,DIRECTION])[1],range(DATA[,DIRECTION])[2]),
         col=COLOR,pch=20,cex=0.5)}
 if(TEXT==T)
 {text(DATA[DATA$TEMP<=24,DIRECTION][seq(1,nrow(DATA[DATA$TEMP<=24,]),2)],
       DATA[DATA$TEMP<=24,"DEPTH"][seq(1,nrow(DATA[DATA$TEMP<=24,]),2)],
       DATA[DATA$TEMP<=24,"TEMP"][seq(1,nrow(DATA[DATA$TEMP<=24,]),2)],pos=4,offset=0.2,cex=0.5)}}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



