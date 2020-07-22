## This script reads the CSV files saved from the Ornitela website ##
## It cleans the data, plot the tracks, creates a metadata table etc. ###


## load packages if not installed install them first
library(ggplot2)
library(ggmap)
require(adehabitatHR);#loads also: require(CircStats); #require(boot) ; require(MASS) ;  

#### key parameters used for filters and analysis ####
##input of relevant deployment-removal dates for each tags for filtering#
# TagsDeployDays=data.frame(
#   tag=       c(192022,       192023,                  192024,       192025,                   192026,                    192027,               192028),
#   DeployDate=c('2019-07-16','2019-07-16',            '2019-12-24', '2020-03-12'            ,'2020-03-12',              '2020-03-12',          '2020-03-12'),
#   LastDay=   c('2019-07-30',as.character(Sys.Date()),'2020-01-02',as.character(Sys.Date()),as.character(Sys.Date()),as.character(Sys.Date()),as.character(Sys.Date())),#use Sys.Date() if device is still active!!! 
#   stringsAsFactors = F) 

MinSatNumPerFix=5 #minimum number of satellites to consider a fix as acceptable
MaxHDOPperFix=2.1 #maximal HDOP value not to be filtered
MaxAltitude=2000; #maximal altitude considered as non error
HomePc=0
RefetLonLat= c( 35.111596,31.819926)#center of the refet
DistThreshold_In_Refet=0.2#200 m is within the refet 
MinLast4daysDisplcm=500 # the minimul distance of daily displacment over the last 4 days to be consided alive! Last4daysDisplcm
PlotAll_withReloctd=F
MinNumPointPerDay=20;#days with less than this number of points will be filtrered.

cat("\014") #clean screen

#### reading the folder with the csv files- YOU NEED TO UPDATE to your computer path ####
if (HomePc==1) {
path1='D:/OrrS2/Box Sync/AA TAU/Miranda Crafton Pigeons Shay/GPS Data as CSV' ## this is the folder to read home pc
}else{
#path1='D:\\OrrS\\Documents\\Box Sync\\AA TAU\\Miranda Crafton Pigeons Shay\\GPS Data as CSV' #lab pc
  path1='C:\\Users\\OrSNB1\\Box Sync\\AA TAU\\Miranda Crafton Pigeons Shay\\GPS Data as CSV'
  }

#read data csv files
filenames <- list.files(path1, pattern="*.csv", full.names=TRUE)#get names of all csv in this folder
list_of_df <- lapply(filenames, read.csv)# read all files in the list of names
names(list_of_df) <- substr(filenames,(nchar(path1)+2),(nchar(path1)+7))# setting the names from the unit
All_CSV_data=do.call(rbind.data.frame, list_of_df) #merge the different files into one dataframe

# read the XLSX file with deployment and removal\mortality dates for each one
TagsDeployDays=as.data.frame(readxl::read_excel(path=paste(path1,"/DeploymentTable.xlsx",sep=''),col_names=T))
TagsDeployDays$LastDay[is.na(TagsDeployDays$LastDay)]=Sys.Date()

#### initial coverting time formats ####
All_CSV_data$UTC_datetime=as.POSIXct(All_CSV_data$UTC_datetime,tz='UTC')#date tim combined
All_CSV_data$UTC_date=as.Date( as.character(All_CSV_data$UTC_date)) #date only
All_CSV_data$UTC_time=as.POSIXct(as.character(All_CSV_data$UTC_time) ,format ='%H:%M:%S', origin = "2000-01-01",tz ='UTC')
  
#previous attemps  
#All_CSV_data$UTC_time= as.POSIXct(as.character(All_CSV_data$UTC_time) %% 86400, origin = "2000-01-01",tz ='UTC')
#head(All_CSV_data$UTC_time);range(All_CSV_data$UTC_time)#just checking
#All_CSV_data$UTC_time= chron::chron(times=as.character(All_CSV_data$UTC_time))
#All_CSV_data$UTC_time=as.POSIXct(as.numeric(All_CSV_data$UTC_time) %% 86400, origin = "2000-01-01",tz ='UTC')
#All_CSV_data$UTC_time=strftime(All_CSV_data$UTC_time, format="%H:%M:%S",tz ='UTC')
#head(All_CSV_data$UTC_datetime);range(All_CSV_data$UTC_datetime)#just checking

#str(All_CSV_data)

#### subsetting data to the relevant dates only####
## making sure all tags ate updated
if (length(unique(All_CSV_data$device_id))>length(TagsDeployDays$tag)){print('error! update the dates in the deployment table'); break()}

##loop on values to remove non relevant dates for each tag 
TagsDeployDays$DeployDate=as.Date(TagsDeployDays$DeployDate, format='%Y-%m-%d')
TagsDeployDays$LastDay=as.Date(TagsDeployDays$LastDay, format='%Y-%m-%d')

#ActiveDates=seq(from =TagsDeployDays$DeployDate[1], to=TagsDeployDays$LastDay[1],by="day")
indx=NULL#indices of lines to remove
for (ii in 1:length(unique(All_CSV_data$device_id))){
  indx=c(indx, #values from previous tag
         which(All_CSV_data$device_id==TagsDeployDays$tag[ii] & All_CSV_data$UTC_date<=TagsDeployDays$DeployDate[ii]), #this tag too early, including the deploymend day!
         which(All_CSV_data$device_id==TagsDeployDays$tag[ii] & All_CSV_data$UTC_date> TagsDeployDays$LastDay[ii])) #this tag after death or removal
  }#loop on tags
All_CSV_data=All_CSV_data[-indx,]#deleting all non relevant lines
All_CSV_data$device_id=as.factor(All_CSV_data$device_id)


## splitting into two deparate dataframes
GPS_data=All_CSV_data[ which(All_CSV_data$datatype=='GPS'),]
ACC_data=All_CSV_data[ which(All_CSV_data$datatype=='SENSORS'),]
#GPS_data$datatype=droplevels(GPS_data$datatype)

#### basic histograms and filtering ####
qplot(GPS_data$satcount, geom="histogram", binwidth=1,  main='number of satellites used', ylab='N_of_Fixes') +theme_bw()
qplot(GPS_data$hdop,     geom="histogram", binwidth=0.2,main='values of hdop'           , ylab='N_of_Fixes') +theme_bw()
qplot(GPS_data$Altitude_m,     geom="histogram", binwidth=50,main='values of Altitude_m', ylab='N_of_Fixes') +theme_bw()

#length(GPS_data$hdop)
## filtering by extreme values
GPS_dataFlt <- GPS_data[ which(GPS_data$satcount>=MinSatNumPerFix  & GPS_data$hdop <= MaxHDOPperFix & GPS_data$Altitude_m <= MaxAltitude), ] # 10380/11344=91.5% so these criteria will lose 8.5% of the data

#removing duplicated points ???? not sure why they exsit
Duplicated=Reduce(intersect, list(which(duplicated(GPS_dataFlt$UTC_datetime)),
                                  which(duplicated(GPS_dataFlt$device_id)),
                                  which(duplicated(GPS_dataFlt$UTC_time))))
#dupz = which(duplicated(GPS_dataFlt$UTC_datetime));unique(Duplicated==dupz)
GPS_dataFlt = GPS_dataFlt[-Duplicated,]
dim(GPS_dataFlt)
#View(GPS_dataFlt)

## plotting AGAIN after removing outlier - to see if they changes as they should have
qplot(GPS_dataFlt$satcount,  geom="histogram", binwidth=1,  main='number of satellites used', ylab='N_of_Fixes') +theme_bw()
qplot(GPS_dataFlt$hdop,      geom="histogram", binwidth=0.2,main='values of hdop'           , ylab='N_of_Fixes') +theme_bw()
qplot(GPS_dataFlt$Altitude_m,geom="histogram", binwidth=50 ,main='values of Altitude_m'     , ylab='N_of_Fixes') +theme_bw()

#removing unnecessary columns
GPS_dataFlt=GPS_dataFlt[!names(GPS_dataFlt) %in% c("datatype",'hdop','satcount', "mag_x", "mag_y",'mag_z',"acc_x", "acc_y",'acc_z', 'X')]
ACC_data=ACC_data[!names(ACC_data) %in% c("datatype",'satcount','U_bat_mV','bat_soc_pct','solar_I_mA','hdop','Latitude','Longitude', 'Altitude_m','speed_km_h','direction_deg','temperature_C',"mag_x", "mag_y",'mag_z','X')]
GPS_dataFlt$device_id=droplevels(GPS_dataFlt$device_id);#dropping levels with no data


##printing for user the data count
print(paste('using the files in: ',path1 ))
print(paste('we have',length(unique(GPS_dataFlt$device_id)),'individuals in the data', sep=' '))
print(paste('we have',dim(GPS_dataFlt)[1],'fixes in the data, after some very basic filtering that excluded',
            round(100*(1-dim(GPS_dataFlt)[1]/dim(GPS_data)[1]),digits=2),'% of the data', sep=' '))



## adding ditance to Refet and days as burst #### 
#bursts as counts with a different name for each indiv
GPS_dataFlt$DaysAsBurst=as.factor(paste(GPS_dataFlt$device_id,(1+difftime(GPS_dataFlt$UTC_date,min(GPS_dataFlt$UTC_date),units='days')),sep="_"));
#distance to the Refet 
GPS_dataFlt$DistToRefet_Km= sp::spDistsN1(pts=as.matrix(GPS_dataFlt[,c("Longitude","Latitude")]), pt=RefetLonLat, longlat = T)
GPS_dataFlt$In_Refet=GPS_dataFlt$DistToRefet_Km<=DistThreshold_In_Refet#within the Refet Or Not?
  names(GPS_dataFlt); 

  #rownames(GPS_dataFlt) <- c()# getting rid of row names


### converting Data Frame to Adehabitat object as.ltraj class in both LatLon and UTM coordinate #####
# as.ltraj Lat Lon
GPS_dataFltWGS_Ltrj <- as.ltraj(xy=coordinates(GPS_dataFlt[,c("Longitude","Latitude")]),
                            typeII = TRUE,
                            date=GPS_dataFlt$UTC_datetime,
                            id=GPS_dataFlt$device_id,
                            burst = GPS_dataFlt$DaysAsBurst,
                            proj4string= CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                            infolocs =GPS_dataFlt)#,infolocs =GPS_dataFltCoor
GPS_dataFltWGS_DF=ld(GPS_dataFltWGS_Ltrj)#now converting back to DataFrme for later

#class(GPS_dataFltWGS_Ltrj)
#GPS_dataFltWGS_Ltrj[2]
#head(GPS_dataFltWGS_Ltrj[[1]])
#plot.ltraj(GPS_dataFltWGS_Ltrj)
#plotltr(GPS_dataFltWGS_Ltrj, which="dist")


## converting to UTM36North, 
utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north
rownames(GPS_dataFlt) <- c()# getting rid of row names
GPS_dataFlt2=coordinates(GPS_dataFlt[,c("Longitude","Latitude")])
GPS_dataFlt2=SpatialPointsDataFrame(coords=GPS_dataFlt2, 
                                    data=GPS_dataFlt,   
                                    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )) #+proj=utm +zone=10+datum=WGS84"
class(GPS_dataFlt2)
Dataset_utmN <- as.data.frame(spTransform(GPS_dataFlt2, CRS(utmN)))
head(Dataset_utmN)

GPS_dataFltUTMLtrj <- as.ltraj(xy=coordinates(Dataset_utmN[,c("Longitude.1","Latitude.1")]),
                            typeII = TRUE,
                            date=Dataset_utmN$UTC_datetime,
                            id=Dataset_utmN$device_id,
                            burst = Dataset_utmN$DaysAsBurst,
                            proj4string= CRS(utmN),
                            infolocs =Dataset_utmN[,c('Altitude_m','UTC_time','UTC_date','UTC_datetime',"DaysAsBurst","DistToRefet_Km" ,"In_Refet" ,'direction_deg',"temperature_C",
                                                      'solar_I_mA','bat_soc_pct','U_bat_mV')])#,infolocs =GPS_dataFltCoor
head(GPS_dataFltUTMLtrj[[1]])
plot.ltraj(GPS_dataFltUTMLtrj)
GPS_dataFltUTMLtrj[1]
GPS_dataFltUTM_DF=ld(GPS_dataFltUTMLtrj)#now converting back to DataFrme for later


# #setting coordinate systems
# xysp <- SpatialPoints(GPS_dataFlt[,c("Longitude","Latitude")])
# proj4string(xysp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #NEED TO check cooridnate system
# #not sure why this step needed but it worked
# GPS_dataFltCoor=as.data.frame(GPS_dataFlt[!names(GPS_dataFlt) %in% c("Longitude","Latitude") ])#as.data.frame(xysp),
# coordinates(GPS_dataFltCoor)=data.frame(xysp)
# 
# coordinates(GPS_dataFlt)
# # as.ltraj 
# GPS_dataFltLtrj <- as.ltraj(xy=coordinates(GPS_dataFlt[,c("Longitude","Latitude")]),typeII = TRUE,date=GPS_dataFlt$UTC_datetime,id=GPS_dataFlt$device_id,burst = GPS_dataFlt$DaysAsBurst)#,infolocs =GPS_dataFltCoor
# GPS_dataFltLtrj <- as.ltraj(xy=coordinates(GPS_dataFltCoor),typeII = TRUE,date=GPS_dataFlt$UTC_datetime,id=GPS_dataFlt$device_id,burst = GPS_dataFlt$DaysAsBurst,infolocs =GPS_dataFltCoor)
# 
# 
# rm(xysp)
# 
# xysp <- SpatialPoints(GPSdataset3or4h[,c("Easting","Northing")])
# proj4string(xysp) <- CRS("+proj=utm +zone=36 +ellps=WGS84") #NEED TO check cooridnate system
# 
## GPSdatasetFltrDF=ld(GPSdatasetFltr)
# GPSdataset3or4hDF=ld(GPSdataset3or4h)
# GPSdataset1hDF=ld(GPSdataset1h)

#### DataFrame By Day by Pigeon #####
DailyData=data.frame(Burst=unique(GPS_dataFltUTM_DF$burst))
DailyData$id=DailyData$date=DailyData$SumDailyTrvlDist_M=DailyData$MxDailyDisplcmnt=DailyData$NetDailyDisplcmnt=NA
DailyData$N_ofPoints=DailyData$MaxdistToRefet=DailyData$Prop_Points_InRefet=NA
DailyData$RoostLat=DailyData$RoostLon=NA

## a loop for string data on every day by every indiviual -(the bursts)
for (burstCnt in 1:length(unique(GPS_dataFltUTM_DF$burst))){
  indx=which(as.character(GPS_dataFltUTM_DF$burst)==as.character(DailyData$Burst[burstCnt]))#finding the lines of this current burst in the main dataframe
  
  DailyData$N_ofPoints[burstCnt]=length(indx)
  DailyData$id[burstCnt]=as.character(GPS_dataFltUTM_DF$id[indx[1]])
  DailyData$date[burstCnt]=GPS_dataFltUTM_DF$UTC_datetime[indx[1]]
  DailyData$SumDailyTrvlDist_M[burstCnt]=round(sum(GPS_dataFltUTM_DF$dist[indx],na.rm=T),digits = 2)
  DailyData$MxDailyDisplcmnt[burstCnt]=sqrt(max(GPS_dataFltUTM_DF$R2n[indx],na.rm=T))
  DailyData$NetDailyDisplcmnt[burstCnt]=sqrt(tail(GPS_dataFltUTM_DF$R2n[indx],na.rm=T,1))
  DailyData$MaxdistToRefet[burstCnt]=max(GPS_dataFltUTM_DF$DistToRefet_Km[indx])
  DailyData$Prop_Points_InRefet[burstCnt]=sum(GPS_dataFltUTM_DF$In_Refet[indx])/length(indx)
  DailyData$RoostLat[burstCnt]=GPS_dataFltWGS_DF$Latitude[tail(indx,n = 1)]
  DailyData$RoostLon[burstCnt]=GPS_dataFltWGS_DF$Longitude[tail(indx,n = 1)]
  }
## simple processing of the formats and sorting by pigeons and days
rm(burstCnt,indx)
DailyData$id=as.factor(DailyData$id)
DailyData=droplevels(DailyData)
DailyData$date=as.POSIXct((DailyData$date), format="%Y-%m-%d ",tz="UTC",origin="1970-01-01")

DailyData <- DailyData[order(DailyData$id,DailyData$date),]



#simple histograms 
hist(DailyData$SumDailyTrvlDist_M,breaks=30)
hist(DailyData$MxDailyDisplcmnt,breaks=30)
hist(DailyData$N_ofPoints[DailyData$N_ofPoints<100],breaks=30)

summary(DailyData)

#### Cecking if there is a problem with the piegoens, did they die? #####
for (ii in 1:length(unique(DailyData$id))){#loop on tags
  CurID=as.character(unique(DailyData$id)[ii])
  #print(CurID)
  indx=which(as.character(DailyData$id)==CurID)
  Last4daysDisplcm=tail(DailyData$MxDailyDisplcmnt[indx],n = 4)
  if (max(Last4daysDisplcm)<MinLast4daysDisplcm){
    print(paste('there is a problem with tag',CurID,'check if its not dead!!!' ,sep=' '))
    print(paste('Its max  displacement over the last 4 days was', round(max(tail(DailyData$MxDailyDisplcmnt[indx],n = 4))),'meters',sep=' '))
    print(paste('number of fixes over the last 4 days are', sep=' '));print(tail(DailyData$N_ofPoints[indx],n = 4))
    }#was below minimum?
}#loop on tags

#### removing days with less than minimum data points
DailyDataFlt2=subset(DailyData,N_ofPoints>=MinNumPointPerDay)
hist(DailyDataFlt2$SumDailyTrvlDist_M,breaks=30)
hist(DailyDataFlt2$MxDailyDisplcmnt,breaks=30)
hist(DailyDataFlt2$N_ofPoints[DailyData$N_ofPoints<100],breaks=30)

#### a meta data table by individual across all dayes #####
## adding the mean daily movement to the byIbe2 dataframe
#creating a dataset to store
ByPigeonMetadata=as.data.frame(levels(GPS_dataFlt$device_id));names(ByPigeonMetadata)='PigeonID'
ByPigeonMetadata$DaysOfTracking=ByPigeonMetadata$lastDay=ByPigeonMetadata$FirstDay=NA;
ByPigeonMetadata$SamplingIntervals=ByPigeonMetadata$N_of_Fixes=NA;
ByPigeonMetadata$NetDailyDisplcmnt=ByPigeonMetadata$MxDailyDisplcmnt=NA;
ByPigeonMetadata$MeanSumDailyTrvlDist_M=NA

## storing data with sapply instaead of a loop
ByPigeonMetadata$MxDailyDisplcmnt=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){mean(na.rm=T,DailyData$MxDailyDisplcmnt[as.character(DailyData$id)==i ])})#
ByPigeonMetadata$MeanSumDailyTrvlDist_M=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){mean(na.rm=T,DailyData$SumDailyTrvlDist_M[as.character(DailyData$id)==i ])})#
ByPigeonMetadata$NetDailyDisplcmnt=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){mean(na.rm=T,DailyData$NetDailyDisplcmnt[as.character(DailyData$id)==i ])})#
ByPigeonMetadata$FirstDay=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){as.character(min(GPS_dataFltWGS_DF$UTC_date[which(GPS_dataFltWGS_DF$id==i)]))})#
ByPigeonMetadata$lastDay=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){as.character(max(GPS_dataFltWGS_DF$UTC_date[which(GPS_dataFltWGS_DF$id==i)]))})#
ByPigeonMetadata$DaysOfTracking=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){length(unique(GPS_dataFltWGS_DF$UTC_date[which(GPS_dataFltWGS_DF$id==i)]))})#
ByPigeonMetadata$N_of_Fixes=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){length(GPS_dataFltWGS_DF$x[which(GPS_dataFltWGS_DF$id==i)])})#
ByPigeonMetadata$MeanProp_Points_InRefet=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){mean(na.rm=T,DailyData$Prop_Points_InRefet[as.character(DailyData$id)==i ])})#
ByPigeonMetadata$Days_wVisitRefet=sapply(as.character(ByPigeonMetadata$PigeonID),function(i){length(which(DailyData$Prop_Points_InRefet[as.character(DailyData$id)==i ]>0))})#



# 
# t.test(SumDailyTrvlDist_M~Sex1m2f,data=ByPigeonMetadata)
# t.test(NetDailyDisplcmnt~Sex1m2f,data=ByPigeonMetadata)
# t.test(MxDailyDisplcmnt~Sex1m2f,data=ByPigeonMetadata)
# 

#### plot tracks- plotting all tags together EXCLUDING relocated individuals ####
#ggmap package
  
GPS_dataFltNoRelocatedBirds=GPS_dataFlt[ !(GPS_dataFlt$device_id %in% c(192027,192028)), ]
Mapbox1 <- make_bbox(lon=GPS_dataFltNoRelocatedBirds$Longitude,lat=GPS_dataFltNoRelocatedBirds$Latitude, f=0.5) # defines the borders of the box
SatImagBox1<- get_map(location=Mapbox1, maptype = "terrain", zoom = 15);ggmap(SatImagBox1)
  #SatImagBox1<- get_map(location=Mapbox1, maptype="satellite", source="osm");ggmap(SatImagBox1)
  #make plot for each variable of interest - colors by tag, including points and lines
mapTry1 <- ggmap(SatImagBox1) +  theme(plot.margin = margin(0,0,0,0, "mm")) +  labs(x="longitude", y="latitude") 
mapTry1 + geom_point(  data=GPS_dataFltNoRelocatedBirds,  alpha = 0.5, aes(x=Longitude, y=Latitude, col=device_id)) +
  geom_path (  data=GPS_dataFltNoRelocatedBirds,               aes(x=Longitude, y=Latitude, col=device_id,group=device_id))



#### loop to plot with ggplot+ggmap each ibex separatly #####
#levels(GPS_dataFlt$device_id)
Color=c('red','blue','green') #coral4','blue','chocolate','brown2','darkorange1','blueviolet','coral','darkgoldenrod1','cyan2','darksalmon', #the first 12 ibex Fem in Blue, males in red-pink
#        'blue','green','rosybrown','royalblue2','darkgreen')#

for (BirdCntr in 1:length(levels(GPS_dataFlt$device_id))){
  #print(paste(levels(GPS_dataFlt$device_id)[BirdCntr],'is in color',Color[BirdCntr])) 
  indxthisPign=which(GPS_dataFlt$device_id==levels(GPS_dataFlt$device_id)[BirdCntr])#these are indices of this pigeon in the dataset
  ##creating the map for this individal
  Mapbox2 <- make_bbox(lon=GPS_dataFlt$Longitude[indxthisPign],lat=GPS_dataFlt$Latitude[indxthisPign], f=0.5)#defines the borders of the box, now by individual
  SatImagBox2<- get_map(location=Mapbox2, maptype="terrain",zoom=15)
  mapTry2 <- ggmap(SatImagBox2) +theme(plot.margin = margin(0,0,0,0, "mm")) + labs(x="longitude", y="latitude") +ggtitle(levels(GPS_dataFlt$device_id)[BirdCntr]) +
    coord_quickmap(xlim=c(Mapbox2[1], Mapbox2[3]),ylim=c(Mapbox2[2], Mapbox2[4]))  
  print(mapTry2 + geom_point(colour = Color[BirdCntr], size = 3, data=GPS_dataFlt[indxthisPign,],  alpha = 0.5, aes(x=Longitude, y=Latitude))) #plotting tracks as points
  print(mapTry2 + geom_path(colour = Color[BirdCntr],            data=GPS_dataFlt[indxthisPign,],               aes(x=Longitude, y=Latitude))) #plotting tracks as a line
}

#### plot tracks- plotting all tags together- will not work with the relocated birds included, too much area ####
#ggmap package
if (PlotAll_withReloctd){
  Mapbox1 <- make_bbox(lon=GPS_dataFlt$Longitude,lat=GPS_dataFlt$Latitude, f=0.5) # defines the borders of the box
  SatImagBox1<- get_map(location=Mapbox1, maptype = "terrain", zoom = 15);ggmap(SatImagBox1)
  #SatImagBox1<- get_map(location=Mapbox1, maptype="satellite", source="osm");ggmap(SatImagBox1)
  
  #make plot for each variable of interest - colors by tag, including points and lines
  mapTry1 <- ggmap(SatImagBox1) +  theme(plot.margin = margin(0,0,0,0, "mm")) +  labs(x="longitude", y="latitude") 
  mapTry1 + geom_point(  data=GPS_dataFlt,  alpha = 0.5, aes(x=Longitude, y=Latitude, col=device_id)) +
    geom_path (  data=GPS_dataFlt,               aes(x=Longitude, y=Latitude, col=device_id,group=device_id))
  
}
  


#### calculate HR ####
#LizDataForHRsbst2015=with(LizMatData2,data.frame(NameFactor,UTM_Easting,UTM_Northing))
#coordinates(LizDataForHRsbst2015) = c("UTM_Easting", "UTM_Northing") # specify column names

#without the relocated pigeons 
ForHR=with(GPS_dataFltUTM_DF[ !(GPS_dataFltUTM_DF$id %in% c(192027,192028)), ],data.frame(id,x,y));ForHR$id=droplevels(ForHR$id);
#ForHR=with(GPS_dataFltUTM_DF,data.frame(id,x,y));

coordinates(ForHR)=c("x","y") #UTM coordinates
proj4string(ForHR)=CRS(utmN)

#calculte UD for all pigeons, TAKES long time!
UD2020=kernelUD(ForHR, h = "href", grid = 100,same4all = TRUE,kern = "bivnorm",extent = 0.8)#, hlim = c(0.1, 1.5),  boundary = NULL
#image(UD2020)


#### Plot HRs #####
### A loop on individuals to plot each one's HR seprately
#par(mfrow = c(2, 3)) #to draw them in the same lines
for (ii in 1:length(unique(ForHR$id))){
  #get lines for current indviual
  ver99 <- getverticeshr(UD2020[[ii]],percent = 99,unin='m',unout='ha') ## home-range contours
  ver95 <- getverticeshr(UD2020[[ii]],percent = 95,unin='m',unout='ha') ## home-range contours
  ver75 <- getverticeshr(UD2020[[ii]],percent = 75,unout='ha') ## home-range contours
  ver50 <- getverticeshr(UD2020[[ii]],percent = 50,unout='ha') ## home-range contours
  
  #plot home-range contours in different colors
   plot(ver99)  + 
     plot(ver95, add=TRUE, col="yellow")+
     plot(ver75, add=TRUE, col="orange") + 
     plot(ver50, add=TRUE, col="red") +  
     points( ForHR[ForHR@data$id==levels(ForHR$id)[ii],],pch='+')   ## Plots points of this individual
}
#par(mfrow = c(1,1))

### A loop on individuals to plot all HRs together
#par(mfrow = c(2, 3)) #to draw them in the same lines
Col=c('red','blue','green','orange','brown')

plot(ver99, add=F, border='white')
for (ii in 1:length(unique(ForHR$id))){
  #get lines for current indviual
  ver99 <- getverticeshr(UD2020[[ii]],percent = 99,unin='m',unout='ha') ## home-range contours
  ver50 <- getverticeshr(UD2020[[ii]],percent = 50,unout='ha') ## home-range contours
  
  #plot home-range contours in different colors
  plot(ver99, add=TRUE, border=Col[ii]) + #col=Col[ii],alpha =10,cex=13
  plot(ver50, add=TRUE, border=Col[ii])  #,col=Col[ii],alpha =10,cex=13 +  
  #points( ForHR[ForHR@data$id==levels(ForHR$id)[ii],],pch=ii)   ## Plots points of this individual
}






########################    DRAFTS ###################
vud_points =getvolumeUD(UD2020)
image(getvolumeUD(UD2020)[[1]])
levels <- c(50, 75, 95, 99)
list <- vector(mode="list", length = 2)

list[[1]] <- as.image.SpatialGridDataFrame(vud_points[[1]])
list[[2]] <- as.image.SpatialGridDataFrame(vud_points[[2]])


# 9. Plot
#par(mfrow = c(2, 1))
image(vud_points[[1]])
contour(list[[1]], add=TRUE, levels=levels)
image(vud_points[[2]])
contour(list[[2]], add=TRUE, levels=levels)
par(mfrow = c(1, 1))


kud <- kernelUD(ForHR)  # h = href is the default - ad hoc method for determining h
ver95 <- getverticeshr(kud) ## home-range contours
plot(ver95)  + plot(ver80, add=TRUE, col="green")  +  points(xy)   ## Plots contours
ud <- kernelUD(tt, id = id, h = "href", grid = 40, same4all = FALSE,
               hlim = c(0.1, 1.5), kern = c("bivnorm"), vextent = 0.5)


image(kud) + title("Bear UD")

LizDataForHRsbst2015=with(LizMatData2,data.frame(NameFactor,UTM_Easting,UTM_Northing))
coordinates(LizDataForHRsbst2015) = c("UTM_Easting", "UTM_Northing") # specify column names
HRlogSbst5=data.frame(unique(GPS_dataFlt2$),2015,unique(LizDataForHRsbst2015$NameFactor)) ; 
names(HRlogSbst)=c('Name','Year','Name_year')





### up to here ###########







