setwd("C:/Users/s2993062/documents/sparrow_project")

#calibration data
   
#calsites<-read.csv("Data_sparrow/station_data/5030082270_NZ_stations.csv")
#calsites<-read.csv("New_zealand/Aquanet_project/calsites_NZ_aquanet_filt.csv")
#calsites<-read.csv("stations/calsites_NZ_hyriv_apr_ton.csv")

calsites<-read.csv("stations/stations_Brazil.csv")

colnames(calsites)

calsites<-calsites[c("HYRIV_ID","load_ton_kg_year","sID","nzsegment",
                     "Long","Lat",
                     "load_kg_year",
                     "depvar")]

unique(calsites$HYRIV_ID)
unique(calsites$load_ton_kg_year)
unique(calsites$load_kg_year)

#Read river network data####

library(sf)
library(raster)
library(rgeos)
library(rgdal)

riv<-sf::st_read("Data_sparrow/Rivers_GIS/New_Zealand_hydroatlas.shp")

#create station point shapefile

cal_shp<-calsites
coordinates(cal_shp)<-cal_shp[c("Long","Lat")]
cal_shp<-st_as_sf(cal_shp)
#specify the CRS of cal_shp
#st_crs(cal_shp)<-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
st_crs(cal_shp)<-st_crs(riv)

if(st_crs(cal_shp)[[1]]!=st_crs(riv)[[1]]){
  cal_shp<-st_transform(cal_shp,st_crs(riv))
}

for(m in 1:nrow(cal_shp)){
  #a<-subset(cal_shp,cal_shp$station_id == "MSCW-.2-B-14A53")
  #a<-st_buffer(a,2000)
  a<-st_buffer(cal_shp[m,],2000)#CH, buffer of 2 km around each station
  #select rivers within buffer
  riv_foc<-tryCatch(st_crop(riv,extent(a)),error=function(e) e)
  
  if((class(riv_foc)[[1]] %in% c("simpleError"))|nrow(riv_foc)==0){
    
    cal_shp[m,"HYRIV_ID"]<-NA}else{
      #calculate absolute difference
      riv_foc$ab_dif<-abs((a$dis_m3_pyr-(riv_foc$dis_m3_pyr)))
      #select river with the lowest difference
      riv_foc<-riv_foc[which(abs(riv_foc$ab_dif) == min(abs(riv_foc$ab_dif))),]
      #add river attributes to the station shapefile
      cal_shp[m,"dis_m3_pyr_hyd"]<-riv_foc$dis_m3_pyr
      cal_shp[m,"ab_dif"]<-riv_foc$ab_dif
      cal_shp[m,"HYRIV_ID"]<-riv_foc$HYRIV_ID
    }
  gc()
  print(m)
}

#Identify rivers with the biggest differences
check<-subset(cal_shp,cal_shp$ab_dif > 10 | cal_shp$ab_dif < -10)
check<-as.data.frame(check)
head(check)

hist(cal_shp$ab_dif)
subset(cal_shp,cal_shp$ab_dif == max(cal_shp$ab_dif, na.rm = T))

cals<-as.data.frame(cal_shp)
#remove column geometry
cals$geometry<-NULL

head(cals)

#calculate proportion of the total discharge area represented by the absolute difference
cals$perd<-cals$ab_dif/cals$dis_m3_pyr

#flag sites have a perd > 0.1 and the discharge is > 50 (we can play with these values)
cals[which(cals$perd > 0.1 & cals$dis_m3_pyr > 50),"check"]<-"yes"

head(cals)


############################

dup_cal<-calsites[duplicated(calsites$HYRIV_ID),]
dup_cal<-dup_cal[complete.cases(dup_cal),]

calsites<-subset(calsites,!(calsites$HYRIV_ID %in% dup_cal$HYRIV_ID))

calsites<-rbind(calsites,dup_cal)

########

data1<-merge(data1,calsites,"HYRIV_ID",all.x = T)

data1<-data1[!duplicated(data1$HYRIV_ID),]

#identify nutrient sites#####

data1[which(data1$Load > 0),"calsites"]<-1

nrow(subset(data1,data1$calsites == 1))

#the calibration sites MUST have a discharge value in m^3/year (also let's agree on a name)

