setwd("C:/Users/s2993062/documents/sparrow_project")

#calsites Ton
calsites_ton<- read.csv("New_Zealand/Aquanet_project/TN-TP/TN_TP_aquanet.csv")

head(calsites_ton)

#Calsites monitoring station
calsites<-read.csv("Data_sparrow/station_data/riverwqmonitoringdata_southisland_2006-2020.csv")

colnames(calsites)

head(calsites)

calsites<- calsites[c("SiteID","LawaSiteID","CouncilSiteID","DateSampled",
           "Lat","Long","Indicator","IndicatorUnitOfMeasure",
           "Value")]

(View(calsites))

unique(calsites$Indicator)

#select only calsites with TN
calsites<-subset(calsites,calsites$Indicator == "TN")
length(unique(calsites$SiteID))
head(calsites)

#get unique stations
calsites_point<-calsites[!duplicated(calsites$LawaSiteID),]
head(calsites_point)

#river discharge data
getwd()
rivdis<- read.csv("New_Zealand/Aquanet_project/Data Original/all_lawa_River_discharge_1975_2018.csv")
head(rivdis)

#select stations that do not have discharge data
missing<-subset(calsites_point,!(toupper(calsites_point$LawaSiteID) %in% 
                                   rivdis$lawa_or_NRWQN_ID))

in_ton<-subset(missing,(toupper(missing$LawaSiteID) %in% 
                          calsites_ton$sID))

unique(missing$LawaSiteID)

####################

#save the missing data in the discharge table as a point file.
#Then use the coordinates to extract the discharge data from the
#new zealand network

library(sf)
library(rgdal)
library(raster)

missing<-missing[!duplicated(missing[c("LawaSiteID","Lat","Long")]),]
head(missing)
cal_shp<-missing
coordinates(cal_shp)<-cal_shp[c("Long","Lat")]
cal_shp<-st_as_sf(cal_shp)
#specify the CRS of cal_shp
riv<-sf::st_read("Data_sparrow/Rivers_GIS/New_Zealand_hydroatlas.shp")
st_crs(cal_shp)<-st_crs(riv)

riv_NZ<-sf::st_read("New_Zealand/Aquanet_project/Hydrological_data_NZ/rec2_rivelines.shp")

cal_shp<-st_transform(cal_shp,st_crs(riv_NZ))

#get segment that intersect with cal_shp
b<-list()
buffer_init<-20
for(i in 1:nrow(cal_shp)){
  buff<-buffer_init
  a<-st_buffer(cal_shp[i,],buff)#CH, buffer of 2 km around each station
  #select rivers within buffer
  riv_foc<-tryCatch(st_crop(riv_NZ,extent(a)),error=function(e) e)
  
  while(buff< 201 & ((class(riv_foc)[[1]] %in% c("simpleError"))|
                       nrow(riv_foc)==0)){
    buff<-buff + 20
    a<-st_buffer(cal_shp[i,],buff)
    riv_foc<-tryCatch(st_crop(riv_NZ,extent(a)),error=function(e) e)
    print(buff)
  }
  
  if(((class(riv_foc)[[1]] %in% c("simpleError"))|
      nrow(riv_foc)==0)){
    b[[i]]<-NULL}else{
      riv_foc$LawaSiteID<-a$LawaSiteID
      riv_foc$buffer<-buff
      b[[i]]<-riv_foc
    }
  print(i)
}


a<-do.call(rbind,b)
unique(a$LawaSiteID)

# 20 meter buffer: 143

cal1<-as.data.frame(a)
cal1$geometry<-NULL
subset(cal_shp,cal_shp$LawaSiteID == "wcrc-00028")

subset(cal_shp,!(cal_shp$LawaSiteID %in% cal1$LawaSiteID))

head(cal1)

#HERE YOU WOULD HAVE TO LOOK FOR DUPLICATES AND ASSIGN IT TO THE CLOSEST
#STREAM MANUALLY

cal1<-cal1[!duplicated(cal1$LawaSiteID),]
calb<-calsites[!duplicated(calsites$LawaSiteID),]


cal1<-plyr::join(cal1,calb[c("LawaSiteID","CouncilSiteID",
                        "Long","Lat")],"LawaSiteID",type ="inner")

#a<-a[!duplicated(a$LawaSiteID),]

head(cal1)

write.csv(cal1,"calsites_buffers_NZ.csv")

###################

#now merge with REC2 discharge data:

REC_runoff<-read.csv("New_zealand/REC2_geodata_version_5/runoff_REC2.csv")
head(REC_runoff)

cal1<-merge(cal1,REC_runoff[c("nzsegment","seg_ro","seg_rain",
                              "seg_catare","seg_ro_mm")],
            "nzsegment",all.x = T)

#calculate Q
#seg_ro l/s (should I divide by 1000?)

cal1$dis_m3_pyr<-cal1$seg_ro


#####################
#calculate discharge from stations wirth yearly data

rivdis<-subset(rivdis,rivdis$lawa_or_NRWQN_ID %in% toupper(calsites$LawaSiteID))

a<-strsplit(rivdis$Date,"-")
for (i in 1:length(a)) {
  a[[i]]<-a[[i]][[1]][[1]]
}
a<-as.numeric(unlist(a))

rivdis$year<-a

rivdis<-subset(rivdis,rivdis$year > 2000)

unique(rivdis$lawa_or_NRWQN_ID)
#161 sites

library(dplyr)

rivdis<-rivdis %>% group_by(lawa_or_NRWQN_ID) %>%summarise(mean = mean(Q_m3s,na.rm = T))

View(rivdis)

rivdis$lawa_or_NRWQN_ID<-tolower(rivdis$lawa_or_NRWQN_ID)
rivdis$buffer<-NA
rivdis$nzsegment<-NA
colnames(rivdis)<-c("LawaSiteID", "dis_m3_pyr","buffer","nzsegment")

#########
#now merge stations assigned based on spatial location with stations with paired data

cal1<-rbind(rivdis,cal1[c("LawaSiteID", "dis_m3_pyr","buffer","nzsegment")])

write.csv(cal1,"dischdataNZ_filter.csv")

#merge discharge information with calsites

calsites<-merge(calsites,cal1,"LawaSiteID",
                 all.x = T)

head(calsites)

#calculate mean nutrient load

a<-strsplit(calsites$DateSampled,"-")
for (i in 1:length(a)) {
  a[[i]]<-a[[i]][[3]]
}
a<-as.numeric(unlist(a))

calsites$year<-a

head(calsites)

length(unique(calsites$LawaSiteID))


#mg to kg
calsites$value_kg<-calsites$Value *1e-6
#calculate total discharge per year
calsites$dis_pyr<-calsites$dis_m3_pyr*86400*365
#calculate load (kg/year)
calsites$depvar<-calsites$value_kg*calsites$dis_pyr

#calculate mean across years


cal2<-calsites %>% group_by(LawaSiteID) %>%summarise(mean = mean(depvar,na.rm = T))
cal2<-merge(calsites,cal2,"LawaSiteID", all.x = T)
head(cal2)
cal2<-cal2[!duplicated(cal2$LawaSiteID),]
#Get data from Ton to compare
Ton_data<-read.csv("New_zealand/Aquanet_project/TN-TP/TN_TP_aquanet.csv")

cal2$LawaSiteID<-toupper(cal2$LawaSiteID)


write.csv(cal2,"New_zealand/Aquanet_project/calsites_NZ_aquanet_filt.csv")

test<-merge(Ton_data,cal2,by.x = "sID", by.y = "LawaSiteID")

plot(test$ExpCoeffLWP,test$mean)

test<-subset(calsites,(toupper(calsites$LawaSiteID) %in% Ton_data$sID))

subset(test,test$ExpCoeffLWP == max(test$ExpCoeffLWP, na.rm = T))
