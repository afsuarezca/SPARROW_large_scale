setwd("~/")

rm(list = ls())

#Data access: https://conservancy.umn.edu/handle/11299/197613
#

library(sf)
library(raster)
hola<-read.csv("Atmospheric_deposition/inorganic_N_deposition.csv")#Kg per sqr km
colnames(hola)
#N_dep<-rasterFromXYZ(hola[c(2,1,39)], res = c(0.08333333, 0.08333333))
#plot(N_dep)

setwd("C:/Users/s2993062/documents/sparrow_project")

#basins<-read_sf("./Data_sparrow/Basins_GIS/6020014330_lvl12.shp")
basins<-read_sf("./Data_sparrow/Basins_GIS/NZ5030082270_REC2.shp")

colnames(basins)

rivers<-read.csv("stations/sparrow_ton_stations_ord4.csv")
#data1<-read.csv("Data_sparrow/Rivers_csv/6020014330_sparrow.csv")


basins<-subset(basins,basins$HYBAS_ID %in% data1$HYBAS_ID)

df<-list()

for(i in 1:nrow(basins)){
  bas<-basins[i,]
  e <- raster::extent(bas)
  a<-subset(hola,hola$longitude >e[1]-2 & hola$longitude < e[2]+2)
  b<-subset(a,a$latitude>e[3]-2 & a$latitude < e[4] + 2)
  b
  r<-raster(xmx = max(b$longitude)+2,xmn = min(b$longitude)-2,
            ymx = max(b$latitude)+2.5,ymn = min(b$latitude)-2.5,
            nrow= length(unique(b$latitude)),
            ncol= length(unique(b$longitude)))

  #r <- raster(e, ncol=4320/25, nrow=2160/25

  N_dep<-rasterize(b[, 2:1], r, b[,27], fun=mean)
  N_dep <- disaggregate(N_dep, fact=200)
  #plot(N_dep)
  #plot(bas,add = T)

  #m<-crop(N_dep,basins[1,])
  m<-fasterize::fasterize(bas,N_dep)
  m<-mask(N_dep,m)
  #plot(m)

  a<-data.frame(rasterToPoints(m))
  
  if(nrow(a) == 0){
    a<-data.frame(layer = mean(b[,27]))
  }
  
  #d<-data.frame(bas$nzsegment, mean(a$layer))
  d<-data.frame(bas$HYBAS_ID, mean(a$layer))
  colnames(d)<-c("HYBAS_ID","N_dep")

  df[[i]]<-d
  print(i)  
}

timestamp()

a<-do.call(rbind,df)

data1<-read.csv("Data_sparrow/Rivers_csv/6020014330_sparrow_may.csv")
#data1<-read.csv("stations/sparrow_ton_stations_ord4.csv")

head(data1)

#colnames(a)<-c("nzsegment","N_dep")

data1<-merge(data1,a,"HYBAS_ID")

data1$ndep<-data1$N_dep*data1$demiarea

#data1<-data1[c(1,48,49)]

#write.csv(data1,"Data_sparrow/ndep_NZ_5.csv")
write.csv(data1,"Data_sparrow/Rivers_csv/6020014330_sparrow_may.csv")  
#############
df_sparrow<-read.csv("Data_sparrow/Rivers_csv/5030082270_sparrow_feb.csv")

hist(hola$pixel_area_km2)

cell_sizes<-plyr::count(hola$pixel_area_km2)

for(i in 1:nrow(cell_sizes)){
  s<-subset(hola,hola$pixel_area_km2 == cell_sizes$x[i])
  coord<-s[2:1]
  colnames(coord)<-c("x","y")
  # set up an 'empty' raster, here via an extent object derived from your data
  e <- raster::extent(coord[2:1])
  
  
  #pixelsize<-cell_sizes$x[i]
  # a<-sort(coord$x)
  # a<-a[!duplicated(a)]
  # b<-sort(coord$y)
  # b<-b[!duplicated(b)]
  # resx<-abs(a[2]-a[1])
  # resy<-abs(a[2]-a[1])
}


e[2]<-180
e[3]<--90
e[4]<-90

writeRaster(N_dep,"N_dep.tif",overwrite = T)
