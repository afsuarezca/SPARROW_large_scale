setwd("~/")

rm(list = ls())

library(raster)

#livestock numbers

lives<-stack(
  rb<-raster("N_fertilizer_data/Manure/Gilbert/buffalo/5_Bf_2010_Da.tif"),
  rc<-raster("N_fertilizer_data/Manure/Gilbert/cattle/5_Ct_2010_Da.tif"),
  rch<-raster("N_fertilizer_data/Manure/Gilbert/Chicken/5_Ch_2010_Da.tif"),
  rd<-raster("N_fertilizer_data/Manure/Gilbert/ducks/5_Dk_2010_Da.tif"),
  rg<-raster("N_fertilizer_data/Manure/Gilbert/Goats/5_Gt_2010_Da.tif"),
  rh<-raster("N_fertilizer_data/Manure/Gilbert/Horses/5_Ho_2010_Da.tif"),
  rp<-raster("N_fertilizer_data/Manure/Gilbert/pigs/5_Pg_2010_Da.tif"),
  rs<-raster("N_fertilizer_data/Manure/Gilbert/sheep/5_Sh_2010_Da.tif")
)

library(sf)
library(fasterize)
setwd("C:/Users/s2993062/documents/sparrow_project")

#basins<-read_sf("./Data_sparrow/Basins_GIS/5030082270_lvl12.shp")
basins<-read_sf("./Data_sparrow/Basins_GIS/5030082270_REC2.shp")

basins<-read_sf("./Data_sparrow/Basins_GIS/6020014330_lvl12_brz.shp")

#rivers<-read.csv("stations/sparrow_ton_stations_ord4.csv")

#basins<-subset(basins,basins$nzsegment %in% rivers$nzsegment)

nlives<-list()

####Calculate livestock######
for(i in 1:nrow(basins)){
  raster.basins<-crop(lives,extent(basins[i,])) #crop raster to basins extent
  raster.basins2<- fasterize(st_as_sf(basins[i,]),raster.basins[[2]]) #crops to basins edge & converts to raster
  raster.basins<-mask(raster.basins,raster.basins2)
  crs(raster.basins)<-crs(basins)
  cells<-rasterToPoints(raster.basins)
  cells<-as.data.frame(cells)
  totliv<-t(data.frame(apply(cells[3:length(cells)],2,sum)))
  totliv<-cbind(cells[1,1:2],totliv)
  totliv$HYBAS_ID<-basins[i,"HYBAS_ID"]$HYBAS_ID
  nlives[[i]]<-totliv
  #target_count[[i]]<-cells
  #N_point_raster[[i]]<-rasterFromXYZ(cells[c(1,2,5)]) 
  print(i)
}

hola<-do.call(rbind,nlives)

colnames(hola)<-c("x","y","Bf","Ct","Ch",
                  "Dk","Gt","Ho","Pg","Sh","HYBAS_ID")
head(hola)

#Kg N ANIMAL PER YEAR

hola$Bf_N<-hola$Bf * 0
hola$Ct_N<-hola$Ct * 55.01
hola$Ch_N<-hola$Ch * 0.7
hola$Dk_N<-hola$Dk * 0
hola$Gt_N<-hola$Gt * 6.02
hola$Ho_N<-hola$Ho * 0
hola$Pg_N<-hola$Pg * 12.30
hola$Sh_N<-hola$Sh * 6.30

head(hola)

hola$Manure_N<- apply(hola,1,function(x)sum(x[12:19],na.rm = T))

head(hola)

setwd("C:/Users/s2993062/documents/sparrow_project")

data1<-read.csv("Data_sparrow/Rivers_csv/6020014330_sparrow_aug.csv")

data1<-merge(data1,hola[c("HYBAS_ID",
                          "Manure_N")],"HYBAS_ID",all.x = T)

data1$MANC_N<-data1$Manure_N*data1$demiarea
data1[which(is.na(data1$MANC_N)),"MANC_N"]<-0

write.csv(data1,"Data_sparrow/Rivers_csv/6020014330_sparrow_aug.csv")
