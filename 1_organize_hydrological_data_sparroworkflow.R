#create SPARROW columns
rm(list = ls())
gc()

library(dplyr)

setwd("C:/Users/s2993062/documents/sparrow_project")

#file paths########

riverPath<-"Data_sparrow/Rivers_csv/rivers_hydroatlas/7020024600.csv"

#Load files

data1<-read.csv(riverPath)

colnames(data1)

#read basin data####
basins_node<-read.csv("Data_sparrow/Basins_csv/allbaslvl12.csv")
colnames(basins_node)

#merge river and basin data
data1<-merge(data1,basins_node[c("HYBAS_ID","MAIN_BAS",   
                                 "DIST_SINK",  
                                 "DIST_MAIN", 
                                 "SUB_AREA",   
                                 "UP_AREA"
)],"HYBAS_ID", all.x = T)

#remove duplicates
data1<-data1[!duplicated(data1$HYRIV_ID),]

data1$X<-NULL


#remove duplicates
data1<-data1[!duplicated(data1$HYRIV_ID),]

#-----------------------------#
########read lake data#########
#_____________________________#


#read river table with lake intersect####

#How to create the dataset:
#select only HYRIV_ID as field in the river network
#select by location and select rivers that intersect with the lake atlas 
#intersect selected rivers with lakes (only leave riv id, lake id and length)
#export table

lakes_riv_inter<-read.csv("Data_sparrow/Lakes_csv/lake_riv_intersect_USA.csv") #TASK ALERT!!!!!!!!!!!!!!!!!!!!!!!!!


#read lakes properties
lakes<-read.csv("Data_sparrow/Lakes_csv/NorthAmerica_lakes.csv") #TASK ALERT!!!!!!!!!!!
colnames(lakes)

#exclude lakes that do not have outlet 
lakes<-subset(lakes,lakes$HYRIV_RCH != 0)


#now merge lake properties with river intersection
lakes<-merge(lakes,lakes_riv_inter, "Hylak_id", all = T)

lakes<-subset(lakes,!is.na(lakes$HYRIV_ID))


#areal hydraulic load: 
#ratio of the outflow rate for the reservoir and the surface area 

#convert lake area to meters:
lakes$Lake_area_m<-lakes$Lake_area*1e6

#convert long-term discharge flowing cubic meters per second 
#to cubic meters per day

#calculate hydraulic load
lakes$hload<- lakes$Dis_avg/lakes$Lake_area_m

#First identify outlet reaches that did not intersect in the spatial join
outlets<-subset(lakes,lakes$HYRIV_RCH !=0)
head(outlets)

#split lake data by lake ID
df<-split(outlets,outlets$Hylak_id)

#check that all outlets have a lake assigned
for(k in 1:length(df)){
  test<-df[[k]]
  #identify the lake outlet
  outlets_good<-subset(test,(test$HYRIV_RCH == test$HYRIV_ID))
  #check if the outlet has not been found
  if(nrow(outlets_good)<1){
    #assign outlet reach
    test$HYRIV_ID<-test$HYRIV_RCH
    test<-test[!duplicated(test$HYRIV_ID),]
  }else{
    test<-outlets_good
  }
  print(nrow(test))
  df[[k]]<-test
}

df<-do.call(rbind,df)

#check that all outlet reaches in df are represented in
#the lake dataframe (nrow must be == 0)

a<-lakes[which(!(lakes$HYRIV_RCH %in% df$HYRIV_RCH)),]
nrow(a)
head(df)

plyr::count(df$HYRIV_RCH)

#merge with lake data
lakes<-rbind(lakes,df)
#eliminate duplicates
lakes<-lakes[!duplicated(lakes[c("HYRIV_ID","HYRIV_RCH","Hylak_id")]),]

#merge data1 with lake data to identify reaches inside lakes and outlet points
data1<-merge(data1,lakes[c("HYRIV_ID","Hylak_id",
                           "HYRIV_RCH","HYRIV_CAT",
                           "hload", "length_int")],"HYRIV_ID", all.x = T)


#rename column
#riv_length_lake<-dplyr::rename(riv_length_lake, LENGTH_KM_in = LENGTH)
data1<-dplyr::rename(data1,  LENGTH_KM_in =length_int)
data1$LENGTH_KM_in<-data1$LENGTH_KM_in/1000 #MAKE SURE YOU CALCULATED THE LENGTH IN METERS

data1[which(is.na(data1$LENGTH_KM_in)),
                "LENGTH_KM_in"]<-0

hist(data1$LENGTH_KM_in)

#calculate the proportion of each stream that is inside each lake
data1$length_perdif<- abs(1 - (data1$LENGTH_KM-data1$LENGTH_KM_in)/data1$LENGTH_KM)

#identify reaches completely inside lakes
inlake<-which(data1$LENGTH_KM < data1$LENGTH_KM_in)
a1<-which(data1$length_perdif > 0.7 & data1$LENGTH_KM < 10)
a2<-which(data1$length_perdif > 0.8)
which((inlake %in% c(a1,a2)))

#stream codes
inlake<- unique(data1[c(a1,a2,inlake),"HYRIV_ID"])

#Reach type indicators############# 

#(0=reach; 1=reservoir internal reach; 2=reservoir outlet reach;
#                      3=coastal segment)

data1[which(data1$NEXT_DOWN != 0 & data1$ENDORHEIC == 0), "rchtype"] <- 0 #reach
data1[which(data1$HYRIV_ID %in% inlake),"rchtype"] <- 1#inside reservoir column created by spatial join
data1[which(data1$HYRIV_RCH == data1$HYRIV_ID),"rchtype"] <- 2#reach outlet
data1[which(data1$NEXT_DOWN == 0 & data1$ENDORHEIC == 0),"rchtype"] <- 3#coastal reach

#summarize hydraulic area info per reach####
df <- data1 %>% 
  group_by(HYRIV_RCH,rchtype) %>% 
  summarise_at(vars(hload), sum, na.rm = T)

head(df)

#select the maximum reach type (if it selects 2 it means is an outlet)
#0 won't assign hydraulic load

df <- df %>% 
  group_by(HYRIV_RCH) %>% 
  summarise_at(vars(rchtype,hload), max, na.rm = T)

#merge hload data with data1
data1<-merge(data1,df,by.x = "HYRIV_ID",
             by.y = "HYRIV_RCH",
             all.x = T)

#select only the highest reach type
data1 <- data1 %>% 
  group_by(HYRIV_ID) %>% 
  top_n(1, rchtype.x)%>%
  dplyr::select(-c(
    hload.x
  )) %>%
  unique()

data1<-dplyr::rename(data1,rchtype = rchtype.x)

#assign transport indicators#####

#"if transport" indicator (0=no; 1=yes);
#nontransport reaches, such as coastal and
#lake shoreline segments, should be set to zero
data1[which(data1$NEXT_DOWN == 0 & data1$ENDORHEIC == 0),"iftran"] <- 0 #coastal reach
data1[which(data1$NEXT_DOWN != 0  |  data1$ENDORHEIC != 0),"iftran"] <- 1 
data1[which(data1$NEXT_DOWN == 0) ,"iftran"] <- 0 #coastal reach

#create terminal flag indicators####

data1$termflag<-0

data1[which(data1$ENDORHEIC == 1),"termflag"] <- 4#endorreic reach
data1[which(!(data1$NEXT_DOWN %in% data1$HYRIV_ID) & 
              data1$ENDORHEIC != 1 &
              data1$ORD_CLAS == 1),
      "termflag"] <- 1#stream reach
data1[which(data1$NEXT_DOWN == 0 & data1$ENDORHEIC == 0),
      "termflag"] <- 3#coastal reach
data1[which(data1$rchtype ==1), "termflag"] <- 2

subset(data1,data1$termflag==2)

# termflag gives the type of terminal reach (1=reach; 3=coastal or
# reservoir shoreline segment)
data1$target<-0

data1[which(data1$termflag %in% c(1,3)),
      "target"]<-1

g<-subset(data1,data1$target==1)
unique(g$HYRIV_ID)


View(data1[which(duplicated(data1$HYRIV_ID)),])

write.csv(data1,riverPath)
