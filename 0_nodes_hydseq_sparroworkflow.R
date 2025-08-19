rm(list = ls())
library(sf)
library(terra)
gc()


setwd("C:/Users/s2993062/documents/sparrow_project")

myfiles<-list.files("Data_sparrow/Rivers_csv/rivers_hydroatlas",".csv")

list_csv<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "dbf"
     |file_ext(myfiles[[i]]) == "ovr"|file_ext(myfiles[[i]]) == "cpg"
     |file_ext(myfiles[[i]]) == "tfw"|file_ext(myfiles[[i]]) == "lock"
     |file_ext(myfiles[[i]]) == "zip"){
    list_csv[[i]]<-NULL
  }else{
    list_csv[[i]]<-myfiles[[i]]
    
  }
}

list_csv<-unlist(plyr::compact(list_csv))


#file paths########

riverPath<-"Data_sparrow/Rivers_csv/rivers_hydroatlas/7020024600.csv"

#CREATE LOOP HERE

#read the rivers dataset#
rivs<-read.csv(riverPath)
head(rivs)
colnames(rivs)

#assign FNODEs using HYRIV_ID

rivs<-rivs[order(rivs$HYRIV_ID),]
rivs$fnode<-rivs$HYRIV_ID

#split by next down to get TNODES
data1<-split(rivs,rivs$NEXT_DOWN)

pb<-txtProgressBar(min = 0, max = length(data1), 
                   style = 3 
                   )

#Assign terminal nodes based on next down and hyriv_id columns
for (i in 1:length(data1)){
  setTxtProgressBar(pb,i)
  focus<-data1[[i]]
  tnode<-subset(rivs,rivs$HYRIV_ID %in% focus$NEXT_DOWN)
  if(nrow(tnode)>0){
    focus$tnode<- tnode$fnode
  }else{
    focus$tnode<- focus$NEXT_DOWN
  }
  data1[[i]]<-focus
  
}

#Merge data
datariv<-plyr::compact(data1)
datariv<-do.call(plyr::rbind.fill,datariv)

#order by FNODE
datariv<-datariv[order(datariv$fnode),]

#get TNODES = 0
tnode0<-subset(datariv,datariv$tnode ==0)
#get TNODES != 0
tnode<-subset(datariv,!(datariv$HYRIV_ID %in% tnode0$HYRIV_ID))

#assign tnodes TO NEXT DOWN = 0
tnode0$tnode<-1:(nrow(tnode0))

#merge data
datariv<-rbind(tnode,tnode0)

#THIS WILL OVERWRITE YOUR ORIGINAL FILE!
write.csv(datariv,riverPath)
