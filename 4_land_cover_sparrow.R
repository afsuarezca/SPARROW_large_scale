

riv_glc<-data1[c("HYRIV_ID",
                   "for_pc_cse","for_pc_use", 
                   "crp_pc_cse", "crp_pc_use", 
                   "pst_pc_cse", "pst_pc_use",
                   "urb_pc_cse", "urb_pc_use")]

df_sparrow<-merge(df_sparrow,riv_glc,all.x = T,
                  by.x = "waterid",by.y ="HYRIV_ID")

head(df_sparrow,2)

#change NAs to 0
colToChange<-c("for_pc_cse", "crp_pc_cse","urb_pc_cse","pst_pc_cse")
df_sparrow[colToChange][is.na(df_sparrow[colToChange])] <- 0


#calculate percentage of urban,forest,crop and pasture per catchment
df_sparrow$LC<-df_sparrow$for_pc_cse + df_sparrow$crp_pc_cse + df_sparrow$urb_pc_cse + df_sparrow$pst_pc_cse
#calculate percentage of other land cover types


# standardized to 100#
#I do this because sometimes the sum of the 
#% of all the land cover types in the original data is > 100

df_sparrow$for_pc_cse <- df_sparrow$for_pc_cse*100/df_sparrow$LC
df_sparrow$crp_pc_cse <- df_sparrow$crp_pc_cse*100/df_sparrow$LC 
df_sparrow$urb_pc_cse <- df_sparrow$urb_pc_cse*100/df_sparrow$LC 
df_sparrow$pst_pc_cse <- df_sparrow$pst_pc_cse*100/df_sparrow$LC 

df_sparrow[colToChange][is.na(df_sparrow[colToChange])] <- 0

df_sparrow$LC2<-df_sparrow$for_pc_cse + df_sparrow$crp_pc_cse + df_sparrow$urb_pc_cse + df_sparrow$pst_pc_cse


df_sparrow$other_pc_cse<-100 - df_sparrow$LC2
df_sparrow[which(df_sparrow$other_pc_cse < 0),"other_pc_cse"]<-0

df_sparrow$other_pc_cse <- df_sparrow$other_pc_cse*100/df_sparrow$LC2 

df_sparrow$for_pc_cse + df_sparrow$crp_pc_cse + df_sparrow$urb_pc_cse +
      df_sparrow$pst_pc_cse + df_sparrow$other_pc_cse

#change NAs product of diving percentage by 0
colToChange<-c("for_pc_cse", "crp_pc_cse","urb_pc_cse","pst_pc_cse")
df_sparrow[colToChange][is.na(df_sparrow[colToChange])] <- 0

# calculate area of land cover within each catchment

df_sparrow$forest<-df_sparrow$demiarea*df_sparrow$for_pc_cse/100
df_sparrow$crops<-df_sparrow$demiarea*df_sparrow$crp_pc_cse/100
df_sparrow$urban<-df_sparrow$demiarea*df_sparrow$urb_pc_cse/100
df_sparrow$pasture<-df_sparrow$demiarea*df_sparrow$pst_pc_cse/100
df_sparrow$other<-df_sparrow$demiarea*df_sparrow$other_pc_cse/100

head(df_sparrow)

# df_sparrow$row<-1:nrow(df_sparrow)
# 
# df_sparrow[which(df_sparrow$tnode == 0),"tnode"]<-df_sparrow[which(df_sparrow$tnode == 0),"row"]

#df_sparrow<-merge(df_sparrow,calsites_ton[c("sID","nzsegment", "ExpCoeffLWP")],"nzsegment",all.x = T)

#df_sparrow$calsites2<-df_sparrow$calsites
#df_sparrow$depvar2<-df_sparrow$ExpCoeffLWP

#df_sparrow[which(!is.na(df_sparrow$buffer)),"depvar2"]<-NA
#df_sparrow[which(is.na(df_sparrow$ExpCoeffLWP)),"calsites2"]<-NA

write.csv(df_sparrow,"Data_sparrow/Rivers_csv/6020014330_sparrow.csv")




####################################

df_sparrow$depvar<-df_sparrow$load_kg_year/31536000

df_sparrow$depvar<-((df_sparrow$load_kg_year)/df_sparrow$demtarea)

##############


df<-read.csv("Data_sparrow/Rivers_csv/5030082270_sparrow.csv")

colnames(df)

hola<-df[c("nzsegment", "sID","waterid", "ExpCoeffLWP",  "calsites2","depvar2")]
test<-subset(hola,!is.na(hola$depvar2))
test$waterid
test$sID

df_sparrow$sID.x<-toupper(df_sparrow$sID.x)

which(test$nzsegment %in% df_sparrow$nzsegment)
which(!(test$nzsegment %in% df_sparrow$nzsegment))

subset(df_sparrow,df_sparrow$nzsegment %in% test$nzsegment)

df_sparrow2<-merge(df_sparrow,test,"waterid",all.x = T)

df_sparrow2$depvar2<-df_sparrow2$depvar2.y

df_sparrow2[which(is.na(df_sparrow2$depvar2) & !is.na(df_sparrow2$depvar)),"depvar2"]<-
  df_sparrow2[which(is.na(df_sparrow2$depvar2) & !is.na(df_sparrow2$depvar)),"depvar"]

write.csv(df_sparrow2,"Data_sparrow/Rivers_csv/5030082270_sparrow_jan.csv")




nrow(subset(test,test$sID %in% toupper(df_sparrow$sID.x)))
test2<-subset(test,!(test$sID %in% toupper(df_sparrow$sID.x)))

unique(df_sparrow$sID.x)
unique(hola$sID)
