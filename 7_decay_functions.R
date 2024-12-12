setwd("C:/Users/s2993062/documents/sparrow_project")
df_sparrow<-read.csv("./Data_sparrow/Rivers_csv/6020014330_sparrow.csv")


#Hydrological / morphological calculations:#####
# Jobson (1996) equation 14 calculation of reach velocity
g <- 9.80665 # gravitational acceleration (meters per second squared)
# demtarea - total drainage area (units=square kilometers)
# meanq - Mean annual streamflow (units=cubic meters per second)
df_sparrow$DAdimless <- (df_sparrow$demtarea ** 1.25 * sqrt (g)) / df_sparrow$meanq # equation 10 from Jobson
# Mean annual velocity (units=meters per second)
df_sparrow$velocity <- 0.02 + 0.051 * df_sparrow$DAdimless ** 0.821 * 1 **- 0.465 * df_sparrow$meanq / df_sparrow$demtarea
# rchtot - reach time of travel (days)
df_sparrow$rchtot <- (df_sparrow$length * 1000) / (df_sparrow$velocity * 86400)
# depth - Mean annual stream depth (units=meters) based on Leopold and Maddock
# relation (1953)
# meanq - Mean annual streamflow (cubic feet per second)
# 35.31467 converts streamflow from ft3/s to m3/s
df_sparrow$depth <- 0.0635 * (df_sparrow$meanq) ** 0.3966

df_sparrow$rchtot <- ifelse(df_sparrow$rchtot <= 0,df_sparrow$rchtot==0.0,df_sparrow$rchtot)
df_sparrow$rchdecay1 <- ifelse(df_sparrow$meanq <= 500 & df_sparrow$rchtype == 0,df_sparrow$rchtot,0.0)
df_sparrow$rchdecay2 <- ifelse(df_sparrow$meanq > 500 & df_sparrow$meanq <= 10000 & df_sparrow$rchtype == 0,df_sparrow$rchtot,0.0)
df_sparrow$rchdecay3 <- ifelse(df_sparrow$meanq > 10000 & df_sparrow$rchtype == 0,df_sparrow$rchtot,0.0)

#df_sparrow<-read.csv("data1_hydro_sparrow_jul_decaym.csv")

#df_sparrow[which(!is.na(df_sparrow$lon)),"hload.y"]<-df_sparrow[which(!is.na(df_sparrow$lon)),"hload.x"]

#reservoir decay (iresload)
df_sparrow$iresload<-ifelse(df_sparrow$hload > 0,1.0/df_sparrow$hload,0.0)

#df_sparrow[which(!is.na(df_sparrow$lon)),"rchtype"]<- 2
#df_sparrow[which(!is.na(df_sparrow$hload.y)),"rchtype"]<- 2

df_sparrow[which(df_sparrow$rchtype %in% c(0,1)),"iresload"]<-0

test<-subset(df_sparrow,df_sparrow$iresload > 0)

nrow(subset(test,test$rchtype == 2))

#stream decay
df_sparrow$strmdecay <- df_sparrow$rchtot/df_sparrow$depth

df_sparrow[which(is.na(df_sparrow$iresload)),"iresload"]<-0

df_sparrow[which(is.na(df_sparrow$strmdecay)),"strmdecay"]<-0

write.csv(df_sparrow,"./Data_sparrow/Rivers_csv/6020014330_sparrow_may.csv")
