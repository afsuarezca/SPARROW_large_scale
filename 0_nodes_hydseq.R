rm(list = ls())

setwd("C:/Users/s2993062/documents/sparrow_project")

df<-read.csv("Data_sparrow/Rivers_csv/6020014330_rivers.csv")
#df<-read.csv("New_Zealand/riverlines_NZ_all.csv")
head(df)

#head(df)

#df<-data1

#assign FNODEs using HYRIV_ID

df<-df[order(df$HYRIV_ID),]
df$fnode<-df$HYRIV_ID

#split by next down to get TNODES
#colnames(df)[3]<-"NEXT_DOWN"
data1<-split(df,df$NEXT_DOWN)

pb<-txtProgressBar(min = 0, max = length(data1), 
                   style = 3 
                   )

#Assign terminal nodes based on next down and hyriv_id columns
for (i in 1:length(data1)){
  setTxtProgressBar(pb,i)
  focus<-data1[[i]]
  tnode<-subset(df,df$HYRIV_ID %in% focus$NEXT_DOWN)
  if(nrow(tnode)>0){
    focus$tnode<- tnode$fnode
  }else{
    focus$tnode<- focus$NEXT_DOWN
  }
  data1[[i]]<-focus
  
}

#Merge data
data1<-plyr::compact(data1)
data1<-do.call(plyr::rbind.fill,data1)

#order by FNODE
data1<-data1[order(data1$fnode),]

#get TNODES = 0
tnode0<-subset(data1,data1$tnode ==0)
#get TNODES != 0
tnode<-subset(data1,!(data1$HYRIV_ID %in% tnode0$HYRIV_ID))

#assign tnodes TO NEXT DOWN = 0
tnode0$tnode<-1:(nrow(tnode0))

#merge data
data1<-rbind(tnode,tnode0)

#write.csv(data1,"Data_sparrow/Rivers_csv/5030082270_rivers.csv")

#Option 1 to assign hydseq####

data1<-read.csv("data1_missi.csv")

#read basin 12 lvl data
basins<-read.csv("basin_lvl12_miss.csv")
#merge data
data1<-merge(data1,basins,by.x = "HYBAS_L12",by.y = "HYBAS_ID")

#sort by SORT and NEXT DOWN columns (could add distance from reach outlet)

data1<-data1[
  with(data1, order(-SORT, -DIST_DN_KM)),
]

data1$hydseq_by_bas<-1:nrow(data1)

write.csv(data1,"data1_dave.csv")


#option 2 adapting hyseq function from RSPARROW##########

data1<-read.csv("data1.csv")

#data1<-subset(data,data$tnode != 0)
#create sequence variable
SEQ<-data.frame(seqvar = seq(1,nrow(data1),1))
#add seqvar to tnode and fnode
tnode<-as.data.frame(cbind(SEQ,data1$tnode))
colnames(tnode)<-c("seqvar","tnode")
fnode<-as.data.frame(cbind(SEQ,data1$fnode))
colnames(fnode)<-c("seqvar","fnode")
#sort data
tnode<-tnode[order(tnode$tnode),]
fnode<-fnode[order(fnode$fnode),]

#save rownumbers
fnode$Row<-seq(1,nrow(fnode),1)
tnode$Row<-seq(1,nrow(tnode),1)

#create list of all flowlines immediately downstram of given fnode
dnstream_list_index<-with(fnode,aggregate(Row~fnode,FUN = function(x) min(x)))
dnstream_list_index2<-with(fnode,aggregate(Row~fnode,FUN = function(x) max(x)))
dnstream_list_index<-merge(dnstream_list_index,dnstream_list_index2, by ="fnode")
names(dnstream_list_index)[2:3]<-c("start","end")
dnstream_list<-data.frame(seqvar = fnode$seqvar)

dnstream_list_index[which(dnstream_list_index$start != dnstream_list_index$end),]

#obtain maximum fnode value, and unique fnode list
maxfnode<-max(fnode$fnode)
fnode<-data.frame(fnode = unique(fnode$fnode))

#Create a list of all flowlines immediately upstream of a given fnode. 
#Create the companion index that references the list by fnode.
upstream_list_index<-merge(fnode,tnode,by.x="fnode",by.y="tnode")
upstream_list_index$Row<-seq(1,nrow(upstream_list_index),1)
upstream_list_index2<-with(upstream_list_index,aggregate(Row~fnode,FUN = function(x) min(x)))
upstream_list_index3<-with(upstream_list_index,aggregate(Row~fnode,FUN = function(x) max(x)))
upstream_list_index4<-merge(upstream_list_index2,upstream_list_index3, by ="fnode")
names(upstream_list_index4)[2:3]<-c("start","end")

which(upstream_list_index4$start != upstream_list_index4$end)

#get order for upstream_list
Start<-upstream_list_index4[,which(names(upstream_list_index4) %in% c("fnode","start"))]
End<-upstream_list_index4[which(!upstream_list_index4$end %in% upstream_list_index4$start),which(names(upstream_list_index4) %in% c("fnode","end"))]
names(End)[2]<-"start"
lineOrder<-rbind(Start,End)

lineOrder<-lineOrder[order(lineOrder$start),]
#get non-continuous list values
for (l in which(!seq(1:max(lineOrder$start)) %in% lineOrder$start)){
  MAX<-min(lineOrder[which(lineOrder$start>l),]$start)
  MIN<-max(lineOrder[which(lineOrder$start<l),]$start)
  Fnode<-lineOrder[which(lineOrder$start==MAX),]$fnode
  sub<-data.frame(fnode = rep(Fnode,length(seq(MIN,MAX,1))),start = seq(MIN,MAX,1))
  sub<-sub[which(!sub$start %in% lineOrder$start),]
  print(sub)
  lineOrder<-rbind(lineOrder,sub)
}

lineOrder$seqvar<-upstream_list_index[match(lineOrder$start,upstream_list_index$Row),]$seqvar
lineOrder<-lineOrder[order(lineOrder$fnode,lineOrder$seqvar),]

#define upstream_list
upstream_list<-data.frame(seqvar = unique(lineOrder$seqvar))

#save upstream_list_index
upstream_list_index<-upstream_list_index4

#Determine terminal flowlines, which serve as the initial group for 
#computing hydro sequence
ifterm<-merge(tnode,fnode,by.x="tnode",by.y="fnode",all=TRUE)
ifterm<-ifterm[which(!ifterm$tnode %in% fnode$fnode),]
ifterm<-data.frame(seqvar = ifterm[order(ifterm$tnode,ifterm$Row),c("seqvar")])

#save as headwaterflag in data1
ifhead<-na.omit(fnode[which(!tnode$tnode %in% fnode$fnode),])
#if (length(grep("headflag",calculate_reach_attribute_list)!=0)){
#  data1$headflag<-ifelse(data1$fnode %in% ifhead,1,0)
#}

#Module creates a sequence of upstream indices based on the 
#values in the two columns of the list index for the
#requested row. 
getuindx<-function(inrow,upstream_list_index){
  list_start<-upstream_list_index[inrow,]
  if (nrow(na.omit(list_start))==0){
    out<-NA
  }else{
    out<-seq(list_start[,1],list_start[,2],1)
  }
  return(out)
}

#Module creates a sequence of downstream indices based on the 
#values in the two columns of the list index for the
#requested row. 
getdindx<-function(inrow,dnstream_list_index){
  list_start<-dnstream_list_index[inrow,]
  return(seq(list_start[,1],list_start[,2],1))
}

#Module returns the sequential ids for all flowlines
#immediately upstream of all ids in the stack group
#that also meet the condition that every flowline
#downstream of stack flowlines fromnode has an 
#assigned hydrosequence value and have not previously
#been processed.
upstream<-function(group,hydseqvar,ifproc,fnode,dnstream_list,
                   upstream_list,upstream_list_index,dnstream_list_index){
  for (i in 1:length(group)){
    #get upstream and dnstream indexes
    ulist_subset<-getuindx(fnode[group[i]],upstream_list_index)
    dlist_subset<-dnstream_list[getdindx(fnode[group[i]],dnstream_list_index)]
    #create upgroup vector, empty
    if (i==1){
      upgroup<-vector("numeric")
    }
    
    #If all reaches downstream of fnode have hydseg then get list of
    #reaches upstream of fnode to be added to upstream group
    if (length(na.omit(ulist_subset))!=0 & !any(is.na(hydseqvar[dlist_subset])) & is.na(ifproc[fnode[group[i]]])){
      #mark as processed
      ifproc[fnode[group[i]]] <- 1
      #save in upgroup
      upgroup<-c(upgroup,upstream_list[ulist_subset])
    }#end if (!is.na(ulist_subset) & ...
  }#for i     
  if (exists("upgroup")){
    #save ifproc changes to parent.frame
    assign("ifproc",ifproc,envir = parent.frame())  
    return(upgroup)
  }else{
    return(NA)
  }#end exists upgroup
}#end upstream

#Load terminal flowlines - used to initial the stack 
stack<-ifterm$seqvar

#Load fnode - used with list indexes to retrieve lists 
#of flowlines immedeately upstream or downstream of
#a fnode 
fnode<-data1$fnode

#Load the list index, categorized by fnode, to find
#in the upstream_list the list of flowline sequence 
#numbers immediately upstream of the fnode.
upstrm_fnode<-upstream_list_index$fnode
upstrm_list_index<-upstream_list_index[c("start","end")]


#Load the list of flowline sequence numbers upstream
#of a given fnode, structured by fnode via the index
#upstream_list_index
upstream_list<-upstream_list$seqvar

#Load the list index, categorized by fnode, to find
#in the dnstream_list the list of flowline sequence 
#numbers immediately dnstream of the fnode.
dnstrm_fnode<-dnstream_list_index$fnode
dnstrm_list_index<-dnstream_list_index[c("start","end")]

#Load the list of flowline sequence numbers downstream
#of a given fnode, structured by fnode via the index
#dnstream_list_index
dnstream_list<-dnstream_list$seqvar

nreaches = length(fnode)
#Initialize the hydrosequence variable
hydseqvar<-rep(NA,nreaches)

#Imbed the indexes in fnode matrices 
upstream_list_index<-as.data.frame(matrix(nrow=maxfnode,ncol=2,NA))
upstream_list_index[upstrm_fnode,]<- upstrm_list_index
dnstream_list_index<-as.data.frame(matrix(nrow=maxfnode,ncol=2,NA))
dnstream_list_index[dnstrm_fnode,]<- dnstrm_list_index

#Initialze a fnode-referenced vector to track if 
#the flowlines upstream of a given fnode have 
#already been processed by assignment of hydrosequence
#values
ifproc<-rep(NA,maxfnode)

#Initialize the hydrosequence counter
h0 = 0 

#Loop through the network in increments defined by 
#the stack of flowlines conditionally selected to 
#receive sequential hydrosequence numbers. Assign 
#hydrosequence numbers to the existing stack and 
##then repopulate the stack based on qualifying 
#flowlines immediately upstream of the existing 
#stack. The loop terminates when there are no more
#upstream flowlines in the network.

while (length(stack)!=0){
  #Determine the upper range of hydrosequence
  #numbers to assign to the existing stack
  h1 = h0 + length(stack)
  
  #Assign hydrosequnce numbers to the flowlines
  #in the existing stack, as referenced by the 
  #flowline sequence id.
  hydseqvar[stack] = seq((h0 + 1),h1,1)
  
  #Repopulate the stack
  stack<-upstream(stack,hydseqvar,ifproc,fnode,dnstream_list,upstream_list,upstream_list_index,dnstream_list_index)
  
  #Increment the hydrosequence counter 
  h0<-h1
  gc()
}#end while

#Update the values of the hydrosequence variable
#in the network data set 

#downstream ordering
data1$hydseq <- hydseqvar*-1 
data<-data1

basins<-read.csv("mississippi_basins_lvl12.csv")
rivbas<-read.csv("mississippi_riveratlas.csv")

data1<-merge(data1,rivbas[c("HYRIV_ID","HYBAS_L12")],"HYRIV_ID")

data1<-merge(data1,basins,by.x = "HYBAS_L12",by.y = "HYBAS_ID")

#sort by SORT and NEXT DOWN columns (could add distance from reach outlet)

data1<-data1[
  with(data1, order(-SORT, -DIST_DN_KM)),
]

data1$hydseq_by_bas<-1:nrow(data1)
data1$hydseq <- data1$hydseq*-1

write.csv(data1,"hydroatlas_7040612840.csv")


hola<-subset(data1,data1$HYRIV_ID %in% df$HYRIV_ID)
write.csv(hola,"mississippi_test.csv")
