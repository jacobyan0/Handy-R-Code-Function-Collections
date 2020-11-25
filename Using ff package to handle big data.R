############ Load transit travel time
library(dplyr)
library(ff)
library(ffbase)
library(stringr)
#library(ffbase2)
options(fftempdir = "D://ffTemp")

gc()
wd <- "D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2"


##### When picking up the work, do not run until line marker "pick up from here"

TAZNeighboring <- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\GIS\\NeighborTAZs.csv")
TAZ_TPO <- read.csv(file=paste(wd,"TAZ_TPO.csv",sep="/"))

files <- list.files(path=paste(wd,"CSV",sep="/"))

for (i in 1:length(files)){
  TransitTT <- read.csv(file=paste(wd,"CSV",files[i],sep="/"))
  #TransitTT <- TransitTT[,c("TAZ_ORIGIN","TAZ_DESTINATION","Total_PublicTransitTime","TIME_ExcludeWalk")]
  #TransitTT$TIME_ExcludeWalk[which(TransitTT$TIME_ExcludeWalk==0)] <-TransitTT$Total_PublicTransitTime[which(TransitTT$TIME_ExcludeWalk==0)]
  #TransitTT$Total_PublicTransitTime <-TransitTT$TIME_ExcludeWalk
  TransitTT <- TransitTT[,c("TAZ_ORIGIN","TAZ_DESTINATION","Total_PublicTransitTime")]
  TransitTT$OriDes <- paste(TransitTT$TAZ_ORIGIN,TransitTT$TAZ_DESTINATION,sep="-")
  
  TAZ_TPO <- TransitTT %>% group_by(TAZ_ORIGIN) %>% summarise()
  ############## If same TAZ as origin or destination, Total_PublicTransitTime is 0, needs to be corrected
  TAZNeighboring0 <- merge(TAZNeighboring,TransitTT,by.x=c("TAZ","nbr_TAZ"),by.y=c("TAZ_ORIGIN","TAZ_DESTINATION"))
  TAZNeighboring0 <- TAZNeighboring0 %>% group_by(TAZ) %>% summarise(TransitTime=mean(Total_PublicTransitTime)/4)
  #####
  TransitTT <- left_join(TransitTT,TAZNeighboring0,by=c("TAZ_ORIGIN" = "TAZ"))
  TransitTT$Total_PublicTransitTime[which(TransitTT$Total_PublicTransitTime==0)]<-TransitTT$TransitTime[which(TransitTT$Total_PublicTransitTime==0)]
  TransitTT<-TransitTT[,!names(TransitTT) %in% c("TAZ_ORIGIN","TAZ_DESTINATION","TransitTime")]
  
  TransitTT$Time <- files[i]
  TransitTT$Time <- strsplit(as.character(TransitTT$Time),split = "Transit_cost_fc_")[[1]][2]
  TransitTT$Time <- strsplit(as.character(TransitTT$Time),split = ".csv")[[1]][1]
  TransitTT$Hour <- strsplit(as.character(TransitTT$Time[i]),split = "_")[[1]][1]
  
  TransitTT$OriDesHour <- paste(TransitTT$OriDes,TransitTT$Hour,sep="-")
  ### ff does not allow characters, need to convert to factor
  #TransitTT$Total_PublicTransitTime <- as.double(TransitTT$Total_PublicTransitTime)
  TransitTT$OriDes <- as.factor(TransitTT$OriDes)
  TransitTT$OriDesHour <- as.factor(TransitTT$OriDesHour)
  TransitTT$Time <- as.factor(TransitTT$Time)
  TransitTT$Hour <- as.factor(TransitTT$Hour)
  
  TransitTT <- as.ffdf(TransitTT)
  if (i==1){out.file=TransitTT}
  else{out.file <- ffdfappend(out.file,TransitTT,adjustvmode = F)}
}

rm(TransitTT,TAZNeighboring0,TAZNeighboring,files)

write.csv(out.file,file=paste(wd,"TransitTT_V2.csv",sep="/"),row.names=FALSE)
#rm(out.file,TransitTT,TAZNeighboring0,files)

###########################################################################################################
######################## Temporal accessibility measure   #################################################
#########################              #################################################
##########################################################################################################
#TAZ_ORIGIN,TAZ_DESTINATION,Total_PublicTransitTime,Time,Hour
# day: mean, median, max, min, std
# by hour (7,12,17,21): mean, median, std
require(doBy)

TransitTT.day.ff <- ffdfdply(x=out.file[c("OriDes","Total_PublicTransitTime")],
                          split=out.file$OriDes,FUN=function(x)
                          {summaryBy(Total_PublicTransitTime ~ OriDes, data=x,FUN=function(t)
                            c(DayMean=mean(t),DayMedian=median(t),DayMin=min(t),Daymax=max(t),DayStd=sd(t))
                          )})
TransitTT.day<-as.data.frame(TransitTT.day.ff)

TransitTT.day$Ori<-str_split_fixed(as.character(TransitTT.day$OriDes),"-",2)[,1]
TransitTT.day$Des<-str_split_fixed(as.character(TransitTT.day$OriDes),"-",2)[,2]
colnames(TransitTT.day) <- c("OriDes","DayMean","DayMedian","DayMin","Daymax","DayStd","Ori","Des")
TransitTT.day <- TransitTT.day[,c("Ori","Des","DayMean","DayMedian","DayMin","Daymax","DayStd")]

TransitTT.hour.ff<-ffdfdply(x=out.file[c("OriDesHour","Total_PublicTransitTime")],
                         split=out.file$OriDesHour,FUN=function(x)
                         {summaryBy(Total_PublicTransitTime ~ OriDesHour, data=x,FUN=function(t)
                           c(HourMean=mean(t),HourMedian=median(t),HourStd=sd(t))
                         )})
TransitTT.hour<-as.data.frame(TransitTT.hour.ff)

rm(out.file,TransitTT.day.ff,TransitTT.hour.ff)

TransitTT.hour$Ori<-str_split_fixed(as.character(TransitTT.hour$OriDesHour),"-",3)[,1]
TransitTT.hour$Des<-str_split_fixed(as.character(TransitTT.hour$OriDesHour),"-",3)[,2]
TransitTT.hour$Hour<-str_split_fixed(as.character(TransitTT.hour$OriDesHour),"-",3)[,3]
colnames(TransitTT.hour) <- c("OriDesHour","HourMean","HourMedian","HourStd","Ori","Des","Hour")
TransitTT.hour <- TransitTT.hour[,c("Ori","Des","Hour","HourMean","HourMedian","HourStd")]

save(TransitTT.day,TransitTT.hour,file=paste(wd,"TransitTT_V2.RDate",sep = "/"))

write.csv(TransitTT.hour,file=paste(wd,"TemporalTransitTT_V2.csv",sep="/"),row.names=FALSE)

summary(TransitTT.day)
summary(TransitTT.hour[TransitTT.hour$Hour=="7",])
summary(TransitTT.hour[TransitTT.hour$Hour=="12",])
summary(TransitTT.hour[TransitTT.hour$Hour=="17",])
summary(TransitTT.hour[TransitTT.hour$Hour=="21",])


##########################################################
################ Run until this line

#wd <- "D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Temp_accessibility_matrix_V2"
#load(file=paste(wd,"TransitTT_V2.RDate",sep = "/"))
########################################################################################################
################## TPO Skims  ##########################################################################
########################################################################################################
### Load data from TPO
PeakAuto.TT<- read.csv(file=paste(wd,"PeakAutoTT.csv",sep="/"))
#PeakAuto.FreeFlowTT<- read.csv(file=paste(wd,"PeakAutoFreeFlowTT.csv",sep="/"))
OffPeakAuto.TT<- read.csv(file=paste(wd,"OffPeakAutoTT.csv",sep="/"))
#OffPeakAuto.FreeFlowTT<- read.csv(file=paste(wd,"OffPeakAutoFreeFlowTT.csv",sep="/"))
PeakTransit<-read.csv(file=paste(wd,"PeakTransit.csv",sep="/"))
OffPeakTransit<-read.csv(file=paste(wd,"OffPeakTransit.csv",sep="/"))

#Overlap <- merge(PeakTransit,TransitTT.day,by.x=c("Origin","Destination"),by.y=c("Ori","Des"))

summary(PeakAuto.TT)
#summary(PeakAuto.FreeFlowTT)
summary(OffPeakAuto.TT)
#summary(OffPeakAuto.FreeFlowTT)
summary(PeakTransit)
summary(OffPeakTransit)

