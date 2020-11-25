#########################################################################################################
######################       Job Accessibility                   ########################################
#########################################################################################################
####Set the value of beta
beta_job <- 0.3
#beta_job <- 0.28
#beta_job <- 0.2
#beta_shp <- 0.39
options(scipen = 999)

###Generate a file with two columes: TAZID and JobCount (CTPP)

###Create a Three-colume Travel Time file: Origin, destinations, Travel Time (skim matrix)
Block.Jobs <- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Jobs\\Workplace_Data.csv")
# colnames(Jobs)
# block,	alljob,	joblowwage,	flexschedule, workerhighschool

TAZ.Block <- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\GIS\\Block in Study Area and TAZs.csv")

#TAZ.Jobs <- merge(TAZ.Block,Block.Jobs,by.x="GEOID",by.y="block",all.x = TRUE)
#Block.emp <- TAZ.Jobs[which(is.na(TAZ.Jobs$alljob)),]

TAZ.Jobs <- merge(TAZ.Block,Block.Jobs,by.x="GEOID",by.y="block")
### 14774 out of 36240 Block has employment data

library(dplyr)
Jobs <- TAZ.Jobs %>% group_by(TAZ) %>% summarise(Jobs=sum(alljob),LowWageJobs=sum(joblowwage),
                                                     FlexJobs=sum(flexschedule))
### 1365 TAZs have employment data
rm(TAZ.Jobs,Block.Jobs,TAZ.Block)

############################# Travel time
#### Auto travel time
PeakAuto.TT<- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\PeakAutoTT.csv")
#PeakAuto.FreeFlowTT<- read.csv(file=paste(wd,"PeakAutoFreeFlowTT.csv",sep="/"))
OffPeakAuto.TT<- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OffPeakAutoTT.csv")

colnames(PeakAuto.TT) <- c("Ori","Des","MatricNum","AutoTT")
colnames(OffPeakAuto.TT) <- c("Ori","Des","MatricNum","AutoTT")

#### Transit travel time ---hourly mean, hourly variation
TransitTT <- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TemporalTransitTT_V2.csv")

######################################## Run from this run   ########################################################
############

### Transit travel time --- by five minutes
#TransitTT.min <- read.csv(file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TransitTT_V2.csv")

#test10_100.min<-TransitTT.min[which(TransitTT.min$OriDes=="10-100"),]
#test10_1000.min<-TransitTT.min[which(TransitTT.min$OriDes=="10-1000"),]
#test565_820.min<-TransitTT.min[which(TransitTT.min$OriDes=="562-820"),]

#test10_100<-TransitTT[which(TransitTT$Ori=="10"&TransitTT$Des=="100"),]
#test10_1000<-TransitTT[which(TransitTT$Ori=="10"&TransitTT$Des=="1000"),]

#write.csv(test10_100,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OD_10_100.csv",row.names = FALSE)
#write.csv(test10_1000,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OD_10_1000.csv",row.names = FALSE)
#write.csv(test10_100.min,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OD_10_100_min.csv",row.names = FALSE)
#write.csv(test10_1000.min,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OD_10_1000_min.csv",row.names = FALSE)
#write.csv(test565_820.min,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OD_565_820_min.csv",row.names = FALSE)

############################################################################################################################
#### Run until this line

### Merge jobs to TAZs
PeakTT.long<-merge(PeakAuto.TT,Jobs,by.x="Des",by.y="TAZ")
OffPeakTT.long<-merge(OffPeakAuto.TT,Jobs,by.x="Des",by.y="TAZ")
TransitTT.long<-merge(TransitTT,Jobs,by.x="Des",by.y="TAZ")

#Transit.min.long<-merge(TransitTT.min,Jobs,by.x="Des",by.y="TAZ")

#TransitTT7 <- TransitTT.long[which(TransitTT.long$Hour==7),]
#TransitTT12 <- TransitTT.long[which(TransitTT.long$Hour==12),]
#TransitTT17 <- TransitTT.long[which(TransitTT.long$Hour==17),]
#TransitTT21 <- TransitTT.long[which(TransitTT.long$Hour==21),]

require(dplyr)
#PeakTT.long$Acc_cum<-0
#PeakTT.long$Acc_cum[which(PeakTT.long$AutoTT<=45)] <- PeakTT.long$Jobs[which(PeakTT.long$AutoTT<=45)]
PeakTT.long$JobAcc <- PeakTT.long$Jobs/exp(beta_job*PeakTT.long$AutoTT)
PeakTT.long$LowWageJobAcc <- PeakTT.long$LowWageJobs/exp(beta_job*PeakTT.long$AutoTT)
PeakTT.long$FlexJobAcc <- PeakTT.long$FlexJob/exp(beta_job*PeakTT.long$AutoTT)
#PeakTT.long$ShpAcc <- PeakTT.long$Shopping/exp(beta_shp*PeakTT.long$AutoTT)

PeakTT.long$JobAcc_cum <- 0
PeakTT.long$LowWageJobAcc_cum <- 0
PeakTT.long$FlexJobAcc_cum <- 0
PeakTT.long$JobAcc_cum[which(PeakTT.long$AutoTT<=60)] <- PeakTT.long$Jobs[which(PeakTT.long$AutoTT<=60)]
PeakTT.long$LowWageJobAcc_cum[which(PeakTT.long$AutoTT<=60)] <- PeakTT.long$LowWageJobs[which(PeakTT.long$AutoTT<=60)]
PeakTT.long$FlexJobAcc_cum[which(PeakTT.long$AutoTT<=60)] <- PeakTT.long$FlexJobs[which(PeakTT.long$AutoTT<=60)]

OffPeakTT.long$JobAcc <- OffPeakTT.long$Jobs/exp(beta_job*OffPeakTT.long$AutoTT)
OffPeakTT.long$LowWageJobAcc <- OffPeakTT.long$LowWageJobs/exp(beta_job*OffPeakTT.long$AutoTT)
OffPeakTT.long$FlexJobAcc <- OffPeakTT.long$FlexJob/exp(beta_job*OffPeakTT.long$AutoTT)
#OffPeakTT.long$ShpAcc <- OffPeakTT.long$Shopping/exp(beta_shp*OffPeakTT.long$AutoTT)

OffPeakTT.long$JobAcc_cum <- 0
OffPeakTT.long$LowWageJobAcc_cum <- 0
OffPeakTT.long$FlexJobAcc_cum <- 0
OffPeakTT.long$JobAcc_cum[which(OffPeakTT.long$AutoTT<=60)] <- OffPeakTT.long$Jobs[which(OffPeakTT.long$AutoTT<=60)]
OffPeakTT.long$LowWageJobAcc_cum[which(OffPeakTT.long$AutoTT<=60)] <- OffPeakTT.long$LowWageJobs[which(OffPeakTT.long$AutoTT<=60)]
OffPeakTT.long$FlexJobAcc_cum[which(OffPeakTT.long$AutoTT<=60)] <- OffPeakTT.long$FlexJobs[which(OffPeakTT.long$AutoTT<=60)]

TransitTT.long$JobAcc <- TransitTT.long$Jobs/exp(beta_job*TransitTT.long$HourMean)
TransitTT.long$LowWageJobAcc <- TransitTT.long$LowWageJobs/exp(beta_job*TransitTT.long$HourMean)
TransitTT.long$FlexJobAcc <- TransitTT.long$FlexJobs/exp(beta_job*TransitTT.long$HourMean)

TransitTT.long$JobAcc_cum <- 0
TransitTT.long$LowWageJobAcc_cum <- 0
TransitTT.long$FlexJobAcc_cum <- 0
TransitTT.long$JobAcc_cum[which(TransitTT.long$HourMean<=60)] <- TransitTT.long$Jobs[which(TransitTT.long$HourMean<=60)]
TransitTT.long$LowWageJobAcc_cum[which(TransitTT.long$HourMean<=60)] <- TransitTT.long$LowWageJobs[which(TransitTT.long$HourMean<=60)]
TransitTT.long$FlexJobAcc_cum[which(TransitTT.long$HourMean<=60)] <- TransitTT.long$FlexJobs[which(TransitTT.long$HourMean<=60)]

#TransitTT.min.long$JobAcc_cum <- 0
#TransitTT.min.long$JobAcc_cum[which(TransitTT.min.long$Total_PublicTransitTime<=60)] <- TransitTT.min.long$Jobs[which(TransitTT.min.long$Total_PublicTransitTime<=60)]

#TransitTT.min.long$JobAcc <- TransitTT.min.long$Jobs/exp(beta_job*TransitTT.min.long$Total_PublicTransitTime)
#TransitTT.min.long$LowWageJobAcc <- TransitTT.min.long$LowWageJobs/exp(beta_job*TransitTT.min.long$Total_PublicTransitTime)
#TransitTT.min.long$FlexJobAcc <- TransitTT.min.long$FlexJobs/exp(beta_job*TransitTT.min.long$Total_PublicTransitTime)

#TransitTT7$JobAcc <- TransitTT7$Jobs/exp(beta_job*TransitTT7$HourMean)
#TransitTT7$LowWageJobAcc <- TransitTT7$LowWageJobs/exp(beta_job*TransitTT7$HourMean)
#TransitTT7$FlexJobAcc <- TransitTT7$FlexJobs/exp(beta_job*TransitTT7$HourMean)
#TransitTT.long$ShpAcc <- TransitTT.long$Shopping/exp(beta_shp*TransitTT.long$AutoTT)

#TransitTT12$JobAcc <- TransitTT12$Jobs/exp(beta_job*TransitTT12$HourMean)
#TransitTT12$LowWageJobAcc <- TransitTT12$LowWageJobs/exp(beta_job*TransitTT12$HourMean)
#TransitTT12$FlexJobAcc <- TransitTT12$FlexJobs/exp(beta_job*TransitTT12$HourMean)

#TransitTT17$JobAcc <- TransitTT17$Jobs/exp(beta_job*TransitTT17$HourMean)
#TransitTT17$LowWageJobAcc <- TransitTT17$LowWageJobs/exp(beta_job*TransitTT17$HourMean)
#TransitTT17$FlexJobAcc <- TransitTT17$FlexJobs/exp(beta_job*TransitTT17$HourMean)

#TransitTT21$JobAcc <- TransitTT21$Jobs/exp(beta_job*TransitTT21$HourMean)
#TransitTT21$LowWageJobAcc <- TransitTT21$LowWageJobs/exp(beta_job*TransitTT21$HourMean)
#TransitTT21$FlexJobAcc <- TransitTT21$FlexJobs/exp(beta_job*TransitTT21$HourMean)

#Transit.minTT.long$JobAcc <- Transit.minTT.long$Jobs/exp(beta_job*Transit.minTT.long$AutoTT)
#Transit.minTT.long$LowWageJobAcc <- Transit.minTT.long$LowWageJobs/exp(beta_job*Transit.minTT.long$AutoTT)
#Transit.minTT.long$FlexJobAcc <- Transit.minTT.long$FlexJob/exp(beta_job*Transit.minTT.long$AutoTT)
#Transit.minTT.long$ShpAcc <- Transit.minTT.long$Shopping/exp(beta_shp*Transit.minTT.long$AutoTT)

PeakAutoAccess <- PeakTT.long %>% group_by(Ori) %>% summarise(JobAcc=sum(JobAcc),
                                                                 LowWageJobAcc=sum(LowWageJobAcc),
                                                                 FlexJobAcc=sum(FlexJobAcc),
                                                              JobAcc_cum=sum(JobAcc_cum),
                                                              LowWageJobAcc_cum=sum(LowWageJobAcc_cum),
                                                              FlexJobAcc_cum=sum(FlexJobAcc_cum))
OffPeakAutoAccess <- OffPeakTT.long %>% group_by(Ori) %>% summarise(JobAcc=sum(JobAcc),
                                                                       LowWageJobAcc=sum(LowWageJobAcc),
                                                                       FlexJobAcc=sum(FlexJobAcc),
                                                                    JobAcc_cum=sum(JobAcc_cum),
                                                                    LowWageJobAcc_cum=sum(LowWageJobAcc_cum),
                                                                    FlexJobAcc_cum=sum(FlexJobAcc_cum))

TransitAccess <- TransitTT.long %>% group_by(Ori,Hour) %>% summarise(JobAcc=sum(JobAcc),
                                                                      LowWageJobAcc=sum(LowWageJobAcc),
                                                                      FlexJobAcc=sum(FlexJobAcc),
                                                                     JobAcc_cum=sum(JobAcc_cum),
                                                                     LowWageJobAcc_cum=sum(LowWageJobAcc_cum),
                                                                     FlexJobAcc_cum=sum(FlexJobAcc_cum))

#TransitAccess.Time <- TransitTT.long %>% group_by(Ori,Time) %>% summarise(JobAcc=sum(JobAcc),
#                                                                          JobAcc_cum=sum(JobAcc_cum),
#                                                                     LowWageJobAcc=sum(LowWageJobAcc),
#                                                                     FlexJobAcc=sum(FlexJobAcc))

TransitAccessCV <- TransitAccess %>% group_by(Ori) %>% summarise(JobAccMean=mean(JobAcc),
                                                                 JobAccStd=sd(JobAcc),
                                                                 JobAccCumMean=mean(JobAcc_cum),
                                                                 JobAccCumStd=sd(JobAcc_cum),
                                                                 LowWageJobAccMean=mean(LowWageJobAcc),
                                                                 LowWageJobAccStd=sd(LowWageJobAcc),
                                                                 LowWageJobAccCumMean=mean(LowWageJobAcc_cum),
                                                                 LowWageJobAccCumStd=sd(LowWageJobAcc_cum),
                                                                 FlexJobAccMean=mean(FlexJobAcc),
                                                                 FlexJobAccStd=sd(FlexJobAcc),
                                                                 FlexJobAccCumMean=mean(FlexJobAcc_cum),
                                                                 FlexJobAccCumStd=sd(FlexJobAcc_cum))
TransitAccessCV$JobAccCV <- TransitAccessCV$JobAccStd/TransitAccessCV$JobAccMean
TransitAccessCV$JobAccCumCV <- TransitAccessCV$JobAccCumStd/TransitAccessCV$JobAccCumMean
TransitAccessCV$LowWageJobAccCV <- TransitAccessCV$LowWageJobAccStd/TransitAccessCV$LowWageJobAccMean
TransitAccessCV$LowWageJobAccCumCV <- TransitAccessCV$LowWageJobAccCumStd/TransitAccessCV$LowWageJobAccCumMean
TransitAccessCV$FlexJobAccCV <- TransitAccessCV$FlexJobAccStd/TransitAccessCV$FlexJobAccMean
TransitAccessCV$FlexJobAccCumCV <- TransitAccessCV$FlexJobAccCumStd/TransitAccessCV$FlexJobAccCumMean

summary(PeakAutoAccess)
summary(OffPeakAutoAccess)
#TransitAccessVariationByHour.Time <- TransitAccess.Time %>% group_by(Ori) %>% summarise(AccMean=mean(JobAcc),
#                                                                              AccStd=sd(JobAcc),
#                                                                              AccCumMean=mean(JobAcc_cum),
#                                                                              AccCumStd=sd(JobAcc_cum),)


write.csv(PeakAutoAccess,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\PeakAutoAccess.csv",row.names = FALSE)
write.csv(OffPeakAutoAccess,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\OffPeakAutoAccess.csv",row.names = FALSE)
write.csv(TransitAccess,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TransitAccessByHour.csv",row.names = FALSE)
#write.csv(TransitAccess.Time,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TransitAccessTime.csv",row.names = FALSE)
write.csv(TransitAccessCV,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TransitAccessCV.csv",row.names = FALSE)
#write.csv(TransitAccessVariationByHour.Time,file="D:\\Dropbox (UFL)\\Accessibility_paper\\Works\\Jacob\\Data\\Temp_accessibility_matrix_V2\\TransitAccessVariationByHourTime.csv",row.names = FALSE)

#TransitAccess7 <- TransitTT7 %>% group_by(Ori,Hour) %>% summarise(JobAcc=sum(JobAcc),
#                                                                       LowWageJobAcc=sum(LowWageJobAcc),
#                                                                       FlexJobAcc=sum(FlexJobAcc))

#TransitAccess12 <- TransitTT12 %>% group_by(Ori,HourMedian) %>% summarise(JobAcc=sum(JobAcc),
#                                                                            LowWageJobAcc=sum(LowWageJobAcc),
#                                                                            FlexJobAcc=sum(FlexJobAcc))

#TransitAccess17 <- TransitTT17 %>% group_by(Ori,HourMedian) %>% summarise(JobAcc=sum(JobAcc),
#                                                                            LowWageJobAcc=sum(LowWageJobAcc),
#                                                                            FlexJobAcc=sum(FlexJobAcc))

#TransitAutoAccess21 <- TransitTT21 %>% group_by(Ori,HourMedian) %>% summarise(JobAcc=sum(JobAcc),
#                                                                            LowWageJobAcc=sum(LowWageJobAcc),
#                                                                            FlexJobAcc=sum(FlexJobAcc))

#Transit.minAutoAccess <- Transit.minTT.long %>% group_by(Ori,Time) %>% summarise(JobAcc=sum(JobAcc),
#                                                                               LowWageJobAcc=sum(LowWageJobAcc),
#                                                                               FlexJobAcc=sum(FlexJobAcc))



#cor(TransitAccess$Access,TransitAccess$SvcAcc)

#write.csv(AutoTT,file="C:\\Users\\xzhao404\\Box Sync\\Dissertation Work\\data\\Data sources\\Puget Region\\Model_AllAutoTT_Peak.csv",row.names = FALSE)
#write.csv(TransitTT,file="C:\\Users\\xzhao404\\Box Sync\\Dissertation Work\\data\\Data sources\\Puget Region\\Model_AllTransitTT_Peak.csv",row.names=FALSE)

#write.csv(AutoAccess_cum,file="AutoAccess_cum.csv",row.names=FALSE)
#write.csv(TransitAccess_cum,file="TransitAccess_cum.csv",row.names=FALSE)
#write.csv(AutoAccess,file="AutoAccess.csv",row.names=FALSE)
#write.csv(TransitAccess,file="TransitAccess.csv",row.names=FALSE)

#plot(sort(AutoAccess$Access))
#plot(sort(TransitAccess$Access))


