setwd("C:\\Users\\yiming.xu\\Desktop")

#install.packages("walkscoreAPI")
library(walkscoreAPI)

test <- read.csv(file="blockgroup_centroid.csv",header = TRUE)
test$Walkscore<-0


for (i in 1:length(test$GEOID)) {
  a<-getWS(test$centroid_x[i],test$centroid_y[i], "f5a55e59a2d6be9b2263c3471e81c86d")
  test$Walkscore[i]<-a$walkscore
}

test$Walkscore[which(test$Walkscore=='NA')] <- 0

write.csv(test,file="Blockgroups_Walkscore.csv",row.names = FALSE)
#getWS(test$X[i],test$Y[i], "823ebf192a9537ddb2cbb92ea29ff225")
#getTS(test$X[i],test$Y[i],"Ann Arbor","MI","823ebf192a9537ddb2cbb92ea29ff225")

#test<- c(42.2999486,-83.7218988)
#a<-getTS(test[2],test[1],"MI","823ebf192a9537ddb2cbb92ea29ff225")

#getTScities("823ebf192a9537ddb2cbb92ea29ff225")
