
setwd("/home/appurvj/Dropbox/Coursework/RTest/")

misuseMaster<- read.csv("MisuseMasterDataSet.csv")
anomalyMaster<- read.csv("AnomalyMasterDataSet.csv")

library(kernlab)
library(caret)
anoRows<-nrow(anomalyMaster)
misRows<-nrow(misuseMaster)
cols<-ncol(anomalyMaster)
sub<-sample(1:anoRows,floor(0.66*anoRows))
anoTrain<- anomalyMaster[sub,]
anoTest<- anomalyMaster[-sub,]
anoClassifier<- ksvm(AttackType~.,data=anoTrain,type = 'C-svc', kernel = 'rbfdot')
predictionAno<-predict(anoClassifier, anoTest[,-cols])

confusionMatrix(predictionAno,anoTest[,cols] )

sub<-sample(1:misRows,floor(0.66*misRows))
misTrain<- misuseMaster[sub,]
misTest<- misuseMaster[-sub,]

misClassifier<- ksvm(AttackType~.,data=misTrain,type = 'C-svc', kernel = 'rbfdot')
predictionMis<-predict(misClassifier, misTest[,-cols])
confusionMatrix(predictionMis,misTest[,cols] )
