sampleData<- function(attackDat, label, numOfRows){
	sampleDat<- cbind(attackDat[sample(1:nrow(attackDat),numOfRows),],label)
	colnames(sampleDat)<- c(colnames(sampleDat)[-ncol(sampleDat)],'AttackType')
	return(sampleDat)
}

getAttackSet<-function(pathToFolder,attackNames,attackColVals, sampleSize ){
	attackVector<-NULL
	for(i in 1:length(attackNames)){
		path<- paste(pathToFolder,"Optimized_",attackNames[i],".csv",sep = "")
		temp<- read.csv(path,header = T)
		temp<- sampleData(temp,attackColVals[i],sampleSize) 
		attackVector<- rbind(attackVector,temp)
	}
	return(attackVector)
}



attackNames<- c('Neptune', 'Satan', 'Smurf', 'PortSweep', 'NMap')
folderPath<- "/home/appurvj/Dropbox/Coursework/RTest/"
attackSampleSize<- 200
normalSampleSize<-2000
misuseAttackSamples<-getAttackSet(folderPath,attackNames,attackNames, attackSampleSize)
anomalyAttackSamples<-getAttackSet(folderPath,attackNames, rep("Attack",length(attackNames)),attackSampleSize)
normalData<- read.csv(paste(folderPath, "Optimized_Normal.csv",sep=""), header = T)
misuseMaster<- sampleData(normalData, "Normal", normalSampleSize)
anomalyMaster<- sampleData(normalData, "Normal", normalSampleSize)
misuseMaster<- rbind(misuseMaster, misuseAttackSamples)
anomalyMaster<- rbind(anomalyMaster, anomalyAttackSamples)
print(dim(misuseMaster))
print(dim(anomalyMaster))
write.csv(misuseMaster, paste(folderPath,"MisuseMasterDataSet.csv"))
write.csv(anomalyMaster, paste(folderPath,"AnomalyMasterDataSet.csv"))

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
