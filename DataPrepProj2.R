
folderPath<- "/home/appurvj/Dropbox/Coursework/RTest/"
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
		print(dim(temp))
		attackVector<- rbind(attackVector,temp)
	}
	return(attackVector)
}


attackNames<- scan('DataPrepAttackNames.txt', what = '',sep='\n')

attackSampleSize<- 200
normalSampleSize<-2000
misuseAttackSamples<-getAttackSet(folderPath,attackNames,attackNames, attackSampleSize)
#anomalyAttackSamples<-getAttackSet(folderPath,attackNames, rep("Attack",length(attackNames)),attackSampleSize)
normalData<- read.csv(paste(folderPath, "Optimized_Normal.csv",sep=""), header = T)
misuseMaster<- sampleData(normalData, "Normal", normalSampleSize)
#anomalyMaster<- sampleData(normalData, "Normal", normalSampleSize)
misuseMaster<- rbind(misuseMaster, misuseAttackSamples)
#anomalyMaster<- rbind(anomalyMaster, anomalyAttackSamples)
print(dim(misuseMaster))
#print(dim(anomalyMaster))
row.names(misuseMaster)<-NULL
#row.names(anomalyMaster)<-NULL
print(levels(misuseMaster$AttackTypes))
write.csv(misuseMaster, paste(folderPath,"Five&Other.csv",sep=""), row.names = F)
#write.csv(anomalyMaster, paste(folderPath,"AnomalyMasterDataSet.csv",sep=""),row.names = F)
