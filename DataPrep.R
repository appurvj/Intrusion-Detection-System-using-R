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


attackNames<- scan('DataAttackNames.txt', what = '',sep='\n')
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
row.names(misuseMaster)<-NULL
row.names(anomalyMaster)<-NULL
write.csv(misuseMaster, paste(folderPath,"MisuseMasterDataSet.csv",sep=""), row.names = F)
write.csv(anomalyMaster, paste(folderPath,"AnomalyMasterDataSet.csv",sep=""),row.names = F)
