
library(neuralnet)

setwd("/home/appurvj/Dropbox/Coursework/CurrentCourses/ISS/ISSProject_Sameera_Appurv_Sagar/Proj2")
########  Checking Variation  and Including Columns Accordingly  #####
attributeThreshold <- 2
misuseMaster<- read.csv("MisuseMasterDataSet.csv")
nRows<- dim(misuseMaster)[1]
nCols<- dim(misuseMaster)[2]
resultCol<-nCols

varns<-c()
for(i in 1:nCols-1){
  varns<-c(varns,var(misuseMaster[,i]))
}

varns <- varns/sum(varns)*100
mainAttCols<- which(varns>attributeThreshold)
mainAttNames<- names(misuseMaster[,mainAttCols])
cat('\nAttributes Considered: \n')
print(mainAttNames)
par(mar=c(3,14,1,1))
boxplot(misuseMaster[,mainAttCols], horizontal=T, las = 1)
cat('\n')
##############################################################



######## Making formula according to  selected columns  ##########

formula<-mainAttNames[1]
for(name in mainAttNames[-1]){
  formula<-paste(formula, name,sep='+')
}
formula<-as.formula(paste('AttackType~',formula,sep=''))

#########################################################



cat('Total Variation Captured: ',sum(varns[mainAttCols]),'%\n')
cat('No of Attributes considered:', length(mainAttCols),'\n\nResults:\n')
attackTypeCol<- misuseMaster$AttackType


######################  Misuse Detection   ###########################

attackNames<- scan('AttackNames.txt', what = '',sep='\n')

cat('\nMisuse Detection:\n')

for(attackName in attackNames){
   b<- Sys.time()
   cat('\nAttack Type: ',attackName,'\n\n')
   misuseMaster$AttackType<- attackTypeCol
   newlist<- rep(1,nRows) # Normal will be classified as zero
   newlist[which(misuseMaster$AttackType == attackName)] = 2
   misuseMaster$AttackType<-newlist
   sub<-sample(1:nRows,floor(0.7*nRows))
   misTrain<- misuseMaster[sub,]
   misTest<- misuseMaster[-sub,]
   
   NNObj<-neuralnet(formula, data=misTrain, hidden=c(8),threshold=0.5)
   #plot(NNObj)
   tempTest<- misTest[,mainAttCols]
   output<-round(compute(NNObj,tempTest)$net.result)
   output[which(output > 2)] <- 2
   output[which(output< 1)] <- 1
   
  #   obtainedOut<-(rep("", length(output)))
   attackSet<- c('Other',attackName) 
   
   ##generate lists of length of mistest[,ncols]/output replacing numbers with values in attacSet
   expectedOutput<- attackSet[misTest[,nCols]]
   obtainedOutput<- attackSet[output]
   ########
   
   op<-table(expectedOutput, obtainedOutput) #make confusion matrix
   print(op)
   
   cat('Neural Network Memory Consumption:', object.size(NNObj),'bytes\n')
   cat('Test Accuracy:',round(sum(diag(op))/sum(op),3),'\n')
   b<-Sys.time() - b
   cat('Execution time for',attackName, 'attack detection model:', round(b,3),' seconds', '\n\n')
  }


################################################################



##############  Anomaly Detection #####################

b<-Sys.time()
cat('\nAnomaly Detection:\n\n')
anoMaster<- read.csv('AnomalyMasterDataSet.csv')
nRows<- dim(anoMaster)[1]
nCols<- dim(anoMaster)[2]
resultCol<-nCols
newList<- rep(1, nRows)
anoIndices<-which(anoMaster$AttackType == 'Attack')
newList[anoIndices] <- 2
anoMaster$AttackType<-newList
par(mar=c(3,14,1,1))
boxplot(anoMaster[,-nCols], horizontal=T, las = 1)
sub<-sample(1:nRows,floor(0.7*nRows))
anoTrain<- anoMaster[sub,]
anoTest<- anoMaster[-sub,]

anoNNObj<- neuralnet(formula, data=anoTrain, hidden=c(8),threshold=0.5)

tempTest<- anoTest[,mainAttCols]
output<-round(compute(anoNNObj,tempTest)$net.result)
output[which(output > 2)] <- 2
output[which(output< 1)] <- 1

attackSet<- c('Normal','Attack')

##generate lists of length of mistest[,ncols]/output replacing numbers with values in attacSet
expectedOutput<- attackSet[anoTest[,nCols]]
obtainedOutput<- attackSet[output]
########

op<-table(expectedOutput, obtainedOutput) #make confusion matrix
print(op)
print(object.size(anoNNObj))
cat('Neural Network Memory Consumption:', object.size(anoNNObj),'bytes\n')
cat('Test Accuracy:',round(sum(diag(op))/sum(op),3),'\n')
b<- Sys.time() - b
cat('Execution time for Anomaly Detection:', round(b,3),'seconds', '\n\n\n')
###################################



###################### Five and Other ##########################

cat('\nFive and Other:\n')
b<-Sys.time()
fiveMaster<- read.csv('Five&Other.csv')
nRows<- dim(fiveMaster)[1]
nCols<- dim(fiveMaster)[2]
resultCol<-nCols
newList<- rep(6, nRows)
for(i in 1:length(attackNames)){
  fiveIndices<-which(fiveMaster$AttackType == attackNames[i])
  newList[fiveIndices] <- i
}
fiveMaster$AttackType<-newList

sub<-sample(1:nRows,floor(0.7*nRows))
fiveTrain<- fiveMaster[sub,]
fiveTest<- fiveMaster[-sub,]

fiveNNObj<- neuralnet(formula, data=fiveTrain, hidden=c(12),threshold=0.8)

tempTest<- fiveTest[,mainAttCols]
output<-round(compute(fiveNNObj,tempTest)$net.result)
output[which(output > 6)] <- 6
output[which(output< 1)] <- 1

attackSet<- c(attackNames,'Other')

##generate lists of length of mistest[,ncols]/output replacing numbers with values in attacSet
expectedOutput<- attackSet[fiveTest[,nCols]]
obtainedOutput<- attackSet[output]
########

op<-table(expectedOutput, obtainedOutput) #make confusion matrix
print(op)
cat('Neural Network Memory Consumption:', object.size(fiveNNObj), 'bytes\n')
cat('Test Accuracy:',round(sum(diag(op))/sum(op),3),'\n')
b<- Sys.time() - b
cat('Execution time for Detecting Five Specified Attacks:', round(b,3),'seconds', '\n\n\n')

####################################################################
