#setwd("/home/appurvj/Dropbox/Coursework/RTest/")
library(neuralnet)
misuseMaster<- read.csv("MisuseMasterDataSet.csv")
nRows<- dim(misuseMaster)[1]
nCols<- dim(misuseMaster)[2]
attackNames<- scan('AttackNames.txt', what = '',sep='\n')
num<-1
resultCol<-nCols
newlist<- rep(0,nRows) # Normal will be classified as zero
 for(i in attackNames){
   newlist[which(misuseMaster$AttackType == i)] = num
   num<- num+1
 }
misuseMaster$AttackType<-newlist

sub<-sample(1:nRows,floor(0.7*nRows))
misTrain<- misuseMaster[sub,]
misTest<- misuseMaster[-sub,]
dim(misTrain)
dim(misTest)

varns<-c()
for(i in 1:nCols-1){
  varns<-c(varns,var(misTrain[,i]))
}
varns <- varns/sum(varns)*100
print(sum(varns[which(varns>2)]))
print(names(misTrain[,c(which(varns>2))]))


NNObj<-neuralnet(AttackType~duration +
                   service+
                   dst_bytes+
                   hot+
                   logged_in+
                   count+
                   srv_count+
                   dst_host_count+
                   dst_host_srv_count,
                   misTrain, hidden=c(12,8),threshold=0.1)


plot(NNObj)

tempTest<- misTest[,c(1,3,6,10,12,23,24,32,33)]

output<-round(compute(NNObj,tempTest)$net.result)
output[which(output > length(attackNames))] <- length(attackNames)
output[which(output< 0)] <- 0
length(output)

newOp<-(rep("", length(output)))

newAt<- c('Normal',attackNames) 

op<-table(newAt[misTest[,nCols]+1], newAt[output+1])
print(op)
sum(diag(op))/sum(op)

