library(fields)
library(sp)
library(rgdal)
library(rgeos)
library(gridExtra)
library(ggplot)
library(FNN)
library(caret)
library(maptools)
library(TunePareto)
library(plyr)

setwd("/Users/misiu-dev/Desktop/one to rule them all/klaudi/magister krol")

dane<-read.csv("dane2.csv", sep=";",dec=",", header=TRUE)
dane$X<-NULL
dane$X.1<-NULL
attach(dane)
summary(cbind(CLAIMS, CLAIMS_VALUE))
#AGE<-factor(AGE)
SEX<-factor(SEX)
#ENGINE_CAPACITY<-as.numeric(levels(ENGINE_CAPACITY))
MAKE<-factor(MAKE)
zi6<-glm(CLAIMS~AGE+MAKE+SEX |1,poisson,offset=log(EXP))
dane<-cbind(dane,fitted(zi6))
names(dane)[c(12,13,14)]<-c('SHORT','VSHORT','FITTED')

dane$SHORT<-revalue(dane$SHORT,c(
  "59-5"="59-5&6", "59-6"="59-5&6", "62-1"="62-1&2", 
  "62-2"="62-1&2", "64-0"="64-0&2", "64-2"="64-0&2", 
  "65-3"="65-0&3", "65-0"="65-0&3", "65-4"="65-4&5",
  "65-5"="65-4&5", "67-2"="67-2&3", "67-3"="67-2&3",
  "68-2"="68-2&3", "68-3"="68-2&3", "77-4"="77-&89-",
  "89-3"="77-&89-"))
dane<-dane[-(which(dane$SHORT=="50-9" | dane$SHORT=="60-9")),]

set.seed(111)
smpl.id <- generateCVRuns(labels = dane$SHORT,
                          ntimes = 10,
                          nfold = 5,
                          stratified=TRUE)

#unl.smpl.id<-as.vector(unlist(smpl.id[[1]][1]))
#test<-dane[unl.smpl.id,]
#train<-dane[-unl.smpl.id,]
#train<-dane[smpl.id]

#set.seed(111)
#gen.smpl.id <- generateCVRuns(labels = train$SHORT,
#                           ntimes = 1,
#                           nfold = 3,
#                           stratified=TRUE)

onefold.list<-list()
fourfold.list<-list()

for(j in 1:10){
  
  for (i in 1:5)
  {
    x<-dane[as.vector(unlist(smpl.id[[j]][i])),] 
    num<- aggregate(x[,14]~SHORT,x,length)
    predictedSum<-aggregate(x[,14]~SHORT,x,sum)
    predictedSum<-cbind(num[,2],aggregate(CLAIMS~SHORT,x,sum), predictedSum[,2])
    names(predictedSum)[c(1,4)] <- c('NUM', 'FITTED')
    predictedSum<-cbind(predictedSum, predictedSum$CLAIMS/predictedSum$FITTED)
    names(predictedSum)[5]<-'RATIO'
    R<-predictedSum[,5]
    index <- R < 1
    R[index]<--1/R[index]
    R[R>0]<-R[R>0]-1
    R[R<0]<-R[R<0]+1
    predictedSum[,5]<-R
    onefold.list[[(i*10+j)-10]]<-predictedSum
    
    x<-dane[as.vector(c(unlist(smpl.id[[j]][(i%%5)+1]),
                        unlist(smpl.id[[j]][((i+1)%%5)+1]),
                        unlist(smpl.id[[j]][((i+2)%%5)+1]),
                        unlist(smpl.id[[j]][((i+3)%%5)+1]))),] 
    num<- aggregate(x[,14]~SHORT,x,length)
    predictedSum<-aggregate(x[,14]~SHORT,x,sum)
    predictedSum<-cbind(num[,2],aggregate(CLAIMS~SHORT,x,sum), predictedSum[,2])
    names(predictedSum)[c(1,4)] <- c('NUM', 'FITTED')
    predictedSum<-cbind(predictedSum, predictedSum$CLAIMS/predictedSum$FITTED)
    names(predictedSum)[5]<-'RATIO'
    R<-predictedSum[,5]
    index <- R < 1
    R[index]<--1/R[index]
    R[R>0]<-R[R>0]-1
    R[R<0]<-R[R<0]+1
    predictedSum[,5]<-R
    fourfold.list[[(i*10+j)-10]]<-predictedSum
  }
}


for (i in 1:50){
  for(j in 1:127){
    if  (fourfold.list[[i]]$RATIO[j]==-Inf){
      if (fourfold.list[[i]]$FITTED[j]<1)
        {
        fourfold.list[[i]]$RATIO[j]<-0
        }
      else
        {
        fourfold.list[[i]]$RATIO[j]<--fourfold.list[[i]]$FITTED[j]
        }
    }
  }
}

for (i in 1:50){
  for(j in 1:127){
    if  (onefold.list[[i]]$RATIO[j]==-Inf){
      if (onefold.list[[i]]$FITTED[j]<1)
      {
        onefold.list[[i]]$RATIO[j]<-0
      }
      else
      {
        onefold.list[[i]]$RATIO[j]<--onefold.list[[i]]$FITTED[j]
      }
    }
  }
}





save(fourfold.list, file="fourfold.Rdata")
save(onefold.list, file="onefold.Rdata")
