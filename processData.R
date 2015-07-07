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
gp6<-glm(CLAIMS~AGE+MAKE,poisson,offset=log(2*EXP))
dane<-cbind(dane,fitted(gp6))
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
                           ntimes = 1,
                           nfold = 10,
                           stratified=TRUE)

unl.smpl.id<-as.vector(unlist(smpl.id[[1]][1]))
test<-dane[unl.smpl.id,]
train<-dane[-unl.smpl.id,]

set.seed(111)
gen.smpl.id <- generateCVRuns(labels = train$SHORT,
                           ntimes = 1,
                           nfold = 5,
                           stratified=TRUE)
onefold.list<-list()
fourfold.list<-list()


for (i in 1:5)
  {
  x<-train[as.vector(unlist(gen.smpl.id[[1]][i])),] 
  num<- aggregate(x[,14]~SHORT,x,length)
  predictedSum<-aggregate(x[,14]~SHORT,x,sum)
  predictedSum<-cbind(num[,2],aggregate(CLAIMS~SHORT,x,sum), predictedSum[,2])
  names(predictedSum)[c(1,4)] <- c('NUM', 'FITTED')
  predictedSum<-cbind(predictedSum, predictedSum$CLAIMS/predictedSum$FITTED)
  names(predictedSum)[5]<-'RATIO'
  onefold.list[[i]]<-predictedSum
  
  x<-train[as.vector(c(unlist(gen.smpl.id[[1]][(i%%5)+1]),
                       unlist(gen.smpl.id[[1]][((i+1)%%5)+1]),
                       unlist(gen.smpl.id[[1]][((i+2)%%5)+1]),
                       unlist(gen.smpl.id[[1]][((i+3)%%5)+1]))),] 
  num<- aggregate(x[,14]~SHORT,x,length)
  predictedSum<-aggregate(x[,14]~SHORT,x,sum)
  predictedSum<-cbind(num[,2],aggregate(CLAIMS~SHORT,x,sum), predictedSum[,2])
  names(predictedSum)[c(1,4)] <- c('NUM', 'FITTED')
  predictedSum<-cbind(predictedSum, predictedSum$CLAIMS/predictedSum$FITTED)
  names(predictedSum)[5]<-'RATIO'
  fourfold.list[[i]]<-predictedSum
  }

num<- aggregate(test[,14]~SHORT,test,length)
predictedSum<-aggregate(test[,14]~SHORT,test,sum)
predictedSum<-cbind(num[,2],aggregate(CLAIMS~SHORT,test,sum), predictedSum[,2])
names(predictedSum)[c(1,4)] <- c('NUM', 'FITTED')
predictedSum<-cbind(predictedSum, predictedSum$CLAIMS/predictedSum$FITTED)
names(predictedSum)[5]<-'RATIO'
test.aggr<-predictedSum
  

save(fourfold.list, file="fourfold.Rdata")
save(onefold.list, file="onefold.Rdata")
save(test.aggr, file="testSample.Rdata")