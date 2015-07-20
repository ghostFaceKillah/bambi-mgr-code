
smooth_whittaker<-function(spData, h, alpha)
{
  nrowCrds <- nrow(spData)
  crds <- data.frame(ID=1:nrowCrds, X=coordinates(spData)[,1], Y=coordinates(spData)[,2])
  coordinates(crds)<-c("X","Y")
  
  proj4string(crds) <- CRS("+proj=longlat +datum=WGS84") 
  
  rep <- spTransform(crds, CRS("+proj=utm +zone=33 ellps=WGS84"))
  crds<-as(rep, "SpatialPoints")
  crds<-as.data.frame(crds)
  
  nrowCrds<-nrow(spData)
  min.ln<-min(crds[,1])
  max.ln<-max(crds[,1])
  min.wd<-min(crds[,2])
  max.wd<-max(crds[,2])
  crds[,1]<-((crds[,1]-min.ln)*1.559337/(max.ln-min.ln)) -0.7796683
  crds[,2]<-((crds[,2]-min.wd)*2/(max.wd-min.wd)) -1
  nearestXs<-knn.index(crds,h-1)
  X <- matrix()
  length(X)<-6*h
  dim(X)<-c(h,6)
  
  A<-matrix()
  length(A)<-6*h
  dim(A)<-c(6,h)
  
  inverseV<-solve(diag(c(spData@data$NUM), nrowCrds,nrowCrds))
  
  C<-diag(c(1,2,1), 3,3)
  
  M<-matrix(0,nrowCrds,nrowCrds)
  Y<-spData@data
  z<-vector()
  for (i in  1:nrowCrds)
  {
    
    for(j in 1:h-1)
    {
      NextPair<-nearestXs[i,j]
      X[j,]<-c(1/2*crds[NextPair,1]^2, crds[NextPair,1]*crds[NextPair,2], 1/2*crds[NextPair,2]^2, crds[NextPair,1], crds[NextPair,2],1)	
    }
    
    X[h,]<-c(1/2*crds[i,1]^2, crds[i,1]*crds[i,2], 1/2*crds[i,2]^2, crds[i,1], crds[i,2],1)
    
    #print(X)
    #print(t(X) %*% X)
    A<-solve((t(X) %*% X)) %*% t(X)
    B<-matrix(0,6,nrowCrds)
    B[,c(nearestXs[i,],i)]<-A 
    B_<-B[1:3,]		
    #print(B)
    M<-M + (t(B_) %*% C %*% B_)	
    
  }
  
  z<- solve(diag(1,nrowCrds,nrowCrds) + (alpha * M)) %*% spData@data$RATIO
  #print(inverseV %*% M)
  return(z)
  
}


#for tps set model =  0 
CrossValid<-function(x,LG,z,model,lambdaStep,firstLambda,n)
{
    data<-LG
    iterNum<-n
    diagnostic<-list()
    y<-coordinates(data)
    CVSmoothed<-list() 
    CVSmoothed<-replicate(length(x), matrix(0,127,1), simplify=F)
    
  
    if(model==0)
    {
        diagnostic<-replicate(length(x), matrix(0,n,3), simplify=F)
    }
    
    else
    {
        diagnostic<-replicate(length(x), matrix(0,7*n,3), simplify=F)
    }
  
    for (i in 1:length(x))
    {
        lambda<-firstLambda
        
        for (j in 1:iterNum)
        {
            h<-7
            print((i*iterNum)+j)
            if (model==0)
            { 
                tps<-Tps(y, x[[i]]$RATIO, lambda=lambda, lon.lat=TRUE)
                fit<-fitted(tps)
                error<-sum((fit-z[[i]]$RATIO)^2)
                print("a")
                diagnostic[[i]][j,]<-c(lambda,error,h)
                print("b")
            }
            
            else
            {
                for (k in 1:7)
                {   data@data<-x[[i]]
                    fit<-smooth_whittaker(data,h,lambda)
                    error<-sum((fit-z[[i]]$RATIO)^2)
                    diagnostic[[i]][j*7+k-7,]<-c(lambda,error,h)
                    h<-h+1
                }
            }
            
            lambda<-lambda+lambdaStep
        }
        
        minlambda<-diagnostic[[i]][which.min(diagnostic[[i]][,2]),1]
        
        if(model==0)
        { 
            tps<-Tps(y, x[[i]]$RATIO, lambda=minlambda, lon.lat=TRUE)
            fit<-fitted(tps)
        }
        
        else
        {
            minh<-diagnostic[[i]][which.min(diagnostic[[i]][,2]),3]
            fit<-smooth_whittaker(data,minh,minlambda) 
        }    
        
        CVSmoothed[[i]]<-fit 
    }
  
    return(c(CVSmoothed,diagnostic))
}

CV<-CrossValid(fourfold.list,LGroup,onefold.list,0,15,10,50)
LGroup@data<-LGroup@data[,-(6:12)]
LGroup@data<-cbind(LGroup@data, CV[[1]],CV[[2]],CV[[3]],CV[[4]],CV[[5]])
spplot(LGroup,6)

for (i in 1:50)
  {
  print((CV[[50+i]][[which.min(CD[[50+i]][,2])]]))
  }

for (i in 1:50)
  {
  plot(CV[i+50][[1]])
  Sys.sleep(0.3)
  }

  

