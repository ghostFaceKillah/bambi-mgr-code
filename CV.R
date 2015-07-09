
#for tps set model =  0 
CrossValid<-function(x,LG,z,model,lambdaStep,firstLambda,n)
{
    data<-LG
    iterNum<-n
    diagnostic<-list()
    y<-coordinates(data)
    CVSmoothed<-list() 
    CVSmoothed<-replicate(5, matrix(0,127,1), simplify=F)
    
  
    if(model==0)
    {
        diagnostic<-replicate(5, matrix(0,n,3), simplify=F)
    }
    
    else
    {
        diagnostic<-replicate(5, matrix(0,7*n,3), simplify=F)
    }
  
    for (i in 1:length(x))
    {
        lambda<-firstLambda
        
        for (j in 1:iterNum)
        {
            h<-7
            print(j)
            if (model==0)
            { 
                tps<-Tps(y, x[[i]]$RATIO, lambda=lambda, lon.lat=TRUE)
                fit<-fitted(tps)
                error<-sum((fit-z[[i]]$RATIO)^2)
                diagnostic[[i]][j,]<-c(lambda,error,h)
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

CV<-CrossValid(fourfold.list,LGroup,onefold.list,1,1,0.01,20)
LGroup@data<-LGroup@data[,-(6:12)]
LGroup@data<-cbind(LGroup@data, CV[[1]],CV[[2]],CV[[3]],CV[[4]],CV[[5]])
spplot(LGroup,6)
