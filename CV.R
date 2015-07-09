
#for tps set model =  0 
CrossValid<-function(x,LG,z,model,lambdaStep)
{
  
    iterNum<-100
    diagnostic<-list()
    y<-coordinates(LG)
    CVSmoothed<-list() 
    CVSmoothed<-replicate(5, matrix(0,127,1), simplify=F)
  
    if(model==0)
    {
        diagnostic<-replicate(5, matrix(0,100,3), simplify=F)
    }
    
    else
    {
        diagnostic<-replicate(5, matrix(0,700,3), simplify=F)
    }
  
    for (i in 1:length(x))
    {
        lambda<-0
        
        for (j in 1:iterNum)
        {
            h<-7
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
                {
                    fit<-smooth_whittaker(LG,h,lambda)
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
            fit<-smooth_whittaker(LG,minh,minlambda) 
        }    
    
        CVSmoothed[[i]]<-fit 
    }
  
    return(c(CVSmoothed,diagnostic))
}

CV<-CrossValid(fourfold.list,LGroup,onefold.list,1,10)
