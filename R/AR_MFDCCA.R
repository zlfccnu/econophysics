#' ARMA based MFDCCA analysis
#' @param x  a numeric vector which convert from a time series
#' @param y  a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param sampleNum the number of sample
#' @param qVec the order of the multifractal
#' @param detrendMethod the detrending polynomial order
#' @param thread the multithread number
#' @param sampleMethod 1 means the determined sample number, other values means the nonoverlap method
#' @param lengthRatio detremine how long is the largest scale
#' @return the the sqrt DCCA fluctuation
#' @export
F_ARq_DCCA=function(x,y,nVec=NULL,sampleNum=NULL,qVec=c(-5:5),thread=3,detrendMethod="AR",sampleMethod=2,lengthRatio=0.05){
  require(parallel)
  require(RcppEigen)
  require(forecast)
  na.fail(x)
  na.fail(y)
  if(0%in%qVec){
    qVec=qVec[-which(qVec==0)]
  }
  ##calcuate the Fq_DCCA
  x=cumsum(x-mean(x))
  y=cumsum(y-mean(y))
  f2_DCCA=list()
  if(is.null(nVec)){
    nNum=floor(log2(lengthRatio*length(x)))
    if(nNum<=4){
      stop("time series is too short!")
    }
    nVec=2^(4:nNum)
  }else{
    nVec=ceiling(nVec)
  }
  
  if(sampleMethod==1){
    
    if(is.null(sampleNum)){
      stop("sampleNum should be given when sampleMethod is 1!")
    }
    registerDoMC(thread)
    f2_DCCA=foreach(n=nVec)%dopar%{
      startIndex=sample((length(x)-n+1),sampleNum)
      f2_DCCA_Tmp=1:sampleNum
      for(i in 1:sampleNum){
        fit_x=fastLm(x[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        fit_y=fastLm(y[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        res_x=fit_x$residuals
        res_y=fit_y$residuals
        f2_DCCA_Tmp[i]=mean(res_x*res_y)
      }
      return(f2_DCCA_Tmp)
    }
    
  }else{
    
    x_inv=rev(x)
    y_inv=rev(y)
    x_len=length(x)
    y_len=length(y)
    registerDoMC(thread)
    f2_DCCA=foreach(n=nVec)%dopar%{
      sampleNum=floor(x_len/n)
      startIndex=1+(0:sampleNum)*n
      f2_DCCA_Tmp=c(1:(2*sampleNum))
      
      ## forward
      for(i in 1:sampleNum){
        fit_x=fastLm(x[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        fit_y=fastLm(y[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        res_x=fit_x$residuals
        res_y=fit_y$residuals
        f2_DCCA_Tmp[i]=mean(res_x*res_y)
      }
      ## backward
      for(i in 1:sampleNum){
        fit_x=fastLm(x_inv[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        fit_y=fastLm(y_inv[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        res_x=fit_x$residuals
        res_y=fit_y$residuals
        f2_DCCA_Tmp[i+sampleNum]=mean(res_x*res_y)
      }
      return(f2_DCCA_Tmp)
    }
    
  }
  ## qth order DCCA
  registerDoMC(thread)
  Fq_DCCA_Tmp=foreach(q=qVec,.combine = cbind)%dopar%{
    sapply(f2_DCCA, function(x){mean(sign(x)*(abs(x)^(q/2)))})
  }
  Fq_DCCA_Tmp=as.data.frame(Fq_DCCA_Tmp)
  colnames(Fq_DCCA_Tmp)<- sprintf("q%.2f",qVec)
  rownames(Fq_DCCA_Tmp)<- sprintf("n%d",nVec)
  return(as.data.frame(cbind(nVec,Fq_DCCA_Tmp)))
}