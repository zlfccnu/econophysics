#'Function used to calculate the generalized hurst exponent
#' @param x  a numeric vector which convert from a time series
#' @param y a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation, the minmum scale should be determined by calculate the autocorrelation length of the time series.
#' @param sampleNum the random sampled number
#' @param thread the parallel thread number
#' @param qVec the qth order
#' @param detrendOrder the detrending polynomial order
#' @param sampleMethod 1 means the determined sample number method, other values mean the nonoverlap method
#' @param lengthRatio determine how long is the largest scale
#' @return the generalized hurst exponent,a data.frame
#' @export

hurstExponent_xy<- function(x,y,nVec=NULL,sampleNum=NULL,thread=3,qVec=c(-5:5),detrendOrder=3,sampleMethod=2,lengthRatio=0.05,returnFluctuation=FALSE){
  if(0%in%qVec){
    qVec=qVec[-which(qVec==0)]
  }
  require(parallel)
  dcca_fluctuation = Fq_DCCA(x=x,y=y,nVec=nVec, qVec=qVec,thread=thread,sampleNum =sampleNum,detrendOrder=detrendOrder,sampleMethod=sampleMethod,lengthRatio=lengthRatio)
  registerDoMC(thread)
  hurst=foreach(i= 2:(dim(dcca_fluctuation)[2]),.combine = c)%dopar%{
    dccaLM = lm(log((dcca_fluctuation[,i])^(1/qVec[i-1]))~log(dcca_fluctuation[,1]))
    return(as.numeric(dccaLM$coefficients[2]))
  }
  if(returnFluctuation==TRUE){
    return(list(dcca_fluctuation,cbind(qVec,hurst)))
  }else{
    return(cbind(qVec,hurst))
  }
}