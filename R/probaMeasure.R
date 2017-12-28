#' Calculate the probability measure of a fractal set
#' @param x  a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param sampleNum the random sampled number
#' @param thread the parallel thread number
#' @return the unnormalized probability measure, a data.frame

probaMeasure=function(x,nVec,sampleNum,thread){
  registerDoMC(thread)
  probaMeasureScale=foreach(n=nVec,.combine = cbind)%dopar%{
  proba=rep(0,sampleNum)
  startIndex=sample((length(x)-n+1),sampleNum)
  seriesSum=sum(x)
  for(i in c(1:sampleNum)){
    proba[i]=sum(x[startIndex[i]:(startIndex[i]+n-1)])/seriesSum
  }
  return(proba)
  }
  return(as.data.frame(probaMeasureScale))
}