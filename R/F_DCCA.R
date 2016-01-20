library("RcppEigen")
#' Function used to calculate the DCCA fluctuation
#'@param x  a numeric vector which convert from a time series
#'@param y  a numeric vector which convert from a time series
#'@param n  the time scale of the detrended operation
#'@param overlap the overlap of the boxes,default 0.99999999
#'@param q the order of the multifractal
#'@return the the DCCA fluctuation
#'@export
F_DCCA=function(x,y,n,overlap=0.99999999,q=2){
  ##calcuate the F2_DCCA series
  x=cumsum(x-mean(x))
  y=cumsum(y-mean(y))
  f2_DCCA=c()
  n1=ceiling((1-overlap)*n)
  for(i in seq(1,(length(x)-n+1),by=n1)){
    modelMat=cbind(1,c(i:(i+n-1)))
    res_x=residuals(fastLmPure(modelMat,x[i:(i+n-1)]))
    res_y=residuals(fastLmPure(modelMat,y[i:(i+n-1)]))
    f2_DCCA=append(f2_DCCA,mean(res_x*res_y))
  }
  
  if(q!=0){
    return((mean(f2_DCCA^(q/2)))^(1/q))
  }else{
    return(exp(0.5*(mean(log(f2_DCCA)))))
  }
}