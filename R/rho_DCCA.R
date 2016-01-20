library("RcppEigen")
#'Function used to calculate the DCCA coefficient
#'@param x  a numeric vector which convert from a time series
#'@param y  a numeric vector which convert from a time series
#'@param nVec  the time scale of the detrended operation
#'@param sampleNum the number of sample
#'@param qVec the fluctuation order
#'@param detrendOrder the order of the polynomial fit
#'@param thread number of the multithread
#'@param sampleMethod 1 means the determined sample number, other values means the nonoverlap method
#'@return the DCCA coefficient ranging from -1 to 1, a dataframe
#'@export
rho_DCCA=function(x,y,nVec,sampleNum=NULL,qVec,detrendOrder=3,thread=3,sampleMethod=1){
  dcca_x=Fq_DCCA(x=x,y=x,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod)
  dcca_y=Fq_DCCA(x=y,y=y,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod)
  dcca_xy=Fq_DCCA(x=x,y=y,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod)
  return(dcca_xy/sqrt(dcca_x*dcca_y))
}