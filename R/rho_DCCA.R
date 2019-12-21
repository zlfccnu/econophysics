library("RcppEigen")
#' Function used to calculate the DCCA coefficient
#' @param x  a numeric vector which convert from a time series
#' @param y  a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param sampleNum the number of sample
#'@param qVec the fluctuation order
#' @param detrendOrder the order of the polynomial fit
#' @param thread number of the multithread
#' @param sampleMethod 1 means the determined sample number, other values means the nonoverlap method
#' @param lengthRatio the ratio of largest detrending scale over the time series length
#' @return the DCCA coefficient ranging from -1 to 1, a dataframe
#' @export

rho_DCCA=function(x,y,nVec=NULL,sampleNum=NULL,qVec,detrendOrder=3,thread=3,sampleMethod=2,lengthRatio=0.05){
  dcca_x=Fq_DCCA(x=x,y=x,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod,lengthRatio)
  dcca_y=Fq_DCCA(x=y,y=y,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod,lengthRatio)
  dcca_xy=Fq_DCCA(x=x,y=y,nVec,sampleNum,qVec,detrendOrder,thread,sampleMethod,lengthRatio)
  resluts=(dcca_xy/sqrt(dcca_x*dcca_y))
  resluts[,1]<- nVec
  return(resluts)
}