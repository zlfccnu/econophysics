library("RcppEigen")
#' Function used to calculate the DCCA fluctuation
#' @param x  a numeric vector which convert from a time series
#' @param y  a numeric vector which convert from a time series
#' @param n  the time scale of the detrended operation
#' @param overlap the overlap of the boxes,default 0.99999999
#' @param q the order of the multifractal
#' @return the the DCCA fluctuation

F_DCCA=function(x,y,nVec=NULL,sampleNum=NULL,qVec=c(-5:5),detrendOrder=3,thread=3,sampleMethod=2,lengthRatio=0.05){
  ##calcuate the F2_DCCA series
  F2_DCCA=Fq_DCCA(x=x,y=y,nVec=nVec,sampleNum=sampleNum,qVec=qVec,detrendOrder=detrendOrder,thread=thread,sampleMethod=sampleMethod,lengthRatio=lengthRatio)
  if(0%in%qVec){
    qVec=qVec[-which(qVec==0)]
  }
  FDCCA=sapply(qVec,function(x){(F2_DCCA[,sprintf("q%.2f",x)])^(1/x)})
  FDCCA=cbind(F2_DCCA[,1],FDCCA)
  colnames(FDCCA)=c("nVec",sprintf("q%.2f",qVec))
  return(as.data.frame(FDCCA))
}