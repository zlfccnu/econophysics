#' Function used to calculate the DFA fluctuation
#' @param x the time series
#' @param nVec the detrended length vector
#' @param qVec the mutifractal order vector
#' @param sampleNum the number of random sample
#' @param thread the parallel threads number
#' @param detrendOrder the detrending polynomial order
#' @param sampleMethod 1 means the determined sample number method, other values mean the nonoverlap method
#' @return the DFA fluctuation which enhanced by q^th order, a dataframe
#' @export
F_DFA=function(x,nVec=NULL,sampleNum=NULL,qVec=c(-5:5),thread=3,detrendOrder=3,sampleMethod=2,lengthRatio=0.05){
  require(parallel)
  require(RcppEigen)
  na.fail(x)
  if(is.null(nVec)){
    nNum=floor(log2(lengthRatio*length(x)))
    if(nNum<=4){
      stop("time series is too short!")
    }
    nVec=2^(4:nNum)
  }
  registerDoMC(thread)
  f2_DFA=F2_DFA(x = x,nVec = nVec,sampleNum = sampleNum,thread = thread,detrendOrder = detrendOrder,sampleMethod=sampleMethod)##a list,every element is fluctuation for one scale
  
  f_DFA=foreach(q=qVec,.combine = cbind)%dopar%{
    if(q!=0){
      sapply(f2_DFA,function(x){(mean(x^(q/2)))^(1/q)})
    }else{
      sapply(f2_DFA,function(x){exp(0.5*(mean(log(x))))})
    }
  }
  f_DFA=as.data.frame(f_DFA)
  colnames(f_DFA)<- sprintf("q%.2f",qVec)
  rownames(f_DFA)<- sprintf("n%d",nVec)
  res=as.data.frame(cbind(nVec,f_DFA))
  zeroID=apply(res,MARGIN = 1,FUN = function(row){all(row!=0)})
  return(res[zeroID,])
}