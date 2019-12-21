#' Function used to calculate the square DFA fluctuation
#' @param x  a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param sampleNum the random sampled number
#' @param thread the parallel threads number 
#' @param detrendOrder the detrending polynomial order 
#' @param sampleMethod 1 means the determined sample number method, other values mean the nonoverlap method
#' @return A list of the square DFA fluctuation for scale nVec
#' @export
F2_DFA=function(x,nVec=NULL,sampleNum=NULL,thread=3,detrendOrder=3,sampleMethod=2,lengthRatio=0.05){
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
  ##calcuate the F2_DFA series
  ## the determined sample number method
  if(sampleMethod==1){
    if(is.null(sampleNum)){
      stop("sampleNum should be given when sampleMethod is 1!")
    }
    registerDoMC(thread)
    x=cumsum(x-mean(x))
    foreach(n = nVec)%dopar%{
      f2_DFA_Tmp=1:sampleNum
      startIndex=sample((length(x)-n+1),sampleNum)
      for(i in 1:sampleNum){
        fitDFA=fastLm(x[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw = TRUE))
        res_x=fitDFA$residuals
        f2_DFA_Tmp[i]=mean(res_x^2)
      }
      return(f2_DFA_Tmp)
    }
  }else{  ### the nonoverlap sample
    registerDoMC(thread)
    x=cumsum(x-mean(x))
    x_inv=rev(x)
    x_len=length(x)
    foreach(n=nVec)%dopar%{
      sampleNum=floor(x_len/n)
      f2_DFA_Tmp=c(1:(2*sampleNum))
      startIndex=1+(0:(sampleNum-1))*n
      ## forward
      for(i in 1:sampleNum){
        fitDFA= lm(x[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw=TRUE))
        res_x=fitDFA$residuals
        f2_DFA_Tmp[i]=mean(res_x^2)
      }
      ## backward
      for(i in 1:sampleNum){
        fitDFA= lm(x_inv[startIndex[i]:(startIndex[i]+n-1)]~poly(c(startIndex[i]:(startIndex[i]+n-1)),degree = detrendOrder,raw=TRUE))
        res_x=fitDFA$residuals
        f2_DFA_Tmp[i+sampleNum]=mean(res_x^2)
      }
      return(f2_DFA_Tmp)
    }
  }
}