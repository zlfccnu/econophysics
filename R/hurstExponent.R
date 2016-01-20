library(doMC)
#'Function used to calculate the generalized hurst exponent
#'@param x  a numeric vector which convert from a time series
#'@param nVec  the time scale of the detrended operation, the minmum scale should be determined by calculate the autocorrelation length of the time series.
#'@param sampleNum the random sampled number
#'@param thread the parallel thread number
#'@param qVec the qth order
#'@param detrendOrder the detrending polynomial order
#'@param sampleMethod 1 means the determined sample number method, other values mean the nonoverlap method
#'@return the generalized hurst exponent
#'@export
hurstExponent=function(x,nVec=NULL,sampleNum=NULL,thread=3,qVec=c(-5:5),detrendOrder=3,sampleMethod=1){
  dfa_fluctuation = F_DFA(x=x,nVec = nVec,qVec = qVec,thread = thread,sampleNum = sampleNum ,detrendOrder=detrendOrder,sampleMethod=sampleMethod)
  registerDoMC(thread)
  foreach(i= 2:(dim(dfa_fluctuation)[2]),.combine = c)%dopar%{
    dfaLM = lm(log(dfa_fluctuation[,i])~log(dfa_fluctuation[,1]))
    return(as.numeric(dfaLM$coefficients[2]))
  }
}