library(doMC)
#' parallel compute the dfa fluctuations for different scale
#' @param x  a numeric vector which convert from a time series
#' @param y a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param overlap the overlap of the boxes
#' @param thread the multithreads number
#' @param q the order of the multifractal
#' @return DCCA fluctuation sequence for different n, a dataframe

F_DCCA_seq=function(x,y,nVec,overlap=0.9999999,thread=3,q=2){
  registerDoMC(thread)
  dcca=foreach(i=nVec,.combine =c)%dopar%{
    F_DCCA(x=x,y=y,n = i,overlap = overlap,q=q)
  }
  f_dcca_seq=cbind(nVec,dcca)
  return(f_dcca_seq)
}