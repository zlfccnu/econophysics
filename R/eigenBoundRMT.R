#' calculate the eigenvalue distribution for a random purely random matix
#'@param L the length of the time series
#'@param N the number of the time series
#'return a vector with the lower and upper bound
#'@export

eigenBoundRMT=function(Q=NULL,L,N,S){
  if(is.null(Q)){
    Q=L/N
  }
  return(c(S^2*(1+1/Q-2/sqrt(Q)),S^2*(1+1/Q+2/sqrt(Q))))
}