#' calculate the eigenvalue distribution for a random purely random matix
#'@param L the length of the time series
#'@param N the number of the time series
#'@param S the standeviation 
#'@param b the bin length
#'return a dataframe with the lower and upper bound
#'@export

eigenDistRMT=function(Q=NULL,L,N,S,n){
  if(is.null(Q)){
    Q=L/N
  }
  bound=eigenBoundRMT(Q,L,N,S)
  binSize=(bound[2] - bound[1])/n
  x=seq(bound[1],bound[2],binSize)
  y=Q/2*pi*S^2*sqrt((bound[2]-x)*(x- bound[1]))/x
  return(cbind(x,y/sum(y*binSize)))
}