#' Function used to random shuffle some time series
#' @param x the time series you want to shuffle
#' @param n how many times you want to shuffle
#' @return a vector
#' @export
randomShuffle <- function(x,n){
  for (i in 1:n ) {
    x = x[.Internal(sample(n,n, FALSE, NULL))]
  }
  return(x)
}