#' Function used to random shuffle some time series
#' @param x the time series you want to shuffle
#' @param n how many times you want to shuffle
#' @return a vector after random shuffle

randomShuffle <- function(x,n){
  for (i in 1:n ) {
    x = x[.Internal(sample(length(x),length(x), FALSE, NULL))]
  }
  return(x)
}