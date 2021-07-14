#' Function used to find the local minimum and local maximum
#' @param x a time series
#' @return two numeric values, the cycle length of local maxima and minimia
#' @export
find_cycles=function(x){
  extremeValues=extrema(x)
  cycles=c(extremeValues[[1]][,1]%>>%diff%>>%mean,extremeValues[[2]][,1]%>>%diff%>>%mean)
  names(cycles)<- c("cycle_min","cycle_max")
}