#' Function used to calculate the averge degree of a graph
#' @param x a time series or 
#' @export
#' @return two numeric values, the cycle length of local maxima and minimia
find_cycles=function(x){
  extremeValues=extrema(x)
  cycles=c(extremeValues[[1]][,1]%>>%diff%>>%mean,extremeValues[[2]][,1]%>>%diff%>>%mean)
  names(cycles)<- c("cycle_min","cycle_max")
}