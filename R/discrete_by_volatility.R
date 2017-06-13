#'discretize the volatility of stock by standard deviation
#'@param x the return OR volatility time series
#'@param sd_magnitude how many sd should use as threshold
#'@export
discrete_by_volatility=function(x,sd_magnitude=2){
  x=abs(x)
  x_sd=sd(x)
  state_seq=seq(0,sd_magnitude)
  x_sd=sort(c(x_sd*state_seq,max(x)+0.1))
  for(i in 1:(length(x_sd)-1)){
    x[which(x>=x_sd[i]&x<x_sd[i+1])]=state_seq[i]
  }
  return(x)
}
