#' discretize the return of stock by standard deviation
#' @param x the return time series
#' @param sd_magnitude how many sd should use as threshold

discrete_by_sd=function(x,sd_magnitude=2){
  x_sd=sd(x)
  state_seq=seq(-(sd_magnitude+1),sd_magnitude+1)
  state_seq=state_seq[which(state_seq!=0)]
  x_sd=sort(c(min(x)-0.1,x_sd*state_seq[-c(1,length(state_seq))],max(x)+0.1,0))
  for(i in 1:(length(x_sd)-1)){
    x[which(x>x_sd[i]&x<=x_sd[i+1]&x!=0)]=state_seq[i]
  }
  x[which(x==0)]=0
  return(x)
}
