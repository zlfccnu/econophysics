#' Function to find the local minimum and maximum of a time series
#' @param x the time sereis
#' @param  stepT the moving window to find the local min and max
#' @export
#' @return a two elements list, the first is the local min time index and the second is local max time index
localMinMax<- function(x,stepT){
  timeX<- time(x)
  t_minP<- c()
  t_maxP<- c()
  for(i in (1+stepT):(length(timeX)- stepT)){
    timeSlice<- timeX[(i- stepT):(i +stepT)]
    x_temp<- x[timeSlice,]
    if(prod(as.vector(x_temp)>=as.vector(x_temp[timeSlice[stepT+1],]))){
      t_minP<- append(timeX[i],t_minP)
    }
    if(prod(as.vector(x_temp)<=as.vector(x_temp[timeSlice[stepT+1],]))){
      t_maxP<- append(timeX[i],t_maxP)
    }
  }
  return(list(timeX,t_minP,t_maxP))
}