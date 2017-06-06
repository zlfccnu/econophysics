#'rollapply to a data frame
#'@param x the data frame
#'@param margin whic dimension you want to apply
#'@param FUN the function
#'@param ... other parameters for FUN
#'@param thread the multithreds number
#'@param windowSize the size of the rolling window
#'@param stepSize the moving window size
#'@export
rollapply.data.frame=function(x,margin=1,FUN=NULL,...,thread=3,windowSize=NULL,stepSize=NULL){
  if(margin==2){
    x=t(x)
  }
  registerDoMC(thread)
  tmp=foreach(i=seq(from=1,to = (dim(x)[1]-windowSize),by = stepSize))%dopar%{
    y=slice(as.data.frame(x),i:(i+windowSize))
    FUN(y,...)
  }
  return(tmp)
}
