#' plot the fluctuation function for DFA
#' @param x the time series
#' @param xlab the x label
#' @param ylab the y label
#' @param ylim the y coordinate range 
#' @param tck the plot tck value for the parameter option
#' @return invisible
#' @export
plot.dfa=function(x,xlab="s",ylab="fq",ylim=NULL,tck=0.02){
  par(tck=tck)
  if(is.null(ylim)){
    ylim=c(min(x[,-1]),max(x[,-1]))
  }
  plot(x$nVec,x[,2],log="xy",xlab=xlab,ylab=ylab,ylim=ylim,type='l')
  for(i in 3:dim(x)[2]){
    lines(x$nVec,x[,i])
  }
}