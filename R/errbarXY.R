#' Function used to add the horizonal errbar and vertical errbar
#' @param x the x coordinate of data
#' @param y the y coordinate of data
#' @param xDelta the statndrad devation of x which will be used to plot the errbar
#' @param yDelta the statndrad devation of y which will be used to plot the errbar
#' @return invisible, a plot
errbarXY=function(x,y,xDelta=NULL,yDelta=NULL,col="red",type="o",pch_h="|",pch_v="-"){
  for(i in 1:length(x)){## horizonal errbar
    lines(c(x[i] - xDelta[i],x[i] + xDelta[i]),rep(y[i], each=2),col=col,type=type,pch=pch_h)
  }
  
  for(i in 1:length(x)){## vertical errbar
    lines(rep(x[i], each=2),c(y[i] - yDelta[i],y[i] + yDelta[i]),col=col,type=type,pch=pch_v)
  }
}