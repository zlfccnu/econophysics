#' get hist density
#' @param x the dataset
#' @param breaks the number of bins
#' @return a data.frame
#' @export

histDensity=function(x,breaks=100){
  tempHist=hist(x=x,breaks=breaks,plot=FALSE)
  densityValue=cbind(tempHist[["mids"]],tempHist[["density"]])
  colnames(densityValue)=c("x","density")
  return(densityValue)
}