#' Function used to reverse the order of the eigen vectors
#' @param x
#' @param symmetric
#' @param only.values
#' @param EISPACK
#' return a list or a data.frame
#' @export
 eigenRev=function(x, symmetric, only.values = FALSE, EISPACK = FALSE){
   eigenResults=eigen(x=x, symmetric=symmetric, only.values = FALSE, EISPACK = FALSE)
    if(only.values==TRUE){
      eigenResults$values=rev(eigenResults$values)
    }else{
      eigenResults$values=rev(eigenResults$values)
      eigenResults$vectors=eigenResults$vectors[,length(eigenResults$values):1]
    }
   return(eigenResults)
 }