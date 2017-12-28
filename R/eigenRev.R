#' Function used to reverse the order of the eigen vectors
#' @param x the matrix
#' @param symmetric symmetric or not
#' @param only.values only calculate the eigen values
#' @param EISPACK use the EISPACK
#' @return a list or a data.frame

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