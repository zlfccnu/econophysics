#' Function used to filter the covariance matrix of a series of return time series
#' @param graph an igraph object
#' @return the modularity matrix
#' @export

modularityMatrix=function(graph){
  if(!is.igraph(graph = graph)){
    stop("must be an igraph object")
  }
  adjMat=as.matrix(get.adjacency(graph = graph))
  strengthSeq=strength(graph = graph)
  nullModelMatrix=strengthSeq%*%t(strengthSeq)/(0.5*sum(strengthSeq))
  diag(nullModelMatrix)=0
  modMat=adjMat - nullModelMatrix
  return(modMat)
  }