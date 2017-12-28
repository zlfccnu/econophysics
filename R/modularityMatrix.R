#' modularity matrix for netowrk
#' @param graph an igraph object
#' @param diag set the diag elements of matrix to zero or not
#' @return the modularity matrix

modularityMatrix=function(graph,diag=FALSE){
  if(!is.igraph(graph = graph)){
    stop("must be an igraph object")
  }
  if(is.weighted(graph = graph)){
    adjMat=as.matrix(get.adjacency(graph = graph,attr = "weight"))
  }else{
    adjMat=as.matrix(get.adjacency(graph = graph))
  }
  
  strengthSeq=strength(graph = graph)
  nullModelMatrix=strengthSeq%*%t(strengthSeq)/(sum(strengthSeq))
  if(diag==TRUE){
    diag(nullModelMatrix)=0
  }
  modMat=adjMat - nullModelMatrix
  return(modMat)
  }