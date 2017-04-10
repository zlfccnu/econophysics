#' modularity matrix for netowrk
#' @param graph an igraph object
#' @return the modularity matrix
#' @export

modularityMatrix=function(graph,diag=TRUE){
  if(!is.igraph(graph = graph)){
    stop("must be an igraph object")
  }
  adjMat=as.matrix(get.adjacency(graph = graph,attr = "weight"))
  strengthSeq=strength(graph = graph)
  nullModelMatrix=strengthSeq%*%t(strengthSeq)/(sum(strengthSeq))
  if(diag==TRUE){
    diag(nullModelMatrix)=0
  }
  modMat=adjMat - nullModelMatrix
  return(modMat)
  }