#' community centrality of network
#' @param graph an igraph object
#' @param diag set the diag elements of null model matrix to zero
#' @return the modularity matrix

communityCentrality=function(graph,diag=FALSE){
  modMat=modularityMatrix(graph = graph,diag=diag)
  modMatEigen=eigen(modMat)
  p=length(which(modMatEigen$values>0))
  x=sqrt(modMatEigen$values[1:p])*t(modMatEigen$vectors[,1:p])
  y=apply(x,MARGIN = 2,function(x){sqrt(sum(x^2))})
  return(y)
}