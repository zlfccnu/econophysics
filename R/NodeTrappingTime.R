#' Function used to calculate the averge trapping time for a node
#' @param GRAPH A igraph object
#' @param vids the vertex ids
#' @export
#' @return A numeric value

nodeTrappingTime=function(GRAPH,vids=V(GRAPH)){
  if(!is_igraph(GRAPH)){
    stop("Not a graph object")
  }
  Laplacian = as.matrix(graph.laplacian(GRAPH))
  LaplacianEigen = eigen(Laplacian)
  vecStrenght= strength(GRAPH)
  S=sum(strength(GRAPH))
  vtt=c()
  a=vcount(GRAPH)/(vcount(GRAPH)-1)
  x = colSums(diag(vecStrenght)%*%LaplacianEigen$vectors)
  for (i in vids){
    b=sum((1/LaplacianEigen$values*(S*LaplacianEigen$vectors[i,]^2-LaplacianEigen$vectors[i,]*x))[- vcount(GRAPH)])
    vtt=append(vtt,a*b) 
  }
  return(vtt)
}