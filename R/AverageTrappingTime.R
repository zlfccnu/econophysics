#' Function used to calculate the averge trapping time for a graph
#' @param GRAPH A igraph object
#' @return A numeric value
averageTrappingTime = function(GRAPH){
  if(!is_igraph(GRAPH)){
    stop("Not a graph object")
  }
  Laplacian = as.matrix(graph.laplacian(GRAPH))
  LaplacianEigen=eigen(Laplacian,only.values = TRUE)
  return((sum(strength(GRAPH))/(vcount(GRAPH)-1))*sum(1/LaplacianEigen$values[- vcount(GRAPH)]))
}



