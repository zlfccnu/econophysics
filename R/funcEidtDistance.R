library("igraph")
#' Function to calculate a simple edit distance or jaccard index
#' @param GRAPH1 a igraph object
#' @param GRAPH2 a igraph object
#' @return A numeric value ranging from 0 to 1
funcEditDistance=function(GRAPH1,GRAPH2){
  return(ecount(GRAPH1%s%GRAPH2)/ecount(GRAPH1%u%GRAPH2))
}
