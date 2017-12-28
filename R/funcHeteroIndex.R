library("igraph")
#' Function to eavaluate the heterogeneity of a graph
#' @param GRAPH a igraph object
#' @return A numeric value ranges from 0 to 1
funcHeteroIndex=function(GRAPH){
  n=vcount(GRAPH)
  edgelist=get.edgelist(GRAPH,names=FALSE)
  degreeSum=apply(edgelist,1,function(x){(degree(GRAPH,x[1])*degree(GRAPH,x[2]))^(-0.5)})
  return(rho=(n-2*sum(degreeSum))/(n-2*sqrt(n-1)))
}
