#' Function used to calculate the basic network topology
#' @param GRAPH a igraph object
#' @return a vector with six elements
basicGraphTopology=function(GRAPH){
  topology=rep(0,8)
  names(topology)=c("vertex","edge","clustering","shortestPath","heterogeneity","averageDegree","density","assortativity")
  topology["vertex"]=vcount(GRAPH)
  topology["edge"]=ecount(GRAPH)
  topology["clustering"]=transitivity(GRAPH)
  topology["shortestPath"]=average.path.length(GRAPH)
  topology["heterogeneity"]=funcHeteroIndex(GRAPH)
  topology["averageDegree"]=mean(degree(GRAPH))
  topology["density"]=graph.density(GRAPH)
  topology["assortativity"]=assortativity.degree(GRAPH)
  return(topology)
}