#' Delete edges end with specific nodes
#' @param GRAPH an igraph object
#' @param v a vector of vids
#' @return an igraph object
delete.edges_vertices=function(GRAPH,v){
  eid=incident_edges(graph = GRAPH,v=v)## very slow
  eid=unique(unlist(sapply(X=eid,FUN = as.integer,simplify = TRUE)))
  GRAPH=delete.edges(GRAPH,edges = E(GRAPH)[eid])
  return(GRAPH)
}
