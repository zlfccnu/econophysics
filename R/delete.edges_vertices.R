#' Delete edges end with specific nodes
#' @param GRAPH an igraph object
#' @param v a vector of vids
#' @param mode delete the outgoing or incoming edges or both, ignored for undirecte graphs
#' @return an igraph object
delete.edges_vertices=function(GRAPH,v,mode=c("out","in","all","total")){
  eid=incident_edges(graph = GRAPH,v=v,mode = mode)## very slow
  eid=unique(unlist(sapply(X=eid,FUN = as.integer,simplify = TRUE)))
  GRAPH=delete.edges(GRAPH,edges = E(GRAPH)[eid])
  return(GRAPH)
}
