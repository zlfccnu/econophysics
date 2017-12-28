#' Function calculate hybird measure for centrality of graph
#' @param GRAPH_W an weighted igraph object 
#' @return a list or a data.frame with colnames X,Y,X-Y,X+Y, the variable X+Y can be used as compound centrality
hybirdMeasure=function(GRAPH_W){
  if(!is.weighted(GRAPH_W))
    stop("The graph must be weighted one!")
  
  GRAPH_U=delete_edge_attr(GRAPH_W,"weight")
  N=vcount(GRAPH_W)
  X=(rank(centr_degree(GRAPH_W)$res)+rank(centr_degree(GRAPH_U)$res)+rank(centr_betw(GRAPH_W)$res)+rank(centr_betw(GRAPH_U)$res)-4)/(4*(N-1))
  Y=(rank(centr_clo(GRAPH_W)$res)+rank(centr_clo(GRAPH_U)$res)+rank(centr_eigen(GRAPH_W)$vector)+rank(centr_eigen(GRAPH_U)$vector)+rank(eccentricity(GRAPH_W))+rank(eccentricity(GRAPH_U))-6)/(6*(N-1))
  results=cbind(X,Y,X+Y,X-Y)
  if(!is.null(get.vertex.attribute(GRAPH_W,name="name"))){
    rownames(results)=get.vertex.attribute(GRAPH_W,name = "name")
  }
  colnames(results)=c("X","Y","XpY","XmY")
  return(results)
}
