#' Find the giant component
#' @param GRAPH a igraph object
#' @param returnGRAPH return graph or not
#' @param normalized a logical, normalize the vertex number or not
#' @return the size of the giant component
findGiantComponent=function(GRAPH,returnGRAPH=FALSE,normalized=TRUE){
  n=vcount(GRAPH)
  if(is.named(GRAPH)){
    g_com=components(GRAPH)
    largest_com_id=which.max(g_com$csize)
    v_id=names(which(g_com$membership==largest_com_id))
    GRAPH=induced_subgraph(graph = GRAPH,v = v_id)
    n=ifelse(normalized==TRUE,vcount(GRAPH)/n,vcount(GRAPH))
    ifelse(returnGRAPH==TRUE,return(list(n,GRAPH)),return(n))
  }else{
    stop("graph must be named!")
  }
}
