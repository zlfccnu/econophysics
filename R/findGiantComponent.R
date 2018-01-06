#' Find the giant component
#' @param GRAPH a igraph object
#' @return the size of the giant component
findGiantComponent=function(GRAPH){
  if(is.named(GRAPH)){
    g_com=components(GRAPH)
    largest_com_id=which.max(g_com$csize)
    v_id=names(which(g_com$membership==largest_com_id))
    GRAPH=induced_subgraph(graph = GRAPH,v = v_id)
    return(vcount(GRAPH))
  }else{
    stop("graph must be named!")
  }
  
}
