#' Find the vertices' ids outside the giant component
#' @param GRAPH an igraph object
#' @return a vector of vertices' ids
#' @export
findSmallComponentsVid=function(GRAPH){
  if(is.named(GRAPH)){
    g_com=components(GRAPH)
    largest_com_id=which.max(g_com$csize)
    v_id=names(which(g_com$membership!=largest_com_id))
  }
  return(v_id)
}
