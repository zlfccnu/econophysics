#' generate the visibility graph from time series
#' @param x a time series
#' @param weighted a logical, whether create the weighted visibility graph or not
#' @return g a igraph object
#' @export
visibilityGraph=function(x,weighted=FALSE){
  x=as.vector(x)
  if(weighted==TRUE){
    edgelist=weightedVisibilityGraph(x)
    edgelist=as.data.frame(edgelist)
  }else{
    edgelist=unweightVisibilityGraph(x)
    edgelist=t(as.data.frame(edgelist))
  }
  g=graph_from_data_frame(edgelist,directed = FALSE)
  return(g)
}