#'Function used to put each community as the same layout
#'@param graph A igraph object
#'@param algorithm A character name for the community detection method
#'@param layout A character name for the layout function you use
#'@export
#'@return A data frame with four columns, the vertex label, the membership of each node, two columns of the coordinates
layout_community<- function(graph,algorithm_name,layout){
  if (!is_igraph(graph)) {
    stop("Not a graph object")
  }
  community_algorithm<- match.fun(algorithm_name)
  graph_commmunity<- community_algorithm(graph)
  graph_membership<- membership(graph_commmunity)
  graph_vertex_list<- lapply(1:max(graph_membership),function(x){which(graph_membership==x)})
  split_community<- lapply(graph_vertex_list,function(x){induced_subgraph(graph,x)})## the vertex name will be renamed
  layout_func<- match.fun(layout)
  layouts<- lapply(split_community,layout_func)
  lay<- merge_coords(split_community,layouts)
  
  lay<- cbind(unlist(graph_vertex_list),sort(graph_membership),lay)
  colnames(lay)<- c("vertex_label","community_mem","coord1","coord2")
  lay<- lay[order(lay[,1]),]
  return(lay)
}