#' Random attack for single network
#' @param GRAPH a igraph object
#' @param p the fraction of remaining nodes
#' @param vids the selected initial attack nodes in the target and localized attack
#' @param attack the type of attack for random, target and localized
#' @param normalized normalize the number of the giant component
#' @param decreasing sort degree from large to small or not
#' @normalized normalized the giant component size
#' @return a number of the giant component node
randomAttackSingNetwork=function(GRAPH,p=0.1,vids=NULL,attack=c("random","target","localized"),decreasing=TRUE,normalized=TRUE){
  n=vcount(GRAPH)
  GRAPH=set.vertex.attribute(GRAPH,"name",value = paste0("A",1:n))
  p=1-p
  if(attack=="random"){
    n_r=sample(x = 1:n,size = ceiling(n*p))
    n_id=names(V(GRAPH)[n_r])
    ## initial attack
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id)
    g_c=findGiantComponent(GRAPH)
  }
  if(attack=="target"){
    n_id=which(degree(GRAPH)%in% sort(degree(GRAPH),decreasing = decreasing)[1:ceiling(n*p)])
    n_id=names(V(GRAPH)[n_id])
    ## initial attack
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id)
    g_c=findGiantComponent(GRAPH)
  }
  if(attack=="localized"){
    n_id=names(V(GRAPH)[vids])## specific the vids
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id)
    g_c=findGiantComponent(GRAPH)
  }
  if(normalized==TRUE){
    return(g_c/n)
  }else{
    return(g_c)
  }
}