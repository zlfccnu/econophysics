#' Random attack for single network
#' @param GRAPH a igraph object
#' @param p the fraction of remaining nodes
#' @param vids the selected initial attack nodes in the target and localized attack
#' @param attack the type of attack for random, target and localized and motif, the attack by id can be proceeded by the localized attack
#' @param normalized normalize the number of the giant component
#' @param decreasing sort degree from large to small or not
#' @param normalized the giant component size
#' @param mode whether to delete the outgoing incoming or both edges for selected nodes
#' @param returnGraph a logical, whether return the remaining graph or not
#' @return a number of the giant component node
#' @export
randomAttackSingleNetwork=function(GRAPH,p=0.1,vids=NULL,attack=c("random","target","localized","motif"),mode=c("out","in","all","total"),decreasing=TRUE,normalized=TRUE,returnGRAPH=FALSE){
  n=vcount(GRAPH)
  GRAPH=set.vertex.attribute(GRAPH,"name",value = paste0("A",1:n))
  p=1-p
  if(attack=="random"){
    n_r=sample(x = 1:n,size = ceiling(n*p))
    n_id=names(V(GRAPH)[n_r])
    ## initial attack
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id,mode=mode)
    g_c=findGiantComponent(GRAPH,returnGRAPH = returnGRAPH,normalized=normalized)
  }
  if(attack=="target"){
    n_id=which(degree(GRAPH)%in% sort(degree(GRAPH),decreasing = decreasing)[1:ceiling(n*p)])
    n_id=names(V(GRAPH)[n_id])
    ## initial attack
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id,mode = mode)
    g_c=findGiantComponent(GRAPH,returnGRAPH = returnGRAPH,normalized=normalized)
  }
  if(attack=="motif"){
    triangles_ids=matrix(triangles(GRAPH),ncol=3,byrow = TRUE)
    n_triangles=dim(triangles_ids)[1]
    n_attack=sample(1:n_triangles,ceiling(n_triangles*p))
    triangles_ids=triangles_ids[n_attack,]
    n_id=unique(as.vector(triangles_ids))
    n_id=names(V(GRAPH)[n_id])
    ## initial attack
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id,mode=mode)
    g_c=findGiantComponent(GRAPH,returnGRAPH = returnGRAPH,normalized=normalized)
  }
  if(attack=="localized"){
    n_id=names(V(GRAPH)[vids])## specific the vids
    GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id,mode=mode)
    g_c=findGiantComponent(GRAPH,returnGRAPH = returnGRAPH,normalized=normalized)
  }
  return(g_c)
}