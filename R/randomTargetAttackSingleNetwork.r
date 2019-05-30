#' @param attack the type of attack for random, target and localized and motif
#' @param GRAPH igraph object
#' @param l the largets attack steps
#' @param vids the vertex ids
#' @param normalized normalize the number of the giant component
#' @param decreasing sort degree from large to small or not
#' @param normalized normalized the giant component size
#' @return a number of the giant component node
#' @export
randomTargetAttackSingleNetwork=function(GRAPH,l=10,vids=NULL,attack=c("random","target","localized","motif"),decreasing=TRUE,normalized=TRUE){
  n1=vcount(GRAPH)
  if(attack=="random"){
    g_c=c()
    for(i in 1:l){
      n=vcount(GRAPH)
      GRAPH=set.vertex.attribute(GRAPH,"name",value = paste0("A",1:n))
      n_r=sample(x = 1:n,size = 1)
      n_id=names(V(GRAPH)[n_r])
      GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id)
      g_c_g=findGiantComponent(GRAPH,returnGRAPH = TRUE)
      GRAPH=g_c_g[[2]]
      g_c=c(g_c,g_c_g[[1]])
    }
    ## initial attack
  }
  if(attack=="target"){
    g_c=c()
    for(i in 1:l){
      n=vcount(GRAPH)
      GRAPH=set.vertex.attribute(GRAPH,"name",value = paste0("A",1:n))
      n_id=which(degree(GRAPH)%in% sort(degree(GRAPH),decreasing = decreasing)[1])
      n_id=names(V(GRAPH)[n_id])
      GRAPH=delete.edges_vertices(GRAPH = GRAPH,v=n_id)
      g_c_g=findGiantComponent(GRAPH,returnGRAPH = TRUE)
      GRAPH=g_c_g[[2]]
      g_c=c(g_c,g_c_g[[1]])
    }
  }
  if(attack=="motif"){
    triangles_ids=matrix(triangles(GRAPH),ncol=3,byrow = TRUE)
    n_triangles=dim(triangles_ids)[1]
    n_attack=sample(1:n_triangles,ceiling(n_triangles*p))
    triangles_ids=triangles_ids[n_attack,]
    n_id=unique(as.vector(triangles_ids))
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
    return(g_c/n1)
  }else{
    return(g_c)
  }
}