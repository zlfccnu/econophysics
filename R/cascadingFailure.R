#' This program is trying to modeling the cascading failure
#' @param GRAPH1 the first layer of inter dependent network
#' @param GRAPH2 the scond layer of inter dependent network
#' @param attack the type of attack, random, target
#' @param p 1-p is the fraction of nodes that the initial attack start
#' @param q the coupling strength between two layers
#' @param plot whether to plot the network or not
#' @return a vector with length two give the node numbers of the giant component
cascadingFailure=function(GRAPH1,GRAPH2,attack="random",p=0.7,q=NULL,plot=FALSE,vids=NULL){
  n1=vcount(graph = GRAPH1)
  n2=vcount(graph = GRAPH2)
  GRAPH1=set.vertex.attribute(GRAPH1,"name",value = paste0("A",1:n1))
  GRAPH2=set.vertex.attribute(GRAPH2,"name",value = paste0("A",1:n2))
  p=1-p
  n1_r=sample(x = 1:n1,size = ceiling(n1*p))## the intial random removed nodes
  n1_id=names(V(GRAPH1)[n1_r])
  
  ## initial attack
  GRAPH1=delete.edges_vertices(GRAPH = GRAPH1,v=n1_id)
  GRAPH2=delete.edges_vertices(GRAPH = GRAPH2,v=n1_id)
  g_c1=findGiantComponent(GRAPH1)
  g_c2=findGiantComponent(GRAPH2)
  print(paste(1,"---",g_c1,"---",g_c2))
  
  i=1
  while(1){
    i=i+1
    small_g_vid=findSmallComponentsVid(GRAPH1)
    GRAPH2=delete.edges_vertices(GRAPH = GRAPH2,v = small_g_vid)
    small_g_vid=findSmallComponentsVid(GRAPH2)
    GRAPH1=delete.edges_vertices(GRAPH = GRAPH1,v = small_g_vid)
    g_c1=findGiantComponent(GRAPH1)
    g_c2=findGiantComponent(GRAPH2)
    print(paste(i,"---",g_c1,"---",g_c2))
    if(g_c1==g_c2){
      break()
    }
  }
  return(c(g_c1,g_c2))
}