#' This program is trying to modeling the cascading failure
#' @param GRAPH1 the first layer of inter dependent network
#' @param GRAPH2 the scond layer of inter dependent network
#' @param attack the type of attack, random, target and localized and motif
#' @param p 1-p is the fraction of nodes that the initial attack start
#' @param q the coupling strength between two layers(not valid)
#' @param plot whether to plot the network or not
#' @param Trace whether to trace the process of the cascading failure
#' @param normalized normalized the ratio of the giant component
#' @param vids the vertex ids for localized attack
#' @return a vector with length two give the node numbers of the giant component
cascadingFailure=function(GRAPH1,GRAPH2,attack=c("random","target","localized","motif"),p=0.7,q=NULL,plot=FALSE,vids=NULL,Trace=TRUE,decreasing=TRUE,normalized=TRUE){
  n1=vcount(graph = GRAPH1)
  n2=vcount(graph = GRAPH2)
  GRAPH1=set.vertex.attribute(GRAPH1,"name",value = paste0("A",1:n1))
  GRAPH2=set.vertex.attribute(GRAPH2,"name",value = paste0("A",1:n2))
  p=1-p
  if(attack=="random"){
    n1_r=sample(x = 1:n1,size = ceiling(n1*p))## the intial random removed nodes
    n1_id=names(V(GRAPH1)[n1_r])
  }
  if(attack=="target"){
    n1_id=which(degree(GRAPH1)%in% sort(degree(GRAPH1),decreasing = decreasing)[1:ceiling(n1*p)])
    n1_id=names(V(GRAPH1)[n1_id])
  }
  if(attack=="localized"){
    n1_id=names(V(GRAPH1)[vids])
  }
  if(attack=="motif"){
    triangles_ids=matrix(triangles(GRAPH1),ncol=3,byrow = TRUE)
    n_triangles=dim(triangles_ids)[1]
    n_attack=sample(1:n_triangles,ceiling(n_triangles*p))
    triangles_ids=triangles_ids[n_attack,]
    n1_id=unique(as.vector(triangles_ids))
    n1_id=names(V(GRAPH1)[n1_id])
  }
  
  par(mfcol=c(1,2),mar=c(1,1,1,1))
  if(isTRUE(plot)){
    layout_cf=layout.fruchterman.reingold(GRAPH1)
    plot(GRAPH1,vertex.size=1.5,vertex.label=NA,layout=layout_cf,main=paste("A",1))
    plot(GRAPH2,vertex.size=1.5,vertex.label=NA,layout=layout_cf,main=paste("B",1))
  }
  ## initial attack
  GRAPH1=delete.edges_vertices(GRAPH = GRAPH1,v=n1_id)
  GRAPH2=delete.edges_vertices(GRAPH = GRAPH2,v=n1_id)
  g_c1=findGiantComponent(GRAPH1)
  g_c2=findGiantComponent(GRAPH2)
  if(isTRUE(Trace)){
    print(paste(1,"---",g_c1,"---",g_c2))
  }
  
  i=1
  while(1){
    i=i+1
    small_g_vid=findSmallComponentsVid(GRAPH1)
    GRAPH2=delete.edges_vertices(GRAPH = GRAPH2,v = small_g_vid)
    small_g_vid=findSmallComponentsVid(GRAPH2)
    GRAPH1=delete.edges_vertices(GRAPH = GRAPH1,v = small_g_vid)
    g_c1=findGiantComponent(GRAPH1)
    g_c2=findGiantComponent(GRAPH2)
    if(isTRUE(Trace)){
      print(paste(i,"---",g_c1,"---",g_c2))
    }
    if(isTRUE(plot)){
      plot(GRAPH1,vertex.size=1.5,vertex.label=NA,layout=layout_cf,main=paste("A",i))
      plot(GRAPH2,vertex.size=1.5,vertex.label=NA,layout=layout_cf,main=paste("B",i))
    }
    if(g_c1==g_c2){
      break()
    }
  }
  if(normalized==TRUE){
    return(c(g_c1/n1,g_c2/n2))
  }else{
    return(c(g_c1,g_c2))
  }
}