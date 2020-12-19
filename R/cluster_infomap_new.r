#' the latest version of infomap algorithm
#' @param net the igraph object
#' @return the community objects from the igraph package
#' @export
cluster_infomap_new<- function(net,multilevel=FALSE){
  library(tidyverse)
  library(igraph)
  write_graph(net,file = "/tmp/net.net",format = "pajek")
  directeFlag=ifelse(is.directed(net),"directed","undirected")
  
  if(isTRUE(multilevel)){
    commandLine=sprintf("Infomap /tmp/net.net /tmp/ -%s --clu --silent",directeFlag)
  }else{
    commandLine=sprintf("Infomap /tmp/net.net /tmp/ -2 -%s --clu --silent",directeFlag)
  }
 
  system(commandLine)
  clusterINFO=read_delim("/tmp/net.clu",skip = 2,delim = ' ',col_names = FALSE)
  clusterINFO=rename(clusterINFO,c(node="X1",cluster="X2",flow="X3"))
  clusterINFO=arrange(clusterINFO,node)
  # clusterINFO=arrange(clusterINFO,cluster)
  # size=clusterINFO$cluster %>% table() %>% sort(decreasing = TRUE) %>% as.data.frame()
  # new_cluster_id=c()
  # for(i in 1:dim(size)[1]){
  #   new_cluster_id<- append(new_cluster_id,values = rep(i,size[,2][i]))
  # }
  # clusterINFO$cluster<- new_cluster_id
  membershipID=clusterINFO$cluster
  comms = make_clusters(net,membership = membershipID,algorithm = "infomap")
  return(comms)
}

