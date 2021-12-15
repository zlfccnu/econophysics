#' the latest version of infomap algorithm
#' @param graph the igraph object
#' @param multilevel whether use the multilevel algorithm or not, default is 2 level detection
#' @return the community objects from the igraph package
#' @export
cluster_infomap_new<- function(graph,multilevel=FALSE){
  library(tidyverse)
  library(igraph)
  net = graph
  write_graph(net,file = "/tmp/net.net",format = "pajek")
  
  if(isTRUE(multilevel)){
    if(is.directed(net)){
      commandLine="Infomap -directed --ftree --clu --silent -i pajek /tmp/net.net /tmp/"
    }else{
      commandLine="Infomap --ftree --clu --silent -i pajek /tmp/net.net /tmp/"
    }
  }else{
    if(is.directed(net)){
      commandLine="Infomap /tmp/net.net /tmp/ -2 -directed --clu --ftree --silent -i pajek "
    }else{
      commandLine="Infomap /tmp/net.net /tmp/ -2 --clu --ftree --silent -i pajek "
    }
  }
 
  system(commandLine)
  clusterINFO=read_delim("/tmp/net.clu",skip = 9,delim = ' ',col_names = FALSE) %>% suppressMessages()
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

