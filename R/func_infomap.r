#' the latest version of infomap algorithm for directed and weighted network
#' @param net a igraph object
#' @return a community class, the same as in igraph package
#' @export
cluster_infomap_new<- function(net){
  write_graph(net,file = "/tmp/net.net",format = "pajek")
  directeFlag=ifelse(is.directed(net),"directed","undirected")
  commandLine=sprintf("Infomap /tmp/net.net /tmp/ -%s --clu --silent",directeFlag)
  system(commandLine)
  clusterINFO=read_delim("/tmp/net.clu",skip = 2,delim = ' ',col_names = FALSE)
  clusterINFO=rename(clusterINFO,c(node="X1",cluster="X2",flow="X3"))
  clusterINFO=arrange(clusterINFO,node)
  membershipID=clusterINFO$cluster
  comms = make_clusters(net,membership = membershipID,algorithm = "infomap")
  return(comms)
}