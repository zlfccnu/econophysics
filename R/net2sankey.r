#' plot the network with sankey diagram, either by country or by continent
#' @param net an igraph or tidygraph object
#' @param percent the cumulative percnet value used to filter out the edges by cumulative weight
#' @param fontSize the font size of the plot
#' @param fontFamily the font type of the plt
#' @param nodeWidth the width of the nodes
#' @param nodePadding the width of the height
#' @param sinksRight whether the plot will align to the right
#' @return a list with the sankey data for both country and continent and the sankey digram object for the country and continent with different coloring scheme
#' @export
net2sankey<- function(net,percent=NULL,fontSize=25,fontFamily="Times New Roman",nodeWidth=20,nodePadding=10,sinksRight=FALSE){
  library(tidyverse)
  library(dplyr)
  library(igraph)
  library(networkD3)
  library(tidygraph)
  library(conflicted)
  ## dplyr functions win
  fs<- showPackageContents("package:dplyr")
  for(i in fs$Non_primitiveFunctions){
    conflicted::conflict_prefer(i,winner = "dplyr",quiet = TRUE)
  }

  net<- as_tbl_graph(net)
  net1<- net
  stopifnot(is.tbl_graph(net))
  sankeyData<-list()
  ## activate the edges
  net<- activate(net,edges)
  net<- net %>% arrange(desc(weight)) %>% mutate(cum_percent=cumsum(weight)/sum(weight)) %>% filter(cum_percent<=percent)
  net<- as.igraph(net)
  net<- delete.vertices(net,which(igraph::degree(net)==0))
  net<- as_tbl_graph(net)
  
  ## add the node groups
  net<- activate(net,nodes)
  net<- net %>% mutate(group=as.factor(membership(cluster_infomap_new(net))))
  sankeyData[[1]]<- as_tibble(net)### sankey data for nodes
  
  
  ## add link groups reference to the node groups
  net<- left_join(net %>% activate(edges),activate(net,nodes) %>% dplyr::select(name,group) %>% as_tibble(),by=c("from_name"="name"))
  net<- activate(net,edges)
  sankeyData[[2]]<- as_tibble(net)### sankey data for edges
  ## id rename from 0
  sankeyData[[2]]<- mutate(sankeyData[[2]],from=from-1,to=to-1)
  ## reorder the edges
  sankeyData[[2]]<- arrange(sankeyData[[2]],from)
  # sankeyData<- networkD3::igraph_to_networkD3(net,group =cluster_infomap_new(net) )
  # sankeyData<- lapply(sankeyData,as_tibble)
  # sankeyData[[1]]<- mutate(sankeyData[[1]],weight=)
  
  names(sankeyData)<- c("nodes","links")
  p_country_community<- networkD3::sankeyNetwork(Links = sankeyData[[2]],Nodes = sankeyData[[1]],Source = "from",Target = "to",Value = "weight",NodeID = "name",fontSize = fontSize,NodeGroup = "group",LinkGroup = "group",fontFamily = fontFamily,nodePadding = nodePadding,nodeWidth = nodeWidth,sinksRight=sinksRight)
  #return(sankeyData)
  sankeyData$links$type <-  sankeyData$nodes[sankeyData$links$from + 1, 'name'] %>% pull
  p_country<- networkD3::sankeyNetwork(Links = sankeyData[[2]],Nodes = sankeyData[[1]],Source = "from",Target = "to",Value = "weight",NodeID = "name",fontSize = fontSize,fontFamily = fontFamily,LinkGroup = "type",nodePadding = nodePadding,nodeWidth = nodeWidth,sinksRight=sinksRight)
  
  ### plot the continent trade flow with sankey
  sankeyDataContinent<- list()
  net1<- activate(net1,edges)
  sankeyDataContinent[[2]]<- net1 %>% as_tibble %>% group_by(from_continent,to_continent) %>% summarise(weight=sum(weight)) %>% ungroup()
  sankeyDataContinent[[2]]<- rename(sankeyDataContinent[[2]],from=from_continent,to=to_continent)
  name<- sankeyDataContinent[[2]] %>% dplyr::select(from,to) %>% pull %>% unique()
  sankeyDataContinent[[1]]<- tibble(name,id=0:(length(name)-1))
  sankeyDataContinent[[2]]<- left_join(sankeyDataContinent[[2]],sankeyDataContinent[[1]],by=c("from"="name")) %>% mutate(from=id) %>% dplyr::select(-c(id)) %>% left_join(.,sankeyDataContinent[[1]],by=c("to"="name")) %>% mutate(to=id) %>% dplyr::select(-c(id))
  names(sankeyDataContinent)<- c("nodes","links")
  sankeyDataContinent$links$type <- sankeyDataContinent$nodes[sankeyDataContinent$links$from + 1,1] %>% pull
  p_continent<- sankeyNetwork(Links = sankeyDataContinent$links, Nodes = sankeyDataContinent$nodes, Source = "from",Target = "to", Value = "weight", NodeID = "name",fontSize = fontSize,nodeWidth = nodeWidth,sinksRight=sinksRight,nodePadding=nodePadding,colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10)"),LinkGroup ="type",fontFamily = fontFamily)
  return(list(country_data=sankeyData,continent_data=sankeyDataContinent,p_country=p_country,p_country_community=p_country_community,p_continent=p_continent))
}
