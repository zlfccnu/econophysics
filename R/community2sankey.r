#' plot community as sankey diagram
#' @param net_list  a list of igraph objects
#' @param font_size the font size of the plot
#' @param scaled whether the edge weight of overlapping nodes between two successive communities should be normalized or not
#' @param sinksRight should the plot align to right or not
#' @param nodePadding the width height
#' @param nodeWidth the width of the nodes
#' @return a list with sankey data and the sankey diagram object
#' @export
community2sankey<- function (net_list,font_size=18,scaled=FALSE,sinksRight=TRUE,nodePadding=10,nodeWidth=30) 
{
  library(tidyverse)
  library(tidygraph)
  library(igraph)
  library(networkD3)
  ### construct the community Coarse-grained network
  community_list=suppressMessages(lapply(net_list,cluster_infomap_new))
  ### convert the community list into sankey data types
  comm_info = lapply(1:length(net_list),FUN = function(x,graph,comm){
    bind_cols(name=V(graph[[x]])$name,comm_name=paste(names(comm)[x],LETTERS[membership(comm[[x]])],sep = "-"),year=as.integer(rep(names(comm)[x],vcount(graph[[x]]))),comm_id=as.numeric(membership(comm[[x]])))},graph=net_list,comm=community_list)
  names(comm_info)<- names(net_list)
  comm_info<- lapply(comm_info,FUN = function(x){
    arrange(x,comm_id)
  })
  comm_info_combine<- bind_rows(comm_info)### combine
  comm_info_combine$comm_id_all<- (comm_info_combine %>% group_indices(comm_name))-1
  
  comm_info_split<- comm_info_combine %>% group_by(year) %>% group_split()### split
  comm_nums<- sapply(community_list,FUN = function(x){max(membership(x))})
  
  community_similarity<- function(x,y,scaled=TRUE){
    x_split<- x %>% group_by(comm_id_all) %>% group_split()
    y_split<- y %>% group_by(comm_id_all) %>% group_split()
    v_names<- c(unique(x$comm_id_all),unique(y$comm_id_all))
    adj_mat<- matrix(data = 0,nrow = length(v_names),ncol = length(v_names))
    rownames(adj_mat)<- v_names
    colnames(adj_mat)<- v_names
    x_names<- as.character(unique(x$comm_id_all))
    y_names<- as.character(unique(y$comm_id_all))
    if(isTRUE(scaled)){
      for(i in 1:length(x_names)){
        for(j in 1:length(y_names)){
          adj_mat[x_names[i],y_names[j]]<- length(intersect(x_split[[i]]$name,y_split[[j]]$name))/length(union(x_split[[i]]$name,y_split[[j]]$name))
        }
      }
    }else{
      for(i in 1:length(x_names)){
        for(j in 1:length(y_names)){
          adj_mat[x_names[i],y_names[j]]<- length(intersect(x_split[[i]]$name,y_split[[j]]$name))
        }
      }
    }
    
    
    g<- graph_from_adjacency_matrix(adjmatrix = adj_mat,mode = "upper",diag = FALSE,weighted = TRUE)
    edge_list<- cbind(as.data.frame(get.edgelist(g),stringsAsFactors = FALSE),E(g)$weight)
    colnames(edge_list)<- c("source","target","value")
    return(edge_list)
  }
  
  edge_list=list()
  for(i in 1:(length(comm_info_split)-1)){## loop on year index
    edge_list[[i]]<- community_similarity(comm_info_split[[i]],comm_info_split[[i+1]],scaled =scaled)
  }
  edge_list_all<- bind_rows(edge_list)
  edge_list_all<- apply(edge_list_all,2,as.numeric)
  edge_list_all<- as.data.frame(edge_list_all,stringsAsFactors = FALSE)
  rownames(edge_list_all)<- as.character(1:dim(edge_list_all)[1])
  nodes<- data.frame(name=unique(comm_info_combine$comm_name),stringsAsFactors = FALSE)
  rownames(nodes)<- as.character(1:dim(nodes)[1])
  sankeyData<- list(nodes=nodes,links=edge_list_all)
  sankeyData$links$type <- sub(' .*', '', sankeyData$nodes[sankeyData$links$source + 1, 'name'])
  units=""
  ### add links groups
  
  if(isTRUE(scaled)){
    sankeyData$links$value<- sankeyData$links$value*100
    units="%"
  }
  p <- sankeyNetwork(Links = sankeyData$links, Nodes = sankeyData$nodes, Source = "source",Target = "target", Value = "value", NodeID = "name",fontSize = font_size,nodeWidth = nodeWidth,units = units,sinksRight=sinksRight,nodePadding=nodePadding,colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10)"),LinkGroup ="type")
  return(list(data=sankeyData,p=p))
}