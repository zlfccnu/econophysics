#' construct the competition network from the trade network
#' @param trade_net a weighted trade network constructed from trade2net() function which use the uncomtrade data
#' @return a list with export and import competition networks
#' @export
competition_network=function(trade_net){
  export_countries=names(which(degree(trade_net,mode = "out")!=0))
  import_countries=names(which(degree(trade_net,mode = "in")!=0))
  ## add edges for the competion network
  ## add edges to the export layer
  export_layer=lapply(import_countries,function(x,y){
    v_ids=neighbors(y,v = x,mode = "in") %>% names()
    g=make_full_graph(n=length(v_ids))
    V(g)$name=v_ids
    return(g)
  },y=trade_net)
  
  import_layer=lapply(export_countries,function(x,y){
    v_ids=neighbors(y,v = x,mode = "out") %>% names()
    g=make_full_graph(n=length(v_ids))
    V(g)$name=v_ids
    return(g)
  },y=trade_net)
  
  union_g_list=function(g_list){
    g=g_list[[1]]
    for(i in 2:length(g_list)){
      g=igraph::union(g,g_list[[i]])
    }
    return(g)
  }
  
  export_layer=do.call(graph.union,export_layer)## use the do.call to combine a graph list into single graph
  #union_g_list(export_layer)
  import_layer=do.call(graph.union,import_layer)
  #union_g_list(import_layer)
  ## add the edge weight as the competetion intensity
  ## tmp function calculate the competetion intensity
  competition_intensity=function(trade_net,e_name,mode=c("out","in")){
    nei_1=neighborhood(trade_net,nodes = e_name[1],mode = mode)
    nei_2=neighborhood(trade_net,nodes = e_name[2],mode = mode)
    com_nei=intersect(nei_1,nei_2)
    com_nei=as.list(com_nei)
    path1=lapply(com_nei,function(x){x<- c(e_name[1],x);return(x)})
    path1=unlist(path1)
    path2=lapply(com_nei,function(x){x<- c(e_name[2],x);return(x)})
    path2=unlist(path2)
    w1=E(trade_net,P=path1)$weight
    w2=E(trade_net,P=path2)$weight
    w1_all=strength(trade_net,vids = e_name[1],mode = mode)
    w2_all=strength(trade_net,vids = e_name[2],mode = mode)
    w_all=sum(strength(trade_net))
    s12=sum(((w1+w2)/w_all)*(1-abs(w1/w1_all-w2/w2_all)/(w1/w1_all+w2/w2_all)))*100
    return(s12)
  }
  ## add the export competetion intensity
  export_intensity=rep(0,ecount(export_layer))
  for(i in 1:length(export_intensity)){
    e_name=ends(export_layer,i)
    export_intensity[i]=competition_intensity(trade_net,e_name = e_name,mode = "out")
  }
  ## add the import competition intensity
  import_intensity=rep(0,ecount(import_layer))
  for(i in 1:length(import_intensity)){
    e_name=ends(import_layer,i)
    import_intensity[i]=competition_intensity(trade_net,e_name = e_name,mode = "in")
  }
  E(export_layer)$weight=export_intensity
  E(import_layer)$weight=import_intensity
  
  ## delete the unnecessary graph atrributes
  graph_attr(export_layer) %>% names()-> attr_list
  for(i in attr_list){
    export_layer<- delete_graph_attr(export_layer,i)
  }
  
  graph_attr(import_layer) %>% names()-> attr_list
  for(i in attr_list){
    import_layer<- delete_graph_attr(import_layer,i)
  }
  ## add the longtitude and latitude info to the competition network
  
  return(list(export_compet_net=export_layer,import_compet_net=import_layer))
}