#' Select stocks based the community detection info,inside community
#' @param GRAPH the correlation-based graph
#' @param mem the community membership of the GRAPH, default NULL
#' @param stock_id the names of the stocks, if the graph is unamed, will be used to name the nodes
#' @param portfolioSize the size of the portfolio, ie., the stocks you want to pick from the graph
#' @param mem_algo the algorithm to perform the community detection
#' @param central_rule the rule use to compare different community by the hybird centrality measure when choose one stocks over another
#' @param type "inside" means the stocks are all picked from one community with very strong connection, "inter" means the stocks are all picked from different communities
#' @return a vector with stock names or id
#' @export
selectPortfolioCommunity=function(GRAPH,mem=NULL,mem_algo="cluster_infomap_new",stock_id=NULL,portfolioSize=4,type=c("inside","inter"),central_rule=c("sum","mean")){
  GRAPH = as_tbl_graph(GRAPH)
  if(is.null(stock_id)&!is.null(V(GRAPH)$name)){
    stock_id=V(GRAPH)$name
  }
  if(is.null(stock_id)&is.null(V(GRAPH)$name)){
    stock_id=1:vcount(GRAPH)
  }
  if(is.null(mem)){
    cluster_algo = get(mem_algo)
    mem= cluster_algo(GRAPH)
  }
  
  if(type=="inside"){
    GRAPH = GRAPH %>% activate(nodes)
    GRAPH = GRAPH %>% mutate(id=stock_id)
    GRAPH = GRAPH %>% mutate(hybird_central=hybirdMeasure(GRAPH)$XpY)
    GRAPH = GRAPH %>% mutate(mem_id = membership(mem) %>% as.character())
    nodes = GRAPH %>% as_tibble() %>% group_by(mem_id) %>% mutate(size=n())
    hybird_central_total = nodes %>% summarise(hybird_central_total= sum(hybird_central))
    hybird_central_avg = nodes %>% summarise(hybird_central_avg= mean(hybird_central))
    nodes = nodes %>% left_join(y = hybird_central_total,by="mem_id")
    nodes = nodes %>% left_join(y = hybird_central_avg, by = "mem_id")
    if(central_rule=="sum"){
      nodes = nodes %>% arrange(desc(hybird_central_total),desc(hybird_central),.by_group = TRUE)
      select_stocks = nodes$id[1:portfolioSize]
    }
    if(central_rule=="mean"){
      nodes = nodes %>% arrange(desc(hybird_central_avg),desc(hybird_central),.by_group = TRUE)
      select_stocks = nodes$id[1:portfolioSize]
    }
  }
  
  if(type=="inter"){
    GRAPH = GRAPH %>% activate(nodes)
    GRAPH = GRAPH %>% mutate(id=stock_id)
    GRAPH = GRAPH %>% mutate(hybird_central=hybirdMeasure(GRAPH)$XpY)
    GRAPH = GRAPH %>% mutate(mem_id = membership(mem) %>% as.integer())
    nodes = GRAPH %>% as_tibble() %>% group_by(mem_id) 
    select_stocks= nodes %>% arrange(hybird_central,.by_group = TRUE)%>%mutate(rank = rank(hybird_central, ties.method = "first")) %>% ungroup()%>% group_by(rank) %>% arrange(hybird_central,.by_group = TRUE) %>% ungroup()
   select_stocks = select_stocks[1:portfolioSize,] %>% dplyr::select(id) %>% pull
  }
  return(select_stocks)
}
