#' Select stocks based the community detection info,inside community
#' @param GRAPH the correlation-based graph
#' @param mem the community membership of the GRAPH, default NULL
#' @param stock_id the names of the stocks, if the graph is unamed, will be used to name the nodes
#' @param portfolioSize the size of the portfolio, ie., the stocks you want to pick from the graph
#' @param mem_algo the algorithm to perform the community detection
#' @param type "inside" means the stocks are all picked from one community with very strong connection, "inter" means the stocks are all picked from different communities
#' @return a vector with stock names or id
#' @export
selectPortfolioCommunity=function(GRAPH,mem=NULL,mem_algo="cluster_infomap",stock_id=NULL,portfolioSize=4,type=c("inside","inter")){
  GRAPH = as_tbl_graph(GRAPH)
  if(is.null(stock_id)&!is.null(V(GRAPH)$names)){
    stock_id=V(GRAPH)$names
  }
  if(is.null(stock_id)&is.null(V(GRAPH)$names)){
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
    communitySize= nodes %>% group_by(mem_id) %>% count(name = "size")
    GRAPH = GRAPH %>% activate(nodes) %>% left_join(y = communitySize,by = c("mem_id"))
    nodes = GRAPH %>% as_tibble() %>% group_by(mem_id)
    nodes = nodes %>% filter(size>= portfolioSize)
    if(nodes %>% ungroup() %>% count()>0){
      highest_central_mem_id = nodes %>% group_by(mem_id) %>% summarise(hybird_central=sum(hybird_central)) %>% arrange(desc(hybird_central)) %>% dplyr::select(mem_id) %>% pull
      highest_central_mem_id =  highest_central_mem_id[1]
      
      select_stocks= nodes %>% filter(mem_id==highest_central_mem_id) %>% ungroup() %>% top_n(wt = hybird_central,n = portfolioSize)%>% dplyr::select(id) %>% pull
      return(select_stocks)
    }else{
      stop("the portfolio size is too large")
    }
  }
  
  if(type=="inter"){
    GRAPH = GRAPH %>% activate(nodes)
    GRAPH = GRAPH %>% mutate(id=stock_id)
    GRAPH = GRAPH %>% mutate(hybird_central=hybirdMeasure(GRAPH)$XpY)
    GRAPH = GRAPH %>% mutate(mem_id = membership(mem) %>% as.integer())
    
    nodes = GRAPH %>% as_tibble() %>% group_by(mem_id) 
    select_stocks= nodes %>% top_n(wt = hybird_central,n = -1) %>% arrange(hybird_central) %>% ungroup()
    
    if(portfolioSize <= length(select_stocks)){
      select_stocks= select_stocks %>% top_n(n= -portfolioSize,wt = hybird_central)
      return(select_stocks)
    }else{
      select_stocks = select_stocks %>% dplyr::select(id) %>% pull
      return(select_stocks)
    }
  }
  
}
