#' Select sotcks based the community detection info and the centrality ranking
#' @param GRAPH the correlation-based graph
#' @param mem the community membership of te GRAPH
#' @param stock_id the names of the stocks 
#' @param portfolioSize the size of the portfolio
#' @return a vector of stock tickers
#' @export
selectPortfolioInterCommunity=function(GRAPH,mem,stock_id,portfolioSize=4){
  inter_com_neighbor=function(GRAPH,mem,v){
    nei_v=neighborhood(graph = GRAPH,order = 1,nodes = v)
    mem_nei_v=lapply(nei_v, function(x){
      membership(mem)[x]
    })
    mem_v=membership(mem)[v]
    out_nei_num_v=sapply(1:length(mem_nei_v),function(x){
      which(mem_nei_v[[x]]!=mem_v[x])
    })
    return(sapply(out_nei_num_v, length))
  }
  
  select_stocks=c()
  for(i in 1:length(sizes(mem))){
    inter_com_nei_num=inter_com_neighbor(GRAPH,mem = mem,v=which(membership(mem)==i))
    v_id=intersect(which(membership(mem)==i),which(membership(mem)==i)[which(inter_com_nei_num==0)])
    
    if(length(v_id)!=0){
      if(length(v_id==1)){
        select_stocks=c(select_stocks,stock_id[v_id])
      }else{
        tmp=hybirdMeasure(GRAPH)[v_id,"XpY"]
        tmp=which.max(tmp)
        select_stocks=c(select_stocks,stock_id[tmp])
      }
    }
  }
  if(portfolioSize<=length(select_stocks)){
    select_stocks=sample(select_stocks,size=portfolioSize)
    return(select_stocks)
  }else{
    return(select_stocks)
  }
}
