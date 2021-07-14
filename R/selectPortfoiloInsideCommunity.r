#' Select sotcks based the community detection info,inside community
#' @param GRAPH the correlation-based graph
#' @param mem the community membership of te GRAPH
#' @param stock_id the names of the stocks 
#' @param portfolioSize the size of the portfolio
#' @return a vector with stock names
#' @export
selectPortfolioInsideCommunity=function(GRAPH,mem,stock_id,portfolioSize=4){
  communitySize=sizes(mem)
  mem_id=which(communitySize>=portfolioSize)
  if(length(mem_id)!=0){
    mem_id=ifelse(length(mem_id)==1,yes =mem_id,no =  sample(mem_id,1))
    selectStocks=stock_id[which(membership(mem)==mem_id)]
    selectStocks=sample(selectStocks,portfolioSize)
    return(selectStocks)
  }else{
    stop("The portfolioSize is too large!")
  }
}
