#' ranking the node importance for the trade network 
#' @param GRAPH a weighted network
#' @param YEAR the year info
#' @return a tibble
#' @export
node_ranking=function(GRAPH,YEAR){
  ranking=bind_cols(country=V(GRAPH)$name,pageRank=page_rank(GRAPH)$vector,degree_out=igraph::degree(GRAPH,mode = "out"),degree_in=igraph::degree(GRAPH,mode = "in"),betweenness=betweenness(GRAPH),strength_out=strength(GRAPH,mode = "out"),strength_in=strength(GRAPH,mode="in"),eigen_central=eigen_centrality(GRAPH)$vector,alpha_central=alpha_centrality(GRAPH),authority_score=authority.score(GRAPH)$vector,closeness=closeness(GRAPH),eccentricity=eccentricity(GRAPH),year=rep(YEAR,vcount(GRAPH)))
  ranking=as_tibble(ranking)
  return(ranking)
}