#' Extract a sub-graph according to the ratio of total edge weight
#' @param GRAPH an igraph object
#' @param prob the ratio of the total edge weight
#' @return an igraph object
#' @export
extract_graph=function(GRAPH,prob=0.8){
  stopifnot(is.weighted(GRAPH))
  E(GRAPH)[order(weight)] %>% rev()-> test
  test[1:which(cumsum(test$weight)>= prob*sum(test$weight))[1]]
  subgraph.edges(GRAPH,eids =test[1:which(cumsum(test$weight)>= prob*sum(test$weight))[1]])
}