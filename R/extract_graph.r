#' Extract a sub-graph according to the ratio of total edge weight
#' @param GRAPH an tidygraph object
#' @param prob the ratio of the total edge weight
#' @return an tidygraph object
#' @export
extract_graph=function(GRAPH,prob=0.8){
  stopifnot(is.tbl_graph(GRAPH))
  stopifnot(is.weighted(GRAPH))
  GRAPH<- GRAPH %>% activate(what = edges)
  GRAPH<- GRAPH %>% arrange(desc(weight))
  GRAPH<- GRAPH %>% mutate(cum_weight=cumsum(weight))
  GRAPH<- GRAPH %>% filter(cum_weight<= prob*sum(weight))
  GRAPH<- induced_subgraph(GRAPH,vids = which(degree(GRAPH)>0)) %>% as_tbl_graph()
  return(GRAPH)
}
