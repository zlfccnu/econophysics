#' convert the uncomtrade data tibble to igraph networks
#' @param data a tidyverse like tibble
#' @param weight use which value as the weight of the edges, import value or export value
#' @return a list with three igraph networks
#' @export
trade2net=function(data,weight=c("max","mean")){
  filter<- dplyr::filter
  el_import<- filter(data,trade_flow_combine=="Import") %>% select(from=partner,to=reporter,weight=netweight_tone)
  graph_import=graph_from_data_frame(d = el_import,directed = TRUE)
  el_export<- filter(data,trade_flow_combine=="Export") %>% select(from=reporter,to=partner,weight=netweight_tone)
  graph_export=graph_from_data_frame(d=el_export,directed = TRUE)
  graph_import=igraph::simplify(graph_import)
  graph_export=igraph::simplify(graph_export)
  graph_combine=graph_import%u%graph_export
  ## tmp function to 
  (function(GRAPH,weight=c("max","mean")){
    E(GRAPH)$weight_1=replace(E(GRAPH)$weight_1,is.na(E(GRAPH)$weight_1),0)
    E(GRAPH)$weight_2=replace(E(GRAPH)$weight_2,is.na(E(GRAPH)$weight_2),0)
    if(weight=="max"){
      E(GRAPH)$weight=pmax(E(GRAPH)$weight_1,E(GRAPH)$weight_2)## use the max weight from export and import 
    }
    if(weight=="mean"){
      E(GRAPH)$weight=mean(c(E(GRAPH)$weight_1,E(GRAPH)$weight_2))## use the max weight from export and import 
    }
    GRAPH=delete_edge_attr(GRAPH,"weight_1")
    GRAPH=delete_edge_attr(GRAPH,"weight_2")
    return(GRAPH)
  })(graph_combine,weight)-> graph_combine
  results=list(graph_combine=graph_combine,graph_import=graph_import,graph_export=graph_export)
  return(results)
}