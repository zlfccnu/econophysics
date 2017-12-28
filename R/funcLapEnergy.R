library("igraph")

#' Function to calculate the laplacian energy of a graph, weighted or unweighted
#' @param GRAPH is a igraph object,
#' @param weights the weights atrribute of a graph object
#' @param normalized means use the normalized energy or not
#' @return A numeric value
funcLaplacianEnergy=function(GRAPH,normalized=FALSE,weights=NULL){
  if(weights==NULL&&is.weighted(GRAPH)){
    lapGraph<- graph.laplacian(GRAPH,normalized=normalized,sparse = FALSE,weights=weights)
    eigenvalueLapGraph<- eigen(lapGraph,only.values = TRUE,symmetric = TRUE)
    if(normalized==FALSE){
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-mean(GRAPH$weight)))
    }
    else{
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-1))##normalized
    }
    return(lapEnergy)
  }
  if(weights==NA&&is.weighted(GRAPH)){##weights==NA
    lapGraph<- graph.laplacian(GRAPH,normalized=normalized,sparse = FALSE,weights=weights)
    eigenvalueLapGraph<- eigen(lapGraph,only.values = TRUE,symmetric = TRUE)
    if(normalized==FALSE){
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-mean(degree(GRAPH))))
    }
    else{
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-1))##normalized
    }
    return(lapEnergy)
  }
  if(is.weighted(GRAPH)==FALSE){
    lapGraph<- graph.laplacian(GRAPH,normalized=normalized,sparse=FALSE)
    eigenvalueLapGraph<- eigen(lapGraph,only.values = TRUE,symmetric = TRUE)
    if(normalized==FALSE){
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-mean(degree(GRAPH))))
    }
    else{
      lapEnergy<- sum(abs(eigenvalueLapGraph$values-1))##normalized
    }
    return(lapEnergy)
  }
}
