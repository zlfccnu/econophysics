library("igraph")
#' Function to calculate the energy of a graph which is the sum of the abs of all the eigenvalue
#' @export
#' @param GRAPH  a igraph object
#' @return Return a numeric value
funcGraphEnergy=function(GRAPH){
  if(is.weighted(GRAPH)){
    eigenvalueGraph<- eigen(get.adjacency(GRAPH,attr="weight",sparse = FALSE),only.values = TRUE,symmetric = TRUE)
    return(sum(abs(eigenvalueGraph$values)))
  }
  if(is.weighted(GRAPH)==FALSE){
    eigenvalueGraph<- eigen(get.adjacency(GRAPH,sparse=FALSE),only.values = TRUE,symmetric = TRUE)
    return(sum(abs(eigenvalueGraph$values)))
  }
}
