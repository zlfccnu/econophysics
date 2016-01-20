#' Function used to calculate the averge degree of a graph
#' @param GRAPH A igraph object
#' @export
#' @return A numeric value
AverageDegree = function( GRAPH ) {
  mean( degree( GRAPH ) )
}