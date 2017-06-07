#' Function used to calculate the averge degree of a graph
#' @param GRAPH A igraph object
#' @return A numeric value
#' @export
averageDegree = function( GRAPH ) {
  mean( degree( GRAPH ) )
}