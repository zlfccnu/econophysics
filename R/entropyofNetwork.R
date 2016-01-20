#'Function used to calculate the entropy of a graph
#'@param GRAPH a complex network, must be connected
#'@return A numeric value entropy
#'@export
funcEntropyofNetwork <- function(GRAPH){
  combineLaplacianMat = graph.laplacian(GRAPH)/(2*ecount(GRAPH))
  graphEigen=eigen(combineLaplacianMat,only.values = TRUE)$values
  graphEigen=graphEigen[which(graphEigen>0)]
  -sum(graphEigen*log2(graphEigen))
}