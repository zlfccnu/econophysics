#' Function used to calculate the Jensen-Shannon between two graphs, i.e, the distance between eigenvalue distributions of laplacian matrix of graphs
#' @param GRAPH1 graph object of igraph, undirected
#' @param GRAPH2 graph object of igraph , undirected
#' @param normalized compute the nornalized laplacian or not
#' @param n the number of equally spaced points at which the density is to be estimated
#' @param bw the bandwidth of the density function
#' @return a numeric value between 0 and 1
#' @export
JS_Distance<- function(GRAPH1,GRAPH2,normalized=TRUE,n=512,bw=0.01){
  Laplacian1<- graph.laplacian(GRAPH1,normalized = normalized)
  Laplacian2<- graph.laplacian(GRAPH2,normalized = normalized)
  registerDoMC(2)
  eigenValues<- foreach(Laplacian=list(Laplacian1,Laplacian2))%dopar%{
    eigen(Laplacian,only.values = TRUE)$values
  }
  p1<- density(eigenValues[[1]],n = n,bw=bw)$y
  p2<- density(eigenValues[[2]],n = n,bw=bw)$y
  p1<- p1/sum(p1)
  p2<- p2/sum(p2)
  p<- 0.5*(p1+p2)
  
  JS_D<- 0.5*sum(p1*log(p1/p))+0.5*sum(p2*log(p2/p))
  return(sqrt(JS_D))
}
