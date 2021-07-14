#' Function used to filter the covariance matrix of a series of return time series
#' @param returns the return matrix
#' @return a list with four matrices: original,market,structure and random
#' @references Random matrix approach to cross correlations in financial data
#' @export
filteredCorMat<- function(returns){
  if(dim(returns)[1]<dim(returns)[2]){
    stop("time series is too short!")
  }
  corMat=cor(returns)
  eigenBound=eigenBoundRMT(L=dim(returns)[1],N=dim(returns)[2],S=1)
  eigenCorMat=eigen(corMat)
  corMatMarket=eigenCorMat$values[1]*eigenCorMat$vectors[,1]%*%t(eigenCorMat$vectors[,1])
  structureIndex=which(eigenCorMat$values>=eigenBound[2])[-1]
  randomIndex=which(eigenCorMat$values<eigenBound[2])
  corMatStructure=eigenCorMat$vectors%*%diag(c(0,eigenCorMat$values[structureIndex],rep(0,dim(returns)[2]-length(structureIndex)-1)))%*%t(eigenCorMat$vectors)
  corMatRandom=eigenCorMat$vectors%*%diag(c(rep(0,dim(returns)[2]-length(randomIndex)),eigenCorMat$values[randomIndex]))%*%t(eigenCorMat$vectors)
  corMatRemoveMarket=eigenCorMat$values[-1]*eigenCorMat$vectors[,-1]%*%t(eigenCorMat$vectors[,-1])
 results=list(corMat,corMatMarket,corMatStructure,corMatRandom,corMatRemoveMarket)
 names(results)=c("corMat","corMatMarket","corMatStructure","corMatRandom","corMatRemoveMarket")
  return(results)
}
