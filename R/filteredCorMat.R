#' Function used to filter the covariance matrix of a series of return time series
#' @param corMat the correlation matrix
#' @param eigenNum the number of eigevalues be removed
#' @return a martix
#' @export
#' @references Random matrix approach to cross correlations in financial data
filteredCorMat<- function(corMat,eigenNum=1){
  
  eigenCorMat<- eigen(corMat)

    lambdaDiag<- diag(c(rep(0,eigenNum),eigenCorMat$values[-(1:eigenNum)]))
 
  corMat<- eigenCorMat$vectors%*%lambdaDiag%*%solve(eigenCorMat$vectors)
  diag(corMat)<- 1
  return(corMat)
}
