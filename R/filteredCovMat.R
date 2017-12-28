#' Function used to filter the covariance matrix of a series of return time series
#' @param returns the N*L dimensional matrix with return time series for each stock per column
#' @param bulk whether use the bulk eigenvalues to construct the correlation matrix or not
#' @return a list with corMat and covMat
#' @references Random matrix approach to cross correlations in financial data
filteredCovMat<- function(returns,bulk=FALSE){
  standardlizing= function(x){
    x=(x-mean(x))/sd(x)
    return(x)
  }
  ## returns should be adjusted to N*L dimension
  if(dim(returns)[1]>= dim(returns)[2]){
    returns<- t(returns)
  }
  dimReturns<- dim(returns)
  sdReturns<- apply(returns,1,sd)
  standardReturns<- t(apply(returns,1, standardlizing))
  corMat<- (1/dimReturns[2])*standardReturns%*%t(standardReturns)
  eigenCorMat<- eigen(corMat)
  D<- diag(sdReturns)
  lambdaMax<- 1+dimReturns[1]/dimReturns[2]+2*sqrt(dimReturns[1]/dimReturns[2])
  if(bulk==FALSE){## using the deviating eigenvalues
    lambdaDiag<- diag(c(eigenCorMat$values[eigenCorMat$values>= lambdaMax],rep(0,sum(eigenCorMat$values< lambdaMax))))
  }else{## using the bulk eigenvalues
    lambdaDiag<- diag(c(rep(0,sum(eigenCorMat$values>=lambdaMax)),eigenCorMat$values[eigenCorMat$values<lambdaMax]))
  }
  corMat<- eigenCorMat$vectors%*%lambdaDiag%*%solve(eigenCorMat$vectors)
  diag(corMat)<- 1
  covMat<- D%*%as.matrix(corMat)%*%D
  return(list(covMat,corMat))
}
