#'Function used to calculate the square DFA fluctuation
#'@param covariance the covariance matrix
#'@param returns argument should be a m x n matrix with one column per security
#'@param max.allocation is the maximum % allowed for any one security (reduces concentration)
#'@param risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
#'@param risk.increment is the increment (by) value used in the for loop
#'@param thread the multithreads argument
#'@return a data.frame
#'@export

eff.frontier <- function (covariance,returns, short="no", max.allocation=NULL, risk.premium.up=20, risk.increment=.05,thread=3){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short selling prohibited)
  # max.allocation is the maximum % allowed for any one security (reduces concentration)
  # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
  # risk.increment is the increment (by) value used in the for loop
  
  #covariance <- cov(returns)
  # print(covariance)
  n <- ncol(covariance)## the covariance is determined
  
  # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  registerDoMC(thread)
  eff<- foreach(i=seq(from=0, to=risk.premium.up, by=risk.increment), .combine='rbind')%dopar%{
    dvec <- colMeans(returns) * i # This moves the solution up along the efficient frontier
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    results<- c(sol$solution,i,sqrt(sum(sol$solution *colSums((covariance * sol$solution)))),as.numeric(sol$solution %*% colMeans(returns)),as.numeric(sol$solution %*% colMeans(returns))/sqrt(sum(sol$solution *colSums((covariance * sol$solution)))))
    names(results)<- c(colnames(returns),"riskAversion","Std.Dev","Exp.Return","sharpe")
    return(results)
  }
  rownames(eff)<- NULL
  return(as.data.frame(eff))
}