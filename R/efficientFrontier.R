#' Function used to find the efficient frontier by using the modern portofilio theory
#' @param covMat the covariance matrix
#' @param returns argument should be a m x n matrix with one column per security
#' @param max.allocation is the maximum % allowed for any one security- reduces concentration
#' @param risk.premium.up is the upper limit of the risk premium modeled
#' @param risk.increment is the increment value used in the for loop
#' @param thread the multithreads argument
#' @return a data.frame
#' @export

eff.frontier <- function (returns, covMat=NULL, short="no", max.allocation=NULL, risk.premium.up=20,thread=3){
  if(is.null(covMat)){
    covMat=cov(returns)
  }
  n <- ncol(covMat)## the covMat is determined
  
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
  riskSeq=exp(seq(0.01,log(risk.premium.up),0.1))
  eff<- foreach(i=riskSeq, .combine='rbind')%dopar%{
    dvec <- colMeans(returns) * i # This moves the solution up along the efficient frontier
    sol <- solve.QP(covMat, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    sol$solution[abs(sol$solution)<= 1e-7]<- 0
    W=as.matrix(as.numeric(sol$solution),nrow=n)
    Std.Dev=sqrt(t(W)%*%cov(returns)%*%W)[1,1]
    Exp.Return=(t(W)%*%as.matrix(colMeans(returns)))[1,1]
    results=c(sol$solution,i,Std.Dev,Exp.Return)
    names(results)<- c(colnames(returns),"riskAversion","Std.Dev","Exp.Return")
    return(results)
  }
  rownames(eff)<- NULL
  return(as.data.frame(eff))
}