#' naive mean field copuling strength reconstruction for ising model
#' @param returns the return data.frame for stock market OR the configuation matrix for other system
#' @return a interaction strength J matrix 

J_nMF=function(returns){
  covMat=cov(returns)
  site_mean=colMeans(returns)
  J_nMF_mat=solve(diag(1- site_mean^2)) - solve(covMat)
  return(J_nMF_mat)
}
