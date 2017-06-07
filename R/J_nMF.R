#' naive mean field copuling strength reconstruction for ising model
#' @param retruns the return data.frame for stock market OR the configuation matrix for other system
#' @return a interaction strength J matrix 
#' @export
J_nMF=function(retruns){
  covMat=cov(returns)
  diag(covMat)=0
  site_mean=colSums(returns)
  J_nMF_mat=solve(diag(1- site_mean^2)) - solve(covMat)
  return(J_nMF_mat)
}
