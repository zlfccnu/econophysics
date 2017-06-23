#' TAP approximation of copuling strength reconstruction for ising model
#' @param retruns the return data.frame for stock market OR the configuation matrix for other system
#' @return a interaction strength J matrix 
#' @export
J_TAP <- function(returns) {
  covMat=cov(returns)
  diag(covMat)=0
  site_mean=colSums(returns)
  site_mean_Mat=site_mean%*%t(site_mean)
  J_TAP_mat=(-2*solve(covMat))/(1+sqrt(1-8*(solve(covMat)*site_mean_Mat)))
  diag(J_TAP_mat)=0
  return(J_TAP_mat)
}