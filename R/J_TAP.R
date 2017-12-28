#' TAP approximation of copuling strength reconstruction for ising model
#' @param returns the return data.frame for stock market OR the configuation matrix for other system
#' @return a interaction strength J matrix 

J_TAP <- function(returns) {
  covMat=cov(returns)
  site_mean=colMeans(returns)
  site_mean_Mat=site_mean%*%t(site_mean)
  J_TAP_mat=(-2*solve(covMat))/(1+sqrt(1-8*(solve(covMat)*site_mean_Mat)))
  return(J_TAP_mat)
}