#' TAP approximation external field reconstruction for ising model
#' @param retruns the return data.frame for stock market OR the configuation matrix for other system
#' @param J_TAP a interaction strength J matrix  
#' @return a vector of external field for every site
#' @export
h_TAP=function(returns,J_TAP) {
  site_mean=colSums(returns)
  J_nMF_Mat=J_nMF(retruns = retruns)
  h_nMF_Mat=h_nMF(returns = returns,J_nMF = J_nMF_Mat)
  return(h_nMF_Mat - site_mean*colSums((J_TAP)^2*(1- site_mean^2)))
}