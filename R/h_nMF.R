#' naive mean field external field reconstruction for ising model
#' @param retruns the return data.frame for stock market OR the configuation matrix for other system
#' @param J_nMF a interaction strength J matrix  
#' @return a vector of external field for every site
#' @export
h_nMF=function(returns,J_nMF){
  site_mean=colSums(returns)
  h_nMF=1/tanh(site_mean) - colSums(J_nMF)*site_mean
  return(h_nMF)
}