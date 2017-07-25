#' naive mean field external field reconstruction for ising model
#' @param returns the return data.frame for stock market OR the configuation matrix for other system
#' @param J_nMF a interaction strength J matrix  
#' @return a vector of external field for every site
#' @export
h_nMF=function(returns,J_nMF){
  site_mean=colMeans(returns)
  h_nMF_Mat=atanh(site_mean) - colSums(J_nMF*site_mean)
  return(h_nMF_Mat)
}