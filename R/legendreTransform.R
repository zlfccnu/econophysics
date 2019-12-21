#' Use the legendre transformation to calculate the lengendre spectrum from the generalized Hurst exponent
#' @param h_q the generalized Hurst exponent
#' @param qVec the fluctuation order
#' @param D_f the dimension of the support, default is 1 for time sereis
#' @return a data.frme with alpha(q) and f(alpha)
#' @export
legendreTransform<- function(h_q,qVec,D_f = 1){
  f_hq=splinefun(qVec,h_q)
  alpha_q = h_q+qVec*f_hq(qVec,deriv=1)
  legendreSpectrum=as.data.frame(cbind(alpha_q,qVec*(alpha_q-h_q) + D_f))
  colnames(legendreSpectrum)<- c("alpha(q)","f(alpha)")
  return(legendreSpectrum)
}