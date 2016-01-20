#' Compute the triplets to evaluate the complexity of the fractal spectrum
#' @param h_q the generalized hurst exponent
#' @param qVec the order of q
#' @param tol the tolerence of splinfun used to determine the x_min and x_max in the fractal spectrum
#' @return A vector with three elements
#' @export
triplePredict<- function(h_q,qVec,tol=1e-6){
  triple=c(0,0,0)
  names(triple)<- c("alpha0","W","r")
  LT=legendreTransform(h_q,qVec)
  f_LT=splinefun(LT[,1],LT[,2],"natural")
  x_index=seq(0,2,tol)
  triple["alpha0"]=x_index[which(f_LT(x_index,deriv = 1)<=0)%>>%min]
  x_min=x_index[min(which(f_LT(x_index)>=0))]
  x_max=x_index[max(which(f_LT(x_index)>=0))]
  triple["W"]=x_max - x_min
  triple["r"]=(x_max- triple["alpha0"])/(triple["alpha0"] - x_min)
  return(triple)
}