#' Compute the triplets to evaluate the complexity of the fractal spectrum
#' @param h_q the generalized hurst exponent
#' @param qVec the order of q
#' @param tol the tolerence of splinfun used to determine the x_min and x_max in the fractal spectrum
#' @return A vector with three elements
#' @export
triplePredict<- function(h_q,qVec,tol=1e-4){
  triple=c(0,0,0,0)
  names(triple)<- c("alpha0","W","r","A")
  LT=legendreTransform(h_q,qVec)
  colnames(LT)= c("x","y")
  triple["alpha0"]=LT[,1][floor(dim(LT)[1]/2)+1]
  x=LT[,"x"]
  y=LT[,"y"]
  f_LT=lm(y~poly(x,4,raw = TRUE))
  x_index=seq(triple["alpha0"]-0.3,triple["alpha0"]+0.4,tol)
  x_min=x_index[min(which(predict(f_LT,data.frame(x=x_index))>=0))]
  x_max=x_index[max(which(predict(f_LT,data.frame(x=x_index))>=0))]
  triple["W"]=x_max - x_min
  triple["r"]=(x_max- triple["alpha0"])/(triple["alpha0"] - x_min)
  triple['A']=(2*triple["alpha0"]-max(x)-min(x))/(max(x)-min(x))
  return(triple)
}
