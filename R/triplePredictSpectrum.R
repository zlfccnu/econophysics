#' Compute the triplets to evaluate the complexity of the fractal spectrum
#' @param Alpha the holder exponent
#' @param fAlpha the probability density of the holder exponent
#' @param tol the tolerence used to determine the x_min and x_max in the fractal spectrum
#' @return A vector with three elements
#' @export
triplePredictSpectrum=function(Alpha,fAlpha,tol=1e-4){
  triple=c(0,0,0,0)
  names(triple)<- c("alpha0","W","r","A")
  Alpha=as.numeric(Alpha)
  fAlpha=as.numeric(fAlpha)
  LT=cbind(Alpha,fAlpha)
  LT=as.data.frame(LT)
  colnames(LT)= c("x","y")
  triple["alpha0"]=LT[,1][floor(dim(LT)[1]/2)+1]
  x=LT[,"x"]
  y=LT[,"y"]
  f_LT=lm(y~poly(x,4))
  x_index=seq(0,2,tol)
  x_min=x_index[min(which(predict(f_LT,data.frame(x=x_index))>=0))]
  x_max=x_index[max(which(predict(f_LT,data.frame(x=x_index))>=0))]
  triple["W"]=x_max - x_min
  triple["r"]=(x_max- triple["alpha0"])/(triple["alpha0"] - x_min)
  triple['A']=(2*triple["alpha0"]-max(Alpha)-min(Alpha))/(max(Alpha)-min(Alpha))
  return(triple)
}