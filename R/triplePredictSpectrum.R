#' Compute the triplets to evaluate the complexity of the fractal spectrum
#' @param Alpha the holder exponent
#' @param fAlpha the probability density of the holder exponent
#' @param tol the tolerence used to determine the x_min and x_max in the fractal spectrum
#' @return A vector with three elements
#' @export
triplePredictSpectrum=function(Alpha,fAlpha,tol=1e-6){
  triple=c(0,0,0)
  names(triple)<- c("alpha0","W","r")
  Alpha=as.numeric(Alpha)
  fAlpha=as.numeric(fAlpha)
  LT=cbind(Alpha,fAlpha)
  LT=as.data.frame(LT)
  colnames(LT)= c("x","y")
  triple["alpha0"]=LT[,1][floor(dim(LT)[1]/2)+1]
  fitEq=function(x,a,b,c,d,e){
    a+b*(x- triple["alpha0"])+c*(x- triple["alpha0"])^2+d*(x- triple["alpha0"])^3+e*(x- triple["alpha0"])^4
  }
  f_LT=nls(y~fitEq(x,a,b,c,d,e),data = LT,start = list(a=0.1,b=0.1,c=0.1,d=0.1,e=0.1))
  x_index=seq(0,2,tol)
  x_min=x_index[min(which(predict(f_LT,list(x=x_index))>=0))]
  x_max=x_index[max(which(predict(f_LT,list(x=x_index))>=0))]
  triple["W"]=x_max - x_min
  triple["r"]=(x_max- triple["alpha0"])/(triple["alpha0"] - x_min)
  return(triple)
}