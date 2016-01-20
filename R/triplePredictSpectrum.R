triplePredictSpectrum=function(Alpha,fAlpha,tol=1e-6){
  triple=c(0,0,0)
  names(triple)<- c("alpha0","W","r")
  Alpha=as.numeric(Alpha)
  fAlpha=as.numeric(fAlpha)
  LT=cbind(Alpha,fAlpha)
  f_LT=splinefun(LT[,1],LT[,2],"natural")
  x_index=seq(min(LT[,1])-0.2,max(LT[,1])+0.2,tol)
  triple["alpha0"]=x_index[which.max(f_LT(x_index))]
  x_min=x_index[min(which(f_LT(x_index)>=0))]
  x_max=x_index[max(which(f_LT(x_index)>=0))]
  print(x_max)
  print(x_min)
  triple["W"]=x_max - x_min
  triple["r"]=(x_max- triple["alpha0"])/(triple["alpha0"] - x_min)
  return(triple)
}