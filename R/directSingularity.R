#' Direct determine the sigularity spectrum of fractal set
#' @param x  a numeric vector which convert from a time series
#' @param nVec  the time scale of the detrended operation
#' @param sampleNum the random sampled number
#' @param thread the parallel thread number
#' @param qVec the qth order
#' @return the holder exponent and singularity spectrum

directSingularity=function(x,nVec,sampleNum,thread,qVec){
  unormalProbaMeasure=probaMeasure(x,nVec,sampleNum,thread)##dim=sampleNum*nVec
  registerDoMC(thread)
  partitionFunction=foreach(q=qVec,.combine = cbind)%dopar%{
    sapply(unormalProbaMeasure, function(x){sum(abs(x)^q)})
  }##dim=nVec*nVeq
  partitionFunction=sweep(partitionFunction,1,length(x)/(sampleNum*nVec),'*')##dim=nVec*nVeq
  
  #######direct alpha regression matrix#########
  directAlpha=foreach(q=qVec,.combine = cbind)%dopar%{
    normalProbaMeasure=sweep(unormalProbaMeasure^q,2,partitionFunction[,which(qVec%in%q)],'/')##dim=sampleNum*nVec
    alphaScale=sapply(normalProbaMeasure*log(unormalProbaMeasure), function(x){sum(x)})
    return(alphaScale)
  }##dim=nVec*qVec
  colnames(directAlpha)<- paste0("q",qVec)
  directAlpha=cbind(nVec,directAlpha)
  ## regression 
  alpha_q=foreach(i=c(2:dim(directAlpha)[2]),.combine = c)%dopar%{
    alphaLM=lm(log(directAlpha[,i])~log(directAlpha[,1]))
    return(as.numeric(alphaLM$coefficients[2]))
  }
  
  #######direct f_alpha regression matrix#######
  directF_Alpha=foreach(q=qVec,.combine = cbind)%dopar%{
    normalProbaMeasure=sweep(unormalProbaMeasure^q,2,partitionFunction[,which(qVec%in%q)],'/')##dim=sampleNum*nVec
    fAlphaScale=sapply(normalProbaMeasure*log(normalProbaMeasure), function(x){sum(x)})
    return(fAlphaScale)
  }##dim=nVec*qVec
  colnames(directF_Alpha)<- paste0("q",qVec)
  directF_Alpha=cbind(nVec,directF_Alpha)
  ## regression
  f_alpha_q=foreach(i=c(2:dim(directF_Alpha)[2]),.combine = c)%dopar%{
    f_alphaLM=lm(log(directF_Alpha[,i])~log(directF_Alpha[,1]))
    return(as.numeric(f_alphaLM$coefficients[2]))
  }
  
  ## the final output
  return(cbind(alpha_q,f_alpha_q))
}



