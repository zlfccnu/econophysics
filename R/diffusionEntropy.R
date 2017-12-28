#' use the difussion entropy method to calculate the Hurst exponent
#' @param x the time series
#' @param nVec the time scale
#' @param thread the multi threads
#' @param binSizeRatio the boxsize, fraction of standard deviation
#' @return a data.frame
diffusionEntropy=function(x,nVec=2^(3:8),thread=3,binSizeRatio=0.5){
  sgma=binSizeRatio*sd(x)
  N=length(x)
  registerDoMC(thread)
  nVec=unique(floor(nVec))
  segmentX=foreach(s=nVec)%dopar%{
      rollapply(x,width=s,by=1,FUN=sum)
  }
  freq=lapply(segmentX,FUN = function(x){hist(x,breaks = ceiling((max(x)-min(x))/sgma),plot=FALSE)$counts})
  
  p=freq
  for(i in 1:length(p)){
    p[[i]]=p[[i]]/(N-nVec[i]+1)
  }
  entropy=sapply(p,FUN = function(x){-sum(x[x!=0]*log(x[x!=0]))})
  
  
  for(i in 1:length(freq)){
    freq[[i]]=cbind(freq[[i]],sapply(freq[[i]],FUN = function(x){sum(1/((x+2):(N- nVec[i]+3)))}))
  }
  balancedEntropy=sapply(freq,FUN = function(x){sum((x[,1]+1)*x[,2])})
  balancedEntropy=(1/(N- nVec+3))*balancedEntropy
  
  results=cbind(log(nVec),entropy,balancedEntropy)
  H_DE=coefficients(lm(results[,2]~results[,1]))[2]
  H_BEDE=coefficients(lm(results[,3]~results[,1]))[2]
  return(list(H_DE,H_BEDE,as.data.frame(results)))
}


