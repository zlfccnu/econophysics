#' calculate the spillover relationship between two time series
#' @param r1 a time series
#' @param r2 a time series
#' @param VaR1 the VaR of r1, calculate by the caviarOptim function
#' @param VaR2 the VaR of r2
#' @param M the lag parameter use to calculate the spillover 
#' @param BETA the significant threshold for the causality test
#' @param weighted logic value, default TRUE, return the spillover value or not
#' @return a vector with two element, 1 or 0 if weighted=FALSE, otherwise return the spillover value
#' @export
spillover=function(r1,r2,VaR1,VaR2,M,BETA,weighted=TRUE){
  k_z=function(x){
    y=pi*x
    return(sin(y)/y)
  }
  Q_M=function(Z1,Z2,M){
    T=length(Z1)
    rho_j=ccf(Z1,Z2,lag.max = T-1,plot = FALSE)## the ccf with 2*(T-1) length, the first T-1 elements are the effect of Z1 to Z2, the second T-1 elements are the effect of Z2 to Z1
    rho_j=rev(rho_j$acf[1:(T-1)])## Z1 leads Z2
    C_T=sum((1-(1:(T-1))/T)*k_z((1:(T-1))/M)^2)
    D_T=2*sum((1-(1:(T-1))/T)*(1-((2:T)/T))*k_z((1:(T-1))/M)^4)
    Q_M=(T*sum(k_z((1:(T-1))/M)^2*rho_j^2)-C_T)/sqrt(D_T)
    return(Q_M)
  }
  indicatorFun=function(r,VaR){
    ifelse(r<(-VaR),1,0)
  }
  z1=indicatorFun(r = r1,VaR = VaR1)
  z2=indicatorFun(r = r2,VaR = VaR2)
  s1_2=Q_M(z1,z2,M=M)
  s2_1=Q_M(z2,z1,M=M)
  if(isTRUE(weighted)){
    s2_1=ifelse(s2_1>qnorm(1 - BETA),s2_1,0)
    s1_2=ifelse(s1_2>qnorm(1 - BETA),s1_2,0)
  }else{
    s2_1=ifelse(s2_1>qnorm(1 - BETA),1,0)
    s1_2=ifelse(s1_2>qnorm(1 - BETA),1,0)
  }
  return(c(s1_2=s1_2,s2_1=s2_1))
}